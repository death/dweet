;;;; +----------------------------------------------------------------+
;;;; | Dweet.io client                                                |
;;;; +----------------------------------------------------------------+

(defpackage #:dweet
  (:use #:cl)
  (:shadow #:listen)
  (:import-from #:drakma #:http-request)
  (:import-from #:com.gigamonkeys.json #:parse-json #:json)
  (:import-from #:babel #:octets-to-string)
  (:export
   #:*base-uri*
   #:*thing*
   #:http-error
   #:http-error-body
   #:http-error-status
   #:http-error-headers
   #:http-error-uri
   #:http-error-reason
   #:with-thing
   #:post
   #:latest
   #:all
   #:listen)
  (:documentation "A dweet.io client for Common Lisp"))

(in-package #:dweet)

(defvar *base-uri*
  "https://dweet.io/"
  "Base URI for the dweet service.")

(defvar *thing*
  "lisp"
  "Name of the thing dweeting.")

(define-condition http-error (error)
  ((body :initarg :body :reader http-error-body)
   (status :initarg :status :reader http-error-status)
   (headers :initarg :headers :reader http-error-headers)
   (uri :initarg :uri :reader http-error-uri)
   (reason :initarg :reason :reader http-error-reason))
  (:report print-http-error)
  (:documentation "Represents a dweet HTTP error."))

(defun print-http-error (error stream)
  "Print a human-friendly HTTP error message to the stream."
  (format stream "Dweet server returned status ~A: ~A."
          (http-error-status error)
          (http-error-reason error)))

(defun request (method path &key content stream-receiver)
  "Send a dweet request of some kind.  METHOD should be either :GET
or :POST.  PATH is the relative path (sans leading slash) to call.  If
posting, CONTENT should be a Lisp representation of a JSON object.  If
listening, STREAM-RECEIVER should be a function to call with the HTTP
stream."
  (multiple-value-bind (body status headers uri stream should-close reason)
      (http-request (concatenate 'string *base-uri* path)
                    :method method
                    :content (when content (json content))
                    :content-type "application/json"
                    :force-binary t
                    :want-stream (if stream-receiver t nil))
    (case status
      (200
       (cond (stream-receiver
              (unwind-protect
                   (funcall stream-receiver body)
                (when (and should-close (open-stream-p body))
                  (close body))))
             (t
              (let ((this nil)
                    (with nil)
                    (because nil)
                    (body-json (json-from-octets body)))
                (loop for (key value) on body-json by #'cddr
                      do (cond ((equal key "this") (setf this value))
                               ((equal key "with") (setf with value))
                               ((equal key "because") (setf because value))))
                (unless (equal this "succeeded")
                  (error 'http-error
                         :body body
                         :status with
                         :headers headers
                         :uri uri
                         :reason because))
                with))))
      (t
       (error 'http-error
              :body body
              :status status
              :headers headers
              :uri uri
              :reason reason)))))

(defmacro with-thing (name &body forms)
  "Evaluate FORMS in the context of a dweet thing named by NAME."
  `(let ((*thing* ,name))
     ,@forms))

(defmacro ignoring-status ((status &optional return-value) &body forms)
  "Evaluate FORMS while \"ignoring\" dweet HTTP errors with status
equal to the evaluation of STATUS, i.e. returning the evaluation of
RETURN-VALUE when such errors are encountered."
  (let ((exit (gensym))
        (err (gensym)))
  `(block ,exit
     (handler-bind ((http-error
                     (lambda (,err)
                       (when (= (http-error-status ,err) ,status)
                         (return-from ,exit ,return-value)))))
       ,@forms))))

(defun post (&rest plist)
  "Post a dweet with supplied keys and values.  The plist is
interpreted as a Lisp representation for a JSON object.  The first
parameter may be a thing's name, in which case the plist comes
afterwards."
  (multiple-value-bind (*thing* plist)
      (if (oddp (length plist))
          (values (first plist) (rest plist))
          (values *thing* plist))
    (request :post (format nil "dweet/for/~A" *thing*) :content plist)))

(defun latest (&optional (*thing* *thing*))
  "Fetch and return the latest dweet, or return NIL if none found."
  (ignoring-status (404)
    (aref (request :get (format nil "get/latest/dweet/for/~A" *thing*)) 0)))

(defun all (&optional (*thing* *thing*))
  "Fetch and return a vector of all dweets on server.  Will return an
empty vector if none found."
  (ignoring-status (404 #())
    (request :get (format nil "get/dweets/for/~A" *thing*))))

(defun listen (handler &optional (*thing* *thing*))
  "Listen for dweets coming from the server, calling the HANDLER
function with them as they arrive."
  (request :get (format nil "listen/for/dweets/from/~A" *thing*)
           :stream-receiver (make-listener handler)))

(defun make-listener (handler)
  "Return a stream receiver function that parses dweet chunks and
calls the HANDLER function with them."
  (lambda (stream)
    (loop for size = (parse-integer (read-line stream) :radix 16 :junk-allowed t)
          do (let ((octets (make-array size :element-type '(unsigned-byte 8))))
               (read-sequence octets stream)
               (funcall handler (parse-json (json-from-octets octets)))
               (read-line stream)))))

(defun json-from-octets (octets)
  "Parse the supplied OCTETS vector into a Lisp representation of a
JSON object."
  (parse-json (octets-to-string octets)))
