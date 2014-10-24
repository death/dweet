;;;; +----------------------------------------------------------------+
;;;; | Dweet.io client                                                |
;;;; +----------------------------------------------------------------+

;;; System definition

;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

(asdf:defsystem #:dweet
  :description "A dweet.io client for Common Lisp"
  :author "death <github.com/death>"
  :license "MIT"
  :depends-on (#:drakma #:com.gigamonkeys.json #:babel)
  :components
  ((:file "dweet")))
