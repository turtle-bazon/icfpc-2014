;;; -*- General Compute Coprocessor -*-

(defpackage #:gcc-asd
  (:use :cl :asdf))

(in-package #:gcc-asd)

(defsystem gcc
  :name "gcc"
  :version "0.1"
  :author "skobochka"
  :depends-on (:iterate :metatilities)
  :components ((:file "package")
               (:file "util" :depends-on ("package"))
               (:file "frontend" :depends-on ("package" "util"))
               (:file "backend-gcc" :depends-on ("frontend"))
               (:file "backend-cl" :depends-on ("frontend"))))

