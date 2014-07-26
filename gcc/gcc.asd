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
               (:file "compiler" :depends-on ("package"))
               (:file "util" :depends-on ("package"))
               (:file "l-man-package")))
