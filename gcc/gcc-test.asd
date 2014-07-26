;;; -*- General Compute Coprocessor Testsuite -*-

(defpackage #:gcc-test-asd
  (:use :cl :asdf))

(in-package #:gcc-test-asd)

(defsystem gcc-test
  :name "gcc-test"
  :version "0.1"
  :author "skobochka"
  :depends-on (:gcc :lift)
  :components ((:module "test"
                :components ((:file "package")
                             (:file "translator" :depends-on ("package"))))))


