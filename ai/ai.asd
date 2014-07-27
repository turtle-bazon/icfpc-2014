;;; -*- Lambdaman AI -*-

(defpackage #:ai-asd
  (:use :cl :asdf))

(in-package #:ai-asd)

(defsystem ai
  :name "ai"
  :version "0.1"
  :author "skobochka"
  :depends-on (:iterate :metatilities)
  :components ((:file "package")
               (:file "map" :depends-on ("package"))
               (:file "ai-cl" :depends-on ("map")))))

