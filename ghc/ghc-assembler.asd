;;; -*- General Compute Coprocessor -*-

(defpackage #:ghc-assembler-asd
  (:use :cl :asdf))

(in-package #:ghc-assembler-asd)

(defsystem ghc-assembler
  :name "ghc-assembler"
  :version "0.1"
  :author "skobochka"
  :depends-on (:iterate :optima :split-sequence)
  :components ((:file "package")
               (:file "assembler" :depends-on ("package"))))
