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
               (:file "util" :depends-on ("package")))

  :in-order-to ((test-op (load-op gcc-test)))
  :perform (test-op :after (op c)
                    (funcall (intern (symbol-name '#:describe) :lift)
                             (funcall (intern (symbol-name '#:run-tests) :lift)))))

(defmethod operation-done-p
    ((o test-op) (c (eql (find-system 'gcc))))
  (values nil))

