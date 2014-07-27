;;; -*- General Compute Coprocessor -*-

(defpackage #:ilisp-asd
  (:use :cl :asdf))

(in-package #:ilisp-asd)

(defsystem ilisp
  :name "ilisp"
  :version "0.1"
  :author "skobochka"
  :depends-on (:iterate :metatilities)
  :components ((:file "package")
               (:file "generic" :depends-on ("package"))
               (:file "stdlib" :depends-on ("generic"))
               (:file "backend-gcc" :depends-on ("generic")))

  :in-order-to ((test-op (load-op ilisp-test)))
  :perform (test-op :after (op c)
                    (funcall (intern (symbol-name '#:describe) :lift)
                             (funcall (intern (symbol-name '#:run-tests) :lift)))))

(defmethod operation-done-p ((o test-op) (c (eql (find-system 'ilisp))))
  (values nil))
