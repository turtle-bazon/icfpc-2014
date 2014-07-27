(cl:defpackage #:ilisp-bumper-ai
  (:use :il))

(cl:in-package :ilisp-bumper-ai)

(defun il-nth (n lst)
  (if (= n 0) (car lst) (il-nth (- n 1) (cdr lst))))

(ilisp.impl:build-ai-core '(il-nth 1 (cons 1 (cons 2 0))))

