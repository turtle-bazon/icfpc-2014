(cl:defpackage #:ilisp-bumper-ai
  (:use :il))

(cl:in-package :ilisp-bumper-ai)

(defun il-nth (n lst)
  (if (= n 0) (car lst) (il-nth (- n 1) (cdr lst))))

(defun step (ai-state world-state)
  (cons ai-state 3))

(defun init (world-initial-state ghosts-statuses)
  (cons (cons 0 0) step))

(ilisp.impl:build-ai-core '(init initial-state unknown))

