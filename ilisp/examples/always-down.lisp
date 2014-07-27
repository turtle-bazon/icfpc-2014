(cl:defpackage #:ilisp-always-down
  (:use :il))

(cl:in-package :ilisp-always-down)

(defun init (initial-state unknown)
  (letrec ((step (lambda (ai-state world-state)
                   (cons (+ ai-state 1) 2))))
    (cons 42 step)))


(ilisp.impl:build-ai-core 'init)

