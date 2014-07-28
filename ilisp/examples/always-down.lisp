(defun init (initial-state unknown)
  (letrec ((step (lambda (ai-state world-state)
                   (cons (+ ai-state 1) 2))))
    (cons 42 step)))
