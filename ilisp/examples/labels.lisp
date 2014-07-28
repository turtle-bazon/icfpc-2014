(defun init (initial-state unknown)
  (labels ((step (ai-state world-state)
             (cons (+ ai-state 1) 1)))
    (cons 42 #'step)))
