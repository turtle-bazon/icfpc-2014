(define (step-function ai-state world-state)
  (cons (+ ai-state 1) :down))

(define (main initial-state undocumented)
  (cons 42 step-function))

