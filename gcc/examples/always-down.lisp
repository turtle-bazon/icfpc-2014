(define (main initial-state undocumented)
  (define (step-function ai-state world-state)
    (cons (+ ai-state 1) :down))
  (cons 42 step-function))

