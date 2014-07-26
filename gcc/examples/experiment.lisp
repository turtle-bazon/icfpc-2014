(lambda (initial-state unknown)
  (letrec ((nth (lambda (n lst) (if (= n 0) (car lst) (nth (- n 1) (cdr lst)))))
           (step (lambda (ai-state world-state)
                   (let ((call-with-world (lambda (map lman ghosts fruits)
                                            (let ((at-coords (lambda (x y)
                                                               (if (= x (car ai-state))
                                                                   (if (= y (cdr ai-state))
                                                                       0
                                                                       (nth x (nth y map)))
                                                                   (nth x (nth y map))))))
                                              (let ((lman-env (lambda (up rg dn lf)
                                                                (let ((decide (lambda (direction)
                                                                                (cons (cons (car lman) (cdr lman)) direction))))
                                                                  (if (= lf 0)
                                                                      (if (= up 0)
                                                                          (if (= rg 0)
                                                                              (if (= dn 0)
                                                                                    (decide :left)
                                                                                    (decide :down))
                                                                              (decide :rigth))
                                                                          (decide :up))
                                                                      (decide :left))))))
                                                (lman-env (at-coords (car lman) (- (cdr lman) 1))
                                                          (at-coords (+ (car lman) 1) (cdr lman))
                                                          (at-coords (car lman) (+ (cdr lman) 1))
                                                          (at-coords (- (car lman) 1) (cdr lman))))))))
                     (call-with-world (car world-state)
                                      (cadr (cadr world-state))
                                      (cadr (cdr world-state))
                                      (cdr (cdr (cdr world-state))))))))
    (let ((init (lambda (initial-state unknown)
                  (cons (cons 0 0) step))))
      (init initial-state unknown))))
