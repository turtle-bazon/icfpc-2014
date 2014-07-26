(let ((level-0 (lambda (x y) (cons (+ x 1) (- y 1)))))
  (let ((level-1 (lambda (x y) (level-0 x y))))
    (level-1 10 100)))
