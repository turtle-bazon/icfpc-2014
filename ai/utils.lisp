
(in-package :ai)

(defun draw-map (map &optional custom-writer)
  (iter (with (width . height) = (map-size map))
        (for y from 0 below height)
        (iter (for x from 0 below width)
              (for user-obj = (when custom-writer (funcall custom-writer x y)))
              (for obj = (or user-obj
                             (ecase (map-cell (cons x y) map)
                                 (0 #\#)
                                 (1 #\Space)
                                 (2 #\.)
                                 (3 #\o)
                                 (4 #\%)
                                 (5 #\\)
                                 (6 #\=))))
              (format t "~c" obj))
        (format t "~%")))

(defun modify-map (map modificator)
  (il-reverse
   (car (il-foldl (lambda (row acc)
                    (let ((y (cdr acc)))
                      (cons (cons (il-reverse
                                   (car (il-foldl (lambda (cell acc)
                                                    (let ((x (cdr acc)))
                                                      (cons (cons (funcall modificator cell x y)
                                                                  (car acc))
                                                            (+ x 1))))
                                                  (cons 0 0)
                                                  row)))
                                  (car acc))
                            (+ y 1))))
                  (cons 0 0)
                  map))))

(defun run-simulator (initial-world-state &key (max-turns 10000))
  (declare (optimize (debug 3)))
  (let ((game (gcc-init initial-world-state nil)))
    (iter (with world-state = initial-world-state)
          (with ai-state = (car game))
          (with step = (cdr game))
          (for (map (lv (lx . ly) ld ll . ls) ghosts . fruit) = world-state)
          (draw-map (car world-state) (lambda (x y)
                                        (if (and (= x lx) (= y ly))
                                            #\X
                                            (il-foldl (lambda (coord v)
                                                        (if (and (= x (car coord)) (= y (cdr coord))) #\@ v))
                                                      nil
                                                      (ghosts-coords ghosts)))))
          (for counter from 0 below max-turns)
          (for (new-ai-state . direction) = (funcall step ai-state world-state))
          (setf ai-state new-ai-state)
          (for (dlx . dly) = (case direction (0 (cons 0 -1)) (1 (cons 1 0)) (2 (cons 0 1)) (3 (cons -1 0))))
          (break "Moving direction ~a as [~a, ~a] -> [~a, ~a] where ~a"
                 direction lx ly (+ lx dlx) (+ ly dly) (map-cell (cons (+ lx dlx) (+ ly dly)) map))
          (for new-world-state =
               (cons (modify-map map (lambda (cell x y) (if (and (= x (+ lx dlx)) (= y (+ ly dly))) 1 cell)))
                     (cons (cons lv (cons (cons (+ lx dlx) (+ ly dly)) (cons ld (cons ll ls))))
                           (cddr world-state))))
          (setf world-state new-world-state)
          (finally (draw-map (car world-state))))))

          
