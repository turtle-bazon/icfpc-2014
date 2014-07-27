(cl:defpackage #:ilisp-power-pills-hunter-ai
  (:use :il))

(cl:in-package :ilisp-power-pills-hunter-ai)

(defun il-nth (n lst)
  (if (= n 0) (car lst) (il-nth (- n 1) (cdr lst))))

(defun map-cell (cell map)
  (il-nth (car cell) (il-nth (cdr cell) map)))

(defun il-foldl (proc accumulator il-list)
  (let ((next-accumulator (proc (car il-list) accumulator))
        (rest (cdr il-list)))
    (if (integerp rest)
        next-accumulator
        (il-foldl proc next-accumulator rest))))

(defun il-length (il-list)
  (il-foldl (lambda (cell counter) (+ counter 1)) 0 il-list))

(defun il-reverse (il-list)
  (il-foldl (lambda (cell reversed) (cons cell reversed)) 0 il-list))

(defun map-size (map)
  (cons (il-length (car map)) (il-length map)))

(defun locate-objects (object map)
  (cdr (il-foldl (lambda (row acc)
                   (let ((y (car acc)) (objects (cdr acc)))
                     (cons (+ y 1)
                           (cdr (il-foldl (lambda (cell acc)
                                            (let ((x (car acc)) (objects (cdr acc)))
                                              (cons (+ x 1)
                                                    (if (= cell object)
                                                        (cons (cons x y) objects)
                                                        objects))))
                                          (cons 0 objects)
                                          row)))))
                 (cons 0 0)
                 map)))

(defun sq-dist (x-a y-a x-b y-b)
  (let ((diff-x (- x-a x-b)) (diff-y (- y-a y-b)))
    (+ (* diff-x diff-x) (* diff-y diff-y))))

(defun pop-nearest-object (cell objects)
  (let ((my-x (car cell)) (my-y (cdr cell)))
    (il-foldl (lambda (object acc)
                (let ((nearest-object (car acc))
                      (nearest-sq-dist (car (cdr acc)))
                      (rest-objects (cdr (cdr acc)))
                      (current-sq-dist (sq-dist my-x my-y (car object) (cdr object))))
                  (if (integerp nearest-object)
                      (cons object
                            (cons current-sq-dist
                                  (if (integerp nearest-object)
                                      rest-objects
                                      (cons nearest-object rest-objects))))
                      (if (> nearest-sq-dist current-sq-dist)
                          (cons object
                                (cons current-sq-dist
                                      (if (integerp nearest-object)
                                          rest-objects
                                          (cons nearest-object rest-objects))))
                          (cons nearest-object (cons nearest-sq-dist (cons object rest-objects)))))))
              (cons 0 (cons 0 0))
              objects)))

(defun coords= (coord-a coord-b)
  (if (= (car coord-a) (car coord-b))
      (if (= (cdr coord-a) (cdr coord-b))
          1
          0)
      0))
      
(defun find-object (object objects)
  (if (integerp objects)
      0
      (let ((current (car objects)))
        (if (coords= current object)
            current
            (find-object object (cdr objects))))))

(defun filter-accessible (coords map visited)
  (il-foldl (lambda (coord acc)
              (if (> (map-cell coord map) 0)
                  (if (integerp (find-object coord visited))
                      (cons coord acc)
                      acc)
                  acc))
            0
            coords))

(defun neighbours (source)
  (let ((x (car source)) (y (cdr source)))
    (cons (cons (- x 1) y)
          (cons (cons x (- y 1))
                (cons (cons (+ x 1) y)
                      (cons (cons x (+ y 1))
                            0))))))

(defun plan-route (source target map rev-path)
  (if (coords= source target)
      (il-reverse (cons target rev-path))
      (letrec ((try-moves (lambda (avail-moves)
                            (if (integerp avail-moves)
                                0
                                (let ((next-move-plan (pop-nearest-object target avail-moves)))
                                  (let ((best-move (car next-move-plan))
                                        (rest-moves (cdr (cdr next-move-plan))))
                                    (let ((path (plan-route best-move target map (cons source rev-path))))
                                      (if (integerp path)
                                          (try-moves rest-moves)
                                          path))))))))
        (try-moves (filter-accessible (neighbours source) map rev-path)))))

(defun choose-dir (source target)
  (let ((xs (car source)) (ys (cdr source)) (xt (car target)) (yt (cdr target)))
    (if (= ys yt)
        (if (>= xt xs) 1 3)
        (if (>= yt ys) 2 0))))

(defun make-ai-state (current-path power-pills)
  (cons current-path (cons power-pills 0)))

(defun game-loop (current-path power-pills map pacman ghosts fruits)
  (if (integerp current-path)
      (if (integerp power-pills)
          (make-ai-state 0 0)
          (let ((nearest-tuple (pop-nearest-object pacman power-pills)))
            (let ((nearest-power-pill (car nearest-tuple))
                  (rest-power-pills (cdr (cdr nearest-tuple))))
              (game-loop (cdr (plan-route pacman nearest-power-pill map 0))
                         rest-power-pills
                         map
                         pacman
                         ghosts
                         fruits))))
      (cons (make-ai-state (cdr current-path) power-pills)
            (choose-dir pacman (car current-path)))))

(defun gcc-step (ai-state world-state)
  (game-loop (car ai-state)
             (car (cdr ai-state))
             (car world-state)
             (car (cdr (car (cdr world-state))))
             (car (cdr (cdr world-state)))
             (cdr (cdr (cdr world-state)))))
             
(defun gcc-init (initial-world-state foreign-ghosts)
  (let ((map (car initial-world-state))
        (pacman (car (cdr (car (cdr initial-world-state))))))
    (let ((power-pills (locate-objects 3 map)))
      (let ((nearest-tuple (pop-nearest-object pacman power-pills)))
        (let ((nearest-power-pill (car nearest-tuple))
              (rest-power-pills (cdr (cdr nearest-tuple))))
          (let ((current-path (cdr (plan-route pacman nearest-power-pill map 0))))
            (cons (make-ai-state current-path rest-power-pills)
                  gcc-step)))))))

(ilisp.impl:build-ai-core 'gcc-init)

