
(in-package :ai)

;; (defparameter *world-state* (cons (load-map #p"maps/emulator-00.txt") 
;;                                   (cons (cons 0 (cons (cons 11 16) (cons 2 (cons 3 0))))
;;                                         (cons (cons (cons 0 (cons (cons 11 8) 2))
;;                                                     (cons (cons 0 (cons (cons 10 10) 2))
;;                                                           (cons (cons 0 (cons (cons 11 10) 2))
;;                                                                 (cons (cons 0 (cons (cons 12 10) 2))
;;                                                                       0))))
;;                                               0))))

(defun il-nth (n lst)
  (if (= n 0) (car lst) (il-nth (- n 1) (cdr lst))))

(defun map-cell (cell map)
  (il-nth (car cell) (il-nth (cdr cell) map)))

(defun il-foldl (proc accumulator il-list)
  (let ((next-accumulator (funcall proc (car il-list) accumulator))
        (rest (cdr il-list)))
    (if (integerp rest)
        next-accumulator
        (il-foldl proc next-accumulator rest))))

(defun il-length (il-list)
  (il-foldl (lambda (cell counter) (declare (ignore cell)) (+ counter 1)) 0 il-list))

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
                      (nearest-sq-dist (cadr acc))
                      (rest-objects (cddr acc))
                      (current-sq-dist (sq-dist my-x my-y (car object) (cdr object))))
                  (if (or (integerp nearest-object)
                          (< current-sq-dist nearest-sq-dist))
                      (cons object
                            (cons current-sq-dist
                                  (if (integerp nearest-object)
                                      rest-objects
                                      (cons nearest-object rest-objects))))
                      (cons nearest-object (cons nearest-sq-dist (cons object rest-objects))))))
              (cons 0 (cons 0 0))
              objects)))

(defun coords= (coord-a coord-b)
  (and (= (car coord-a) (car coord-b))
       (= (cdr coord-a) (cdr coord-b))))

(defun find-object (object objects)
  (if (integerp objects)
      0
      (let ((current (car objects)))
        (if (coords= current object)
            current
            (find-object object (cdr objects))))))

(defun filter-accessible (coords map visited)
  (il-foldl (lambda (coord acc)
              (if (and (> (map-cell coord map) 0)
                       (integerp (find-object coord visited)))
                  (cons coord acc)
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
      (labels ((try-moves (avail-moves)
                 (if (integerp avail-moves)
                     0
                     (let ((next-move-plan (pop-nearest-object target avail-moves)))
                       (let ((best-move (car next-move-plan))
                             (rest-moves (cddr next-move-plan)))
                         (let ((path (plan-route best-move target map (cons source rev-path))))
                           (if (integerp path)
                               (try-moves rest-moves)
                               path)))))))
        (try-moves (filter-accessible (neighbours source) map rev-path)))))

(defun choose-dir (source target)
  (let ((xs (car source)) (ys (cdr source)) (xt (car target)) (yt (cdr target)))
    (if (= ys yt)
        (if (< xs xt) 1 3)
        (if (< ys yt) 2 0))))

(defun make-ai-state (current-path)
  current-path)

(defun call-with-ai-state (ai-state proc)
  (funcall proc ai-state))

(defun choose-next-target-for (object map pacman)
  (let ((objects (locate-objects object map)))
    (if (integerp objects)
        0
        (let ((nearest-object (car (pop-nearest-object pacman objects))))
          (let ((path-to-object (cdr (plan-route pacman nearest-object map 0))))
            path-to-object)))))

(defun choose-next-target (map pacman objects-by-priority)
  (if (integerp objects-by-priority)
      0
      (let ((path (choose-next-target-for (car objects-by-priority) map pacman)))
        (if (integerp path)
            (choose-next-target map pacman (cdr objects-by-priority))
            path))))

(defun analyze-ghosts (ghosts)
  (il-foldl (lambda (ghost-info acc)
              (let ((angry-ghosts (car acc))
                    (cowardly-ghosts (cdr acc))
                    (ghost-state (car ghost-info)))
                (if (= ghost-state 0)
                    (cons (cons ghost-info angry-ghosts) cowardly-ghosts)
                    (if (= ghost-state 1)
                        (cons angry-ghosts (cons ghost-info cowardly-ghosts))
                        acc))))
            (cons 0 0)
            ghosts))

(defun ghosts-coords (ghosts)
  (il-foldl (lambda (ghost-info acc) (cons (car (cdr ghost-info)) acc)) 0 ghosts))
  
(defun nearest-ghost (pacman ghosts-coords)
  (car (pop-nearest-object pacman ghosts-coords)))

(defun game-loop (ai-state map pacman ghosts fruits)
  (call-with-ai-state
   ai-state
   (lambda (current-path)
     (if (integerp current-path)
         (game-loop (make-ai-state (choose-next-target map pacman (cons 3 (cons 2 0))))
                    map pacman ghosts fruits)
         (cons (make-ai-state (cdr current-path))
               (choose-dir pacman (car current-path)))))))

(defun gcc-step (ai-state world-state)
  (game-loop ai-state
             (car world-state)
             (car (cdr (car (cdr world-state))))
             (car (cdr (cdr world-state)))
             (cdr (cdr (cdr world-state)))))
             
(defun gcc-init (initial-world-state foreign-ghosts)
  (declare (ignore initial-world-state foreign-ghosts))
  (cons (make-ai-state 0) #'gcc-step))
              
          
    
