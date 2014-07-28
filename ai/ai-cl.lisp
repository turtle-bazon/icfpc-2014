
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

(defun call-with-tuple/2 (tuple proc)
  (funcall proc (car tuple) (cdr tuple)))
(defun call-with-tuple/3 (tuple proc)
  (funcall proc (car tuple) (car (cdr tuple)) (cdr (cdr tuple))))
(defun call-with-tuple/4 (tuple proc)
  (funcall proc (car tuple) (car (cdr tuple)) (car (cdr (cdr tuple))) (cdr (cdr (cdr tuple)))))

(defun locate-objects-if (test map)
  (cdr (il-foldl (lambda (row acc)
                   (let ((y (car acc)) (objects (cdr acc)))
                     (cons (+ y 1)
                           (cdr (il-foldl (lambda (cell acc)
                                            (let ((x (car acc)) (objects (cdr acc)))
                                              (cons (+ x 1)
                                                    (let ((probe (funcall test cell x y)))
                                                      (if (integerp probe) objects (cons probe objects))))))
                                          (cons 0 objects)
                                          row)))))
                 (cons 0 0)
                 map)))

(defun locate-objects (object map)
  (locate-objects-if (lambda (cell x y) (if (= cell object) (cons x y) 0)) map))

(defun sq-dist (x-a y-a x-b y-b)
  (let ((diff-x (- x-a x-b)) (diff-y (- y-a y-b)))
    (+ (* diff-x diff-x) (* diff-y diff-y))))

(defun pop-min-dist-object (dist cell objects)
  (let ((my-x (car cell)) (my-y (cdr cell)))
    (il-foldl (lambda (object acc)
                (let ((nearest-object (car acc))
                      (nearest-sq-dist (cadr acc))
                      (rest-objects (cddr acc))
                      (current-sq-dist (funcall dist my-x my-y (car object) (cdr object))))
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

(defun pop-nearest-object (cell objects)
  (pop-min-dist-object #'sq-dist cell objects))

(defun pop-farest-object (cell objects)
  (pop-min-dist-object (lambda (sx sy tx ty) (- 0 (sq-dist sx sy tx ty))) cell objects))

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
  (if (integerp coords)
      0
      (il-foldl (lambda (coord acc)
                  (if (and (> (map-cell coord map) 0)
                           (integerp (find-object coord visited)))
                      (cons coord acc)
                      acc))
                0
                coords)))

(defun neighbours (source)
  (let ((x (car source)) (y (cdr source)))
    (cons (cons (- x 1) y)
          (cons (cons x (- y 1))
                (cons (cons (+ x 1) y)
                      (cons (cons x (+ y 1))
                            0))))))

(defun get-patch (patches coord)
  (if (integerp patches)
      0
      (let ((patch (car patches)))
        (if (coords= (cdr patch) coord)
            (car patch)
            (get-patch (cdr patches) coord)))))

(defun set-patch (patches coord value)
  (cons (cons value coord) patches))

(defun patches-coords/append (patches lst)
  (il-foldl (lambda (patch acc) (cons (cdr patch) acc)) lst patches))

(defun plan-route-limit (source target map rev-path forbidden depth-limit)
  (declare (ignore rev-path)
           (optimize (debug 3)))
  (let ((zero-counters (set-patch 0 source 1)))
    (labels ((wave-spread (counters depth-limit)
               (if (= depth-limit 0)
                   counters
                   (if (= (get-patch counters target) 0)
                       (wave-spread (il-foldl (lambda (not-marked-point counters)
                                                (let ((my-mark (car not-marked-point))
                                                      (source (cdr not-marked-point)))
                                                  (let ((accessible (filter-accessible (neighbours source)
                                                                                       map
                                                                                       (patches-coords/append counters forbidden))))
                                                    (if (integerp accessible)
                                                        counters
                                                        (il-foldl (lambda (point counters)
                                                                    (set-patch counters point (+ my-mark 1)))
                                                                  counters
                                                                  accessible)))))
                                              counters
                                              counters)
                                    (- depth-limit 1))
                       counters))))
      (labels ((backtrack (counters point path) 
                 (if (coords= point source)
                     path
                     (let ((my-mark (get-patch counters point)))
                       (let ((next-point (labels ((find-next (avail-neighbours)
                                                    (if (integerp avail-neighbours)
                                                        0
                                                        (let ((checkpoint (car avail-neighbours)))
                                                          (if (= (- my-mark (get-patch counters checkpoint)) 1)
                                                              checkpoint
                                                              (find-next (cdr avail-neighbours)))))))
                                           (find-next (neighbours point)))))
                         (if (integerp next-point)
                             0
                             (backtrack counters next-point (cons point path))))))))
        (backtrack (wave-spread zero-counters depth-limit) target 0)))))

(defun greedy-plan-route (source target map rev-path forbidden)
  (labels ((plan-route-rec (source rev-path limit)
             (if (or (= limit 0))
                 (cons (il-reverse (cons target rev-path)) limit)
                 (if (coords= source target)
                     (cons (il-reverse (cons target rev-path)) limit)
                     (labels ((try-moves (avail-moves limit)
                                (if (integerp avail-moves)
                                    (cons 0 limit)
                                    (let ((next-move-plan (pop-nearest-object target avail-moves)))
                                      (let ((best-move (car next-move-plan))
                                            (rest-moves (cddr next-move-plan)))
                                        (let ((path+new-limit (plan-route-rec best-move (cons source rev-path) limit)))
                                          (if (integerp (car path+new-limit))
                                              (try-moves rest-moves (- (cdr path+new-limit) 1))
                                              (cons (car path+new-limit) (cdr path+new-limit)))))))))
                       (try-moves
                        (filter-accessible (filter-accessible (neighbours source) map rev-path)
                                           map
                                           forbidden)
                        (- limit 1)))))))
  (car (plan-route-rec source rev-path 26))))

(defun plan-route (source target map rev-path forbidden)
  (let ((wave-plan (plan-route-limit source target map rev-path forbidden 7)))
    (if (integerp wave-plan)
        (let ((greedy-plan (greedy-plan-route source target map rev-path forbidden)))
          (break)
          (if (integerp greedy-plan)
              0
              (cdr greedy-plan)))
        wave-plan)))

(defun choose-dir (source target)
  (declare (optimize (debug 3)))
  (let ((xs (car source)) (ys (cdr source)) (xt (car target)) (yt (cdr target)))
    (if (= ys yt)
        (if (< xs xt) 1 3)
        (if (< ys yt) 2 0))))

(defun make-ai-state (current-path i-see-fruit)
  (cons current-path i-see-fruit))

(defun call-with-ai-state (ai-state proc)
  (funcall proc (car ai-state) (cdr ai-state)))

(defun choose-next-target (map pacman angry-ghosts objects-by-priority)
  (declare (ignore objects-by-priority) (optimize (debug 3)))
  (let ((objects (locate-objects-if (lambda (cell x y) (if (or (= cell 2) (= cell 3))  (cons x y) 0)) map)))
    (if (integerp objects)
        0
        (let ((nearest-object (car (pop-nearest-object pacman objects))))
          (let ((path-to-object (plan-route pacman nearest-object map 0 angry-ghosts)))
            (if (integerp path-to-object)
                0
                path-to-object))))))

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
  (if (integerp ghosts)
      0
      (il-foldl (lambda (ghost-info acc) (cons (car (cdr ghost-info)) acc)) 0 ghosts)))
  
(defun nearest-ghost (pacman ghosts-coords)
  (pop-nearest-object pacman ghosts-coords))

(defun flee-point (map pacman ghost-coord)
  (car (pop-farest-object ghost-coord (filter-accessible (neighbours pacman) map 0))))

(defun check-angry-ghost-too-close (map pacman)
  (lambda (nearest-ghost ghost-sq-dist rest-ghosts)
    (declare (ignore rest-ghosts ghost-sq-dist))
    (let ((possible-path (plan-route-limit pacman nearest-ghost map 0 0 3)))
      (if (integerp possible-path)
          0
          (cons (flee-point map pacman nearest-ghost) 0)))))

(defun check-cowardly-ghost-close (map pacman)
  (lambda (nearest-ghost ghost-sq-dist rest-ghosts)
    (declare (ignore rest-ghosts))
    (if (>= 32 ghost-sq-dist)
        (let ((route (plan-route pacman nearest-ghost map 0 0)))
          (if (integerp route) 0 route))
        0)))
  
(defun estimate-ghosts-threat (map pacman ghosts)
  (call-with-tuple/2
   (analyze-ghosts ghosts)
   (lambda (angry-ghosts cowardly-ghosts)
     (cons (if (integerp angry-ghosts)
               (if (integerp cowardly-ghosts)
                   0
                   (call-with-tuple/3 (nearest-ghost pacman (ghosts-coords cowardly-ghosts))
                                      (check-cowardly-ghost-close map pacman)))
               (let ((flee-plan (call-with-tuple/3 (nearest-ghost pacman (ghosts-coords angry-ghosts))
                                                   (check-angry-ghost-too-close map pacman))))
                 (if (integerp flee-plan)
                     (if (integerp cowardly-ghosts)
                         0
                         (call-with-tuple/3 (nearest-ghost pacman (ghosts-coords cowardly-ghosts))
                                            (check-cowardly-ghost-close map pacman)))
                     flee-plan)))
           (ghosts-coords angry-ghosts)))))

(defun maybe-hunt-for-fruit (map pacman angry-ghosts fruit i-see-fruit)
  (if (or (= fruit 0) (= i-see-fruit 1))
      0
      (let ((fruit-position (car (locate-objects 4 map))))
        (let ((path-to-fruit (plan-route pacman fruit-position map 0 angry-ghosts)))
          (if (integerp path-to-fruit)
              0
              path-to-fruit)))))

(defun make-game-loop (ai-state)
  (declare (optimize (debug 3)))
  (lambda (map pacman-info ghosts fruit)
    (call-with-ai-state
     ai-state
     (lambda (current-path i-see-fruit)
       (let ((pacman (car (cdr pacman-info))))
         (call-with-tuple/2
          (estimate-ghosts-threat map pacman ghosts)
          (lambda (ghosts-threat angry-ghosts)
            (if (integerp ghosts-threat)
                (let ((fruit-path (maybe-hunt-for-fruit map pacman angry-ghosts fruit i-see-fruit)))
                  (if (integerp fruit-path)
                      (if (integerp current-path)
                          (let ((next-path (choose-next-target map pacman angry-ghosts (cons 3 (cons 2 0)))))
                            (if (integerp next-path)
                                (cons (make-ai-state 0 0) 1)
                                (funcall (make-game-loop (make-ai-state next-path 0)) map pacman-info ghosts fruit)))
                          (cons (make-ai-state (cdr current-path) i-see-fruit)
                                (choose-dir pacman (car current-path))))
                      (cons (make-ai-state (cdr fruit-path) 1)
                            (choose-dir pacman (car fruit-path)))))
                (cons (make-ai-state (cdr ghosts-threat) i-see-fruit)
                      (choose-dir pacman (car ghosts-threat)))))))))))

(defun gcc-step (ai-state world-state)
  (call-with-tuple/4 world-state (make-game-loop ai-state)))
             
(defun gcc-init (initial-world-state foreign-ghosts)
  (declare (ignore initial-world-state foreign-ghosts))
  (cons (make-ai-state 0 0) #'gcc-step))
              
