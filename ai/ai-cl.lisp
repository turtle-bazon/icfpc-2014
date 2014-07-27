
(in-package :ai)

(defun il-foldl (proc accumulator il-list)
  (let ((next-accumulator (funcall proc (car il-list) accumulator))
        (rest (cdr il-list)))
    (if (integerp rest)
        next-accumulator
        (il-foldl proc next-accumulator rest))))

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

(defun pop-nearest-object (my-x my-y objects)
  (declare (optimize (debug 3)))
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
            objects))

