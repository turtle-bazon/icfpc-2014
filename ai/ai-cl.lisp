
(in-package :ai)

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

        
    
