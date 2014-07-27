
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

                 
