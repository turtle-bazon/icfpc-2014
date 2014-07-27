
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
