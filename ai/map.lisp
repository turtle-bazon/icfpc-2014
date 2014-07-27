
(in-package :ai)

(defun load-map (filename)  
  (with-open-file (map filename)
    (list->il-list
     (iter (for line in-stream map using #'read-line)
           (collect (list->il-list
                     (iter (for char in-string line)
                           (collect
                               (ecase char
                                 (#\# 0)
                                 (#\Space 1)
                                 (#\. 2)
                                 (#\o 3)
                                 (#\% 4)
                                 (#\\ 5)
                                 (#\= 6))))))))))

(defun list->il-list (list)
  (if list (cons (car list) (list->il-list (cdr list))) 0))

