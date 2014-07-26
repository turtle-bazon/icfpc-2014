(in-package :gcc)

(defun pretty-print-gcc (gcc &key (stream *standard-output*) (minimize nil)
			       (pad t))
  (iter (for form in gcc)
        (let ((unlabeled (eq 'unlabeled (first form))))
	  (bind (((op &rest args) (if unlabeled
				      (rest form)
				      form)))
	    (case op
	      (:label (unless minimize
			(format stream (if unlabeled
					   ";~a:~%"
					   "~a:~%") (first args))))
	      (t (format stream "~a~a~{ ~a~}~%"
			 (if pad "    " "") op args)))))))

(defun compile-gcc (gcc-input-file gcc-output-file)
  (with-open-file (fo gcc-output-file :direction :output :if-exists :overwrite)
    (pretty-print-gcc
     (translate
      (with-open-file (fi gcc-input-file)
	(read fi))
      :unlabel t)
     :stream fo)))
