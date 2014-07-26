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
