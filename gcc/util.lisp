(in-package :gcc)

(defun pretty-print-gcc (gcc &optional (stream *standard-output*) &key (minimize nil))
  (iter (for form in gcc)
        (bind (((op &rest args) form))
          (case op
            (:label (unless minimize
		      (format stream ";~a:~%" (first args))))
            (t (format stream "    ~a~{ ~a~}~%" op args))))))
