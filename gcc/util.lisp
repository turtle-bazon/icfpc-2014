(in-package :gcc)

(defun pretty-print-gcc (gcc &optional stream)
  (iter (for form in gcc)
        (bind (((op &rest args) form))
          (case op
            (:label (format stream "~a:~%" (first args)))
            (t (format stream "    ~a~{ ~a~}~%" op args)))))))
