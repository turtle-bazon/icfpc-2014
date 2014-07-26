(in-package :ghc-assembler)

(defun assemble-file (file-in file-out)
  (with-open-file (fouts file-out :direction :output :if-exists :overwrite :if-does-not-exist :create)
    (with-open-file (fins file-in)
      (assemble fins fouts))))

(defun assemble-from-file-to-console (file-in)
  (with-open-file (fins file-in)
    (assemble fins *standard-output*)))

(defun assemble (stream-in stream-out)
  (let* ((program (iter (for line = (read-line stream-in nil))
			(while line)
			(for normalized-line = (normalize-line line))
			(unless (equal "" normalized-line)
			  (collect (parse-line normalized-line)))))
	 (pprog (iter (for prog-line in program)
		      (for pc from 0)
		      (match prog-line
			((list :label label-name) (progn
						    (collect `(,label-name ,pc) into refs)
						    (decf pc)))
			((list :def def-name def-value) (progn
							  (collect `(,def-name ,def-value) into refs)
							  (decf pc)))
			((list (list :op op-name) op-params) (collect `(,op-name ,op-params) into prog)))
		      (finally (return (list prog refs))))))
    
    (destructuring-bind (prog refs)
	pprog
      (iter (for (instruction params) in prog)
	    (format stream-out "~a~a~%" (string-downcase (symbol-name instruction))
		    (if params
			(format nil " ~a"
				(reduce
				 (lambda (a b)
				   (format nil "~a,~a" a b))
				 (mapcar
				  (lambda (a)
				    (param-value a refs))
				  params)))
			""))))))

(defun param-value (param refs)
  (match param
    ((list :reg reg-name) (string-downcase (symbol-name reg-name)))
    ((list :def def-name) (let ((def-record (find def-name refs :key #'car)))
			    (if def-record
				(param-value (second def-record) refs)
				(error "Undefined def ~a" def-name))))
    ((list :const value) value)
    ((list :ref sub-param) (format nil "[~a]" (param-value sub-param refs)))))

(defun normalize-line (line)
  (string-downcase
   (string-trim
    '(#\Space #\Tab)
    (subseq line 0 (position #\; line)))))

(defun parse-line (line)
  (cond
    ((position #\: line) (parse-label line))
    ((string-equal "%def" line :end2 (min (length line) 4)) (parse-def line))
    (t (parse-instruction line))))

(defun parse-def (line)
  (let* ((space-position (or (position #\Space line)
                             (position #\Tab line)))
         (params-string (subseq line (1+ (or space-position (1- (length line))))))
         (params (split-sequence #\, params-string :remove-empty-subseqs t)))
    `(:def ,(read-from-string (first params))
	 ,(parse-parameter (second params)))))

(defun parse-label (line)
  `(:label ,(read-from-string (subseq line 0 (1- (length line))))))

(defun parse-instruction (line)
  (let* ((space-position (or (position #\Space line)
			     (position #\Tab line)))
	 (operator (subseq line 0 space-position))
	 (params-string (subseq line (1+ (or space-position (1- (length line))))))
	 (params (split-sequence #\, params-string :remove-empty-subseqs t)))
    `((:op ,(read-from-string operator))
      ,(mapcar (lambda (param)
		 (if (equal "" param)
		     nil
		     (parse-parameter param)))
	       params))))

(defun parse-parameter (param-string)
  (let ((parsed-param (read-from-string param-string))
	(param-end (1- (length param-string))))
    (cond
      ((and (= 0 (or (position #\[ param-string) -1))
            (= param-end (or (position #\] param-string) -1)))
       `(:ref ,(parse-parameter (subseq param-string 1 param-end))))
      ((numberp parsed-param) `(:const ,parsed-param))
      ((symbolp parsed-param) (case parsed-param
				((a b c d e f g h i) `(:reg ,parsed-param))
				(t `(:def ,parsed-param)))))))
