
(in-package :gcc)

(defclass translate-state ()
  ((known-procs :initform (make-hash-table :test 'eq) :reader known-procs)))

(defun translate (ast)
  (translate-walker ast (make-instance 'translate-state)))

(defun translate-walker (ast state)
  (flet ((assume (form) (return-from translate-walker form)))
    (if-match (define (?proc-name . ?proc-args) ?forms) ast
              (assume (translate-define ?proc-name ?proc-args ?forms state)))
    (if-match ?atom ast
              (assume (translate-atom ?atom state)))
    (error "Invalid AST: ~s" ast)))

(defun translate-define (proc-name proc-args proc-body state)
  (declare (ignore proc-args))
  (setf (gethash proc-name (known-procs state)) t)
  (list (list :label proc-name)
        (translate-walker proc-body state)
        (list :rtn)))
                
(defun translate-atom (atom state)
  (declare (ignore state))
  (assert (typep atom 'integer)) ;; only integers allowed
  (list (list :ldc atom)))