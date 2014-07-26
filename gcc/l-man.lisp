;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(cl:in-package #:l-man)

(cl:defmacro plus (a b)
  `(+ ,a ,b))

(cl:defun macroexpand-all (ast)
  (cl:if (cl:listp ast)
	 (cl:macroexpand-1
	  (cl:mapcar (cl:lambda (inner-ast)
		       (macroexpand-all inner-ast))
		     ast))
	 ast))

(deflib/gcc l-man-state (w-state)
  (car (cdr w-state)))

(deflib/gcc l-man-coord (l)
  (car (cdr l)))

(deflib/gcc map-obj (map x y)
  (nth x (nth y map)))

(deflib/gcc nth (n lst)
  (if (= n 0) (car lst) (nth (- n 1) (cdr lst))))

(deflib/gcc map-size (map x y)
  (if (integerp (cdr map))
      (if (= x 0)
	  (map-size (car map) (+ 1 x) (+ 1 y))
	  (cons x y))
      (if (= x 0)
	  (map-size (cdr map) x (+ 1 y))
	  (map-size (cdr map) (+ 1 x) y))))

(deflib/gcc move (ai-state w-state)
  (if (= (map-obj (car w-state) 2 4) 2)
      (cons ai-state :left)
      (cons ai-state :rigth)))

#+nil(pretty-print-gcc
 (build-ai-core '(let ((init (lambda (initial-state unknown)
			       (cons 42 move))))
		  (init initial-state unknown))
		:debug nil))
