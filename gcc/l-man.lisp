;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package :gcc)

(deflib/gcc l-man-state (w-state)
  (car (cdr w-state)))

(deflib/gcc l-man-coord (l)
  (car (cdr l)))

(deflib/gcc move (ai-state w-state)
  (if (= (car (l-man-coord (l-man-state w-state))) 11)
      (cons ai-state :left)
      (cons ai-state :rigth)))

(pretty-print-gcc
 (build-ai-core '(let ((init (lambda (initial-state unknown)
			       (cons 42 move))))
		  (init initial-state unknown))
		:debug nil))
