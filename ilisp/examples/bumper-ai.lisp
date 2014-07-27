(cl:defpackage #:ilisp-bumper-ai
  (:use :il))

(cl:in-package :ilisp-bumper-ai)

(defun nth (n lst)
  (if (= n 0) (car lst) (nth (- n 1) (cdr lst))))

(defun call-with-world (ai-state map lman ghosts fruits)
  (letrec ((at-coords (lambda (x y) (nth x (nth y map))))
           (can-walk (lambda (x y)
                       (if (= x (car ai-state))
                           (if (= y (cdr ai-state))
                               0
                               (at-coords x y))
                           (at-coords x y)))))
    (let ((up (can-walk (car lman) (- (cdr lman) 1)))
          (rg (can-walk (+ (car lman) 1) (cdr lman)))
          (dn (can-walk (car lman) (+ (cdr lman) 1)))
          (lf (can-walk (- (car lman) 1) (cdr lman))))
      (let ((decide (lambda (direction)
                      (cons (cons (car lman) (cdr lman)) direction))))
        (if (= lf 0)
            (if (= up 0)
                (if (= rg 0)
                    (if (= dn 0)
                        (decide :left)
                        (decide :down))
                    (decide :rigth))
                (decide :up))
            (decide :left))))))

(defun step (ai-state world-state)
  (call-with-world ai-state
                   (car world-state)
                   (car (cdr (car (cdr world-state))))
                   (car (cdr (cdr world-state)))
                   (cdr (cdr (cdr world-state)))))

(defun init (world-initial-state ghosts-statuses)
  (cons (cons 0 0) step))

(ilisp.impl:build-ai-core 'init)

