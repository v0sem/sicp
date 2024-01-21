(defpackage newton-sqrt 
  (:use :cl))

(in-package newton-sqrt)

(defun sqrt-iter (guess x)
  (if (or (good-enough? guess x) (not-better? (improve guess x) guess))
      guess
      (sqrt-iter (improve guess x)
                 x)))

(defun improve (guess x)
  (average guess (/ x guess)))

(defun average (x y)
  (/ (+ x y) 2))

(defun good-enough? (guess x)
  (< (abs (- (square guess) x)) 0.001))

(defun square (x)
  (* x x))

(defun newton-sqrt (x)
  (sqrt-iter 1.0 x))

(defun not-better? (guess prev-guess)
  (> (abs (- 1 (/ guess prev-guess))) 0.1))