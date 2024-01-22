(defpackage newton-sqrt 
  (:use :cl))

(in-package newton-sqrt)

(defun sqrt-iter (guess x)
  (if (not-better? (improve guess x) guess)
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
  (< (abs (/ (- guess prev-guess) guess)) 0.0000000001))

(defun cubert (x)
  (cubert-iter 1.0 x))

(defun cubert-iter (guess x)
  (if (not-better? (improve-cube guess x) guess)
      guess
      (cube-iter (improve-cube guess x) 
                 x)))

(defun improve-cube (guess x)
  (/ (+ (/ x (square guess)) (* 2 guess)) 3))