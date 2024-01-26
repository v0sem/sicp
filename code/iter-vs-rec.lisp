(defpackage iter-vs-rec
  (:use :cl))

(in-package iter-vs-rec)

(defun f-1.11-rec (n)
  (if (< n 3)
      n
      (+ 
       (f-1.11-rec (- n 1)) 
       (* 2 (f-1.11-rec (- n 2))) 
       (* 3 (f-1.11-rec (- n 3))))))

(defun f-1.11 (n)
  (f-1.11-iter 2 1 0 n))

(defun f-1.11-iter (a b c count)
  (if (= count 0)
      c
      (f-1.11-iter (+ a (* 2 b) (* 3 c)) a b (- count 1))))