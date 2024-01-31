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

(defun pasc-num (row col)
  (cond ((< row 1) 1)
        ((or (<= col 1) (>= col row)) 1)
        (T (+ (pasc-num (- row 1) (- col 1)) (pasc-num (- row 1) col)))))

(defun pasc-row (row)
  (loop for item from 1 to row do
           (write (pasc-num row item))
        (write-char #\Space))
  (write-char #\Newline))

(defun pasc (row)
  (loop for i from 1 to row do
        (pasc-row i)))

