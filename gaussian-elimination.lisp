(defpackage :gaussian-elimination
  (:use :cl))

(in-package :gaussian-elimination)

(defparameter *matrix*
  (make-array '(3 3) :element-type 'double-float :initial-element 0.0d0))

(defun random-matrix (rows cols)
  (loop
    with m = (make-array `(,rows ,cols)
                         :element-type 'double-float
                         :initial-element 0d0)
    for r from 0 below rows
        do (loop for c from 0 below cols
                 do (setf (aref m r c) (random 1d0)))
    finally (return m)))

(defun swap-rows (matrix r1 r2)
  (when (= r1 r2)
    (return-from swap-rows matrix))
  (loop for i from 0 below (array-dimension matrix 1)
        do (rotatef (aref matrix r1 i) (aref matrix r2 i)))
  matrix)

(defun add-rows (matrix r1 r2 &optional (scalar 1.0d0))
  (loop for i from 0 below (array-dimension matrix 1)
        do (incf (aref matrix r2 i) (* scalar (aref matrix r1 i))))
  matrix)

(defun multiply-row (matrix row scalar)
  (loop for i from 0 below (array-dimension matrix 1)
        do (setf (aref matrix row i) (* scalar (aref matrix row i))))
  matrix)

(defun eliminate (matrix &optional (epsilon 0.000001d0))
  (let ((pivot-row 0)
        (pivot-col 0)
        (rows (array-dimension matrix 0))
        (cols (array-dimension matrix 1)))
    (loop while (and (< pivot-row rows)
                     (< pivot-col cols))
          for imax = (loop
                       with imax = pivot-row
                       for r from pivot-row below rows
                       if (>= (aref matrix r pivot-col)
                              (aref matrix imax pivot-col))
                         do (setf imax r)
                       finally (return imax))
          do (if (< (abs (aref matrix imax pivot-col)) epsilon)
                 (incf pivot-col)
                 (progn
                   (swap-rows matrix pivot-row imax)
                   (loop for i from (+ 1 pivot-row) below rows
                         for f = (/ (aref matrix i pivot-col)
                                    (aref matrix pivot-row pivot-col))
                         do (setf (aref matrix i pivot-col) 0d0)
                         do (loop for j from (+ 1 pivot-col) below cols
                                  do (decf (aref matrix i j)
                                        (* (aref matrix pivot-row j) f))))
                   (incf pivot-col)
                   (incf pivot-row)))))
  matrix)
