;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; bn-utils - helper macros and functions for Bayesian Network
;; compilation.
;;
;; Copyright (c) 2007-2013, Lucas Hope <lucas.r.hope@gmail.com>.
;; Copyright other contributors as noted in the AUTHORS file.
;;
;; This file is part of cl-bayesnet - a Common Lisp Bayesian Network
;; Inference Engine.
;;
;; This file is licensed under the terms of the LLGPL.
;;
;; This library is free software; you can redistribute it and/or modify
;; it under the terms of the Lisp Lesser General Public License version
;; 3, which consists of the GNU Lesser General Public License, either
;; version 3 or (at your option) any later version, as published by the
;; Free Software Foundation, and the Franz preamble.
;;
;; This library is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.

(in-package :cl-bayesnet)

;;(declaim (optimize (speed 3)))

(defmacro multiply-floats (floats)
  (let ((gfloats (gensym)))
  `(let ((,gfloats ,floats))
    (cond ((endp ,gfloats) 1.0d0)
	  ((cdr ,gfloats) (reduce #'* ,gfloats))
	  ((car ,gfloats))))))

(defmacro sum-floats (floats)
  (let ((gfloats (gensym)))
  `(let ((,gfloats ,floats))
    (cond ((endp ,gfloats) 0.0d0)
	  ((cdr ,gfloats) (reduce #'+ ,gfloats))
	  ((car ,gfloats))))))

(defmacro vlength (vec)
  `(array-dimension ,vec 0))

(defmacro do-upper ((i j len &optional result) &body body)
  "do for upper triangular i and j.  i from 0 to len, j from i+1 to len." 
  (let ((glen (gensym)))
    `(let ((,glen ,len))
       (dotimes (,i ,glen ,result)
	 (loop for ,j from (1+ ,i) to (1- ,len)
	    do ,@body)))))

(defmacro reset-potential (place)
  `(setf ,place (make-vector (length ,place) 1)))

(defparameter *temporary-directory* "/tmp/"
  "Temporary work directory for C-based network compilation")

(defun random-choose (seq)
  (declare (sequence seq))
  (elt seq (random (length seq))))

;; This is not really good. Fix sometime.
(defun find-temporary-file (prefix suffix &optional (directory *temporary-directory*))
  (ensure-directories-exist directory)
  (loop
     for i fixnum = 0 then (1+ i)
     for file = (merge-pathnames (format nil "~A~A~A" prefix i suffix) directory)
     unless (probe-file file) do (return file)))

(defun float-to-fixnums (float)
  "This converts a float to a list of fixnums, useful for hashing floats using fixnum-based schemes."
  (with-calculation float
    (multiple-value-bind (bignum exponent) (integer-decode-float float)
      (loop
	 for fixnum = (logand bignum #.most-positive-fixnum) then 
	   (logand next #.most-positive-fixnum)
	 for next = (ash bignum #.(- (integer-length most-positive-fixnum))) then
	   (ash next #.(- (integer-length most-positive-fixnum)))
	 collect fixnum into fixnums
	 until (= next 0)
	 finally (return (cons exponent fixnums))))))

(defun split (p x)
  (do ((y x z)
       (z (cdr x) (cdr z))
       (pt ())
       (pn ()))
      ((endp y)
       (values pt pn))
    (if (funcall p (car y))
	(setq pt (rplacd y pt))
	(setq pn (rplacd y pn)))))

(defun degen-prob (i length)
  "Creates a 0-vector of the given length, with the i'th argument set to 1."
  (let ((vec (make-array length :initial-element 0.0d0)))
    (setf (aref vec i) 1.0d0)
    vec))

(defun map-pairs (fn list)
  "applies fn to each pair of items and returns a list of the resulting values.
> (map-pairs #'cons '(1 2 3 4))
=> ((1 . 2) (1 . 3) (1 . 4) (2 . 3) (2 . 4) (3 . 4))"
  (mapcon (lambda (x)
	    (let ((me (car x)))
	      (mapcar (lambda (you) (funcall fn me you)) (cdr x))))
	  list))

(defun symmetric (array x y)
  "Treat 2D array as a symmetric array. Settable."
  (if (< x y)
      (aref array x y)
      (aref array y x)))

(defun (setf symmetric) (val array x y)
  (if (< x y)
      (setf (aref array x y) val)
      (setf (aref array y x) val)))

(defun node-combinations (initial node-mask)
  "Returns a vector of all node index combinations built from initial.
node-mask is a vector which is nil for non-incremented nodes and
numstates for included nodes. DEPRECATED for being horribly slow."
  (let* ((num-combs (reduce #'* (remove nil node-mask)))
	 (combs (make-array num-combs))
	 (net-state (copy-array initial)))
    (dotimes (i (vlength node-mask))
      (when (svref node-mask i) (setf (svref net-state i) 0)))
    (setf (svref combs 0) (copy-array net-state))
    (do ((i 1 (1+ i)))
	((= i num-combs) combs)
      (cpt-incf net-state node-mask)
      (setf (svref combs i) (copy-array net-state)))))

(defun split-probs (probs prob-length)
  (declare (type list probs)
	   (fixnum prob-length))
  (loop with len = (length probs)
     with prob-vec = (make-array (the fixnum (/ len prob-length))
				 :element-type 'vector :initial-element #())
     for prob-num = 0 then (1+ prob-num)
     while probs
     for prob = (make-array prob-length :element-type 'float :initial-element 0.0)
     do (dotimes (i prob-length)
	    (setf (aref prob i) (pop probs)))
       (setf (aref prob-vec prob-num) prob)
     finally (return prob-vec)))

(defun edge< (edge1 edge2)
  (let ((e1a (car edge1)) (e2a (car edge2)))
    (declare (fixnum e1a e2a))
    (or (< e1a e2a) (and (= e1a e2a)
			 (let ((e1d (cdr edge1)) (e2d (cdr edge2)))
			   (declare (fixnum e1d e2d))
			   (< e1d e2d))))))

(defun edge= (edge1 edge2)
  (and (= (car edge1) (car edge2)) (= (cdr edge1) (cdr edge2))))

(defun vecfn (fn num)
  "Build a vector by calling fn num times."
  (declare (fixnum num))
  (let ((vec (make-array num)))
    (dotimes (i num vec)
      (setf (svref vec i) (funcall fn)))))
  

