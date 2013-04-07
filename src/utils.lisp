;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; utils - general utility macros and functions.
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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro with-gensyms ((&rest symbols) &body body)
    `(let ,(mapcar #'(lambda (symbol) `(,symbol (gensym)))
                   symbols)
       ,@body)))

(defmacro while (expr &body body)
  "Continue performing body until expr is false."
  `(do ()
       ((not ,expr))
     ,@body))

(defmacro aif (test-form then-form &optional else-form)
  "Anaphoric if.  (lexically sets 'it' to the test-form, which can
  then be referenced in then-form and else-form."
  `(let ((it ,test-form))
     (if it ,then-form ,else-form)))

(defmacro awhen (test-form &body body)
  "Anaphoric when.  See aif."
  `(aif ,test-form
	(progn ,@body)))

(defmacro returnit (val &body body)
    "An anaphorous macro. binds the first val to `it', allows possible modifications
in the body, then returns the values.  A more general prog1." 
    `(let ((it ,val)) ,@body it))

(defmacro multf (ref multiplier)
  "(setf ref (* ref multiplier)). Like incf."
  `(setf ,ref (* ,ref ,multiplier)))

(defvar *calculation-context* nil
  "Used by with-calculation(-context) to define a limited caching
   environment.")

(defmacro with-calculation-context ((&optional (test 'equalp))
				    &body body)
  "Defines a calculation context within which calculations performed
   within with-calculation are cached according to their label."
  `(let ((*calculation-context* (make-hash-table :test ',test)))
     ,@body))

(defmacro with-calculation (label &body body)
  "Unless label is nil, caches the calculation performed by body using label.
   If there is no calculation-context, does not evaluate label, so
   don't rely on side effects."
  (with-gensyms (glabel gvalue gpresent-p)
    `(let ((,glabel (and *calculation-context* ,label)))
       (multiple-value-bind (,gvalue ,gpresent-p)
	   (and ,glabel *calculation-context*
		(gethash ,glabel *calculation-context*))
	 (if ,gpresent-p ,gvalue
	     (prog1 (setf ,gvalue (progn ,@body))
	       (when (and ,glabel *calculation-context*)
		 (setf (gethash ,glabel *calculation-context*)
		       ,gvalue))))))))

;; function composition
(defun compose (&rest fns)
  "Create a function which is the composition of the given functions."
  (destructuring-bind (fn1 . rest) (reverse fns)
    (lambda (&rest args)
      (reduce (lambda (v f) (funcall f v))
	      rest
	      :initial-value (apply fn1 args)))))

(defun maparray (fn array)
  "maparray applies fn to all elements in
array, returning the result in an array the
same dimension as the first array"
  (let ((retval (make-array (array-dimensions array)))
	(length (array-total-size array)))
    (dotimes (i length retval)
      (setf (row-major-aref retval i)
	    (funcall fn (row-major-aref array i))))))

(defun copy-array (array)
  (maparray #'identity array))

(defun cpt-incf (att-state att-mask)
  "Increments att-state according to att-mask; att-state and att-mask
  are vectors.  att-mask is nil for non-included attributes and the
  number of values for included attributes. It takes (reduce
  #'* (remove nil att-mask)) to iterate through att-state.  att-state
  must be a positive integer below the mask's value, wherever mask is
  non-nil."
  (dotimes (i (length att-mask))
    (awhen (aref att-mask i)
      (let ((state (incf (aref att-state i))))
	(if (= it state) 
	    (setf (aref att-state i) 0)
	    (return)))))
  att-state)

(defun cpt-index (mask att-state)
  "Given the mask and att-state, return an index.  Used to map
  attribute values to a single lookup value in a table."
  (loop with multiplier = 1
     with index = 0
     for state across mask
     for value across att-state
     do (when state
	  (incf index (* multiplier value))
	  (setf multiplier (* multiplier state)))
     finally (return index)))

(defun map-int (fn n)
  "Map fn across integers from 0 below n, returning the results in
  an (ordered) list."
  (let ((acc nil))
    (dotimes (i n)
      (push (funcall fn i) acc))
    (nreverse acc)))

(defun best-in-list (list compare &key (key #'identity))
  "return the best value in list, along with its position and its comparison value.  See best."
  (loop with best-arg = 0 and best-val = (car list)
     with best-eval = (funcall key best-val)
     for arg from 1
     for val in (cdr list)
     for eval = (funcall key val)
     when (funcall compare eval best-eval)
     do (setq best-arg arg best-val val best-eval eval)
     finally (return (values best-val best-arg best-eval))))

(defun best-in-array (array compare &key (key #'identity))
  "return the best value in array, along with its position and its comparison value.  See best."
  (loop with best-arg = 0 and best-val = (aref array 0)
     with best-eval = (funcall key best-val)
     for arg from 1 below (length array)
     for val = (aref array arg)
     for eval = (funcall key val)
     when (funcall compare eval best-eval)
     do (setq best-arg arg best-val val best-eval eval)
     finally (return (values best-val best-arg best-eval))))

(defun best-in-hash (hash compare &key (key #'identity))
  "return the best value in hash along with its hash-key and its comparison value.  See best."
  (loop with best-arg and best-val and best-eval
     for arg being the hash-key of hash using (hash-value val)
     for eval = (funcall key val)
     when (or (null best-eval) (funcall compare eval best-eval))
     do (setq best-arg arg best-val val best-eval eval)
     finally (return (values best-val best-arg best-eval))))

(defun best (seq/hash compare &key (key #'identity))
  "Returns the best value in seq/hash, along with its position (or key
for the hash), according to compare. The comparison value is returned
as a third value. key extracts the value to be used for comparison."
  (etypecase seq/hash
    (null nil)
    (cons (best-in-list seq/hash compare :key key))
    (sequence (best-in-array seq/hash compare :key key))
    (hash-table (best-in-hash seq/hash compare :key key))))

(defun make-vector (len &optional initarg)
  (make-array len :initial-element initarg))

(defun normalize (vec &optional smoothing-factor)
  "Normalize a vector.  Optionally adds smoothing-factor (which should
  be a number) to each element first.  If tot is less than or equal to
  0."
  (let ((tot (reduce #'+ vec :initial-value
		     (if smoothing-factor (* smoothing-factor (length vec)) 0))))
    (cond ((= (length vec) 0) #())
	  ((<= tot 0) (make-array (length vec) :initial-element (/ (length vec))))
	  (t (map 'vector (if smoothing-factor
			      (lambda (x) (/ (+ x smoothing-factor) tot))
			      (lambda (x) (/ x tot)))
		  vec)))))

(defun order-vector (end &optional (start 0))
  "Create a vector of length (end - start), filled with numbers from
  start...(end - 1). Start defaults to 0."
  (let ((vec (make-array (- end start) :element-type 'fixnum :initial-element 0)))
    (dotimes (i (length vec) vec)
      (setf (aref vec i) (+ i start)))))

(defgeneric shuffle (object)
  (:documentation
  "Randomise the object, which should represent a sequence."))

(defmethod shuffle ((vector vector))
  "shuffle the items in vector into a random order."
  (let ((len (length vector)))
    (dotimes (i (1- len) vector)
      (rotatef (aref vector i) (aref vector (+ i (random (- len i))))))))

(defmethod shuffle ((sequence sequence))
  "shuffle the items in the sequence into a random order. Converts the sequence to a vector."
  (let ((type (type-of sequence)))
    (coerce (shuffle (coerce sequence 'vector)) type)))

(defmethod shuffle ((length integer))
  "Create a vector of the given length using order-vector, and shuffle it."
  (shuffle (order-vector length)))
