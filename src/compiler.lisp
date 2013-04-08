;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; compiler - compile a Bayesian Network to a set of Arithmetic
;; Circuit instructions.
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

;; this is the class that the net gets compiled to.
(defclass instructions ()
  ((forms        :initform nil
	         :accessor forms :type list)
   ;; lookups are defined in initialize-instance
   (match-lookup :reader match-lookup)
   (+-lookup     :reader +-lookup)
   (*-lookup     :reader *-lookup)
   (form-count   :initform 0 :accessor form-count :type fixnum)
   (assignment   :accessor assignment :type (simple-array fixnum *))
   (cliques      :accessor cliques :type simple-vector)
   (tree         :accessor tree :type simple-vector)))

(defmethod initialize-instance :after ((ins instructions) &key net)
  (when net
    (with-slots (cliques tree assignment match-lookup +-lookup *-lookup) ins
      (multiple-value-setq (cliques tree) (clique-tree net))
      (setf assignment (assign-nodes net cliques))
      (setf match-lookup
	    (make-array (num-nodes net) :element-type '(simple-array fixnum)))
      (dotimes (i (num-nodes net))
	(setf (aref match-lookup i)
	      (make-array (num-states (aref (node-vec net) i))
			  :element-type 'fixnum :initial-element -1)))
      (setf +-lookup (vecfn #'make-trie (1+ (length cliques))))
      (setf *-lookup (vecfn #'make-trie (1+ (length cliques)))))))

(defun set-match (ins node-num value form-num)
;;  (declare (type (mod 536870912) node-num value form-num) (optimize (speed 3) (safety 0)))
  (let ((match-lookup (match-lookup ins)))
    (declare (type (simple-array (simple-array fixnum *) *) match-lookup))
    (setf (aref (aref match-lookup node-num) value) form-num)))

(defun get-match (ins node-num value)
;;  (declare (type (mod 536870912) node-num value) (optimize (speed 3) (safety 0)))
  (let ((match-lookup (match-lookup ins)))
    (declare (type (simple-array (simple-array fixnum *) *) match-lookup))
    (aref (aref match-lookup node-num) value)))

(defun set-+ (ins vals form-num depth)
;;  (declare (optimize (speed 3) (safety 0)))
  (let ((first (car vals)))
    (trie-insert form-num (aref (+-lookup ins) depth)
		 (if (floatp first)
		     (append (float-to-fixnums first) (cdr vals))
		     vals))))

(defun get-+ (ins vals depth)
;;  (declare (optimize (speed 3) (safety 0)))
  (let ((first (car vals)))
    (awhen (trie-search (aref (+-lookup ins) depth)
			(if (floatp first)
			    (append (float-to-fixnums first) (cdr vals))
			    vals))
      (trie-value it))))

(defun set-* (ins vals form-num depth)
;;  (declare (optimize (speed 3) (safety 0)))
  (let ((first (car vals)))
    (trie-insert form-num (aref (*-lookup ins) depth)
		 (if (floatp first)
		     (append (float-to-fixnums first) (cdr vals))
		     vals))))

(defun get-* (ins vals depth)
;;  (declare (optimize (speed 3) (safety 0)))
  (let ((first (car vals)))
    (awhen (trie-search (aref (*-lookup ins) depth)
			(if (floatp first)
			    (append (float-to-fixnums first) (cdr vals))
			    vals))
      (trie-value it))))

(defun add-form (form ins &optional depth)
  (let ((form-num (form-count ins))
	(op (car form)))
    (cond ((numberp op)  (set-match ins op (cdr form) form-num))
	  ((eq op '*)    (set-* ins (cdr form) form-num depth))
	  ((eq op '+)    (set-+ ins (cdr form) form-num depth)))
    (push form (forms ins)); form-num)
    (incf (form-count ins))
    form-num))

(defun push-form (form ins &optional depth)
  "Pushes a form onto instructions, and returns an index into forms.
Won't actually add a form if a matching one exists (acts like pushnew)."
  (let ((op (car form)))
    (cond ((numberp op) (let ((form-num (get-match ins op (cdr form))))
			  (if (>= form-num 0) form-num
			      (add-form form ins))))
	  ((eq op '*)   (or (get-* ins (cdr form) depth) (add-form form ins depth)))
	  ((eq op '+)   (or (get-+ ins (cdr form) depth) (add-form form ins depth))))))

(defun gen-evidence (net net-state node-num)
  (declare (ignore net))
  (cons node-num (svref net-state node-num)))

(defun post-process-times (depth args ins)
  "Puts the form in canonical form for insertion."
  (multiple-value-bind (floats indices)
      (split #'floatp args)
    (let ((total (multiply-floats floats)))
      (cond ((endp indices) total)                            ; no indices
	    ((endp (cdr indices))                             ; one new index
	     (if (= total 1.0d0)                              ;   and no num
		 (car indices)                                ;     delegate
		 (push-form `(* ,total ,(car indices)) ins depth))) ;   else push total and index
	    (t (let ((sorted (sort indices #'<)))                     ; sort multiple indices
		 (if (= total 1.0d0) (push-form (cons '* sorted) ins depth) ; no total, just push the form.
		     ;; make a new index for sorted and push the result.
		     (push-form (list* '* total sorted) ins depth))))))))

(defun post-process-plus (depth args ins)
  "Puts the form in canonical form for insertion."
  (multiple-value-bind (floats indices)
      (split #'floatp args)
    (let ((total (sum-floats floats)))
      (cond ((endp indices) total)                                    ; no indices
	    ((endp (cdr indices))                                     ; one new index
	     (if (= total 0.0d0)                                      ;   and no num
		 (car indices)                                        ;     delegate
		 (push-form `(+ ,total ,(car indices)) ins depth)))   ;   else push total and index
	    (t (let ((sorted (sort indices #'<)))                     ; sort multiple indices
		 (if (= total 0.0d0) (push-form (cons '+ sorted) ins depth) ; no total, just push the form.
		     ;; make a new index for sorted and push the result.
		     (push-form (list* '+ total sorted) ins depth))))))))

(defun gen-separator (net ins net-state traversal depth)
  "bn - a net object.
clique-tree - a clique-tree built from the bn's dag.  Symmetric array
  with node-num lists on the diagonal and t where connected.
node-assignment - (aref node-assignment node-num) == node's clique index.
net-state - array of the current node states.  Nil means the node hasn't 
  been processed.
traversal - the current traversal to operate on: (clique trav1 ... travn)
depth - the current depth of the traversal."
  (post-process-plus 
   depth
   ;; generate a multiply for each net-state instantiation.
   (destructuring-bind (clique-num net-mask combs) (car traversal)
     (declare (ignore clique-num))
     (dotimes (i (length net-mask))
       (when (aref net-mask i) (setf (aref net-state i) 0)))
     (loop repeat combs ;; iterate through the node assigment
	collect (gen-clique net ins net-state traversal depth)
	do (cpt-incf net-state net-mask)
	finally
	  (dotimes (i (vlength net-mask))
	    (when (aref net-mask i) (setf (aref net-state i) nil)))))
   ins))

(defun gen-clique (net ins net-state traversal depth)
  (flet ((check-zero (x) (if (and (floatp x) (= 0.0d0 (the double-float x)))
			     (return-from gen-clique 0.0d0)
			     x)))
    (with-slots (assignment) ins
      (post-process-times depth
       ;; multiplies floats and sorts indices. Also pushes onto forms.
       (let (this-form)
	 ;; assigned node parameters and evidence indicators.
	 (loop for node-num in (aref assignment (caar traversal)) do
	      (push (push-form (gen-evidence net net-state node-num) ins) this-form)
	      (push (check-zero (gen-parameter net net-state node-num)) this-form))
	 ;; children (connected cliques which haven't yet been assigned)
	 (dolist (trav (cdr traversal) this-form)
	   (push (check-zero (gen-separator net ins net-state trav (1+ depth))) this-form)))
       ins))))

(defun best-traversal (net ins)
  (with-slots (tree cliques) ins
    (labels ((trav-third (trav)
	    (cons (funcall #'third (car trav))
	 	   (mapcar #'trav-third (cdr trav))))
	   (gen-trav (node)
	     (net-masks net cliques (traverse tree node)))
	   (traversal-max-mult (trav)
	     (* (car trav)
		(reduce #'max (mapcar #'traversal-max-mult (cdr trav))
			:initial-value 1))))
      (let ((travs (map-int #'gen-trav (length tree))))
	(best travs #'< :key (compose #'traversal-max-mult #'trav-third))))))

(defun gen-instructions (net)
  "Generate an arithmetic circuit for the bn represented by a
net (bn.lisp). Note that this version assumes the clique-tree is a
single tree (which is correct at 15/12/2007)."
  (unless (slot-boundp net 'node-order) (preprocess-network net))
  (with-calculation-context (eql)
    (let ((ins (make-instance 'instructions :net net))
	  (net-state (make-array (num-nodes net) :initial-element nil)))
      (gen-separator net ins net-state (best-traversal net ins) 0)
      (setf (forms ins) (nreverse (forms ins)))
      ins)))

(defun match (val1 val2)
  (declare (type fixnum val1 val2))
  "Matches evidence."
  (the double-float
    (if (< val1 0) 1.0d0 (if (= val1 val2) 1.0d0 0.0d0))))
(declaim (ftype (function (fixnum fixnum) double-float) match)
	 (inline match))

(defun interpret (ins net-spec)
  (loop with results = (make-array (form-count ins)); :element-type 'double-float)
     for i fixnum from 0
     for (inst-type . vals) in (forms ins) do
       (setf (aref results i)
	     (cond ((numberp inst-type)
		    (match (aref net-spec inst-type) vals))
		   ((eq inst-type '+)
		    (loop with val1 = (car vals) 
		       with retval = (if (floatp val1) val1 (aref results val1))
		       for val in (cdr vals)
		       do (incf retval (aref results val))
		       finally (return retval)))
		   ((eq inst-type '*)
		    (loop with val1 = (car vals) 
		       with retval = (if (floatp val1) val1 (aref results val1))
		       for val in (cdr vals)
		       do (when (= retval 0.0d0) (return 0.0d0))
			 (setf retval (* retval (aref results val)))
		       finally (return retval)))))
     finally (return (aref results (1- (vlength results))))))

(defun use-interpreted (net)
  "Use interpreted arithmetic circuit instructions for the
net. Returns the instructions object."
  (let ((ins (gen-instructions net)))
    (setf (compiled net)
	  (lambda (net-spec)
	    (interpret ins net-spec)))
    ins))
