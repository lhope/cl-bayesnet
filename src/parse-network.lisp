;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; parse-network - Reading external Bayesian Network files.
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

(defun keyify (string)
  (intern (string-upcase string) :keyword))

(defun dimension-list (list)
  "Calculates the dimensionality of the list. EG:
  (make-array (dimension-list list) :initial-contents list)))"
  (loop
     for this = list then (car this)
     while (and (listp this) (not (endp this)))
     collect (length this)))

(defun dimension-list-but-one (list)
  "Calculates the dimensionality of the list, minus one. EG:
  (make-array (dimension-list list) :initial-contents list)))"
  (loop
     for this = list then (car this)
     while (and (listp (car this)) (not (endp (car this))))
     collect (length this)))

(defun parse-netica-stream (stream &key (net (make-instance 'net)))
  "Idea taken from trivial-configuration-parser by Brian Mastenbrook."
  (let ((*readtable* (copy-readtable *readtable*))
        (*package* (find-package :keyword))
        (*read-eval* nil)
        (*read-base* 10)
        (*read-default-float-format* 'double-float)
        (*read-suppress* nil))
    (setf (readtable-case *readtable*) :upcase)
    ; comments are "//"
    (make-dispatch-macro-character #\/)
    (let ((fun (get-macro-character #\;)))
      (set-dispatch-macro-character #\/ #\/
				    (lambda (s c1 c2)
				      (declare (ignore c2))
				      (funcall fun s c1))))
    ;;(set-syntax-from-char #\# #\;)
    (set-syntax-from-char #\; #\ )
    (set-syntax-from-char #\, #\Space)
    (set-macro-character #\{
                         (lambda (stream char)
                           (declare (ignore char))
                           (read-delimited-list #\} stream t)))
    (set-macro-character #\}
                         (lambda (stream char)
                           (declare (ignore stream char))
                           (error "Unmatched close brace!")))
    (set-syntax-from-char #\: #\ )
    (let* ((eof-value (gensym "EOF"))
	   (read-file (loop for thing = (read stream nil eof-value)
			 while (not (eql thing eof-value))
			 collect thing)))
      (parse-netica-net read-file :net net))))

(defun parse-netica-net (list &key (net (make-instance 'net)))
  (let ((first (first list))
	(name (second list))
	(defs (third list)))
    (assert (eql first :bnet))
    (setf (slot-value net 'name) (symbol-name name))
    (do* ((first (pop defs) (pop defs))
	  (second (pop defs) (pop defs))
	  (third (pop defs) (pop defs)))
	 ((null first) net)
      (cond ((eql :node first) 
 	     (setf (gethash second (slot-value net 'nodes))
		   (returnit (parse-netica-node second third)
		     (set-net net it) net)))
	    ((eql second :=)
	     (setf (gethash first (slot-value net 'properties)) third))
	    (t (format t "Couldn't parse triple ~A, ~A, ~A, ignoring." 
		       first second third))))))

(defun parse-netica-node (name list)
  (let ((node (make-instance 'node)))
    (setf (slot-value node 'name) name)
    (do* ((first (pop list) (pop list))
	  (second (pop list) (pop list))
	  (third (pop list) (pop list)))
	 ((null first) node)
      (if (eql second :=)
	  (case first
	    (:probs
	     (setf (slot-value node 'table) 
		   (maparray (lambda (x) (coerce x 'vector))
			     (make-array (dimension-list-but-one third)
					 :initial-contents third))))
	    (:functable
	     (awhen (states node)
	       (setf (slot-value node 'table) 
		     (maparray (lambda (x)
				 (degen-prob (position x it)
					     (length it)))
			       (make-array (dimension-list third)
					   :initial-contents third)))))
	    (:numstates
	     (setf (slot-value node 'states) (returnit (make-array third)
					       (dotimes (i third)
						 (setf (aref it i) i)))))
	    (:states
	     (setf (slot-value node 'states) (coerce third 'vector)))
	    (:parents
	     (setf (slot-value node 'parents) (coerce third 'vector)))
	    (otherwise
	     (setf (gethash first (slot-value node 'properties)) third)))
	  (format t "Couldn't parse triple ~A, ~A, ~A, ignoring." 
		  first second third)))))

(defun load-dne (file)
  "Load a file in Netica's dne file format. Returns a net."
  (with-open-file (s file :direction :input)
    (parse-netica-stream s)))

;;; ace ;;;
(defun parse-ace-stream (stream &key (net (make-instance 'net)))
  "Idea taken from trivial-configuration-parser by Brian Mastenbrook."
  (let ((*readtable* (copy-readtable *readtable*))
        (*package* (find-package :keyword))
        (*read-eval* nil)
        (*read-base* 10)
        (*read-default-float-format* 'double-float)
        (*read-suppress* nil))
    (setf (readtable-case *readtable*) :upcase)
    ; comments are "//"
    (make-dispatch-macro-character #\/)
    (let ((fun (get-macro-character #\;)))
      (set-dispatch-macro-character #\/ #\/
				    (lambda (s c1 c2)
				      (declare (ignore c2))
				      (funcall fun s c1))))
    ;;(set-syntax-from-char #\# #\;)
    (set-syntax-from-char #\| #\Space)
    (set-syntax-from-char #\; #\Space)
    (set-syntax-from-char #\, #\Space)
    (set-macro-character #\{
                         (lambda (stream char)
                           (declare (ignore char))
                           (read-delimited-list #\} stream t)))
    (set-macro-character #\}
                         (lambda (stream char)
                           (declare (ignore stream char))
                           (error "Unmatched close brace!")))
    (set-syntax-from-char #\: #\ )
    (loop 
       with eof-value = (gensym "EOF")
       with ignore = 0 
       for thing = (read stream nil eof-value)
       while (not (eql thing eof-value))
       if (> ignore 0) do (decf ignore)
       else do
       (case thing
	 (:net (setf ignore 1))
	 (:node 
	  (let ((name (read stream nil eof-value))
		(list (read stream nil eof-value)))
	    (setf (gethash name (slot-value net 'nodes))
		  (returnit (parse-netica-node name list)
		    (set-net net it) net))))
	 (:potential
	  (let ((parents (read stream nil eof-value))
		(data-list (read stream nil eof-value)))
	    (parse-ace-potential net parents data-list))))
	 finally (return net))))

(defun parse-ace-potential (net parents data-list)
  (let* ((node (node (pop parents) net))
	 (num-states (num-states-1 node))
	 (probs (third data-list)))
    (declare (list probs) (fixnum num-states))
    (setf (slot-value node 'parents) (coerce parents 'vector))
    (setf (slot-value node 'table) 
	  (cond ((some #'consp probs)
		 (maparray (lambda (x) (coerce x 'vector))
			   (make-array (the list (dimension-list-but-one probs))
				       :initial-contents probs)))
		((> (length probs) num-states)
		 (make-array (mapcar (lambda (x) (num-states-1 (node x net))) parents)
			     :displaced-to (split-probs probs num-states)))
		(t 
		 (make-array nil
			     :initial-element
			     (make-array num-states :element-type 'float
						    :initial-contents probs)))))))

(defun parse-ace-node (name list)
  (let ((node (make-instance 'node)))
    (setf (slot-value node 'name) name)
    (do* ((first (pop list) (pop list))
	  (second (pop list) (pop list))
	  (third (pop list) (pop list)))
	 ((null first) node)
      (if (eql second :=)
	  (case first
	    (:states
	     (setf (slot-value node 'states)
		   (map 'vector #'keyify third)))
	    (otherwise
	     (setf (gethash first (slot-value node 'properties)) third)))
	  (format t "Couldn't parse triple ~A, ~A, ~A, ignoring." 
		  first second third)))))

(defun load-ace (file)
  "Load a file in ace file format. Returns a net."
  (with-open-file (s file :direction :input)
    (parse-ace-stream s)))

(defun load-xmlbif (file)
  "Load a file in xmlbif file format. Returns a net."
  (let ((net (make-instance 'net))
	(lxml-net (cdr (assoc :network (s-xml:parse-xml-file file :output-type :lxml)))))
    (dolist (node lxml-net)
      (let ((node-name (if (listp (car node)) (caar node) (car node))))
	(case node-name
	  (:name
	   (set-name (second node) net))
	  (:variable
	   (xmlbif-add-variable net node))
	  (:definition
	   (xmlbif-add-definition net node))
	  (t (warn "load-xmlbif-file: Ignoring node ~S" node)))))
    net))

(defun xmlbif-add-variable (net node)
  ;; could check for node TYPE = "nature" here.
  (loop with bnode = (make-instance 'node)
     with outcomes
     for (key value) in (cdr node) do
       (case key
	 (:name (set-name (keyify value) bnode))
	 (:outcome (push (keyify value) outcomes))
	 (t (warn "xmlbif-add-variable: ignoring ~A=~A" key value)))
     finally
       (assert (name bnode) () "xmlbif-add-variable: variable does not have a name!")
       (set-states (coerce (nreverse outcomes) 'simple-vector) bnode)
       (setf (gethash (name bnode) (nodes net)) bnode)
       (set-net net bnode)
       (return bnode)))

(defun xmlbif-add-definition (net def)
  ;; could check for node TYPE = "nature" here.
  (loop with node with parents with probs
     for (key value) in (cdr def) do
       (case key
	 (:for (setf node (or (gethash (keyify value) (nodes net))
			      (error "xmlbif-add-definition: could not find node for definition ~A" def))))
	 (:given (push (keyify value) parents)) ;; reverse order.
	 (:table (setf probs (with-input-from-string (in value)
			       (loop with *read-eval* = nil
				  for float = (read in nil nil)
				  while float collect float)))))
     finally
       (setf parents (nreverse parents))
       (set-parents (coerce parents 'simple-vector) node)
       ;; this isn't the most efficient table representation. But remember this gets compiled later.
       (loop with state-count = (length (states node))
	  with table-arity = (loop for parent in parents collect (length (states (gethash parent (nodes net)))))
	  with flat-table = (make-array (reduce #'* table-arity :initial-value 1)) ;; initial-value handles table-arity = nil.
	  for index from 0
	  for probs1 on probs by (lambda (x) (nthcdr state-count x)) do
	    (setf (aref flat-table index) (coerce (subseq probs1 0 state-count) 'simple-vector))
	  finally (setf (table node) (make-array table-arity :displaced-to flat-table)))))

(defmethod initialize-instance :after ((net net) &key netica-file ace-file bif-file)
  (cond (netica-file (load-netica-file netica-file :net net))
	(ace-file (load-ace-file ace-file :net net))
	(bif-file (load-xmlbif-file bif-file :net net))))

