;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tries - trie implementation for compiling Arithmetic Circuits.
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

(defconstant +TRIE-HASH-THRESHOLD+ 7)

(defstruct TRIE
  "A trie node."
  (value nil)
  (count 0) ;; only incremented when branch is a list.
  (test 'eql)
  (branch nil)) 

(defun TRIE-CLEAR (trie)
  "Empties the trie, but leaves the test as is."
  (setf (trie-value trie) nil
	(trie-count trie) 0
	(trie-branch trie) nil)
  trie)

(defun MAKE-TRIE-HASH (alist &key (test 'eql))
  "Converts a trie alist to a trie-specific hash."
  (let ((hash (make-hash-table :test test)))
    (dolist (element alist hash)
      (setf (gethash (car element) hash) (cdr element)))))

(defun TRIE-BRANCH-COUNT (trie)
  "Count the number of branches of trie."
  (if (typep (trie-branch trie) 'hash-table)
      (hash-table-count (trie-branch trie))
      (trie-count trie)))

(defun TRIE-SEARCH (trie key)
  "Recursively search the trie for the given key.  If key is nil,
  We're at the right node, so we return it.  The value (which may be
  nil) can then be accessed with trie-value."
  (etypecase key
    (null trie)
    (list (let ((branch (trie-branch trie)))
	    (cond ((null branch)  nil)
		  ((listp branch) (awhen (assoc (car key) branch :test (trie-test trie))
				    (trie-search (cdr it) (cdr key))))
		  (t              (awhen (gethash (car key) branch)
				    (trie-search it (cdr key)))))))
    (sequence (trie-search trie (coerce key 'list)))))

(defun TRIE-INSERT (value trie key)
  "Insert the value in the trie with the given key.  Returns the
  accessed trie node."
  (etypecase key
    (null (progn (setf (trie-value trie) value) trie))
    (list (symbol-macrolet ((count (trie-count trie))
			    (branch (trie-branch trie))
			    (test (trie-test trie)))
	    (declare (type fixnum count))
	    (if (listp branch)
		(aif (assoc (car key) branch :test test)
		     (trie-insert value (cdr it) (cdr key))
		     (let ((new-trie (make-trie :test test)))
		       (push (cons (car key) new-trie) branch)
		       (incf count)
		       (when (= (the fixnum count) (the fixnum +trie-hash-threshold+))
			 (setf branch (make-trie-hash branch :test test)))
		       (trie-insert value new-trie (cdr key))))
		(aif (gethash (car key) branch)
		     (trie-insert value it (cdr key))
		     (let ((new-trie (make-trie :test test)))
		       (setf (gethash (car key) branch) new-trie)
		       (trie-insert value new-trie (cdr key)))))))
    (sequence (trie-insert value trie (coerce key 'list)))))

(defun TRIE-PRUNE (to-prune example)
  "Recursively remove all branches in to-prune that don't also appear
  in example, thus to-prune is guaranteed to be as small as example or
  smaller."
  (symbol-macrolet ((branch (trie-branch to-prune)))
    (if (listp branch)
	(setf branch
	      (loop for cons in branch
		 for next-example = (trie-search example `(,(car cons)))
		 when next-example
		 collect cons
		 and do (trie-prune (cdr cons) next-example))
	      (trie-count to-prune) (length branch))
	(loop for key being the hash-key of branch using (hash-value next-to-prune)
	   for next-example = (trie-search example `(,key))
	   if next-example do (trie-prune next-to-prune next-example)
	   else do (remhash key branch))))
  to-prune)

(defun TRIE-LIST-VALUES (trie)
  "Recursively list all values in the trie."
  (let* ((branch (trie-branch trie))
	 (children
	  (if (listp branch)
	      (loop for (nil . next-trie) in branch
		 nconc (trie-list-values next-trie))
	      (loop for next-trie being the hash-value of branch
		 nconc (trie-list-values next-trie)))))
    (aif (trie-value trie)
	 (cons it children)
	 children)))

(defun TRIE-LIST-DEPTH (trie depth)
  "List all values at the given depth (0 is the top level)."
  (cond ((< depth 0) nil)
	((= depth 0) (awhen (trie-value trie) (list it)))
	(t (let ((branch (trie-branch trie)))
	     (if (listp branch)
		 (loop for (nil . next-trie) in branch
		    nconc (trie-list-depth next-trie (1- depth)))
		 (loop for next-trie being the hash-value of branch
		    nconc (trie-list-depth next-trie (1- depth))))))))

(defun TRIE-LIST-BOTTOM (trie)
  "List all values at the bottom of the trie (i.e. values of all tries
  with no branches)."
  (if (= (trie-count trie) 0)
      (list (trie-value trie))
      (let ((branch (trie-branch trie)))
	(if (listp branch)
	    (loop for (nil . next-trie) in branch
	       nconc (trie-list-bottom next-trie))
	    (loop for next-trie being the hash-value of branch
	       nconc (trie-list-bottom next-trie))))))

(defun TRIE-BOTTOM-P (trie)
  "Returns t if the given trie has no branches."
  (let ((branch (trie-branch trie)))
    (or (null branch)
	(and (hash-table-p branch)
	     (= 0 (hash-table-count branch))))))

(defun %TRIE-MAP-TRIE (fn new-trie trie &rest tries)
  "Create a new trie with the same structure as trie, applying fn to
  each trie-node in turn."
  (setf (trie-value new-trie) (apply fn trie tries))
  (let ((branch (trie-branch trie)))
    (if (listp branch)
	(loop for (key . next-trie) in branch
	   for next-tries = (mapcar (lambda (x) (trie-search x `(,key))) tries)
	   for no-op = (member nil next-tries) ;; abort if we don't find a key.
	   for next-new-trie = (unless no-op (trie-insert nil new-trie `(,key)))
	   unless no-op
	   do (apply #'%trie-map-trie fn next-new-trie next-trie next-tries))
	;; regrettable copying of code.
	(loop for next-trie being the hash-value of branch using (hash-key key)
	   for next-tries = (mapcar (lambda (x) (trie-search x `(,key))) tries)
	   for no-op = (member nil next-tries) ;; abort if we don't find a key.
	   for next-new-trie = (unless no-op (trie-insert nil new-trie `(,key)))
	   unless no-op
	   do (apply #'%trie-map-trie fn next-new-trie next-trie next-tries))))
  new-trie)

(defun TRIE-MAP-B (fn trie)
  "Apply fn to each branch of trie.  Returns the trie."
  (let ((branch (trie-branch trie)))
    (if (listp branch)
	(loop for (nil . next-trie) in branch
	   do (funcall fn next-trie))
	(loop for next-trie being the hash-value of branch
	   do (funcall fn next-trie)))))

(defmacro DO-TRIE-BRANCHES ((next-trie trie &optional result) &body body)
  "Loop over the trie's branches."
  `(progn 
     (trie-map-b (lambda (,next-trie) ,@body) ,trie)
     ,result))

(defun TRIE-MAP-BRANCHES (fn trie)
  "Apply fn to each branch of trie.  Returns a list of the returned values."
  (let (list)
    (trie-map-b (lambda (x) (push (funcall fn x) list)) trie)
    (nreverse list)))

(defun TRIE-BRANCH-VALUES (trie)
  "Return (key . trie-value) for each branch of trie."
  (let ((branch (trie-branch trie)))
    (if (listp branch)
	(loop for (key . next-trie) in branch
	   collect (cons key (trie-value next-trie)))
	(loop for next-trie being the hash-value of branch using (hash-key key)
	   collect (cons key (trie-value next-trie))))))

(defun TRIE-MAP-TRIE (fn trie &rest tries)
  "Apply fn to the given trie and its children, returning the results
  as the values of a matching trie."
  (apply #'%trie-map-trie fn (make-trie :test (trie-test trie)) trie tries))

(defun TRIE-MAP-VALUES (fn trie)
  "Apply fn to the given trie's non-nil values, returning the results
  as the values of a matching trie."
  (trie-map-trie (lambda (trie)
		   (awhen (trie-value trie)
		     (funcall fn it)))
		 trie))

(defmacro DO-TRIE-VALUES ((value trie &optional retval) &body body)
  `(block nil
     (trie-map-values (lambda (,value) ,@body) ,trie)
     ,retval))

(defun %TRIE-MAP-KEY-VALUE (fn key trie new-trie)
  "Create a new trie with the same structure as trie, applying fn to
  the key and the old trie node."
  (setf (trie-value new-trie) (funcall fn key trie))
  (let ((branch (trie-branch trie)))
    (if (listp branch)
	(loop for (next-key . next-trie) in branch
	   for next-new-trie = (trie-insert nil new-trie `(,next-key))
	   do (%trie-map-key-value fn `(,@key ,next-key) next-trie next-new-trie))
	(loop for next-trie being the hash-value of branch using (hash-key next-key)
	   for next-new-trie = (trie-insert nil new-trie `(,next-key))
	   do (%trie-map-key-value fn `(,@key ,next-key) next-trie next-new-trie))))
  new-trie)

(defun TRIE-MAP-KEY-VALUE (fn trie)
  "Create a new trie with the same structure as trie, applying fn to
  the key and the old trie node."
  (%trie-map-key-value fn nil trie (make-trie :test (trie-test trie))))

(defun HASH-TO-TRIE (hash &key (trie (make-trie :test (hash-table-test hash)))
		     (trie-key #'identity) (trie-value #'identity) bagp)
  "Add the elements of hash to the given trie (if no trie is given,
  make one with the same test as the hash table.  trie-key (default
  #'identity) is a function to apply to the hash-key.  It should
  convert the hash-key to a list of atoms which are compatible with
  the trie's test.  trie-value (default #'identity) is a function to
  extract the value of interest from the actual value (or the key if
  bagp is non-nil) held in the hash."
  (loop
     for key being the hash-key of hash using (hash-value value) do
       (trie-insert (funcall trie-value (if bagp key value))
		    trie (funcall trie-key key))
     finally (return trie)))

