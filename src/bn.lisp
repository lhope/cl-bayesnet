;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; bn - Bayesian Network classes, evidence and querying APIs.
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

(defgeneric name (net/node)
  (:documentation
   "For a net, a string identifier and for a node, a keyword
identifier."))

(defgeneric net (node/join-tree)
  (:documentation
   "Returns the containing net for a node or join-tree."))

(defgeneric node-order (net)
  (:documentation
   "Returns a list of node-names as keywords. The ordering matches
their index order. This is set as part of the network compilation
process."))

(defgeneric num-nodes (net)
  (:documentation
   "The amount of nodes in the network."))

(defclass net ()
  ((name :reader name :writer set-name)
   (compiled :initform nil :accessor compiled)
   (node-order :reader node-order :writer set-node-order); :type (simple-vector))
   ;; for rapid access by index.
   (num-nodes :reader num-nodes)
   (node-vec :accessor node-vec); :type (simple-array node *))
   (nodes :initform (make-hash-table) :reader nodes)
   (properties :initform (make-hash-table) :reader properties)))

(defgeneric num-states (node)
  (:documentation
   "The amount of states for the given node."))

(defgeneric parents (node)
  (:documentation
   "A vector of parent names as keywords for the given node."))

(defgeneric states (node)
  (:documentation
   "A vector of state names as keywords for the given node."))

(defclass node ()
  ((name :reader name :writer set-name)
   (net :reader net :writer set-net :initarg :net)
   (table :accessor table)
   (states :reader states :writer set-states)
   (parents :reader parents :writer set-parents)
   ;; for rapid access by index.
   (parent-vec :accessor parent-vec); :type (simple-array node *))
   (parent-indices :accessor parent-indices :type (simple-array fixnum *))
   (num-states :reader num-states)
   (index :accessor index)
   (evidence :accessor %evidence :initform -1); :type fixnum)
   (properties :initform (make-hash-table) :reader properties)))

(defun statep (node state)
  (or (and (integerp state) (>= state 0) (< state (num-states node)))
      (position state (states node))))
      
(defun num-states-1 (node)
  (length (states node)))

(defun num-nodes-1 (net)
  (hash-table-count (nodes net)))

(defun preprocess-node (node)
  "Store information for efficient lookup."
  (with-slots (parents) node
    (declare (simple-vector parents))
    (loop with parent-vec = 
	 (make-array (length parents)); :element-type 'node :initial-element node)
       with parent-indices = 
	 (make-array (length parents) :element-type 'fixnum :initial-element 0)
       for parent-name across parents
       for parent = (node parent-name (net node))
       for index = 0 then (1+ index)
       do
	 (setf (aref parent-vec index) parent)
	 (setf (aref parent-indices index) (index parent))
       finally (setf (slot-value node 'num-states) (length (states node))
		     (slot-value node 'parent-vec) parent-vec
		     (slot-value node 'parent-indices) parent-indices))))

(defun preprocess-network (net)
  "Readies the network for efficient lookup.
Sets a node ordering for the network.  Also sets node parent-vec and
parent-indices."
  (loop
     with array = (make-array (hash-table-count (nodes net)))
     with node-vec = (make-array (length array)); :element-type 'node
					;:initial-element (make-instance 'node))
     for name being each hash-key in (nodes net)
     for node being each hash-value in (nodes net)
     for index = 0 then (1+ index)
     do 
       (setf (aref array index) name)
       (setf (aref node-vec index) node)
       (setf (index node) index)
     finally
       (setf (slot-value net 'num-nodes) (length node-vec))
       (set-node-order array net)
       (setf (node-vec net) node-vec)
       (loop for node across (node-vec net) ;; now do node parents
	  do (preprocess-node node))
       (return (node-order net))))

(defun num-arcs (net)
  (loop
     for node being each hash-value in (nodes net)
     sum (length (the simple-vector (parents node)))))

(defgeneric node (name/index net/join-tree)
  (:documentation
   "Retrieve the node represented by a keyword name or index from the
given net or join-tree."))

(defmethod node ((name symbol) (net net))
  (gethash name (nodes net)))

(defmethod node ((index fixnum) (net net))
  (with-slots (node-vec) net
    ;(declare (type (simple-array node *) node-vec))
    (aref node-vec index)))

(defun table-lookup (table indices)
  (apply #'aref table indices))

;; evidence
(defgeneric clear-evidence (object)
  (:documentation "Clear all evidence from object."))

(defmethod clear-evidence ((node node))
  (setf (%evidence node) -1))

(defmethod clear-evidence ((net net))
  (loop
     for node being each hash-value in (nodes net)
     do (clear-evidence node)))

(defgeneric evidence-index (object)
  (:documentation
   "Returns the evidence in a node as a state index and nets as a
vector of state indices. -1 indicates no evidence."))

(defmethod evidence-index ((node node))
  (%evidence node))

(defmethod evidence-index ((net net))
  (loop
     for node being each hash-value in (nodes net)
     for evidence = (evidence-index node)
     when evidence nconc (list (name node) evidence)))

(defgeneric evidence (object)
  (:documentation
   "Returns the evidence in a node as its state symbol (nil if no
evidence), and for nets and join-trees a plist of node-state
pairs. Settable."))

(defmethod evidence ((node node))
  (let ((evidence (%evidence node)))
    (declare (fixnum evidence))
    (unless (< evidence 0)
      (svref (states node) evidence))))

(defmethod evidence ((net net))
  (loop
     for node being each hash-value in (nodes net)
     for evidence = (evidence node)
     when evidence nconc (list (name node) evidence)))

(defgeneric add-evidence (object evidence)
  (:documentation
   "Sets the evidence in a node via state index or symbol. For a net
or join-tree, adds the evidence as a plist of node-state pairs."))

(defmethod add-evidence ((net net) (evidence list))
  (when evidence
    (destructuring-bind (name state &rest next) evidence
      (setf (evidence (or (node name net)
			  (error "add-evidence: Could not find node ~A in net!" name)))
	    state)
      (add-evidence net next)))
  evidence)

(defmethod add-evidence (object evidence)
  (setf (evidence object) evidence))

(defgeneric (setf evidence) (evidence object)
  (:documentation 
   "Sets the evidence in a node via state index or symbol. For a net
or join-tree, sets the evidence as either a plist of node-state pairs,
or a vector of state indices (or -1 for unobserved) corresponding to
the net's node-order."))

(defmethod (setf evidence) ((null null) (node node))
  (setf (%evidence node) -1))

(defmethod (setf evidence) ((index integer) (node node))
  (setf (%evidence node) index))

(defmethod (setf evidence) ((state symbol) (node node))
  (setf (%evidence node) (position state (states node)))
  state)

(defmethod (setf evidence) ((evidence list) (net net))
  (clear-evidence net)
  (add-evidence net evidence))

(defmethod (setf evidence) ((evidence vector) (net net))
  "Sets net's evidence according to net-state vec."
  (loop
     for name across (node-order net)
     for ev across evidence
     for node = (node name net)
     do (setf (evidence node) ev)))

(defun %evidence-1 (net array)
  ;(declare (type (array (array fixnum *))))
  (let ((nodes (node-order net)))
    (dotimes (i (vlength nodes) array)
      (setf (aref array i) (evidence-index (node (aref nodes i) net))))))

(defgeneric evidence-1 (object))

(defmethod evidence-1 ((net net))
  (%evidence-1 net (make-array (num-nodes net)))); :element-type 'fixnum)))

(defmacro save-evidence (object &body body)
  (with-gensyms (gobject gstore)
    `(let* ((,gobject ,object)
	    (,gstore (evidence-1 ,gobject)))
       (unwind-protect (progn ,@body)
	 (setf (evidence ,gobject) ,gstore)))))

(defmacro with-evidence ((net &rest evidence) &body body)
  "Saves the existing evidence in the net and sets the evidence to
evidence. Restores the net to its previous state after leaving the
with-evidence block."
  (let ((gnet (gensym)))
    `(let ((,gnet ,net))
       (save-evidence ,gnet
	 (setf (evidence ,gnet) ',evidence)
	 ,@body))))

(defgeneric %query (net query)
  (:documentation "Query a net/node."))

(defmethod %query ((node node) (query integer))
  (when (and (>= query 0) (< query (num-states node)))
    (let ((old (evidence-index node)))
      (unwind-protect (/ (progn (setf (evidence node) query)
				(%query (net node) nil))
			 (progn (setf (evidence node) nil)
				(%query (net node) nil)))
	(setf (evidence node) old)))))

(defmethod %query ((node node) (query symbol))
  (awhen (position query (states node))
    (%query node it)))

(defmethod %query ((node node) (query null))
  (let ((old (evidence-index node)))
    (unwind-protect
	 (let ((probs (make-array (num-states node))); :element-type 'double-float))
	       (total 0.0d0))
	   (dotimes (i (num-states node))
	     (let ((val (progn (setf (evidence node) i)
			       (%query (net node) nil))))
	       (incf total val)
	       (setf (aref probs i) val)))
	   (dotimes (i (num-states node) probs)
	     (setf (aref probs i) (/ (aref probs i) total))))
      (setf (evidence node) old))))

(defmethod %query ((node node) (query list))
  (let ((old (evidence-index node)))
    (unwind-protect
	 (let ((probs (make-array (num-states node))); :element-type 'double-float))
	       (total 0.0d0))
	   (dotimes (i (num-states node))
	     (let ((val (%query (net node) (list* (name node) i query))))
	       (incf total val)
	       (setf (aref probs i) val)))
	   (dotimes (i (num-states node) probs)
	     (setf (aref probs i) (/ (aref probs i) total))))
      (setf (evidence node) old))))


(defmethod %query ((net net) (query vector))
  (funcall (compiled net) query))

(defmethod %query ((net net) (query symbol))
  (awhen (node query net)
    (%query it nil)))


(defmethod %query ((net net) (query list))
  (with-evidence (net)
    (add-evidence net query)
    (%query net nil)))

(defmethod %query ((net net) (query null))
  (funcall (compiled net) (evidence-1 net)))

(defun query (object &optional query)
  "Queries a net or node object.

node only        - A probability vector for the states of node.

node and integer - The probability the node is in that numbered state
                   (according to (states node)).

node and symbol  - The probability the node is in that state.

node and list -    A probability vector for the states of node given
                   list, which is a plist of node-state pairs.

net only         - The joint probability for the whole net.

net and vector   - The joint probability for the whole net, given the
                   vector. Vector has the num-nodes length, and
                   contains either a state number for each node, or -1
                   if the node state is unknown.

net and symbol   - A probability vector for the states of the node
                   designated by symbol.

net and list     - The joint probability for the whole net, given list,
                   which is a plist of node-state pairs.
"
  (%query object query))

(defun gen-parameter (net net-state node-num)
  "Generate a parameter for the net with the given net-state and node-num."
  (declare (optimize (speed 3) (safety 0))
	   (type (mod #.most-positive-fixnum) node-num))
  (loop with node = (node node-num net)
     and state = (aref net-state node-num)
     for parent-index across (the (simple-array fixnum *) (parent-indices node))
     ;; collect the state for this parent.
     collect (svref net-state parent-index)
     into dims
     finally (return (coerce (svref (apply #'aref (table node) dims) state) 
			     'double-float))))
