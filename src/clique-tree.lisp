;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; clique-tree - tools for clique-trees, which are intermediate
;; structures for building join-trees.
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

(defun neighbours (graph node-num)
  "Generates a list of neighbours of the node."
  (aref graph node-num))

(defun neighbour (graph a b)
  (find b (aref graph a)))

(defun connect (graph a b)
  (pushnew a (aref graph b) :test #'=)
  (pushnew b (aref graph a) :test #'=))

(defun floyd-warshall (graph)
  "This finds all shortest paths in graph.  I could make it more
efficient by using the fact that the paths are symmetric."
  ;; Assume a function edgeCost(i,j) which returns the cost of the edge from i to j
  ;; (infinity if there is none).
  ;; Also assume that n is the number of vertices and edgeCost(i,i)=0
  (let ((n (length graph)))
    (flet ((edge-cost (i j)
	     (cond ((= i j) 0)
		   ((neighbour graph i j) 1)
		   (t n))))
      ;; int path[][] - A 2-Dimensional matrix. At each step in the
      ;; algorithm, path[i][j] is the shortest path from i to j using
      ;; intermediate values in (1..k-1).  Each path[i][j] is
      ;; initialized to edgeCost(i,j).
      (let ((path (make-array `(,n ,n) :element-type 'fixnum :initial-element 0)))
	(dotimes (i n)
	  (dotimes (j n) (setf (aref path i j) (edge-cost i j))))
	;; procedure FloydWarshall ()
	(dotimes (k n path)
	  (dotimes (i n)
	    (dotimes (j n)
	      (unless (or (= i j) (= i k) (= j k))
		(setf (aref path i j)
		      (min (aref path i j)
			   (+ (aref path i k) (aref path k j))))))))))))

(defun moral (net)
  (loop with moral = (make-array (length (node-order net)) :initial-element nil)
     for i from 0 to (1- (length moral))
     for node across (node-vec net)
     do
       (loop for par across (parent-indices node)
	  do (connect moral i par)
	    (loop for par2 across (parent-indices node)
	       do (unless (= par par2) ; inefficient
		    (connect moral par par2))))
     finally (return moral)))

(defun cluster-node (graph node)
  "Generates a list of added links (node1 . node2) which are necessary
to form a cluster around node.  Uses an (edge-tree)."
  (let (cluster)
    (map-pairs (lambda (a b)
		 (unless (neighbour graph a b)
		   (pushnew  (if (< a b) (cons a b) (cons b a)) cluster)))
	       (aref graph node))
    cluster))

(defun weight-cluster (net graph node)
  "a cluster's weight is the product of its nodes' states."
  (let ((val (num-states (node node net))))
    (dolist (neighbour (aref graph node) val)
      (setf val (* val (num-states (node neighbour net)))))))

(defun delete-node (graph node)
  "Remove references to the node from graph.  Replaces the node with t"
  (dolist (neighbour (aref graph node))
    (setf (aref graph neighbour) (delete node (aref graph neighbour))))
  (setf (aref graph node) nil))

(defun gen-cliques (net graph)
  "Graph is an array of (itree), where each tree contains that node's neighbours.
Returns a list of cliques obtained from triangulating the graph. The
  triangulation can be recovered by starting with a moral graph and
  ensuring each clique is completely connected.  Each clique is an itree
  of nodes. Destroys graph."
  (let ((candidates (map-int #'identity (length graph)))
	cliques)
    (flet ((clique< (node1 node2)
	     (let ((cluster1 (cluster-node graph node1))
		   (cluster2 (cluster-node graph node2)))
	       (or (< (length cluster1) (length cluster2))
		   (and (= (length cluster1) (length cluster2))
			(< (weight-cluster net graph node1)
			   (weight-cluster net graph node2)))))))
      (while candidates
	(setf candidates (sort candidates #'clique<))
	(let* ((best (pop candidates)) 
	       (clique (copy-list (aref graph best))))
	  (push best clique)
	  (unless (member-if (lambda (x) (subsetp clique x))
			     cliques)
	    (push clique cliques))
	  (dolist (edge (cluster-node graph best))
	    (connect graph (car edge) (cdr edge)))
	  (delete-node graph best)))
      (setf cliques (coerce (nreverse cliques) 'vector))
      (dotimes (i (length cliques) cliques) ;; sort the cliques.
	(setf (aref cliques i) (sort (aref cliques i) #'<))))))

(defun sep-mass (a b)
  "Find |intersection| of lists clique1 and clique2"
  (length (intersection a b)))

(defun sep-cost (a b states)
  "Find prod states of x in X + prod states of y in Y.  clique1
and clique2 are lists, states is a vector."
  (+ (reduce #'* a :key (lambda (x) (aref states x)))
     (reduce #'* b :key (lambda (x) (aref states x)))))

(defun sep-sets (cliques states)
  (sort (map-pairs #'cons (map-int #'identity (length cliques)))
	(lambda (x y)
	  (let ((x-mass (sep-mass (aref cliques (car x)) (aref cliques (cdr x))))
		(y-mass (sep-mass (aref cliques (car y)) (aref cliques (cdr y)))))
	    (cond ((> x-mass y-mass))
		  ((= x-mass y-mass) ;; below is still slow. Could cache.
		   (< (sep-cost (aref cliques (car x)) 
				(aref cliques (cdr x)) states)
		      (sep-cost (aref cliques (car y)) 
				(aref cliques (cdr y)) states))))))))

(defun path (graph a b)
  (labels ((%path (graph a b path)
	     (when (member b (aref graph a) :test #'=) (return-from path path))
	     (dolist (next (aref graph a))
	       (when (not (member next path :test #'=))
		 (awhen (%path graph next b (cons next path))
		   (return-from path it))))))
    (%path graph a b (list a))))

(defun cycle (graph a)
  (labels ((%path (graph a b path)
	     (when (member b (aref graph a) :test #'=) (return-from %path path))
	     (dolist (next (aref graph a))
	       (when (not (member next path :test #'=))
		 (awhen (%path graph next b (cons next path))
		   (return-from %path it))))))
    (dolist (next (aref graph a))
      (dolist (next-next (aref graph next))
	(unless (= next-next a)
	  (%path graph next-next a (list next-next next a)))))))

(defun roots (graph)
  "Returns the roots of the graph as a list."
  (loop with roots = (list 0)
     for i from 1 to (1- (length graph))
     do (unless (dolist (root roots) ;; unless i is connected to a root...
		  (when (path graph root i) (return t)))
	  (push i roots))
     finally (return roots)))

(defun clique-tree (net)
  "Builds a clique-tree for dag.  The tree is represented by an
upper triangular matrix which has lists of clique-nodes in its
diagonal."
  (let* ((cliques (gen-cliques net (moral net)))
	 (len (length cliques))
	 (states (returnit (make-array (num-nodes net)); :element-type 'fixnum)
		   (dotimes (i (num-nodes net))
		     (setf (aref it i) (num-states (node i net))))))
	 (tree (make-array len :initial-element nil))
	 (sep-sets (sep-sets cliques states))
	 (edge-number 0))
    (while (and (< edge-number len) sep-sets)
      (let ((sep-set (pop sep-sets)))
	(unless (path tree (car sep-set) (cdr sep-set))
	  (connect tree (car sep-set) (cdr sep-set))
	  (incf edge-number))))
    (values cliques tree)))

(defun assign-nodes (net cliques)
  "Returns an assignment of a clique node for each net node.  Since
lookup within a clique is sequential, just uses an array of lists.  An
array of assignments from the node perspective is returned as a second
value."
  (loop
     with assignment = 
       (make-array (length cliques) :initial-element nil)
     with node-assignment =
       (make-array (num-nodes net) :initial-element 0)
     for node-num = 0 then (1+ node-num)
     for node across (node-vec net)
     for family = (cons node-num (coerce (parent-indices node) 'list))
     do (dotimes (clique (vlength cliques) (error "Node ~A is unassigned!" node-num))
	  (when (subsetp family (aref cliques clique))
	    (setf (aref node-assignment node-num) clique)
	    (push node-num (aref assignment clique))
	    (return)))
     finally (return (values assignment node-assignment))))

(defun traverse (tree &optional (root 0))
  "Builds a traversal for the undirected tree structure tree.  Structure is 
;(tree-node subtraversal1 ... subtraversaln)"
  (labels
      ((trav (node visited)
	 (cons node
	       (let (trav)
		 (dotimes (next-node (length tree) trav)
		   (when (and (neighbour tree node next-node) ;; they are connected
			      ;; and a node in the separator has not been assigned
			      (not (member next-node visited :test #'=)))
		     (push (trav next-node (cons node visited)) trav)))))))
    (trav root nil)))

(defun tree-depth (tree root)
  (loop with closed = (returnit (make-hash-table :test 'eql)
			(setf (gethash root it) t))
     with open = (aref tree root) with len = (length tree) 
     for depth from 0 for new-open = nil
     until (= (hash-table-count closed) len) do
       (dolist (node open)
	 (setf (gethash node closed) t))
       (dolist (node open)
	 (dolist (new-node (aref tree node)) ;; this works because it's a tree.
	   (unless (gethash new-node closed)
	     (push new-node new-open))))
       (setf open new-open)
     finally (return depth)))

(defun best-root (tree)
  (let ((list (map-int (lambda (x) (tree-depth tree x)) (length tree))))
    (multiple-value-bind (val pos) (best list #'<)
	(declare (ignore val))
      pos)))

(defun net-masks (net cliques traversal)
  "Shadowing traversal, returns (tree-node node-mask combinations) for
each tree-node in traversal."
  (with-slots (node-vec) net
    (labels ((net-mask (net-state clique)
	       (loop with combinations = 1
		  with net-mask = (make-array (num-nodes net) :initial-element nil)
		  for node-state across net-state
		  for node-num = 0 then (1+ node-num)
		  do
		    (when (and (null node-state) 
			       (member node-num clique))
		      (let ((num-states (num-states (aref node-vec node-num))))
			(setf (aref net-mask node-num) num-states)
			(setf combinations (* combinations num-states))))
		  finally (return (values net-mask combinations))))
	     (traverse (traversal net-state)
	       (multiple-value-bind (net-mask combs) 
		   (net-mask net-state (svref cliques (car traversal)))
		 (dotimes (i (length net-mask)) ;; set net-state as per net-mask
		   (when (aref net-mask i) (setf (aref net-state i) t)))
		 (prog1 (cons (list (car traversal) net-mask combs)
			      (loop for trav in (cdr traversal)
				 collect (traverse trav net-state)))
		   (dotimes (i (length net-mask)) ;; unset net-state.
		     (when (aref net-mask i) (setf (aref net-state i) nil)))))))
      (traverse traversal (make-array (num-nodes net) :initial-element nil)))))
