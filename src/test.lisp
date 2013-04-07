;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test - Internal test tools to make random networks and evidence.
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

(defun random-graph (num-nodes num-arcs &key ensure-connected)
  (declare (fixnum num-nodes num-arcs))
  ;; make graph
  (let ((graph (make-array num-nodes :initial-element nil)))
    (when (> num-arcs 0)
      (loop with i = 0
	 for a = (random num-nodes)
	 for b = (random num-nodes)
	 do (unless (or (= a b) (neighbour graph a b))
	      (connect graph a b) (incf i))
	 until (= i num-arcs)))
    (when ensure-connected
      (loop for roots = (roots graph)
	 while (cdr roots)
	 for shuffle = (shuffle (length roots))
	 do (connect graph (nth (aref shuffle 0) roots)
		     (nth (aref shuffle 1) roots))))
    graph))

(defun random-prob (num-states)
  (declare (fixnum num-states))
  (loop with prob = (make-array num-states :element-type 'float :initial-element 0.0)
     with total = 0.0d0
     for i from 0 to (1- num-states)
     do (incf total (setf (aref prob i) (random 1.0)))
     finally 
       (dotimes (j num-states) (setf (aref prob j) (/ (aref prob j) total)))
       (return prob)))

(defun random-table (node)
  (loop
     with net = (net node)
     with parent-states = 
       (map 'list (lambda (x) (num-states-1 (node x net))) (parents node))
     with combs = (reduce #'* parent-states)
     with num-states = (num-states-1 node)
     with prob-vec = (make-array combs)
     for i from 0 to (1- combs)
     do (setf (aref prob-vec i) (random-prob num-states))
     finally (return (make-array parent-states
				 :displaced-to prob-vec))))

(defun random-network (max-states &rest random-graph-args)
  ;; make net
  (loop with graph = (apply #'random-graph random-graph-args)
     with net = (make-instance 'net)
     with total-order = (shuffle (length graph))
     for i of-type fixnum across total-order
     for node = (make-instance 'node :net net)
     do
       (setf (slot-value node 'name) (intern (format nil "V~A" i) :keyword))
       (setf (slot-value node 'parents) 
	     (map 'vector (lambda (x) (intern (format nil "V~A" x) :keyword))
		       (aref graph i)))
       (setf (slot-value node 'states)
	     (coerce (let ((min (if (consp max-states) (first max-states) 2))
			   (max (if (consp max-states) (second max-states)
				    max-states)))
		       (map-int #'identity (+ min (random (1+ (- max min))))))
		     'vector))
       (setf (gethash (name node) (nodes net)) node)
       (delete-node graph i)
     finally
       (loop
	  for node being each hash-value in (nodes net)
	  do (setf (table node) (random-table node)))
       (return net)))

(defun net-graph (net)
  "Output the net's structure in a viewable form."
  (coerce
   (loop for i from 0 to (1- (num-nodes net))
      collect (coerce (parent-indices (node i net)) 'list))
   'vector))

(defun test-net-random (net &optional (iterations 100))
  (assert (compiled net) () "Choose a compilation type for the net.")
  (let (probs 
	(net-spec (make-array (num-nodes net) 
			      :element-type 'fixnum))
	(array (returnit (make-array (num-nodes net))
		 (dotimes (i (num-nodes net))
		   (setf (aref it i) i)))))
    (dotimes (i iterations probs)
      (push (%query net (random-evidence net net-spec array)) probs))))
		    
(defun random-evidence (net &optional 
			(net-spec (make-array (num-nodes net) 
					      :element-type 'fixnum))
			(arr (returnit (make-array (num-nodes net)
						   :element-type 'fixnum)
			       (dotimes (i (num-nodes net))
				 (setf (aref it i) i)))))
  (shuffle arr)
  (dotimes (i (vlength net-spec)) (setf (aref net-spec i) -1))
  (dotimes (j (random (num-nodes net)))
    (setf (aref net-spec (aref arr j))
	  (random (num-states (node (aref arr j) net)))))
  net-spec)

(defun junction-p (cliques tree)
  "Test whether the tree is in fact a junction tree, meaning for each
pair of cliques, all cliques in between contain their intersection."
  (dotimes (i (1- (length cliques)) t)
    (loop for j from (1+ i) to (1- (length cliques))
       for intersection = (intersection (aref cliques i) (aref cliques j))
       do (loop for clique-num in (path tree i j) ;; inefficiently checks node j.
	     for clique = (aref cliques clique-num)
	     do (dolist (node intersection)
		  (unless (member node clique)
		    (return-from junction-p nil)))))))
