;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; message - join-tree and message passing implementation.
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

;; LH 2013-04-01: I don't know what these are anymore. :(
;; TODO mv gen-parameter into bn.
;; - message pass should work better with indices, so we don't have to use find.
;; - we naively get probs and enter likelihoods in the "default"
;;   cluster.  Better to use the cluster with smallest potential.
;; - Cache probabilities.
;; - When generating a probability, generate and cache other assigned
;;   probabilities for that cluster, so we don't have to loop 

(defclass potential-container ()
  ((mask :reader mask :initarg :mask)
   (potential :accessor potential :initarg :potential)))

(defclass cluster (potential-container)
  ((mark :accessor mark :initform nil)
   (clean-potential :initarg :clean-potential :reader clean-potential)))

(defclass sep-set (potential-container) ())

(defclass join-tree ()
  ((net :initarg :net :reader net)
   (nodes :initarg :nodes :reader nodes)
   (clusters :initarg :clusters :reader clusters)
   (links :initarg :links :reader links)
   (all-sep-sets :initarg :sep-sets :reader all-sep-sets)
   (consistent :initform nil :accessor consistent)
   (assignment :initarg :assignment :reader assignment)
   (obs :initarg :obs :accessor obs)
   (evidence :initarg :evidence :reader evi)))

(defclass jt-node ()
  ((index      :initarg :index      :reader index)
   (assignment :initarg :assignment :reader assignment)
   (evidence   :initarg :evidence   :accessor evi)
   (obs        :initarg :obs        :accessor obs)
   (name       :initarg :name       :reader name)
   (states     :initarg :states     :reader states)
   (join-tree  :initarg :join-tree  :accessor join-tree)))

(defun make-potential (net cluster assignment)
  (loop with mask = (returnit (make-array (num-nodes net) :initial-element nil)
		      (map nil (lambda (node)
				 (setf (aref it node) (num-states (node node net))))
			   cluster))
     with state = (make-array (length mask)  :initial-element 0)
     for probs  = (mapcar (lambda (x) (gen-parameter net state x)) assignment)
     repeat (reduce #'* cluster :key (lambda (x) (num-states (node x net))))
     collect (reduce #'* probs) into potential
     do
       (cpt-incf state mask)
     finally (return (values (coerce potential 'vector) mask))))

(defun make-join-tree (net)
  (unless (slot-boundp net 'node-order) (preprocess-network net))
  (multiple-value-bind (cliques tree) (clique-tree net)
    (multiple-value-bind (assignment node-assignment) (assign-nodes net cliques)
      (let ((clusters ;; generate the clusters.
	     (make-array (length cliques)
			 :initial-contents (loop for clique across cliques for clique-assign across assignment
					      collect (multiple-value-bind (potential mask)
							  (make-potential net clique clique-assign)
							(make-instance 'cluster :mask mask :clean-potential potential :potential (copy-seq potential)))))))
	;; Add tree information.  Links contains sets of cluster-num-sep-set pairs.
	(loop with sep-sets with links = (make-array (length cliques) :initial-element nil)
	   for link    across tree
	   for clique  across cliques
	   for cluster across clusters
	   for counter from   0
	   do
	     (setf (aref links counter)
		   (loop for link-num in link	collect 
			(cons link-num (if (> link-num counter) ;; make the separator
					   (multiple-value-bind (potential mask)
					       (make-potential net (intersection clique (aref cliques link-num)) nil)
					     (returnit (make-instance 'sep-set :mask mask :potential potential)
					       (push it sep-sets)))
					   ;; else find the existing separator and use that.
					   (cdr (find counter (aref links link-num) :key #'car))))))
	   finally
	     (let ((nodes (loop for assign across node-assignment
			     for index from 0
			     for node = (node index net)
			     collect (make-instance 'jt-node 
						    :index index
						    :assignment assign
						    :evidence (make-array (num-states node)
									  :initial-element 1)
						    :obs -1
						    :name (name node)
						    :states (states node)))))
	       (return (returnit (make-instance 'join-tree :clusters clusters :links links
						:sep-sets sep-sets ;; :net net
						:nodes (coerce nodes 'vector)
						:assignment node-assignment)
			 (loop for node across (nodes it)
			    do (setf (join-tree node) it))))))))))

(defun marginalise (sep-set cluster)
  "Return a new potential for sep-set to match cluster."
  (with-slots (mask potential) cluster
    (loop with sep-mask = (mask sep-set)
       with sep-pot = (make-array (length (potential sep-set))
				     :initial-element 0)
       with state = (make-array (length mask)
				 :initial-element 0)
       repeat (length potential)
       do (incf (aref sep-pot (cpt-index sep-mask state))
		(aref potential (cpt-index mask state)))
	 (cpt-incf state mask)
       finally (return sep-pot))))

(defgeneric message-pass (jt clique1 clique2))

(defmethod message-pass ((jt join-tree) (clique1 fixnum) (clique2 fixnum))
  (with-slots (clusters links) jt
    (with-slots (mask potential) (aref clusters clique2)
      (loop
	 with sep-set = (cdr (find clique2 (aref links clique1)
				   :key #'car))
	 with sep-mask = (mask sep-set)
	 with old = (potential sep-set)
	 with new = (marginalise sep-set (aref clusters clique1))
	 with state = (make-array (length mask)
				  :initial-element 0)
	 for c-index = (cpt-index mask state)
	 for s-index = (cpt-index sep-mask state)
	 repeat (length potential)
	 do
	   (multf (aref potential c-index)
		  (if (= 0 (aref new s-index)) 0
		      (/ (aref new s-index)
			 (aref old s-index))))
	   (cpt-incf state mask)
	 finally (return (setf (potential sep-set) new))))))

(defun collect-evidence (jt cluster)
  (with-slots (clusters links) jt
    (setf (mark (aref clusters cluster)) t)
    (loop for (cluster2 . nil) in (aref links cluster)
       do (unless (mark (aref clusters cluster2))
	    (collect-evidence jt cluster2)
	    (message-pass jt cluster2 cluster)))
    ))

(defun distribute-evidence (jt cluster)
  (with-slots (clusters links) jt
    (setf (mark (aref clusters cluster)) t)
    (loop for (cluster2 . nil) in (aref links cluster)
       do (unless (mark (aref clusters cluster2))
	    (message-pass jt cluster cluster2)
	    (distribute-evidence jt cluster2)))))

(defun jt-propagate (jt)
  (with-slots (consistent clusters) jt
    (when consistent (return-from jt-propagate))
    (loop for cluster across clusters
       do (setf (mark cluster) nil))
    (collect-evidence jt 0)
    (loop for cluster across clusters
       do (setf (mark cluster) nil))
    (distribute-evidence jt 0)
    (setf consistent t)))

(defun jt-prob (jt node-num)
  "Calculates the probability of node-num given junction tree jt."
  (let ((cluster (aref (clusters jt) (aref (assignment jt) node-num))))
    (with-slots (mask potential) cluster
      (loop 
	 with node-prob = (make-array (aref mask node-num) :initial-element 0)
	 with node-mask = (returnit (make-array (length mask) :initial-element nil)
			    (setf (aref it node-num) (aref mask node-num)))
	 with state = (make-array (length mask) :initial-element 0)
	 repeat (length potential)
	 do (incf (aref node-prob (cpt-index node-mask state))
		  (aref potential (cpt-index mask state)))
	   (cpt-incf state mask)
	 finally (return node-prob)))))

(defun jt-retract (jt)
  (with-slots (consistent clusters all-sep-sets nodes) jt
    (when (every (lambda (x) (= -1 (obs x))) nodes) (return-from jt-retract))
    (setf consistent nil)
    (loop for cluster across clusters
       do (setf (potential cluster) (copy-seq (clean-potential cluster))))
    (loop for sep-set in all-sep-sets
       do (reset-potential (potential sep-set)))
    (loop for node across nodes
       do (with-slots (obs evidence) node
	    (setf obs -1)
	    (reset-potential evidence)))))

(defun jt-obs (jt node-num obs)
  "Enter the observation that node-num is obs."
  (let ((cluster (aref (clusters jt) (aref (assignment jt) node-num))))
    (with-slots (mask potential) cluster
      (when (= obs (obs (aref (nodes jt) node-num))) (return-from jt-obs))
      (setf (consistent jt) nil)
      (loop 
	 with likelihood = (returnit (make-array (aref mask node-num) :initial-element 0)
			     (setf (aref it obs) 1))
	 with node-mask = (returnit (make-array (length mask) :initial-element nil)
			    (setf (aref it node-num) (aref mask node-num)))
	 with state = (make-array (length mask) :initial-element 0)
	 repeat (length potential)
	 for c-index = (cpt-index mask state)
	 do (multf (aref potential c-index)
		   (aref likelihood (cpt-index node-mask state)))
	   (cpt-incf state mask)
	 finally
	   (setf (obs (aref (nodes jt) node-num)) obs)
	   (setf (evi (aref (nodes jt) node-num)) likelihood)))))
