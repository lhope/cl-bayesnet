;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; evidence - implementation of the evidence API for join-tree
;; objects.
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

;;primitives
;;(defun make-join-tree (net)
;;(defun jt-propagate (jt)
;;(defun jt-prob (jt node-num)
;;(defun jt-retract (jt)
;;(defun jt-obs (jt node-num obs)

(defmethod node ((name symbol) (jt join-tree))
  (find name (nodes jt) :key #'name))

(defmethod node ((index fixnum) (jt join-tree))
  (aref (nodes jt) index))

(defmethod clear-evidence ((jt join-tree))
  (jt-retract jt))

(defmethod evidence-index ((jt join-tree))
  (evidence-1 jt))

(defmethod evidence-index ((node jt-node))
  (obs node))

(defmethod evidence-1 ((jt join-tree))
  (map 'vector #'obs (nodes jt)))

(defmethod evidence ((jt join-tree))
  (with-slots (nodes) jt
    (loop
       for node across nodes
       for obs = (obs node)
       when (>= obs 0) nconc (list (name node) (aref (states node) obs)))))

(defmethod evidence ((node jt-node))
  (with-slots (obs states) node
    (when (>= obs 0)
      (aref states obs))))

(defmethod add-evidence ((jt join-tree) (evidence list))
  (with-slots (nodes) jt
    (loop for (name state) on evidence by #'cddr
       for node-num = (if (numberp name) name
			  (position name nodes :key #'name))
       for state-num = (if (numberp state) state
			   (position state (states (aref nodes node-num))))
       do (jt-obs jt node-num state-num))))

(defmethod (setf evidence) ((evidence list) (jt join-tree))
  (clear-evidence jt)
  (add-evidence jt evidence))

(defmethod (setf evidence) ((evidence vector) (jt join-tree))
  "Sets net's evidence according to net-state vec."
  (clear-evidence jt)
  (loop
     for i = 0 then (1+ i)
     for ev across evidence
     do (when (>= ev 0) (jt-obs jt i ev))))

(defmethod (setf evidence) ((state-num integer) (node jt-node))
  (with-slots (join-tree index) node
    (jt-obs join-tree index state-num)
    state-num))

(defmethod (setf evidence) ((state symbol) (node jt-node))
  (setf (evidence node) (position state (states node))))

(defmethod %query ((jt join-tree) (query fixnum))
  (jt-propagate jt)
  (normalize (jt-prob jt query)))

(defmethod %query ((jt join-tree) (query symbol))
  (%query jt (position query (nodes jt) :key #'name)))

(defmethod %query ((jt join-tree) (query null))
  (loop for node across (nodes jt)
     for obs = (obs node)
     do (when (>= obs 0)
	  (jt-propagate jt)
	  (return (aref (jt-prob jt (index node)) obs)))
     finally (return 1)))

(defmethod %query ((jt join-tree) (query vector))
  (save-evidence jt
    (setf (evidence jt) query)
    (%query jt nil)))

(defmethod %query ((jt join-tree) (query list))
  "FIXME: should return a single prob for the joint distribution."
  (with-slots (nodes) jt
    (loop with evidence = (make-array (length nodes) :initial-element -1)
       for (name state) on query by #'cddr
       for node-num = (if (numberp name) name
			  (position name nodes :key #'name))
       for state-num = (if (numberp state) state
			   (position state (states (aref nodes node-num))))
       do (setf (aref evidence node-num) state-num)
       finally (return (%query jt evidence)))))

(defmethod %query ((node jt-node) (query integer))
  (with-slots (index join-tree states) node
    (when (and (>= query 0) (< query (length states)))
      (aref (%query join-tree index) query))))

(defmethod %query ((node jt-node) (query symbol))
  (awhen (position query (states node))
    (%query node it)))

(defmethod %query ((node jt-node) (query null))
  (with-slots (index join-tree) node
      (%query join-tree index)))


(defmethod %query ((node jt-node) (query vector))
  (with-slots (index join-tree) node
    (save-evidence join-tree
      (setf (evidence join-tree) query)
      (%query join-tree index))))

(defmethod %query ((node jt-node) (query list))
  "FIXME: should return a single prob for the joint distribution."
  (with-slots (join-tree) node
    (with-slots (nodes) join-tree
      (loop with evidence = (make-array (length nodes) :initial-element -1)
	 for (name state) on query by #'cddr
	 for node-num = (if (numberp name) name
			    (position name nodes :key #'name))
	 for state-num = (if (numberp state) state
			     (position state (states (aref nodes node-num))))
	 do (setf (aref evidence node-num) state-num)
	 finally (return (%query node evidence))))))

(defun use-join-tree (net)
  "Use a join-tree to evaluate this net. Returns the join-tree object."
  (let ((jt (make-join-tree net)))
    (setf (compiled net)
	  (lambda (net-spec)
	    (%query jt net-spec)))
    jt))
