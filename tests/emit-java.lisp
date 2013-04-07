;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; emit-java - Write out java source code from Arithmetic Circuit
;; instructions.
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

;; this is copied from hunchentoot/test/test.lisp, used to locate static files.
(defvar *emit-java-file* (load-time-value
			   (or #.*compile-file-pathname* *load-pathname*)))

(defun emit-java-double (double)
  (let ((str (write-to-string double)))
    (concatenate 'string (nsubstitute #\E #\d str) "d")))

(defun emit-java-header (package-name class-name stream)
  "Write out the package and class names for the net."
  (format stream "// Created using the cl-bayesnet package.~%")
  (format stream "package ~A;~%" package-name)
  (write-line "import java.util.Hashtable;" stream)
  (format stream "public class ~A implements bn.java.BN {~%" class-name)
  (format stream "    public ~A() {~%" class-name)
  (with-open-file (resource (merge-pathnames "java/BN.resource"
					     (directory-namestring *emit-java-file*)))
    (loop for line = (read-line resource nil nil)
       while line do (write-line line stream))))

(defun emit-java-num-nodes (num stream)
  "public abstract int getNumNodes();"
  (format stream "public int getNumNodes() { return ~A; }~%" num))

(defun emit-java-num-states (state-list stream)
  "public abstract int getNumStates(int node);"
  (let ((*standard-output* stream))
    (format t "private int[] numStates = new int[] { ~{~a~^, ~} };~%" state-list)
    (write-line "public int getNumStates(int node) { return numStates[node]; }")
    nil))

(defun emit-java-node-names (names stream)
  "public abstract java.lang.String[] getNodeNames();"
  (let ((*standard-output* stream))
    (write-line "private java.lang.String[] nodeNames =")
    (format t "  new java.lang.String[] { ~{~s~^, ~} };~%" names)
    (write-line "public java.lang.String[] getNodeNames() { return nodeNames; }")
    nil))

(defun emit-java-state-names (names-list stream)
  "public abstract java.lang.String[] getStateNames(int node);"
  (let ((*standard-output* stream))
    (write-line "private java.lang.String[][] stateNames =")
    (write-string "  new java.lang.String[][] { ")
    (format t "~{~%    new java.lang.String[] { ~{~s~^, ~} }~^, ~} };~%" names-list)
    (write-line
     "public java.lang.String[] getStateNames(int node) { return stateNames[node]; }")
    nil))
  
(defun emit-java-query (forms stream)
  "public abstract double query(int[] netspec);"
  (write-line "public double query(int[] netspec) {" stream)
  (loop
     for inst in forms
     for var = 0 then (1+ var)
     for rhs = (if (numberp (car inst))
		   (format nil "match(netspec[~A], ~A)" (car inst) (cdr inst))
		   (let ((op-string (format nil " ~A " (car inst))))
		     (with-output-to-string (*standard-output*)
		       (loop for cons on (cdr inst)
			  do (let ((val (car cons)))
			       (if (floatp val) (write-string (emit-java-double val))
				   (format t "v~A" val)))
			  when (cdr cons) do (write-string op-string)))))
     do
       (format stream "  double v~A = ~A;~%" var rhs)
       finally (format stream "  return v~A;~%}~%" var)))


(defun emit-java-footer (stream)
  "Write out the footer for the net."
  (write-line "}" stream))

(defun emit-java-net (net 
		      &optional (package-name "bn.java") (stream *standard-output*))
  (let ((ins (gen-instructions net)))
    (with-slots (forms form-count) ins
      (emit-java-header package-name (name net) stream)
      (emit-java-num-nodes (num-nodes net) stream)
      (emit-java-num-states (map 'list #'num-states (node-vec net))
			    stream)
      (emit-java-node-names (map 'list (lambda (x) (string-downcase (string (name x))))
				 (node-vec net)) stream)
      (emit-java-state-names 
       (map 'list (lambda (x) (map 'list (lambda (x) (string-downcase (string x)))
				   (states x))) (node-vec net)) stream)
      (emit-java-query forms stream)
      (emit-java-footer stream)
      nil)))

(defun write-java-net (file net &optional (package-name "bn.java"))
  (with-open-file (s file :direction :output)
    (emit-java-net net package-name s)))

