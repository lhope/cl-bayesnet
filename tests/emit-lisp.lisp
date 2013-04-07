;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; emit-lisp - Write out lisp source code from Arithmetic Circuit
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
;;;;  
;;;; The below code is obsolete.  There is not yet any point to
;;;; rewriting it, because the SBCL compiler is too slow to be relied
;;;; upon.
;;;;

(defun emit-lisp-bind (array-symbol var-array)
  (loop
     for var across var-array
     for index = 0 then (1+ index)
     collect `(,var (svref ,array-symbol ,index))))

(defun compile-lisp-net (net)
  (multiple-value-bind (forms result var-array form-count) 
      (emit-instructions net)
    (let* ((net-spec (gentemp "NET"))
	   (func `(lambda (,net-spec)
		    (let ,(emit-lisp-bind net-spec var-array)
		      (let* ,forms
			,result)))))
      (setf (compiled net) (compile nil func))
      (values func form-count))))
