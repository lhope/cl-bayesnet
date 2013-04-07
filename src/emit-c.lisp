;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; emit-c - Write out C source code from Arithmetic Circuit
;; instructions, compile and load the resultant shared object.
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

(defun emit-c-double (double)
  (let ((str (princ-to-string double)))
    (nsubstitute #\E #\d str)))

(defun emit-c-forms (func-name net-spec forms stream)
  (loop
     for inst in forms
     for var = 0 then (1+ var)
     for rhs = (if (numberp (car inst))
		   (format nil "~Am(~A[~A], ~A)"
			   func-name net-spec (car inst) (cdr inst))
		   (let ((op-string (format nil " ~A " (car inst))))
		     (with-output-to-string (s)
		       (loop for cons on (cdr inst)
			  do (let ((val (car cons)))
			       (princ (if (floatp val) (emit-c-double val)
					  (format nil "v~A" val)) s))
			  when (cdr cons) do (princ op-string s)))))
     do
       (format stream "double v~A = ~A;~%" var rhs)))


(defun emit-c-preamble (func-name net-spec stream)
  "Write out the matching function for state and val.  Returns 1.0 if
state is negative (missing) or state = val.  Returns 0.0 otherwise."
  (format stream "double ~Am(int state, int val) {~%" func-name)
  (format stream "  if(state < 0) { return 1.0; }~%")
  (format stream "  return (state == val) ? 1.0 : 0.0;~%")
  (format stream "}~%~%double ~A(int* ~A) {~%" func-name net-spec))

(defun emit-c-net (net stream)
  (let ((ins (gen-instructions net))
	(func-name (gentemp "__BN")))
    (with-slots (forms form-count) ins
      (emit-c-preamble func-name 'NETSPEC stream)
      (emit-c-forms func-name 'NETSPEC forms stream)
      (format stream "return v~A;~%}~%" (1- form-count))
      (values func-name form-count))))

#-cl-bayesnet-no-cffi
(defparameter *gcc-format-string* "gcc -O2 -shared ~A -o ~A"
  "String to pass to format. First arg is source, second is target.")

#-cl-bayesnet-no-cffi
(defun use-compiled-c (net &optional source-location)
  "Writes out arithmetic circuit instructions to a c file, compiles it
with gcc, loads the shared object, and prepares the net to use the
loaded function. If source-location is specified, writes the c file
there. Otherwise uses a temporary file.

Returns the closure used, the raw cffi function pointer, and the
number of instructions."
  ;; TODO: c-spec ends up a dangling pointer.
  (let ((source (or source-location (find-temporary-file "" ".c")))
	(target (find-temporary-file "" ".so")))
    (multiple-value-bind (func-name form-count)
	(with-open-file (s source :direction :output :if-exists :supersede)
	  (emit-c-net net s))
      (trivial-shell:shell-command (format nil *gcc-format-string* source target))
      (unless source-location (delete-file source))
      (cffi:load-foreign-library target)
      (let* ((len (num-nodes net))
	     (c-spec (cffi:foreign-alloc :int :count len))
	     (func-pointer (cffi:foreign-symbol-pointer (princ-to-string func-name))))
	(values
	 (setf (compiled net)
	       (lambda (net-spec)
		 (dotimes (i len) (setf (cffi:mem-aref c-spec :int i)
					(aref net-spec i)))
		 (cffi:foreign-funcall-pointer func-pointer nil :pointer c-spec :double)))
	 func-pointer form-count)))))
;;(foreign-free c-spec)
