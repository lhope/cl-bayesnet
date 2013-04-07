;;;; -*- Mode: LISP -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cl-bayesnet - ASDF system definition for the cl-bayesnet system.
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

(asdf:defsystem :cl-bayesnet
  :name "cl-bayesnet"
  :author "Lucas Hope <lucas.r.hope@gmail.com>"
  :version "0.1.0"
  :maintainer "Lucas Hope <lucas.r.hope@gmail.com>"
  :depends-on (:s-xml 
	       #-cl-bayesnet-no-cffi :trivial-shell
	       #-cl-bayesnet-no-cffi :cffi)
  :serial t
  :components ((:module :src
                :serial t
		:components ((:file "packages")
			     (:file "utils")
			     (:file "tries")
			     (:file "bn-utils")
			     (:file "bn")
			     (:file "parse-network")
			     (:file "clique-tree")
			     (:file "message")
			     (:file "evidence")
			     (:file "compiler")
			     (:file "emit-c")
			     (:file "test")))))
