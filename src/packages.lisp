;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; packages - Package and external API definition.
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

(in-package :cl-user)

(defpackage :cl-bayesnet
  (:documentation "A Common Lisp Bayesian Network Inference Engine")
  (:use :cl)
  (:nicknames :bn)
  (:export 
   ;; create
   :load-xmlbif
   :load-ace
   :load-dne
   ;; prepare
   :use-compiled-c ;; slow compile, fastest execution.
   :use-interpreted ;; slow compile, fast execution.
   :use-join-tree ;; fast compile, slow execution.
   ;; classes
   :net
   :node ;; also a reader function.
   :join-tree
   :instructions
   ;; readers
   :node-order
   :name
   :states
   :statep
   :parents
   :num-states
   :num-nodes
   ;; probability querying.
   :query
   ;; evidence
   :clear-evidence
   :evidence-index
   :evidence
   :add-evidence
   :with-evidence
   ))

