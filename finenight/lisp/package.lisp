
(in-package :cl-user)

(defpackage :com.rrette.finenight.utils
  (:use :common-lisp)
  (:export :copy-hash-table
	   :equal-set
	   :for-each-line-in-file
	   :generate-name
	   :hash-table-ref/default
	   :hash-table-update!
	   :hash-table-update!/default
	   :hash-values
	   :hash-keys
	   :uniqueness-set
	   :vector-walk
	   :with-syms))

(defpackage :com.rrette.finenight.fsa
  (:use :common-lisp
	:com.rrette.finenight.utils)
  (:export :accept?
	   :fsa-start-node
	   :node-add-edge!
	   :node-arity
	   :node-reset
	   :node-destinations
	   :node-edges
	   :node-final
	   :node-label
	   :node-remove-edge!
	   :node-remove-dsts-for-input!
	   :node-symbols
	   :node-transition
	   :node-walk
	   :make-empty-fsa
	   :make-empty-node
	   :make-fsa
	   :make-node
	   :extract-words))

(defpackage :com.rrette.finenight.fsa-builder
  (:use :common-lisp
	:com.rrette.finenight.utils
	:com.rrette.finenight.fsa)
  (:export :build-fsa
	   :build-fsa-builder-with-nodes
	   :copy-fsa-builder
	   :fsa-add-edge!
	   :fsa-add-final!
	   :fsa-add-final-node!
	   :fsa-add-node!
	   :fsa-builder
	   :fsa-builder-accept?
	   :fsa-builder-finals
	   :fsa-builder-initial-state
	   :fsa-builder-nodes
	   :fsa-builder-p
	   :fsa-edges
	   :fsa-initial-node
	   :make-fsa-builder
	   :fsa-remove-edge!
	   :fsa-remove-node!
	   :graphviz-export
	   :graphviz-export-to-file
	   :make-empty-fsa-builder
	   :make-fsa-builder
	   :make-fsa-builder-from-fsa))

(defpackage :com.rrette.finenight.iadfa
  (:use :common-lisp 
	:com.rrette.finenight.fsa
	:com.rrette.finenight.fsa-builder
	:com.rrette.finenight.utils)
  (:export :add-edge
	   :build-fsa
	   :build-fsa-from-ancestrors
	   :build-iadfa
	   :debug-gen-iadfa
	   :debug-gen-iadfa-from-file
	   :detect-problems-from-file
	   :gen-iadfa-from-file
	   :iadfa-fsa
	   :iadfa-state-ancestrors
	   :make-fsa-builder-from-fsa
	   :nadd-edge
	   :print-stats
	   :test-equivalence
	   :transition))
