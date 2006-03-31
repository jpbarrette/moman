(defpackage :com.rrette.finenight
  (:use "COMMON-LISP")
  (:nicknames "finenight")
  (:export "add-edge"
	   "build-fsa"
	   "nadd-edge"
	   "transition"))

(in-package :com.rrette.finenight)
(provide :com.rrette.finenight.iadfa)

(require :com.rrette.finenight.fsa "fsa.lisp")
(require :com.rrette.finenight.utils "utils.lisp")

(defstruct iadfa
  (register (make-hash-table))
  (index 0)
  fsa)

(defmethod last-child ((node node))
  (car (node-transition (last-input node) node)))

(defmethod last-child (node)
  nil)

(defun last-input (node)
  (car (sort (mapcar (lambda (edge) 
		       (edge-symbol edge))
		     (node-edges node))
	     #'string>)))


(defun has-children (node fsa)
  (if (> (hash-table-count (node-symbols (fsa-node node fsa))) 0)
      t))

(defmethod common-prefix (word fsa &key (node (fsa-start fsa)) (prefix ""))
  (if (equal 0 (length word))
      (cons node prefix)
    (let ((next-node (car (transition (aref word 0) node fsa))))
      (if (null next-node)
	  (cons node prefix)
	(common-prefix (subseq word 1) 
		       fsa
		       :prefix (format nil "~A~A" prefix (aref word 0))
		       :node next-node)))))

(defun marked-as-registered (state iadfa)
  (if (gethash state (iadfa-register iadfa))
      t))
  
(defun mark-as-registered (state iadfa)
  (setf (gethash state (iadfa-register iadfa)) t))


(defun generate-state (iadfa)
  (let ((name (generate-name (iadfa-index iadfa))))
    (setf (iadfa-index iadfa) (1+ (iadfa-index iadfa)))
    name))
  

(defun build-iadfa ()
  (let ((iadfa (make-iadfa)))
    (setf (iadfa-fsa iadfa) (build-fsa '()
				 '()
				 (generate-state iadfa)
				 '()))
    iadfa))
  
(defun gen-iadfa (words)
  (progn 
    (setf iadfa (reduce (lambda (iadfa word) 
			  (handle-word word iadfa))
			words 
			:initial-value (build-iadfa)))
    (iadfa-fsa (replace-or-register (fsa-start (iadfa-fsa iadfa)) iadfa))))
  
(defun handle-word (word iadfa)
  (let* ((fsa (iadfa-fsa iadfa))
	 (common (common-prefix word fsa))
	 (common-prefix (cdr common))
	 (last-state (car common))
	 (current-suffix (subseq word (length common-prefix))))
    (if (has-children last-state fsa)
	(replace-or-register last-state iadfa))
    (add-suffix last-state current-suffix iadfa)))


(defun replace-or-register (last-state iadfa)
  (let* ((fsa (iadfa-fsa iadfa))
	 (child (last-child (fsa-node last-state fsa)))
	 (register (iadfa-register iadfa)))
    (if (marked-as-registered child iadfa)
	iadfa
      (progn
	(if (has-children child (iadfa-fsa iadfa))
	    (replace-or-register child iadfa))
	(handle-equivalent-states last-state child iadfa)
	iadfa))))

(defun registered-states (iadfa)
  (let ((states nil))
    (with-hash-table-iterator
     (my-iterator (iadfa-register iadfa))
     (loop
      (multiple-value-bind (entry-p key value)
			   (my-iterator)
			   (if entry-p
			       (setf states (cons key states))
			     (return)))))
     states))
  
	    
(defun handle-equivalent-states (state-label child-label iadfa)
  (let* ((states (registered-states iadfa))
	 (fsa (iadfa-fsa iadfa))
	 (child (fsa-node child-label fsa)))
    (setf equivalent (some (lambda (state)
			     (are-equivalent child-label state fsa))
			   (registered-states iadfa)))
    (if equivalent
	(progn 
	  (delete-branch child fsa)
	  (replace-last-child state-label equivalent fsa))
      (mark-as-registered child-label iadfa))
    iadfa))
	  

(defun replace-last-child (state-label new-child-label fsa)
  (let* ((node (fsa-node state-label fsa))
	 (input (last-input node))
	 (current-child-label (last-child node)))
    (nremove-edge (list state-label input current-child-label) fsa)
    (nadd-edge (list state-label input new-child-label) fsa)))

	
(defmethod delete-branch (child (fsa fsa))
  (if child
      (progn
	(delete-branch (last-child (fsa-node child fsa)) fsa)
	(nremove-node child fsa))
    fsa))



(defun add-suffix (last-state current-suffix iadfa)
  (let ((fsa (iadfa-fsa iadfa)))
    (reduce (lambda (fsa input)
	      (let ((new-state (generate-state iadfa)))
		(nadd-edge (list last-state input new-state) fsa)
		(setf last-state new-state)
		fsa))
	    current-suffix
	    :initial-value (iadfa-fsa iadfa))
    (setf (fsa-finals fsa) (cons last-state (fsa-finals fsa)))
    iadfa))
