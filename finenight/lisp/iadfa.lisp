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


(defun last-child (node)
  (car (sort (mapcar (lambda (edge) 
		       (edge-symbol edge))
		     (node-edges node))
	     #'string>)))

(defmethod common-prefix (word (fsa fsa) 
			       &key (node (fsa-start fsa)) (prefix ""))
  (if (equal 0 (length word))
      (cons node prefix)
    (let ((next-node (car (transition (aref word 0) node fsa))))
      (if (null next-node)
	  (cons node prefix)
	(common-prefix (subseq word 1) 
		       fsa
		       :prefix (format nil "~A~A" prefix (aref word 0))
		       :node next-node)))))

;; (defun gen-iadfa (words)
;;   (let ((register (make-hash-table)))
;;     (mapcar (lambda (word)
;; 	      (let ((common-prefix (common-prefix 


