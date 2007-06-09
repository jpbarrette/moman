(defpackage :com.rrette.finenight
  (:use "COMMON-LISP")
  (:nicknames "finenight")
  (:export "make-node"
	   "make-empty-node"
	   "node-walk"
	   "node-add-edge!"
	   "node-remove-edge!"
	   "node-remove-dsts-for-input!"
	   "node-transition"
	   "make-fsa"
	   "make-empty-fsa"
	   "accept?"))


(in-package :com.rrette.finenight)
(provide :com.rrette.finenight.fsa)

(require :com.rrette.finenight.utils "utils.lisp")


;; the node consists of a label and a map a symbol to 
;; a destination object. 
(defstruct (node (:copier nil))
  label 
  (symbols-map (make-hash-table :test 'equal))
  (final nil))


(defun make-empty-node (label)
    (make-node label))

(defun node-arity (node)
    (hash-table-size (node-symbols-map node)))

(defun node-walk (node proc)
    (maphash proc (node-symbols-map node)))

(defun node-add-edge! (node input-symbol dst-node)
  (hash-table-update! (lambda (lst)
			(cons dst-node lst))
		      input-symbol 
		      (node-symbols-map node)))

(defun node-remove-edge! (node input-symbol dst-node)
  (let ((symbols-map (node-symbols-map node)))
    (if (< 1
	   (length (gethash input-symbol symbols-map)))
	(hash-table-update! (lambda (lst)
			      (delete dst-node lst))
			    input-symbol 
			    symbols-map)
      (remhash input-symbol symbols-map))
    node))

;; (define node-remove-dst!
;;   (lambda (node dst-node)
;;     (let ((symbols-map (node-symbols-map node)))
;;       (map (lambda (symbol)
;; 	     (hash-table-update!/default symbols-map 
;; 				 symbol 
;; 				 (lambda (lst)
;; 				   (delete! dst-node lst eq?))
;; 				 '()))
;; 	   (node-symbols node)))
;;     node))

(defun node-remove-dsts-for-input! (node input)
  (let ((symbols-map (node-symbols-map node)))
    (remhash input symbols-map)
    node))


;; will return the list of destination nodes for the
;; given node.
(defun node-transition (node symbol)
    (gethash symbol (node-symbols-map node)))


;; (define node-is-equivalent
;;   (lambda (lhs rhs)
;;       (if (not (eq? (node-final lhs) (node-final rhs)))
;; 	  #f
;;           (let ((lhs-map (node-symbols-map lhs))
;;                 (rhs-map (node-symbols-map rhs)))
;;             (map-equal? lhs-map rhs-map)))))
		  


(defstruct (fsa (:copier nil))
  start-node)

(defun make-empty-fsa (start-label)
  (make-fsa :start-node (make-empty-node start-label)))

(defun accept? (fsa word)
  (labels ((T (node word)
	      (if (null word) 
		  (node-final node)
		(let ((nodes (node-transition node (car word))))
		  (if (null nodes)
		      nil
		    (T (car nodes) (cdr word)))))))
	  (T (fsa-start-node fsa) word)))


;(define-record-printer (fsa x out)
;  (fprintf out
;           "(fsa ~S ~S ~S)"
;	   (fsa-initial-state x) (fsa-finals x) (hash-table->alist (fsa-nodes x))))

  

  

