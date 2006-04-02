(defpackage :com.rrette.finenight
  (:use "COMMON-LISP")
  (:nicknames "finenight")
  (:export "node-transition"
	   "build-node"
	   "nadd-edge"
	   "add-edge"))

(in-package :com.rrette.finenight)
(provide :com.rrette.finenight.node)

(require :com.rrette.finenight.edge "edge.lisp")
(require :com.rrette.finenight.utils "utils.lisp")


(defstruct node
  name
  (symbols (make-hash-table :test 'equal))
  edges
  epsilons
  :copier copy-node)

;; (defmethod node-access (label (node node))
;;   (some (lambda (edge)
;; 	  (if (equal (edge-destination edge)
;; 		     label)
;; 	      t))))

(defun copy-node (node)
  (make-node :name (node-name node)
	     :symbols (copy-hash-table (node-symbols node) :test 'equal)
	     :epsilons (copy-list (node-epsilons node))
	     :edges (copy-list (node-edges node))))


;;;This function will return a node built with the edges.
;;;The name of the node will be the source of the first edge.
(defmethod build-node (edges)
  (reduce #'(lambda (node edge)
		    (add-edge edge node))
	  edges
	  :initial-value (make-node)))
  

;;;This function will return a new node (a copy) with the edge added.
(defmethod add-edge (edge (node node))
  (nadd-edge edge (copy-node node)))

;;;This function will return a new node with the edge added.
;;;The name of the node will be the source of the edge.
(defmethod add-edge (edge (node (eql ())))
  (nadd-edge edge (make-node 
		   :name (edge-source edge))))
   
;;;This function will add an edge to the current node
(defmethod nadd-edge (edge (n node))
  (let* ((edge (edgify edge))
	 (symbols (node-symbols n))
	 (edges (node-edges n)))
    (setf (node-edges n) (cons edge edges))
    (if (null (edge-symbol edge))
	(setf (node-epsilons n) (cons edge (node-epsilons n)))
      (setf (gethash (string (edge-symbol edge)) symbols) 
	    (cons edge (gethash (string (edge-symbol edge)) symbols))))
    n))

(defmethod nremove-edge (edge (node node))
  (progn
    (setf (node-edges node) (remove (edgify edge) (node-edges node) :test #'equal))
    (setf (gethash (edge-symbol edge) (node-symbols node))
	  (remove (edgify edge) (gethash (edge-symbol edge) (node-symbols node)) :test #'equal))
    node))

(defmethod remove-edge (edge (node node))
  (let ((n (copy-node node)))
    (nremove-edge edge n)
    n))


;;;This will return the epsilons of this state
(defmethod e-close ((node node))
  (uniqueness-set (mapcar (lambda (edge)
			    (edge-destination edge))
			  (node-epsilons node))))

(defmethod e-close ((ignore node))
  nil)

;;;This will return the destination state for
;;;the given input.
(defmethod node-transition (input (node node))
  (uniqueness-set
   (mapcar (lambda (edge)
	     (edge-destination edge))
	   (gethash (string input) (node-symbols node)))))

(defmethod node-transition (input node)
  nil)

(defun edges-are-equivalent (lhs-edges rhs-edges)
  (and (equal (length lhs-edges)
	      (length rhs-edges))
       (every (lambda (lhs-edge rhs-edge)
		(equal (cdr lhs-edge)
			   (cdr rhs-edge)))
	      lhs-edges 
	      rhs-edges)))

(defun are-equivalent (lhs-label rhs-label fsa)
  "Returns nil if they are not equivalent, return the rhs-label otherwise"
  (let ((lhs (fsa-node lhs-label fsa))
	(rhs (fsa-node rhs-label fsa)))
    (if (and (equal (is-final lhs-label fsa)
		    (is-final rhs-label fsa))
	     (edges-are-equivalent (node-edges lhs)
				   (node-edges rhs)))
	rhs-label)))