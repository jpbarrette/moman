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

;;;This function returns a copy of the given node.
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
		(cons nil edges)))
  

;;;This function will return a new node (a copy) with the edge added.
(defmethod add-edge (edge (node node))
  (let ((n (copy-node node)))
    (nadd-edge edge n)
    n))

;;;This function will return a new node with the edge added.
;;;The name of the node will be the source of the edge.
(defmethod add-edge (edge (node (eql ())))
  (nadd-edge edge (make-node 
		   :name (edge-source edge))))
   
;;;This function will add an edge to the current node
(defmethod nadd-edge (edge (n node))
  (let ((symbols (node-symbols n))
	(source (edge-source edge))
	(edges (node-edges n)))
    (setf (node-edges n) (cons edge edges))
    (if (null (edge-symbol edge))
	(setf (node-epsilons n) (cons edge (node-epsilons n)))
      (setf (gethash (edge-symbol edge) symbols) 
	    (cons edge (gethash (edge-symbol edge) symbols))))
    n))

;;;This will return the epsilons of this state
(defmethod e-close ((node node))
  (uniqueness-set (mapcar (lambda (edge)
			    (edge-destination edge))
			  (node-epsilons node))))

(defmethod e-close (node)
  nil)

;;;This will return the destination state for
;;;the given input.
(defmethod node-transition (input (node node))
  (uniqueness-set
   (mapcar (lambda (edge)
	     (edge-destination edge))
	   (gethash input (node-symbols node)))))

(defmethod node-transition (input node)
  nil)
