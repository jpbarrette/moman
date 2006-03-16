(defpackage "FINENIGHT"
  (:use "COMMON-LISP")
  (:nicknames "fn")
  (:export "add-edge"
	   "nadd-edge"))

(load "utils.lisp")
(load "edge.lisp")

(defstruct node
  name
  (symbols (make-hash-table))
  edges
  :copier copy-node)

;;;This function returns a copy of the given node.
(defun copy-node (node)
  (make-node :name (node-name node)
	     :symbols (copy-hash-table (node-symbols node))
	     :edges (copy-list (node-edges node))))

;;;This function will return a node built with the edges.
;;;The name of the node will be the source of the first edge.
(defmethod build-node (edges)
  (step (reduce #'(lambda (node edge)
		    (add-edge edge node))
		(cons nil edges))))
  

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
    (setf (gethash (edge-symbol edge) symbols) 
	  (cons edge (gethash (edge-symbol edge) symbols)))
    n))

;;;This will return the destination state for
;;;the given input.
(defmethod transition (input node)
  (gethash input (node-symbols node)))


