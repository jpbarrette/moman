(defpackage :com.rrette.finenight
  (:use "COMMON-LISP")
  (:nicknames "finenight")
  (:export "add-edge"
	   "build-fsa"
	   "nadd-edge"
	   "transition"))

(in-package :com.rrette.finenight)
(provide :com.rrette.finenight.fsa)

(require :com.rrette.finenight.node "node.lisp")
(require :com.rrette.finenight.utils "utils.lisp")

(defstruct fsa
  states ;list of all states
  alphabet 
  start ;the starting state
  finals ;list of final states
  (nodes (make-hash-table)) ;the mapping from symbol -> states
  :copier copy-fsa)

(defun copy-fsa (f)
  "This function will copy the FSA.
The hash table is a new instance."
  (make-fsa :states (copy-list (fsa-states f))
	    :alphabet (copy-list (fsa-alphabet f))
	    :start (fsa-start f)
	    :finals (fsa-finals f)
	    :nodes (copy-hash-table (fsa-nodes f))))


(defmethod build-fsa (alphabet edges start finals)
  "This function build a fsa. 
The 'edges' argument is a list of 3-tuple. 
The 'final' argument is a list of vertices."
  (let ((f (make-fsa :alphabet (copy-list alphabet)
		     :start start 
		     :finals finals)))
    (mapcar (lambda (edge) 
	      (nadd-edge edge f))
	    edges)
    f))


(defmethod add-edge (edge (f fsa))
  "This function adds an edge to a copy of the FSA."
  (let* ((fsa (copy-fsa f))
	 (src (edge-source edge))
	 (dst (edge-destination edge))
	 (nodes (fsa-nodes fsa))
	 (node (gethash src nodes)))
    (setf (gethash src nodes) 
	  (add-edge edge node))
    (if (null (gethash dst nodes))
	(setf (gethash dst nodes) (make-node :name dst)))
    fsa))



;;;This function adds a node to an FSA.
;;;This function is the destructive version
;;;of add-edge.
(defmethod nadd-edge (edge (f fsa))
  (let* ((src (edge-source edge))
	 (dst (edge-destination edge))
	 (nodes (fsa-nodes f)))
    (if (null (gethash dst nodes)) ;dst might not be in FSA
	(add-node (make-node :name dst) f))
    (if (null (gethash src nodes)) ;src might not be in FSA
	(add-node (make-node :name src) f))
    (nadd-edge edge (gethash src nodes))
    f))


(defun add-node (node fsa)
  "This function add a node to the copy of the FSA"
  (let ((name (node-name node))
	(nodes (fsa-nodes fsa)))
    (if (null (gethash name nodes))
	(setf (gethash name nodes) node))))

;;;This function returns the node identified 
;;;by the id specified.
(defmethod fsa-node (id fsa)
  "This function returns the node identified by the id specified."
  (gethash id (fsa-nodes fsa)))

(defmethod e-close-nodes (nodes-id fsa)
  (uniqueness-set (append nodes-id
			  (mapcan (lambda (src)
				    (e-close (fsa-node src fsa)))
				  nodes-id))))

(defmethod transition (input id fsa)
  (let ((node (fsa-node id fsa)))
      (e-close-nodes 
       (mapcan (lambda (src) 
		 (node-transition input (fsa-node src fsa)))
	       (cons id (e-close node)))
       fsa)))

(defmethod transition (input (ids cons) fsa)
  (uniqueness-set (mapcan (lambda (id)
			    (transition input id fsa))
			  ids)))

(defmethod e-transition (word fsa)
  (let ((nodes (cons (fsa-start fsa) nil)))
    (reduce (lambda (ids input)
	      (transition (string input) ids fsa))
	    word
	    :initial-value nodes)))
    
(defmethod accepts (word fsa)
  (some (lambda (node)
	  (if (member node (fsa-finals fsa))
	      t))
	  (e-transition word fsa)))

(defmethod graphviz-export (stream xsize ysize fsa)
  "This function will write the dot description of the FSA in the stream."
  (progn
    (format stream 
	    "digraph G {~%  rankdir = LR;~%  size = \"~A, ~A\";~%" 
	    xsize 
	    ysize)
    (format stream "  rotate = 90;~%")
    (if (not (null (fsa-finals fsa)))
	(progn 
	  (format stream "~%  node [shape = doublecircle];~% ")
	  (mapcar (lambda (x) 
		    (format stream " \"~A\"" x))
		  (fsa-finals fsa))))
    (format stream ";~%~%  node [shape = circle];~% ")
    (maphash (lambda (key node)
	       (format stream " \"~A\"" (node-name node)))
	     (fsa-nodes fsa))
    (format stream ";~%~%")
    (maphash (lambda (key node)
	       (mapcar (lambda (edge)
			 (format stream "  \"~A\" -> \"~A\" [label = \"~A\"];~%" 
				 (edge-source edge)
				 (edge-destination edge)
				 (if (null (edge-symbol edge))
				     "epsilon"
				   (edge-symbol edge))))
		       (node-edges node)))
	     (fsa-nodes fsa))
    (format stream "}~%")
    fsa))
  





