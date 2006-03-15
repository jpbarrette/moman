(load "utils.lisp")
(load "node.lisp")
(load "edge.lisp")


(defstruct fsa
  states
  symbols
  start 
  finals
  (nodes (make-hash-table))
  :copier copy-fsa)

;;;This function will copy the FSA.
;;;The hash table is a new instance.
(defun copy-fsa (f)
  (make-fsa :states (copy-list (fsa-states f))
	    :symbols (copy-list (fsa-symbols f))
	    :start (fsa-start f)
	    :finals (fsa-finals f)
	    :nodes (copy-hash-table (fsa-nodes f))))


;;;This function build a fsa. 
;;;The "transitions" argument is a list of 3-tuple. 
;;;The "final" argument is a list of vertices.
(defmethod build-fsa (symbols transitions start finals)
  (let ((f (make-fsa :symbols (copy-list symbols)
		     :start start 
		     :finals finals)))
    (mapcar (lambda (x) 
	      (nadd-edge x f))
	    transitions)
    f))


(defmethod add-edge (edge (f (eql ())))
  (break))


;;;This function adds an edge to an FSA.
;;;It returns the copy of the FSA.
(defmethod add-edge (edge (f fsa))
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



;;;This function adds a transition to an FSA.
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


;;;This function add a node to the FSA.
(defun add-node (node fsa)
  (let ((name (node-name node))
	(nodes (fsa-nodes fsa)))
    (if (null (gethash name nodes))
	(setf (gethash name nodes) node))))

;;;This function returns the node identified 
;;;by the id specified.
(defmethod fsa-node (id fsa)
  (gethash id (fsa-nodes fsa)))


;;;This will return the destination state for
;;;the given input.
(defmethod transition (node input)
  (gethash input (node-symbols node)))


;;; This function will write the dot description of the
;;; FSA in the stream
(defmethod graphviz-export (stream xsize ysize fsa)
  (progn
    (format stream 
	    "digraph G {~%  rankdir = LR;~%  size = \"~A, ~A\";~%" 
	    xsize 
	    ysize)
    (format stream "  rotate = 90;~%")
    (if (not (null (fsa-final fsa)))
	(progn 
	  (format stream "~%  node [shape = doublecircle];~% ")
	  (mapcar (lambda (x) 
		    (format stream " \"~A\"" x))
		  (fsa-final fsa))))
    (format stream ";~%~%  node [shape = circle];~% ")
    (maphash (lambda (key node)
	       (format stream " \"~A\"" (node-name node)))
	     (fsa-nodes fsa))
    (format stream ";~%~%")
    (maphash (lambda (key node)
	       (maphash (lambda (key dst)
			  (format stream "  \"~A\" -> \"~A\" [label = \"~A\"];~%" 
				  (node-name node) dst key))
			(node-symbols node)))
	     (fsa-nodes fsa))
    (format stream "}~%")))
  





