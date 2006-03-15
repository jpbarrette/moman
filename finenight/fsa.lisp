(load "utils.lisp")
(load "node.lisp")
(load "edge.lisp")


(defstruct fsa
  states ;list of all states
  alphabet 
  start ;the starting state
  finals ;list of final states
  (nodes (make-hash-table)) ;the mapping from symbol -> states
  :copier copy-fsa)

;;;This function will copy the FSA.
;;;The hash table is a new instance.
(defun copy-fsa (f)
  (make-fsa :states (copy-list (fsa-states f))
	    :alphabet (copy-list (fsa-alphabet f))
	    :start (fsa-start f)
	    :finals (fsa-finals f)
	    :nodes (copy-hash-table (fsa-nodes f))))


;;;This function build a fsa. 
;;;The "edges" argument is a list of 3-tuple. 
;;;The "final" argument is a list of vertices.
(defmethod build-fsa (alphabet edges start finals)
  (let ((f (make-fsa :alphabet (copy-list alphabet)
		     :start start 
		     :finals finals)))
    (mapcar (lambda (edge) 
	      (nadd-edge edge f))
	    edges)
    f))


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
  





