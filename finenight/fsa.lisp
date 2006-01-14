(load "utils.lisp")
(load "node.lisp")


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
	    :symbols (copy-list (fsa-states f))
	    :start (fsa-start f)
	    :finals (fsa-finals f)
	    :nodes (copy-hash-table (fsa-nodes f))))


;;;This function build a fsa. 
;;;The "transitions" argument is a list of 3-tuple. 
;;;The "final" argument is a list of vertices.
(defmethod build-fsa (symbols transitions start finals)
  (let ((fsa (make-fsa :symbols (copy-list symbols)
		       :start start 
		       :finals finals)))
    (mapcar (lambda (x) 
	      (nadd-transition x fsa))
	    transitions)
    fsa))


;;;This function adds a transition to an FSA.
;;;Returns the copy of the FSA.
(defmethod add-transition (transition (f fsa))
  (let* ((fsa (copy-fsa f))
	 (src (car transition))
	 (dst (third transition))
	 (nodes (fsa-nodes fsa))
	 (node (gethash src nodes)))
    (setf (gethash src nodes) 
	  (node-add-transition transition node))
    (if (null (gethash dst nodes))
	(setf (gethash dst nodes) (make-node :name dst)))
    fsa))



;;;This function adds a transition to an FSA.
;;;This function is the destructive version
;;;of add-transition.
(defmethod nadd-transition (transition (f fsa))
  (let* ((src (car transition))
	 (dst (third transition))
	 (nodes (fsa-nodes f)))
    (if (null (gethash dst nodes));dst might not be in FSA
	(add-node (make-node :name dst) f))
    (if (null (gethash src nodes));src might not be in FSA
	(add-node (make-node :name src) f))
    (node-nadd-transition transition (gethash src nodes))
    f))


;;;This function add a node to the FSA.
(defun add-node (node fsa)
  (let ((name (node-name node))
	(nodes (fsa-nodes fsa)))
    (if (null (gethash name nodes))
	(setf (gethash name nodes) node))))


(defmethod fsa-node (id fsa)
  (gethash id (fsa-nodes fsa)))



(defmethod transition (node input)
  (gethash input (node-symbols node)))



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
  





