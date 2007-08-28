
(define-record fsa)
  alphabet 
  start ;the starting state
  finals ;list of final states
  nodes) ;the mapping from symbol -> states

(define final?
  (lambda (fsa label)
    (if (member label (fsa-finals fsa))
	t)))


(define build-fsa
  "This function build a fsa. 
The 'edges' argument is a list of 3-tuple. 
The 'final' argument is a list of vertices."
  (lambda (alphabet edges start finals)
    (let ((states (make-hash-table)))
      (mapcar (lambda (edge) 
		(add-edge! states edge))))))
  
(define ensure-nodes
  (lambda (states nodes)
    (if (null? nodes) 
	'()
	(let ((node (hash-table-ref states (car nodes) #f)))
	  ;; if the node is equal to false, it means it wasn't present before
	  (if (not node)
	      (set! node (make-node (car nodes))))
	  (cons node (cdr nodes))))))
	      
;;This function adds a node to an FSA.
;;;This function is the destructive version
;;;of add-edge.
(define nadd-edge
  (lambda (states edge)
    (let ((src (edge-source edge))
	  (dst (edge-destination edge)))
    (nadd-edge edge (gethash src nodes))
    f))

(defmethod nremove-edge (edge (fsa fsa))
  (let ((node (fsa-node (edge-source edge) fsa)))
    (if node
	(progn 
	  (nremove-edge edge node)
	  (if (is-final node fsa)
	      (setf (fsa-finals fsa) 
		    (remove (node-name node) (fsa-finals fsa))))))
    fsa))
	
(defmethod is-accessible (label fsa)
  (let ((accessors nil))
    (maphash (lambda (key node)
		 (declare (ignore key))
		 (if (node-access label node)
		     (setf accessors (cons (node-name node) accessors))))
	     (fsa-nodes fsa))
    accessors))
	     

(defmethod is-accessible ((node node) fsa)
  (is-accessible (node-name node) fsa))

(defmethod nremove-node ((node node) fsa)
  (nremove-node (node-name node) fsa))

(defmethod nremove-node (label fsa)
  (progn
    (setf (fsa-finals fsa) 
	  (remove label (fsa-finals fsa)))
    (remhash label (fsa-nodes fsa))
    fsa))

(defun add-node (node fsa)
  "This function add a node to the copy of the FSA"
  (let ((name (node-name node))
	(nodes (fsa-nodes fsa)))
    (if (null (gethash name nodes))
	(setf (gethash name nodes) node))))


(defun are-equivalent (lhs-label rhs-label fsa)
  "Returns nil if they are not equivalent, return the rhs-label otherwise"
  (let ((lhs (fsa-node lhs-label fsa))
	(rhs (fsa-node rhs-label fsa)))
    (if (and (equal (is-final lhs-label fsa)
		    (is-final rhs-label fsa))
	     (edges-are-equivalent (node-edges lhs)
				   (node-edges rhs)))
	rhs-label)))


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
  "This is the extended transition function for the FSA."
  (let ((nodes (cons (fsa-start fsa) nil)))
    (reduce (lambda (ids input)
	      (transition (string input) ids fsa))
	    word
	    :initial-value nodes)))
    
(defmethod accepts (word fsa)
  "This function returns true if the word is accepted by the FSA."
  (some (lambda (node)
	  (if (member node (fsa-finals fsa))
	      t))
	  (e-transition word fsa)))

(defmethod graphviz-export (fsa &key (file nil) (xsize 8) (ysize 11))
  "This function will write the dot description of the FSA in the stream."
  (progn
    (if (null file)
	(graphviz-export-stream fsa :stream t :xsize xsize :ysize ysize)
      (with-open-file (stream 
		       file 
		       :direction :output 
		       :if-exists :supersede
		       :if-does-not-exist :create)
		      (graphviz-export-stream fsa 
					      :stream stream
					      :xsize xsize
					      :ysize ysize)))
    fsa))


(defmethod graphviz-export-stream (fsa &key (stream t) (xsize 8) (ysize 11))
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
	       (declare (ignore key))
	       (format stream " \"~A\"" (node-name node)))
	     (fsa-nodes fsa))
    (format stream ";~%~%")
    (maphash (lambda (key node)
	       (declare (ignore key))
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
  

