(declaim (optimize (speed 0) (space 0) (debug 3)))

(defstruct iadfa 
  (ancestrors (make-array 100 :initial-element nil))
  (index 2) ;; this is used for automatic node name generation
  (fsa (make-fsa :start-node (make-empty-node 0)))
  final)

(defun ancestror-transition (iadfa node input final)
  (let ((ancestrors (aref (iadfa-ancestrors iadfa) (node-label node))))
    (if (not ancestrors)
	nil
	(reduce #'(lambda (result node)
		    (if (eq (node-final node) final)
			node
			result))
		(gethash input ancestrors '())
		:initial-value nil))))

(defun node-add-ancestror! (iadfa dst-node input src-node)
  (let ((ancestrors (aref (iadfa-ancestrors iadfa) (node-label dst-node))))
    (if (not ancestrors)
	(progn
	  (setf ancestrors (make-hash-table))
	  (setf (aref (iadfa-ancestrors iadfa) (node-label dst-node)) ancestrors)))
    (hash-table-update!	#'(lambda (nodes)
			    (cons src-node nodes))
			input
			ancestrors)))
    
(defun node-remove-ancestror! (iadfa dst-node input src-node)
  (let ((ancestrors (aref (iadfa-ancestrors iadfa) (node-label dst-node))))
    (if ancestrors
	(progn 
	  (hash-table-update! #'(lambda (nodes)
				  (remove src-node nodes))
			      input
			      ancestrors)))))

(defun remove-ancestror-to-childs (iadfa node)
  (node-walk node #'(lambda (input destination-nodes)
		      (dolist (dst-node destination-nodes iadfa)
			(node-remove-ancestror! iadfa dst-node input node)))))

(defun delete-branch (iadfa stem-start-node stem-start-input stem-end-node)
  (remove-ancestror-to-childs iadfa stem-end-node)
  (node-remove-dsts-for-input! stem-start-node stem-start-input))


(defun build-fsa-from-ancestrors (iadfa)
  (let ((fsa (make-fsa-builder)))
    (vector-walk
     (iadfa-ancestrors iadfa)
     #'(lambda (label node-ancestrors)
	 (if node-ancestrors
	     (maphash
	      #'(lambda (input nodes)
		  (dolist (node nodes nil)
		    (fsa-add-edge! fsa label input (node-label node))))
	      node-ancestrors))))
    fsa))
    
(defun iadfa-state-ancestrors (iadfa dst-label input)
  (let ((ancestrors (aref (iadfa-ancestrors iadfa) dst-label)))
    (if ancestrors
	(mapcar #'(lambda (node)
		    (node-label node))
		(gethash input ancestrors))
      '())))
    
(defun node-ancestrors (iadfa dst-node input)
  (iadfa-state-ancestrors iadfa (node-label dst-node) input))
    
(defun generate-state (iadfa)
  (let ((name (iadfa-index iadfa)))
    (setf (iadfa-index iadfa) (+ 1 (iadfa-index iadfa)))
    name))

(defun build-iadfa ()
  (let ((iadfa (make-iadfa :final (make-empty-node 1))))
    (setf (node-final (iadfa-final iadfa)) t)
    iadfa))

(defun common-prefix (iadfa word node)
  (let ((stem '())
	(stem-start-node node)
	(stem-start-input (car word))
	(stem-end-node nil)
	(profile '())
	(found-stem '()))
    (labels ((c-prefix (word node prefix)
		       (if (not found-stem)
			   (if (< 1 (node-arity node))
			       (progn
				 (setf stem-start-node node)
				 (setf stem-start-input (car word))
				 (setf stem '())
				 (setf profile '()))))
		       (if (eq (iadfa-final iadfa) node)
			   (progn
			     (delete-branch iadfa stem-start-node stem-start-input stem-end-node)
			     (values stem-start-node (append stem word) (append profile (make-list (- (length word) 1) :initial-element nil))))
			 (let ((next-node (node-transition node (car word))))
			   (if (null next-node)
			       (values node word (make-list (length word) :initial-element nil))
			     (progn (setf next-node (car next-node))
				    (if (not found-stem)
					(progn
					  (setf profile (append profile (list (node-final next-node))))
					  (setf stem (append stem (list (car word))))
					  (setf stem-end-node node)
					  (if (> (node-label node) (node-label next-node))
					      (setf found-stem t))))
                                   (c-prefix (cdr word)
                                             next-node
                                             (append prefix
                                                     (list (car word))))))))))
	    (c-prefix word node '()))))

             


(defun c-suffix (iadfa current-suffix node profile)
  (if (eq 1 (length current-suffix))
      (values node (reverse current-suffix) (reverse profile))
      (let ((next-node (ancestror-transition iadfa node (car current-suffix) (car profile))))
	(if (or (not next-node) (eq next-node (fsa-start-node (iadfa-fsa iadfa))))
	    (values node (reverse current-suffix) (reverse profile))
	    (c-suffix iadfa
		      (cdr current-suffix)
		      next-node
		      (cdr profile))))))


(defun common-suffix  (iadfa current-suffix node profile)
  ;; this function takes a suffix to be consumed
  ;; and a node to start from and the current stem
  (c-suffix iadfa (reverse current-suffix) node (reverse profile)))

(defun iadfa-add-edge! (iadfa src-node input dst-node)
  (node-add-edge! src-node input dst-node)
  (node-add-ancestror! iadfa dst-node input src-node))

(defun add-stem (iadfa prefix-node suffix-node current-stem profile)
  (let ((last-node prefix-node)
	(last-input (car (last current-stem)))
	(processing-stem (butlast current-stem)))
    (reduce #'(lambda (iadfa input)
		(let ((new-node (make-empty-node (generate-state iadfa))))
		  (setf (node-final new-node) (car profile))
		  (setf profile (cdr profile))
		  (iadfa-add-edge! iadfa last-node input new-node)
		  (setf last-node new-node)
		  iadfa))
	    processing-stem
	    :initial-value iadfa)
    (iadfa-add-edge! iadfa last-node last-input suffix-node)
    iadfa))


(defun handle-word (iadfa word)
  (let* ((fsa (iadfa-fsa iadfa)))
    (multiple-value-bind (prefix-node current-suffix profile) (common-prefix iadfa word (fsa-start-node fsa))
      (multiple-value-bind (suffix-node current-stem current-profile)
	  (common-suffix iadfa current-suffix (iadfa-final iadfa) profile)
	(add-stem iadfa prefix-node suffix-node current-stem current-profile)
	(if (> (node-arity prefix-node) 1)
	    (remove-ancestror-to-childs iadfa prefix-node)))
      iadfa)))

(defun gen-iadfa (words)
  (reduce #'(lambda (iadfa word) 
	      (handle-word iadfa word))
	words
	:initial-value (build-iadfa)))

(defun debug-gen-iadfa (words)
  (let ((index 0))
    (reduce #'(lambda (iadfa word)
		(handle-word iadfa (concatenate 'list word))
		(graphviz-export-to-file (make-fsa-builder-from-fsa (iadfa-fsa iadfa)) (concatenate 'string "iadfa" (format nil "~A" index) ".dot"))
		(graphviz-export-to-file (build-fsa-from-ancestrors iadfa) (concatenate 'string "iadfa-ances" (format nil "~A" index) ".dot"))
		(setf index (+ index 1))
		iadfa)
	    words
	    :initial-value (build-iadfa))))
  
(defun gen-iadfa-from-file (file)
  (let ((iadfa (build-iadfa))
	(index 0))
    (for-each-line-in-file 
     file
     #'(lambda (line)
	 (format t "~A ~%" line)
	 (handle-word iadfa line)
	 (graphviz-export-to-file (make-fsa-builder-from-fsa (iadfa-fsa iadfa)) (concatenate 'string "iadfa" (format nil "~A" index) ".dot"))
	 (graphviz-export-to-file (build-fsa-from-ancestrors iadfa) (concatenate 'string "iadfa-ances" (format nil "~A" index) ".dot"))
	 (setf index (+ index 1))
	 iadfa))
    (iadfa-fsa iadfa)))



