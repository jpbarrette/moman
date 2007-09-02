;(declaim (optimize (speed 3) (space 3) (debug 0)))
;(declaim (optimize (speed 0) (space 0) (debug 3)))

(in-package :com.rrette.finenight.iadfa)

(defstruct iadfa 
  (ancestrors (make-array 1000000 :initial-element nil :fill-pointer 0) :type vector)
  (index 0) ;; this is used for automatic node name generation
  (unused-nodes nil)
  (fsa (make-fsa :start-node (make-empty-node 0)))
  final)

(defun ancestror-transition (iadfa node input final)
  (declare (iadfa iadfa))
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
    (hash-table-update!	input ancestrors nodes
			(cons src-node nodes))))
    
(defun node-remove-ancestror! (iadfa dst-node input src-node)
  (let ((ancestrors (aref (iadfa-ancestrors iadfa) (node-label dst-node))))
    (if ancestrors
	(progn 
	  (hash-table-update! input ancestrors nodes
			      (remove src-node nodes))))))

(defun get-fresh-node (iadfa)
  (if (null (iadfa-unused-nodes iadfa))
      (progn 
	(let ((new-label (generate-state iadfa)))
	  (if (>= new-label (length (iadfa-ancestrors iadfa)))
	      (vector-push (make-hash-table) (iadfa-ancestrors iadfa)))
	  (make-empty-node new-label)))
      (let* ((unused-nodes (iadfa-unused-nodes iadfa))
	     (new-node (car unused-nodes)))
	(setf (iadfa-unused-nodes iadfa) (cdr unused-nodes))
	new-node)))

(defun remove-ancestror-to-childs (iadfa node)
  (node-walk node #'(lambda (input destination-nodes)
		      (dolist (dst-node destination-nodes iadfa)
			(node-remove-ancestror! iadfa dst-node input node)))))

(defun reclaim-branch (iadfa node node-end)
  (declare (ignore node-end))
  ;(node-reset node-end)
  (do ((i (node-label node) (+ i 1)))
      ((>= i (iadfa-index iadfa)))
    (clrhash (aref (iadfa-ancestrors iadfa) i))))

;;     (do () 
;; 	((null nodes-to-clean))
;;       (let ((node-to-clean (pop nodes-to-clean)))
;; 	(setf (aref (iadfa-ancestrors iadfa) (node-label node-to-clean)) nil)
;; 	(setf nodes-to-clean (append nodes-to-clean 
;; 				     (node-destinations node-to-clean)))
;; 	(node-reset node-to-clean)
;; 	(setf (iadfa-unused-nodes iadfa) 
;; 	      (append (iadfa-unused-nodes iadfa) (list node-to-clean)))))))


(defun remove-last-nodes (iadfa node node-end)
  (reclaim-branch iadfa node node-end)
  (setf (iadfa-index iadfa) (node-label node)))

(defun delete-branch (iadfa stem-start-node stem-start-input stem-end-node)
  (remove-ancestror-to-childs iadfa stem-end-node)
  (let ((old-node (car (node-transition stem-start-node stem-start-input))))
    (remove-last-nodes iadfa old-node stem-end-node))
  (node-remove-dsts-for-input! stem-start-node stem-start-input))


(defun build-fsa-from-ancestrors (iadfa)
  (let ((fsa (make-fsa-builder)))
    (vector-walk (label node-ancestrors (iadfa-ancestrors iadfa))
		 (if node-ancestrors
		     (maphash
		      #'(lambda (input nodes)
			  (dolist (node nodes nil)
			    (fsa-add-edge! fsa label input (node-label node))))
		      node-ancestrors)))
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
  (let ((iadfa (make-iadfa)))
    (setf (iadfa-fsa iadfa) (make-fsa :start-node (get-fresh-node iadfa)))
    (setf (iadfa-final iadfa) (get-fresh-node iadfa))
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
					(setf stem (append stem (list (car word))))
					(setf profile (append profile (list (node-final next-node))))
					(if (not found-stem)
					    (progn 
					      (setf stem-end-node node)
					      (if (> (node-label node) (node-label next-node))
						  (setf found-stem t))))
					(c-prefix (cdr word)
						  next-node
						  (append prefix
							  (list (car word))))))))))
	    (c-prefix word node '()))))

             


(defun c-suffix (iadfa current-suffix node prefix-node profile)
  (if (eq 1 (length current-suffix))
      (values node (reverse current-suffix) (reverse profile))
      (let ((next-node (ancestror-transition iadfa node (car current-suffix) (car profile))))
	(if (or (not next-node) (eq next-node prefix-node) (eq next-node (fsa-start-node (iadfa-fsa iadfa))))
	    (values node (reverse current-suffix) (reverse profile))
	    (c-suffix iadfa
		      (cdr current-suffix)
		      next-node
		      prefix-node
		      (cdr profile))))))


(defun common-suffix  (iadfa current-suffix node prefix-node profile)
  ;; this function takes a suffix to be consumed
  ;; and a node to start from and the current stem
  (c-suffix iadfa (reverse current-suffix) node prefix-node (reverse profile)))

(defun iadfa-add-edge! (iadfa src-node input dst-node)
  (node-add-edge! src-node input dst-node)
  (node-add-ancestror! iadfa dst-node input src-node))

(defun add-stem (iadfa prefix-node suffix-node current-stem profile)
  (let ((last-node prefix-node)
	(last-input (car (last current-stem)))
	(processing-stem (butlast current-stem)))
    (reduce #'(lambda (iadfa input)
		(let ((new-node (get-fresh-node iadfa)))
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
	  (common-suffix iadfa current-suffix (iadfa-final iadfa) prefix-node profile)
	(add-stem iadfa prefix-node suffix-node current-stem current-profile)
	(if (> (node-arity prefix-node) 1)
	    (remove-ancestror-to-childs iadfa prefix-node)))
      iadfa)))

(defun gen-iadfa (words)
  (reduce #'(lambda (iadfa word) 
	      (handle-word iadfa (concatenate 'list word)))
	words
	:initial-value (build-iadfa)))

(defun debug-gen-iadfa (words)
  (let ((index 0))
    (reduce #'(lambda (iadfa word)
		(handle-word iadfa (concatenate 'list word))
		(graphviz-export-to-file (make-fsa-builder-from-fsa (iadfa-fsa iadfa)) (concatenate 'string "output/iadfa" (format nil "~A" index) ".dot"))
		(graphviz-export-to-file (build-fsa-from-ancestrors iadfa) (concatenate 'string "output/iadfa-ances" (format nil "~A" index) ".dot"))
		(setf index (+ index 1))
		iadfa)
	    words
	    :initial-value (build-iadfa))))
  
(defun gen-iadfa-from-file (file &key dump)
  (let ((iadfa (build-iadfa))
	(index 0)
	(last-time (get-internal-real-time))
	(nb-per-hours 0)
	(nb-hours-for-all 0))
    (for-each-line-in-file 
     file
     #'(lambda (line)
	 (format t "~,2F w/h ~,2F Hours ~A ~A ~%"  nb-per-hours nb-hours-for-all index line)
	 (handle-word iadfa (concatenate 'list line))
	 (when (member index dump)
	   (graphviz-export-to-file (make-fsa-builder-from-fsa (iadfa-fsa iadfa)) (concatenate 'string "output/iadfa" (format nil "~A" index) ".dot"))
	   (graphviz-export-to-file (build-fsa-from-ancestrors iadfa) (concatenate 'string "output/iadfa-ances" (format nil "~A" index) ".dot")))
	 (incf index)
	 (if (zerop (mod index 1000))
	     (let ((current-time (get-internal-real-time)))
	       (setf nb-per-hours (float (* 1000 (/ 1 (/ (- current-time last-time) internal-time-units-per-second)) 60 60)))
	       (setf nb-hours-for-all (float (/ (* 65000 (/ (- current-time last-time) internal-time-units-per-second)) 60 60)))
	       (setf last-time current-time)))
	 iadfa))
    (iadfa-fsa iadfa)))


(defun debug-gen-iadfa-from-file (file)
  (let ((iadfa (build-iadfa))
	(index 0)
	(last-time (get-internal-real-time))
	(nb-per-hours 0)
	(nb-hours-for-all 0))
    (for-each-line-in-file 
     file
     #'(lambda (line)
	 (format t "~,2F w/h ~,2F Hours ~A ~A ~%"  nb-per-hours nb-hours-for-all index line)
	 (handle-word iadfa (concatenate 'list line))
	 (graphviz-export-to-file (make-fsa-builder-from-fsa (iadfa-fsa iadfa)) (concatenate 'string "output/iadfa" (format nil "~A" index) ".dot"))
	 (graphviz-export-to-file (build-fsa-from-ancestrors iadfa) (concatenate 'string "output/iadfa-ances" (format nil "~A" index) ".dot"))
	 (incf index)
	 (if (zerop (mod index 1000))
	     (let ((current-time (get-internal-real-time)))
	       (setf nb-per-hours (float (* 1000 (/ 1 (/ (- current-time last-time) internal-time-units-per-second)) 60 60)))
	       (setf nb-hours-for-all (float (/ (* 65000 (/ (- current-time last-time) internal-time-units-per-second)) 60 60)))
	       (setf last-time current-time)))
	 iadfa))
    (iadfa-fsa iadfa)))

;; (defun dump-words (iadfa)
;;   (let ((fsa (iadfa-fsa))
;; 	(states (list (cons "" (fsa-start-node start)))))
;;     states))

