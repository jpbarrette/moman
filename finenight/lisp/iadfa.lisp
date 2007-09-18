;(declaim (optimize (speed 3) (space 3) (debug 0)))
;(declaim (optimize (speed 0) (space 0) (debug 3)))

(in-package :com.rrette.finenight.iadfa)

(defstruct iadfa 
  (ancestrors (make-array 1000000 :initial-element nil :fill-pointer 0))
  (parent-arities (make-array 1000000 :fill-pointer 0))
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
    (incf (aref (iadfa-parent-arities iadfa) (node-label dst-node)))
    (hash-table-update!	input ancestrors nodes
			(cons src-node nodes))))
    
(defun node-remove-ancestror! (iadfa dst-node input src-node)
  (let ((ancestrors (aref (iadfa-ancestrors iadfa) (node-label dst-node))))
    (if ancestrors
	(hash-table-update! input ancestrors nodes
			    (remove src-node nodes)))))

(defun get-fresh-node (iadfa)
  (if (null (iadfa-unused-nodes iadfa))
      (progn 
	(let ((new-label (generate-state iadfa)))
	  (when (>= new-label (length (iadfa-ancestrors iadfa)))
	    (vector-push 0 (iadfa-parent-arities iadfa))
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
    (setf (aref (iadfa-parent-arities iadfa) i) 0)
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
  (decf (aref (iadfa-parent-arities iadfa) (node-label (car (node-destinations stem-end-node))))) 
  (remove-ancestror-to-childs iadfa stem-end-node)
  (when  (not (eq stem-start-node stem-end-node))
    (let ((old-node (car (node-transition stem-start-node stem-start-input))))
      (remove-last-nodes iadfa old-node stem-end-node)))
  (node-remove-dsts-for-input! stem-start-node stem-start-input))


(defun build-fsa-from-ancestrors (iadfa)
  (let ((fsa (make-fsa-builder)))
    (vector-walk (label node-ancestrors (iadfa-ancestrors iadfa))
		 (if node-ancestrors
		     (maphash #'(lambda (input nodes)
				  (dolist (node nodes nil)
				    (fsa-add-edge! fsa label input (node-label node))))
			      node-ancestrors)))
    fsa))
    
(defun iadfa-state-ancestrors-for-input (iadfa dst-label input)
  (let ((ancestrors (aref (iadfa-ancestrors iadfa) dst-label)))
    (if ancestrors
	(mapcar #'(lambda (node)
		    (node-label node))
		(gethash input ancestrors))
      '())))

(defun node-ancestrors (iadfa node)
  (aref (iadfa-parent-arities iadfa) (node-label node)))
    
(defun node-ancestrors-for-input (iadfa dst-node input)
  (iadfa-state-ancestrors-for-input iadfa (node-label dst-node) input))
    
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
	(prefix-stem '())
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
				 (setf prefix-stem '())
				 (setf profile '()))))
		       (if (eq (iadfa-final iadfa) node)
			   (progn
			     (delete-branch iadfa stem-start-node stem-start-input stem-end-node)
			     (values stem-start-node (append stem word) (append profile (make-list (- (length word) 1) :initial-element nil))))
			   (let ((next-node (node-transition node (car word))))
			     (if (null next-node)
				 (progn 
				   (if found-stem 
				       (let ((prefix-symbol (car (node-symbols node)))
					     (prefix-node (car (node-destinations node))))
					 ;; we are in a suffix of a subsumed stem
					 ;; the node should have only one destination.
					 (delete-branch iadfa stem-start-node stem-start-input stem-end-node)
					 (values stem-start-node 
						 (append stem word) 
						 (append profile (make-list (- (length word) 1) :initial-element nil))
						 (append stem (list prefix-symbol))
						 prefix-node))
				       (values node word (make-list (length word) :initial-element nil))))
				 (progn (setf next-node (car next-node))
					(setf stem (append stem (list (car word))))
					(setf profile (append profile (list (node-final next-node))))
					(when (not found-stem)
					  (setf prefix-stem (append prefix-stem (list (car word))))
					  (setf stem-end-node node)
					  (when (< 1 (node-ancestrors iadfa next-node))
					    (setf found-stem t)))
					(c-prefix (cdr word)
						  next-node
						  (append prefix
							  (list (car word))))))))))
	    (c-prefix word node '()))))

             


(defun c-suffix (iadfa current-suffix node prefix-node profile sub-stem)
  (if (or (= 1 (length current-suffix)) 
	  (= 1 (length profile)) 
	  ;(= 1 (length sub-stem))
	  (= (length sub-stem) (length current-suffix)))
      (values node (reverse current-suffix) (reverse profile))
      (let ((next-node (ancestror-transition iadfa node (car current-suffix) (car profile))))
;; 	(if (equal '(#\0 #\- #\7) current-suffix)
;; 	    (break))
	(if (or (not next-node) 
		(eq next-node prefix-node) 
		(eq next-node (fsa-start-node (iadfa-fsa iadfa))))
	    (values node (reverse current-suffix) (reverse profile))
	    (c-suffix iadfa
		      (cdr current-suffix)
		      next-node
		      prefix-node
		      (cdr profile)
		      sub-stem)))))


(defun common-suffix  (iadfa current-suffix node prefix-node profile sub-stem)
  ;; this function takes a suffix to be consumed
  ;; and a node to start from and the current stem
  (c-suffix iadfa (reverse current-suffix) node prefix-node (reverse profile) sub-stem))

(defun iadfa-add-edge! (iadfa src-node input dst-node)
  (node-add-edge! src-node input dst-node)
  (node-add-ancestror! iadfa dst-node input src-node))

(defun add-stem (iadfa prefix-node suffix-node current-stem profile sub-prefix sub-node)
  (let ((last-node prefix-node)
	(last-input (car (last current-stem)))
	(processing-stem (butlast current-stem))
	(sub-prefix sub-prefix))
    (reduce #'(lambda (iadfa input)
		(let ((new-node (get-fresh-node iadfa)))
		  (setf (node-final new-node) (car profile))
		  (setf profile (cdr profile))
		  (iadfa-add-edge! iadfa last-node input new-node)
		  (when sub-prefix
		    (when (= 1 (length sub-prefix))
		      (iadfa-add-edge! iadfa last-node (car sub-prefix) sub-node)
		      (remove-ancestror-to-childs iadfa last-node))
		    (setf sub-prefix (cdr sub-prefix)))
		  (setf last-node new-node)
		  iadfa))
	    processing-stem
	    :initial-value iadfa)
    (iadfa-add-edge! iadfa last-node last-input suffix-node)
    (when (= 1 (length sub-prefix))
      (iadfa-add-edge! iadfa last-node (car sub-prefix) sub-node)
      (remove-ancestror-to-childs iadfa last-node))
    iadfa))


(defun handle-word (iadfa word)
  (let* ((fsa (iadfa-fsa iadfa)))
    (multiple-value-bind (prefix-node current-suffix profile sub-prefix sub-node) (common-prefix iadfa word (fsa-start-node fsa))
      (multiple-value-bind (suffix-node current-stem current-profile)
	  (common-suffix iadfa current-suffix (iadfa-final iadfa) prefix-node profile sub-prefix)
	(add-stem iadfa prefix-node suffix-node current-stem current-profile sub-prefix sub-node)
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
		(graphviz-export-to-file (make-fsa-builder-from-fsa (iadfa-fsa iadfa)) 
					 (concatenate 'string "output/iadfa" 
						      (format nil "~A" index) ".dot"))
		(graphviz-export-to-file (build-fsa-from-ancestrors iadfa) 
					 (concatenate 'string "output/iadfa-ances" 
						      (format nil "~A" index) ".dot"))
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
    (for-each-line-in-file (line file)
      (handle-word iadfa (concatenate 'list line))
      (when (member index dump)
	(graphviz-export-to-file (make-fsa-builder-from-fsa (iadfa-fsa iadfa)) (concatenate 'string "output/iadfa" (format nil "~A" index) ".dot"))
	(graphviz-export-to-file (build-fsa-from-ancestrors iadfa) (concatenate 'string "output/iadfa-ances" (format nil "~A" index) ".dot")))
      (incf index)
      (if (zerop (mod index 1000))
	  (let ((current-time (get-internal-real-time)))
	    (setf nb-per-hours (float (* 1000 (/ 1 (/ (- current-time last-time) 
						      internal-time-units-per-second)) 60 60)))
	    (setf nb-hours-for-all (float (/ (* 65000 (/ (- current-time last-time) 
							 internal-time-units-per-second)) 60 60)))
	    (setf last-time current-time)
	    (format t "~2,12$ w/h ~2,2$ Hours ~A ~A ~%"  nb-per-hours nb-hours-for-all index line)))
      iadfa)
    iadfa))


(defun debug-gen-iadfa-from-file (file)
  (let ((iadfa (build-iadfa))
	(index 0)
	(last-time (get-internal-real-time))
	(nb-per-hours 0)
	(nb-hours-for-all 0))
    (for-each-line-in-file (line file)
      (format t "~,2F w/h ~,2F Hours ~A ~A ~%"  nb-per-hours nb-hours-for-all index line)
      (handle-word iadfa (concatenate 'list line))
      (graphviz-export-to-file (make-fsa-builder-from-fsa (iadfa-fsa iadfa)) (concatenate 'string "output/iadfa" (format nil "~A-~A" index line) ".dot"))
      (graphviz-export-to-file (build-fsa-from-ancestrors iadfa) (concatenate 'string "output/iadfa-ances" (format nil "~A" index) ".dot"))
      (incf index)
      (if (zerop (mod index 1000))
	  (let ((current-time (get-internal-real-time)))
	    (setf nb-per-hours (float (* 1000 (/ 1 (/ (- current-time last-time) internal-time-units-per-second)) 60 60)))
	    (setf nb-hours-for-all (float (/ (* 65000 (/ (- current-time last-time) internal-time-units-per-second)) 60 60)))
	    (setf last-time current-time)))
      iadfa)
  iadfa))


(defmacro test-equivalence (words)
  (with-syms (w iadfa output)
    `(let* ((,w ,words)
            (,iadfa (debug-gen-iadfa ,w))
	    (,output nil))
       (setf ,output (extract-words (iadfa-fsa ,iadfa)))
       (format t "input:~%~S~%output:~%~S~%" ,w ,output)
       (equal ,w ,output))))


(defun detect-problems (words)
  (let ((iadfa (build-iadfa))
	(words-to-be-checked nil))
    (dolist (word words)
      (setf words-to-be-checked (nconc words-to-be-checked (list word)))
      (handle-word iadfa (concatenate 'list word))
      (when (not (equal words-to-be-checked 
			(extract-words (iadfa-fsa iadfa))))
	(return)))
    ;; We got the first entry that trigger the problem.
    ;; we need now to see which entry is needed to start
    ;; the problem
    words-to-be-checked))


(defun detect-first-starting-problematic-word (words-to-be-checked)
  (let ((wtbc (cdr words-to-be-checked))
	(last-word (car words-to-be-checked)))
    (do ((iadfa (gen-iadfa wtbc) (gen-iadfa wtbc)))
	((null wtbc))
      (if (equal wtbc
		 (extract-words (iadfa-fsa iadfa)))
	  (return (cons last-word wtbc)))
      (setf last-word (car wtbc))
      (setf wtbc (cdr wtbc)))))

(defun filter-non-problematic-words (words-to-be-checked)
  (let ((problematics-words (list (car words-to-be-checked)))
	(last-word (cadr words-to-be-checked))
	(words-to-be-checked (cddr words-to-be-checked)))
    (do ((iadfa (gen-iadfa (append problematics-words words-to-be-checked))
		(gen-iadfa (append problematics-words words-to-be-checked))))
	((null words-to-be-checked))
      (if (equal (append problematics-words words-to-be-checked)
		 (extract-words (iadfa-fsa iadfa)))
	  (setf problematics-words (nconc problematics-words (list last-word))))
      (setf last-word (car words-to-be-checked))
      (setf words-to-be-checked (cdr words-to-be-checked)))
    (setf problematics-words (nconc problematics-words (list last-word)))
    problematics-words))
	     

(defun detect-problems-from-file (filename)
  (let ((words-to-be-checked nil))
    (let ((iadfa (build-iadfa)))
      (for-each-line-in-file (word filename)
	(setf words-to-be-checked (nconc words-to-be-checked (list word)))
	(format t "Processing word [~A].~%" word)
	(handle-word iadfa (concatenate 'list word))
	(when (not (equal words-to-be-checked 
			  (extract-words (iadfa-fsa iadfa))))
	  (format t "Word [~A] triggered a problem.~%" word)
	  (return))
	nil))
    ;; We got the first entry that trigger the problem.
    ;; we need now to see which entry is needed to start
    ;; the problem
    (setf words-to-be-checked 
	  (detect-first-starting-problematic-word words-to-be-checked))
    (setf words-to-be-checked
	  (filter-non-problematic-words words-to-be-checked))
    words-to-be-checked))
  

(defun print-stats (iadfa)
  (format t 
	  "ancestrors length: ~A~%parent-arities length: ~A~%" 
	  (length (iadfa-ancestrors iadfa))
	  (length (iadfa-parent-arities iadfa))))




;; (defun dump-words (iadfa)
;;   (let ((fsa (iadfa-fsa))
;; 	(states (list (cons "" (fsa-start-node start)))))
;;     states))

