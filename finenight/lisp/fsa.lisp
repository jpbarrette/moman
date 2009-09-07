;(declaim (optimize (speed 3) (space 3) (debug 0)))

(in-package :com.rrette.finenight.fsa)

;; the node consists of a label and a map a symbol to 
;; a destination object. 
(defstruct (hash-node (:copier nil))
  label 
  (symbols-map (make-hash-table :test 'equal))
  (final nil))


(defun make-empty-node (label)
    (make-node :label label))

(defun node-reset (node)
  (clrhash (node-symbols-map node))
  (setf (node-final node) nil))

(defun node-arity (node)
  (hash-table-count (node-symbols-map node)))

(defun node-edges (node)
  (let ((label (node-label node)))
    (labels ((S (symbols)
	       (if (null symbols)
		   '()
		   (concatenate 'list (mapcar #'(lambda (dest-node)
						  (list label 
							(car symbols) 
							(node-label dest-node)))
					      (node-transition node (car symbols)))
				(S (cdr symbols))))))
      (S (node-symbols node)))))

(defun node-symbols (node)
    (hash-keys (node-symbols-map node)))

(defun node-destinations (node)
  (apply #'concatenate 'list (hash-values (node-symbols-map node))))

(defmacro node-walk (node proc)
  `(maphash ,proc (node-symbols-map ,node)))

(defun node-sorted-walk (node func &key (test #'char<))
  (let* ((hash (node-symbols-map node))
	 (keys (hash-keys hash)))
    (setf keys (sort keys test))
    (dolist (key keys)
      (funcall func key (gethash key hash)))))
      
(defun node-add-edge! (node input-symbol dst-node)
  (hash-table-update! input-symbol 
		      (node-symbols-map node)
		      lst 
		      (cons dst-node lst)))

(defun node-remove-edge! (node input-symbol dst-node)
  (let ((symbols-map (node-symbols-map node)))
    (if (< 1
	   (length (gethash input-symbol symbols-map)))
	(hash-table-update! input-symbol 
			    symbols-map
			    lst
			    (delete dst-node lst))
      (remhash input-symbol symbols-map))
    node))

;; (define node-remove-dst!
;;   (lambda (node dst-node)
;;     (let ((symbols-map (node-symbols-map node)))
;;       (map (lambda (symbol)
;; 	     (hash-table-update!/default symbols-map 
;; 				 symbol 
;; 				 (lambda (lst)
;; 				   (delete! dst-node lst eq?))
;; 				 '()))
;; 	   (node-symbols node)))
;;     node))

(defun node-remove-dsts-for-input! (node input)
  (let ((symbols-map (node-symbols-map node)))
    (remhash input symbols-map)
    node))


;; will return the list of destination nodes for the
;; given node.
(defun node-transition (node symbol)
  ;(format t "~S~%" (com.rrette.finenight.utils::hash->alist (node-symbols-map node)))
  (gethash symbol (node-symbols-map node)))


;; (define node-is-equivalent
;;   (lambda (lhs rhs)
;;       (if (not (eq? (node-final lhs) (node-final rhs)))
;; 	  #f
;;           (let ((lhs-map (node-symbols-map lhs))
;;                 (rhs-map (node-symbols-map rhs)))
;;             (map-equal? lhs-map rhs-map)))))
		  


(defstruct (fsa (:copier nil))
  start-node)

(defun make-empty-fsa (start-label)
  (make-fsa :start-node (make-empty-node start-label)))

(defun accept? (fsa word)
  (labels ((T (node word)
	      (if (null word) 
		  (node-final node)
		(let ((nodes (node-transition node (car word))))
		  (if (null nodes)
		      nil
		    (T (car nodes) (cdr word)))))))
	  (T (fsa-start-node fsa) word)))

 (defun extract-words (fsa)
   (let ((words nil)
	 (states (list (list (fsa-start-node fsa) ""))))
     (do ()
	 ((null states))
       (destructuring-bind (src-node word) (pop states)
	 (when (node-final src-node)
	   (setf words (nconc words (list word))))
	 (node-sorted-walk src-node 
			   #'(lambda (input dst-nodes)
			       (push (list (car dst-nodes)
					   (concatenate 'string word (string input)))
				     states))
			   :test #'char>)))
     words))
			      

(defun save-fsa (fsa)
  (write fsa :circle t))

;(define-record-printer (fsa x out)
;  (fprintf out
;           "(fsa ~S ~S ~S)"
;	   (fsa-initial-state x) (fsa-finals x) (hash-table->alist (fsa-nodes x))))

  

  

