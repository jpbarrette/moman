(in-package :com.rrette.finenight.fsa-builder)

;; initial-state speak of itself.
;; final-states is a list of nodes considered as final
;; transitions is a list of 3-tuple. (source-node input-symbol destination-node)
(defstruct (fsa-builder (:copier nil))
  (initial-state 0)
  (nodes (make-hash-table))
  (finals '()))


(defun fsa-edges (fsa)
  (labels ((E  (nodes) 
	       (if (null nodes)
		   '()
		 (append (node-edges (car nodes))
			 (E (cdr nodes))))))
	  (E (hash-values (fsa-builder-nodes fsa)))))



;; (define fsa-node-ancestrors
;;   (lambda (fsa label)
;;     (hash-table-ref/default (fsa-ancestrors-nodes fsa)
;;                             label
;;                             '())))

;; (define fsa-remove-ancestror!
;;   (lambda (fsa node)
;;     (map (lambda (child)
;;            (hash-table-update!/default (fsa-ancestrors-nodes fsa)
;;                                        (node-label child)
;;                                        (lambda (lst)
;;                                          (delete! node lst))
;;                                        '()))
;;          (node-destinations node))))

(defun fsa-add-edge! (fsa src-label input-symbol dst-label)
  (let ((src-node (hash-table-update!/default (lambda (x) x) src-label (fsa-builder-nodes fsa) (make-empty-node src-label)))
	(dst-node (hash-table-update!/default (lambda (x) x) dst-label (fsa-builder-nodes fsa) (make-empty-node dst-label))))
    (node-add-edge! src-node input-symbol dst-node)
    fsa))

(defun fsa-remove-node! (fsa node)
  (let* ((label (node-label node)))
    (remhash label (fsa-builder-nodes fsa))
    (setf (fsa-builder-finals fsa) (delete node (fsa-builder-finals fsa)))
    fsa))

(defun fsa-remove-edge! (fsa src-label input-symbol dst-label)
  (let ((src-node (hash-table-ref/default (fsa-builder-nodes fsa) src-label nil))
	(dst-node (hash-table-ref/default (fsa-builder-nodes fsa) dst-label nil)))
    (if (and src-node dst-node)
	(node-remove-edge! src-node input-symbol dst-node))
    fsa))
  
(defun build-fsa (initial-label edges finals)
  (let ((fsa (reduce #'(lambda (edge fsa)
			 (fsa-add-edge! fsa (car edge) (cadr edge) (caddr edge)))
		     edges
		     :initial-value (make-fsa-builder :initial-state initial-label))))
    (reduce #'(lambda (final fsa)
		(fsa-add-final! fsa final))
	  finals
	  :initial-value fsa)))

(defun make-empty-fsa-builder (initial-label)
  (let ((fsa (make-fsa-builder :initial-state initial-label)))
    (hash-table-update!/default  (lambda (x) x) initial-label (fsa-builder-nodes fsa) (make-empty-node initial-label))
    fsa))

(defun get-node (fsa node-label) 
  (gethash node-label (fsa-builder-nodes fsa)))

;(define get-node 
;  (lambda (fsa node-label) 
;    (my-hash-table-get! (fsa-nodes fsa) node-label (lambda () (make-empty-node node-label)))))


(defun get-state (fsa label) 
  (node-label (get-node fsa label)))

;; (define build-fsa
;;   (lambda (alphabet initial-states final-states edges)
;;     (let* ((node-map (make-hash-table))
;; 	   (get-node 
;; 	    (lambda (node) 
;; 	      (hash-table-ref node-map 
;; 			      node 
;; 			      (make-node node (make-hash-table) #f)))))
;;       (letrec ((update-final-nodes 
;; 		(lambda (nodes) 
;; 		  (if (null? nodes)
;; 		      #f
;; 		      (set! (node-final (get-node (car nodes))) #t)
;; 		      (update-final-nodes (cdr nodes)))))
;; 	       (B (lambda (edges)
;; 		    (if (null? edges)
;; 			;;
;; 			(let* ((edge (car edges))
;; 			       (src-node (get-node (source-node edge)))
;; 			       (dst-node (get-node (destination-node edge))))
;; 			  (node-add-edge! src-node 
;; 					  (input-symbol edge)
;; 					  dst-node))))
;; 		  (B (cdr edges))))
;; 	(B edges)
;; 	(make-fsa alphabet 
;; 		  (get-node initial-state)
		  

;; this function returns a list of destination nodes
;; for a given source node and an input symbol
;; (define transition 
;;   (lambda (fsa node input)
;;     (letrec 
;; 	((T (lambda (edges)
;; 	      (if (null? edges)
;; 		  '()
;; 		  (let ((edge (car edges)))
;; 		    (if (and (eq? (source-node edge) node)
;; 			     (eq? (input-symbol edge) input))
;; 			(cons (destination-node edge) 
;; 			      (T (cdr edges)))
;; 			(T (cdr edges))))))))
;;       (T (edges fsa)))))

;; this function returns true if the node is 
;; part of the final states.
(defun final? (node)
  (node-final node))


(defun fsa-add-final! (fsa node-label)
  (fsa-add-final-node! fsa (get-node fsa node-label)))

(defun fsa-add-final-node! (fsa node)
  (setf (fsa-builder-finals fsa) (append (fsa-builder-finals fsa) (list node)))
  (setf (node-final node) t)
  fsa)

(defun fsa-add-node! (fsa node)
  (if (node-final node)
      (fsa-add-final-node! fsa node))
  (hash-table-update! (node-label node)  
		      (fsa-builder-nodes fsa)
		      n
		      (declare (ignore n))
		      node))

(defun graphviz-export (fsa) 
    (graphviz-export-to-file fsa "test.dot"))

(defun graphviz-export-to-file (fsa file) 
  "This function will write the dot description of the FSA in the stream."
  (let ((p (open file :direction :output :if-exists :supersede)))
    (format p "digraph G {~%  rankdir = LR;~%  size = \"8, 10\";~%") 
    (if (not (null (fsa-builder-finals fsa)))
	(progn
	  (format p "~%  node [shape = doublecircle];~% ")
	  (dolist (x (fsa-builder-finals fsa))
            (format p " \"~A\"" (node-label x)))
	  (format p ";")))
    (format p "~%~%  node [shape = circle];~% ")
    (dolist (label (hash-keys (fsa-builder-nodes fsa)))
      (format p " \"~A\"" label))
    (format p ";~%~%")
    (dolist (node (hash-values (fsa-builder-nodes fsa)))
      (dolist (edge (node-edges node))
        (format p 
                "  \"~A\" -> \"~A\" [label = \"~A\"];~%"
                (car edge)
                (caddr edge)
                (if (null (cadr edge))
                    "epsilon"
                  (cadr edge)))))
    (format p "}~%") 
    (close p)
    fsa))




(defun fsa-initial-node (fsa)
    (get-node fsa (fsa-builder-initial-state fsa)))


(defun make-fsa-builder-from-fsa (fsa)
    (let ((fsa-builder (make-empty-fsa-builder (node-label (fsa-start-node fsa))))
          (nodes (list (fsa-start-node fsa))))
      (labels ((retreive-nodes (n)
			       (if (null n)
				   (build-fsa-builder-with-nodes)
				 (progn
				   (setf nodes (cons (car n) nodes))
				   (retreive-nodes (append (cdr n) (set-difference (node-destinations (car n))
										   nodes
										   :test #'eq))))))
	       (build-fsa-builder-with-nodes ()
                                             (dolist (node nodes)
                                               (fsa-add-node! fsa-builder node))))
        (retreive-nodes nodes))
      fsa-builder))

      
(defun fsa-builder-accept? (fsa-builder word)
  (labels ((T (node word)
	      (if (null word) 
		  (node-final node)
		(let ((nodes (node-transition node (car word))))
		  (if (null nodes)
		      nil
		    (T (car nodes) (cdr word)))))))
	  (T (fsa-initial-node fsa-builder) word)))

