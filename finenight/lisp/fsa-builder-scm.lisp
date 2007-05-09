(defpackage :com.rrette.finenight
  (:use "COMMON-LISP")
  (:nicknames "finenight")
  (:export "make-fsa-builder"))


(in-package :com.rrette.finenight)
(provide :com.rrette.finenight.fsa-builder)

(require :com.rrette.finenight.utils "utils.lisp")

;; initial-state speak of itself.
;; final-states is a list of nodes considered as final
;; transitions is a list of 3-tuple. (source-node input-symbol destination-node)
(defstruct (fsa-builder (:copier nil))
  initial-state 
  (nodes (make-hash-table))
  (finals '()))

(defun make-fsa-builder-from-fsa (fsa)
    (let ((fsa-builder (make-empty-fsa-builder (node-label (fsa-start-node fsa))))
          (nodes (list (fsa-start-node fsa))))
      (labels ((retreive-nodes (n)
			       (if (null n)
				   (build-fsa-builder-with-nodes)
				 (progn
				   (setf nodes (cons (car n) nodes))
				   (retreive-nodes (append (cdr n) (lset-difference eq
										    (node-destinations (car n))
										    nodes)))))
			       (build-fsa-builder-with-nodes
				(lambda ()
				  (for-each (lambda (node)
					      (fsa-add-node! fsa-builder node))
					    nodes)))))
	      (retreive-nodes nodes))
      fsa-builder))

      

(defun fsa-initial-node (fsa)
    (get-node fsa (fsa-builder-initial-state fsa)))


(defun fsa-edges (fsa)
  (labels ((E  (nodes) 
	       (if (null? nodes)
		   '()
		 (append (node-edges (car nodes))
			 (E (cdr nodes))))))
	  (E (hash-table-values (fsa-builder-nodes fsa)))))



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
  (let ((src-node (hash-table-update!/default (fsa-builder-nodes fsa) src-label (lambda (x) x) (make-empty-node src-label)))
	(dst-node (hash-table-update!/default (fsa-builder-nodes fsa) dst-label (lambda (x) x) (make-empty-node dst-label))))
    (node-add-edge! src-node input-symbol dst-node)
    fsa))

(defun fsa-remove-node! (fsa node)
  (let* ((label (node-label node)))
    (remhash label (fsa-builder-nodes fsa))
    (setf (fsa-builder-finals fsa) (delete node (fsa-builder-finals fsa)))
    fsa))

(defun fsa-remove-edge! (fsa src-label input-symbol dst-label)
  (let ((src-node (hash-table-ref/default (fsa-builder-nodes fsa) src-label #f))
	(dst-node (hash-table-ref/default (fsa-builder-nodes fsa) dst-label #f)))
    (if (and src-node dst-node)
	(node-remove-edge! src-node input-symbol dst-node))
    fsa))
  
(defun build-fsa (initial-label edges finals)
  (let ((fsa (fold (lambda (edge fsa)
		     (fsa-add-edge! fsa (car edge) (cadr edge) (caddr edge)))
		   (make-empty-fsa-builder initial-label)
		   edges)))
    (fold (lambda (final fsa)
	    (fsa-add-final! fsa final))
	  fsa
	  finals)))

(defun make-empty-fsa-builder (initial-label)
  (let ((fsa (make-fsa-builder initial-label (make-hash-table) (list))))
    (hash-table-update!/default (fsa-builder-nodes fsa) initial-label (lambda (x) x) (make-empty-node initial-label))
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
  (fsa-builder-finals-set! fsa (append (fsa-builder-finals fsa) (list node)))
  (setf (node-final node) t)
  fsa)

(defun fsa-add-node! (fsa node)
  (if (node-final node)
      (fsa-add-final-node! fsa node))
  (hash-table-update! (lambda (n) node) (node-label node)  (fsa-builder-nodes fsa)))

(defun graphviz-export (fsa) 
    (graphviz-export-to-file fsa "test.dot"))

(defun graphviz-export-to-file (fsa file) 
  "This function will write the dot description of the FSA in the stream."
  (let ((p (open-output-file file)))
    (display (format "digraph G {~%  rankdir = LR;~%  size = \"8, 10\";~%") 
	     p)
    (if (not (null? (fsa-builder-finals fsa)))
	(progn
	  (display (format "~%  node (shape = doublecircle);~% ")
		   p)
	  (map (lambda (x) 
		 (display (format " \"~A\"" (node-label x))
			  p))
	       (fsa-builder-finals fsa))
	  (display ";")))
    (display (format "~%~%  node (shape = circle);~% ")
	     p)
    (map (lambda (label)
	   (display (format " \"~A\"" label)
		    p))
	 (hash-table-keys (fsa-builder-nodes fsa)))
    (display (format ";~%~%")
	     p)
    (map (lambda (node)
	   (map (lambda (edge)
		  (display (format "  \"~A\" -> \"~A\" (label = \"~A\");~%"
				   (car edge)
				   (caddr edge)
				   (if (null? (cadr edge))
				       "epsilon"
				     (cadr edge)))
			   p))
		(node-edges node)))
	 (hash-table-values (fsa-builder-nodes fsa)))
    (display (format "}~%") 
	     p)
    (close-output-port p)
    fsa))


(defun fsa-builder-accept? (fsa-builder word)
  (labels ((T (node word)
	      (if (null? word) 
		  (node-final node)
		(let ((nodes (node-transition node (car word))))
		  (if (null? nodes)
		      nil
		    (T (car nodes) (cdr word)))))))
	  (T (fsa-initial-node fsa-builder) word)))
