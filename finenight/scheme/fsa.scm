(define-extension fsa)
(require-extension utils-scm)
;;(declare (unit fsa))

;; (define edge 
;;   (lambda (source-node input-symbol destination-node)
;;     (lambda (selector)
;;       (selector source-node input-symbol destination-node))))

;; (define source-node
;;   (lambda (edge)
;;     (car edge)))

;; (define input-symbol
;;   (lambda (edge)
;;     (car (cdr edge))))

;; (define destination-node
;;   (lambda (edge)
;;     (car (cdr (cdr edge)))))


(define rember
  (lambda (values value)
    (cond ((null? values) '())
	  ((eq? (car values) value)
	   (rember (cdr values) value))
	  (else (cons (car values) 
		      (rember (cdr values)))))))

;; the node consists of a label and a map a symbol to 
;; a destination object. 
(define-record node 
  label
  symbols-map
  final)

(define-record-printer (node x out)
  (fprintf out "~S"
	   (node-edges x)))



(define make-empty-node
  (lambda (label)
    (make-node label (make-hash-table) #f)))

(define node-symbols
  (lambda (node)
    (hash-table-keys (node-symbols-map node))))

(define node-edges
  (lambda (node)
    (letrec ((label (node-label node))
	     (S (lambda (symbols)
		  (if (null? symbols)
		      '()
		      (append (map (lambda (dest-node)
				     (list label 
					   (car symbols) 
					   (node-label dest-node)))
				   (node-transition node (car symbols)))
			      (S (cdr symbols)))))))
      (S (node-symbols node)))))

(define node-add-edge!
  (lambda (node input-symbol dst-node)
    (let ((symbols-map (node-symbols-map node)))
      (hash-table-set! symbols-map 
		       input-symbol 
		       (append 
			(hash-table-ref symbols-map
					input-symbol
					(lambda () (quote ())))
			(list dst-node))))))

(define node-remove-edge!
  (lambda (node input-symbol dst-node)
    (let ((symbols-map (node-symbols-map node)))
      (hash-table-set! symbols-map 
		       input-symbol 
		       (rember 
			(hash-table-ref symbols-map
					input-symbol
					(lambda () (quote ())))
			dst-node)))))


;; will return the list of destination nodes for the
;; given node.
(define node-transition
  (lambda (node symbol)
    (hash-table-ref (node-symbols-map node) symbol (lambda () '()))))



;; initial-state speak of itself.
;; final-states is a list of nodes considered as final
;; transitions is a list of 3-tuple. (source-node input-symbol destination-node)
(define-record fsa
  initial-state
  nodes)

(define-record-printer (fsa x out)
  (fprintf out "(fsa ~S ~S ~S)"
	   (fsa-initial-state x) (fsa-finals x) (fsa-edges x)))

(define fsa-initial-node
  (lambda (fsa)
    (get-node fsa (fsa-initial-state fsa))))

(define my-hash-table-update!
  (lambda (hash-table key default-func)
    (if (hash-table-exists? hash-table key)
	(hash-table-ref hash-table key)
	(let ((value (default-func)))
	  (hash-table-set! hash-table key value)
	  value))))

(define fsa-edges
  (lambda (fsa)
    (letrec ((E (lambda (nodes) 
		  (if (null? nodes)
		      '()
		      (append (node-edges (car nodes))
			      (E (cdr nodes)))))))
      (E (hash-table-values (fsa-nodes fsa))))))

(define fsa-finals
  (lambda (fsa)
    (letrec ((E (lambda (nodes) 
		  (cond
		   ((null? nodes) '())
		   ((final? (car nodes)) 
		    (cons (node-label (car nodes))
			  (E (cdr nodes))))
		   ((E (cdr nodes)))))))
      (E (hash-table-values (fsa-nodes fsa))))))
	  

(define fsa-add-edge!
  (lambda (fsa src-label input-symbol dst-label)
    (let ((src-node (my-hash-table-update! (fsa-nodes fsa) src-label (lambda () (make-empty-node src-label))))
	  (dst-node (my-hash-table-update! (fsa-nodes fsa) dst-label (lambda () (make-empty-node dst-label)))))
      (node-add-edge! src-node input-symbol dst-node)
      fsa)))

(define fsa-remove-edge!
  (lambda (fsa src-label input-symbol dst-label)
    (let ((src-node (my-hash-table-update! (fsa-nodes fsa) src-label (lambda () (make-empty-node src-label))))
	  (dst-node (my-hash-table-update! (fsa-nodes fsa) dst-label (lambda () (make-empty-node dst-label)))))
      (node-remove-edge! src-node input-symbol dst-node)
      fsa)))
  
(define build-fsa
  (lambda (edges)
    (reduce (lambda (fsa edge)
	      (fsa-add-edge! fsa (car edge) (cadr edge) (caddr edge)))
	    (make-empty-fsa)
	    edges)))

(define get-node 
  (lambda (fsa node-label) 
    (my-hash-table-update! (fsa-nodes fsa) node-label (lambda () (make-empty-node node-label)))))

(define get-state 
  (lambda (fsa label) 
    (node-label (get-node fsa label))))

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
(define final?
  (lambda (node)
    (node-final node)))



(define accept? 
  (lambda (fsa word)
    (let ((initial-node (get-node fsa (fsa-initial-state fsa))))
      (letrec ((T (lambda (node word)
		    (if (null? word) 
			(node-final node)
			(let ((nodes (node-transition node (car word))))
			  (if (null? nodes)
			      #f
			      (T (car nodes) (cdr word))))))))
	(T initial-node word)))))

(define fsa-add-final! 
  (lambda (fsa node-label)
    (node-final-set! (get-node fsa node-label) #t)
    fsa))
			      
