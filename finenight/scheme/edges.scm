;; edges is seen by others as a list of 3-tupes 
;; (source-node input-symbol destination-node)

;; this is the internal representation of edges
;; ((src-node1 (input-symbol1 dst-node1) (input-symbol2 dst-node2)) 
;;  (src-node2 (input-symbol3 dst-node1) (input-symbol2 dst-node3)))

;; should return ((input-symbol3 dst-node1) (input-symbol2 dst-node3))
(define get-edges-for-source-node 
  (lambda (edges node)
    (cond 
     ((null? edges) 
      '())
     ((eq? (car (car edges)) node) 
      (cdr (car edges)))
     (else 
      (get-edges-for-source-node (cdr edges) node)))))
	
;; should return (dst-node1 dst-node2)
(define transition-for-source-node
  (lambda (node input)
    (letrec ((T 
	      (lambda (transition-table-for-node)
		(if ((null? (transition-table-for-node)
		       '())
		    (let ((entry (car (transition-table-for-node))))
		      (if (eq? input
			       (car (entry)))
			  (cons (cdr (entry)) 
				(T (cdr transition-table-for-node)))
			  (T (cdr transition-table-for-node)))))))))
      (T (node)))))


;; from a set of edges it returns the destination nodes for the given 
;; input and node.
(define transition
  (lambda (edges node-label input)
    (transition-for-source-node (get-edges-for-source-node edges node) input)))
		    

(define add-edge
  (lambda (edges edge)
    (if (null? edges)
	(cons (car edge) (cons (cdr edge) '()))
	(let ((node (car (car edges)))
	      (entries (cdr (car edges))))
	  (if (eq? (car edge) (car (edges)))
	      (cons (car edges) 
