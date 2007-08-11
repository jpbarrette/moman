;(define-extension fsa-builder)

;(require-extension fsa)

;; initial-state speak of itself.
;; final-states is a list of nodes considered as final
;; transitions is a list of 3-tuple. (source-node input-symbol destination-node)
(define-record fsa-builder initial-state nodes finals)

(define-record-printer (fsa-builder x out)
  (fprintf out "(fsa ~S ~S ~S)"
	   (fsa-builder-initial-state x) (fsa-builder-finals x) (hash-table->alist (fsa-builder-nodes x))))

(define make-fsa-builder-from-fsa
  (lambda (fsa)
    (let ([fsa-builder (make-empty-fsa-builder (node-label (fsa-start-node fsa)))]
          [nodes (list (fsa-start-node fsa))])
      (letrec ([retreive-nodes (lambda (n)
                                 (if (null? n)
                                     (build-fsa-builder-with-nodes)
                                     (begin
                                       (set! nodes (cons (car n) nodes))
                                       (retreive-nodes (append (cdr n) (lset-difference eq?
                                                                                        (node-destinations (car n))
                                                                                        nodes))))))]
               [build-fsa-builder-with-nodes
                (lambda ()
                  (for-each (lambda (node)
                              (fsa-add-node! fsa-builder node))
                            nodes))])
        (retreive-nodes nodes))
      fsa-builder)))

      

(define fsa-initial-node
  (lambda (fsa)
    (get-node fsa (fsa-builder-initial-state fsa))))


(define fsa-edges
  (lambda (fsa)
    (letrec ((E (lambda (nodes) 
		  (if (null? nodes)
		      '()
		      (append (node-edges (car nodes))
			      (E (cdr nodes)))))))
      (E (hash-table-values (fsa-builder-nodes fsa))))))

;; (define node-is-equivalent
;;   (lambda (lhs rhs)
;;     (letrec ((edges-are-equivalent 
;; 	      (lambda (lhs-edges rhs-edges)
;; 		(cond ((null? lhs-edges) #t)
;; 		      ((not (member (car lhs-edges) rhs-edges)) #f)
;; 		      (else (edges-are-equivalent (cdr lhs-edges) rhs-edges))))))
;;       (if (not (equal? (node-final lhs) (node-final rhs)))
;; 	  #f
;; 	  (let ((lhs-edges (node-edges2 lhs))
;; 		(rhs-edges (node-edges2 rhs)))
;; 	    (cond ((not (equal? (length lhs-edges) (length rhs-edges))) #f)
;; 		  (else (edges-are-equivalent lhs-edges rhs-edges))))))))


(define map-equal?
  (lambda (lhs rhs)
    (and (eq? (hash-table-size lhs) (hash-table-size rhs))
         (equal? lhs rhs))))
		  


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

(define fsa-add-edge!
  (lambda (fsa src-label input-symbol dst-label)
    (let ((src-node (hash-table-update!/default (fsa-builder-nodes fsa) src-label (lambda (x) x) (make-empty-node src-label)))
	  (dst-node (hash-table-update!/default (fsa-builder-nodes fsa) dst-label (lambda (x) x) (make-empty-node dst-label))))
      (node-add-edge! src-node input-symbol dst-node)
      fsa)))

(define fsa-remove-node!
  (lambda (fsa node)
    (let* ((label (node-label node)))
      (hash-table-delete! (fsa-builder-nodes fsa) label)
      (fsa-builder-finals-set! fsa (delete! node (fsa-builder-finals fsa)))
      fsa)))

(define fsa-remove-edge!
  (lambda (fsa src-label input-symbol dst-label)
    (let ((src-node (hash-table-ref/default (fsa-builder-nodes fsa) src-label #f))
	  (dst-node (hash-table-ref/default (fsa-builder-nodes fsa) dst-label #f)))
      (if (and src-node dst-node)
          (node-remove-edge! src-node input-symbol dst-node))
      fsa)))
  
(define build-fsa
  (lambda (initial-label edges finals)
    (let ((fsa (fold (lambda (edge fsa)
			     (fsa-add-edge! fsa (car edge) (cadr edge) (caddr edge)))
			   (make-empty-fsa-builder initial-label)
			   edges)))
      (fold (lambda (final fsa)
              (fsa-add-final! fsa final))
            fsa
            finals))))

(define make-empty-fsa-builder
  (lambda (initial-label)
    (let ((fsa (make-fsa-builder initial-label (make-hash-table) (list))))
      (hash-table-update!/default (fsa-builder-nodes fsa) initial-label (lambda (x) x) (make-empty-node initial-label))
      fsa)))

(define get-node 
  (lambda (fsa node-label) 
    (hash-table-ref/default (fsa-builder-nodes fsa) node-label #f)))

;(define get-node 
;  (lambda (fsa node-label) 
;    (my-hash-table-get! (fsa-nodes fsa) node-label (lambda () (make-empty-node node-label)))))


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


(define fsa-add-final! 
  (lambda (fsa node-label)
    (fsa-add-final-node! fsa (get-node fsa node-label))))

(define fsa-add-final-node! 
  (lambda (fsa node)
    (fsa-builder-finals-set! fsa (append (fsa-builder-finals fsa) (list node)))
    (node-final-set! node #t)
    fsa))

(define fsa-add-node!
  (lambda (fsa node)
    (if (node-final node)
        (fsa-add-final-node! fsa node))
    (hash-table-update!/default (fsa-builder-nodes fsa) (node-label node) (lambda (n) node) node)))

(define graphviz-export
  (lambda (fsa) 
    (graphviz-export-to-file fsa "test.dot")))

(define graphviz-export-to-file
  (lambda (fsa file) 
    "This function will write the dot description of the FSA in the stream."
    (let ((p (open-output-file file)))
     (display (format "digraph G {~%  rankdir = LR;~%  size = \"8, 10\";~%") 
	      p)
     ;(display (format "  rotate = 90;~%")
     ;	      p)
     (if (not (null? (fsa-builder-finals fsa)))
	 (begin
	  (display (format "~%  node [shape = doublecircle];~% ")
		   p)
	  (map (lambda (x) 
		 (display (format " \"~A\"" (node-label x))
			  p))
	       (fsa-builder-finals fsa))
	  (display ";")))
     (display (format "~%~%  node [shape = circle];~% ")
	      p)
     (map (lambda (label)
	    (display (format " \"~A\"" label)
		     p))
	  (hash-table-keys (fsa-builder-nodes fsa)))
     (display (format ";~%~%")
	      p)
     (map (lambda (node)
	    (map (lambda (edge)
		   (display (format "  \"~A\" -> \"~A\" [label = \"~A\"];~%"
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
     fsa)))


(define fsa-builder-accept? 
  (lambda (fsa-builder word)
    (letrec ((T (lambda (node word)
                  (if (null? word) 
                      (node-final node)
                      (let ((nodes (node-transition node (car word))))
                        (if (null? nodes)
                            #f
                            (T (car nodes) (cdr word))))))))
      (T (fsa-initial-node fsa-builder) word))))
