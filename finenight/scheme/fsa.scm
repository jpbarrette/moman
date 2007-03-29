(define-extension fsa)
(require-extension format)

(require-extension utils-scm)
(require-extension srfi-1)

;(load "utils-scm.scm")

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


;; the node consists of a label and a map a symbol to 
;; a destination object. 
(define-record node 
  label 
  symbols-map
  final)

;;(print-struct #t)

(define-record-printer (node x out)
  (fprintf out "(node ~S ~S ~A)"
	   (node-label x) 
	   (node-edges2 x)
	   (node-final x)))



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

(define node-edges2
  (lambda (node)
    (letrec ((label (node-label node))
	     (S (lambda (symbols)
		  (if (null? symbols)
		      '()
		      (append (map (lambda (dest-node)
				     (cons (car symbols) 
					   (node-label dest-node)))
				   (node-transition node (car symbols)))
			      (S (cdr symbols)))))))
      (S (node-symbols node)))))


(define node-add-edge!
  (lambda (node input-symbol dst-node)
    (hash-table-update!/default (node-symbols-map node)
			input-symbol 
			(lambda (lst)
			  (cons dst-node lst))
			'())))


(define node-remove-edge!
  (lambda (node input-symbol dst-node)
    (let ((symbols-map (node-symbols-map node)))
      (if (< 1
	     (length hash-table-href/default symbols-map input-symbol '()))
	  (hash-table-update!/default symbols-map 
			      input-symbol 
			      (lambda (lst)
				(delete! dst-node lst eq?))
			      '())
	  (hash-table-delete! symbols-map input-symbol))
      node)))

(define node-remove-dst!
  (lambda (node dst-node)
    (let ((symbols-map (node-symbols-map node)))
      (map (lambda (symbol)
	     (hash-table-update!/default symbols-map 
				 symbol 
				 (lambda (lst)
				   (delete! dst-node lst eq?))
				 '()))
	   (node-symbols node)))
    node))


;; will return the list of destination nodes for the
;; given node.
(define node-transition
  (lambda (node symbol)
    (hash-table-ref (node-symbols-map node) symbol (lambda () '()))))



;; initial-state speak of itself.
;; final-states is a list of nodes considered as final
;; transitions is a list of 3-tuple. (source-node input-symbol destination-node)
(define-record fsa initial-state nodes ancestrors-nodes finals)

(define-record-printer (fsa x out)
  (fprintf out "(fsa ~S ~S ~S)"
	   (fsa-initial-state x) (fsa-finals x) (hash-table->alist (fsa-nodes x))))

(define fsa-initial-node
  (lambda (fsa)
    (get-node fsa (fsa-initial-state fsa))))



(define fsa-edges
  (lambda (fsa)
    (letrec ((E (lambda (nodes) 
		  (if (null? nodes)
		      '()
		      (append (node-edges (car nodes))
			      (E (cdr nodes)))))))
      (E (hash-table-values (fsa-nodes fsa))))))

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

(define node-is-equivalent
  (lambda (lhs rhs)
      (if (not (equal? (node-final lhs) (node-final rhs)))
	  #f
	  (equal? (node-symbols-map lhs)
		  (node-symbols-map rhs)))))

(define fsa-node-ancestrors
  (lambda (fsa label)
    (map (lambda (parent-label)
           (get-node fsa parent-label))
         (hash-table-ref/default (fsa-ancestrors-nodes fsa)
                                 label
                                 '()))))

(define fsa-add-edge!
  (lambda (fsa src-label input-symbol dst-label)
    (let ((src-node (my-hash-table-get! (fsa-nodes fsa) src-label (lambda () (make-empty-node src-label))))
	  (dst-node (my-hash-table-get! (fsa-nodes fsa) dst-label (lambda () (make-empty-node dst-label))))
	  (ancestrors (hash-table-ref/default (fsa-ancestrors-nodes fsa) dst-label '())))
      (if (not (memq src-label ancestrors))
	  (hash-table-set! (fsa-ancestrors-nodes fsa) dst-label (cons src-label ancestrors)))
      (node-add-edge! src-node input-symbol dst-node)
      fsa)))

(define fsa-remove-node!
  (lambda (fsa node)
    (let* ((label (node-label node))
	   (ancestrors (hash-table-ref/default (fsa-ancestrors-nodes fsa) label (list))))
      (map (lambda (ancestror)
	     (node-remove-dst! (get-node fsa ancestror) node))
	   ancestrors)
      (hash-table-delete! (fsa-nodes fsa) label)
      (fsa-finals-set! fsa (delete! node (fsa-finals fsa)))
      fsa)))

(define fsa-remove-edge!
  (lambda (fsa src-label input-symbol dst-label)
    (let ((src-node (my-hash-table-get! (fsa-nodes fsa) src-label (lambda () (make-empty-node src-label))))
	  (dst-node (my-hash-table-get! (fsa-nodes fsa) dst-label (lambda () (make-empty-node dst-label)))))
      (hash-table-update!/default (fsa-ancestrors-nodes fsa) 
			  dst-label 
			  (lambda (lst)
			    (delete! dst-label lst eq?))
			  '())
      (node-remove-edge! src-node input-symbol dst-node)
      fsa)))
  
(define build-fsa
  (lambda (initial-label edges finals)
    (let ((fsa (fold (lambda (edge fsa)
			     (fsa-add-edge! fsa (car edge) (cadr edge) (caddr edge)))
			   (make-empty-fsa initial-label)
			   edges)))
      (fold (lambda (final fsa)
              (fsa-add-final! fsa final))
            fsa
            finals))))

(define make-empty-fsa
  (lambda (initial-label)
    (make-fsa initial-label (make-hash-table) (make-hash-table) (list))))

(define get-node 
  (lambda (fsa node-label) 
    (my-hash-table-get! (fsa-nodes fsa) node-label (lambda () (make-empty-node node-label)))))

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
    (fsa-add-final! fsa (get-node fsa node-label))))

(define fsa-add-final-node! 
  (lambda (fsa node)
    (fsa-finals-set! fsa (cons node (fsa-finals fsa)))
    (node-final-set! node #t)
    fsa))

(define graphviz-export
  (lambda (fsa) 
    "This function will write the dot description of the FSA in the stream."
    (let ((p (open-output-file "test.dot")))
     (display (format "digraph G {~%  rankdir = LR;~%  size = \"8, 10\";~%") 
	      p)
     (display (format "  rotate = 90;~%")
	      p)
     (if (not (null? (fsa-finals fsa)))
	 (begin
	  (display (format "~%  node [shape = doublecircle];~% ")
		   p)
	  (map (lambda (x) 
		 (display (format " \"~A\"" (node-label x))
			  p))
	       (fsa-finals fsa))
	  (display ";")))
     (display (format "~%~%  node [shape = circle];~% ")
	      p)
     (map (lambda (label)
	    (display (format " \"~A\"" label)
		     p))
	  (hash-table-keys (fsa-nodes fsa)))
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
	  (hash-table-values (fsa-nodes fsa)))
     (display (format "}~%") 
	      p)
     (close-output-port p)
     fsa)))

  

  