;(define-extension fsa)

(require-extension format)
;(require-extension utils-scm)
(require-extension srfi-1)


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

(define node-destinations
  (lambda (node)
    (apply append (hash-table-values (node-symbols-map node)))))

(define node-arity
  (lambda (node)
    (hash-table-size (node-symbols-map node))))

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

(define node-walk
  (lambda (node proc)
    (hash-table-walk (node-symbols-map node) proc)))

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
	     (length (hash-table-ref/default symbols-map input-symbol '())))
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

(define node-remove-dsts-for-input!
  (lambda (node input)
    (let ((symbols-map (node-symbols-map node)))
      (hash-table-delete! symbols-map input)
      node)))


;; will return the list of destination nodes for the
;; given node.
(define node-transition
  (lambda (node symbol)
    (hash-table-ref (node-symbols-map node) symbol (lambda () '()))))


(define node-is-equivalent
  (lambda (lhs rhs)
      (if (not (eq? (node-final lhs) (node-final rhs)))
	  #f
          (let ((lhs-map (node-symbols-map lhs))
                (rhs-map (node-symbols-map rhs)))
            (map-equal? lhs-map rhs-map)))))
		  


(define-record fsa start-node)

(define make-empty-fsa
  (lambda (start-label)
    (make-fsa (make-empty-node start-label))))

(define accept? 
  (lambda (fsa word)
    (letrec ((T (lambda (node word)
                  (if (null? word) 
                      (node-final node)
                      (let ((nodes (node-transition node (car word))))
                        (if (null? nodes)
                            #f
                            (T (car nodes) (cdr word))))))))
      (T (fsa-start-node fsa) word))))


;(define-record-printer (fsa x out)
;  (fprintf out
;           "(fsa ~S ~S ~S)"
;	   (fsa-initial-state x) (fsa-finals x) (hash-table->alist (fsa-nodes x))))

  

  