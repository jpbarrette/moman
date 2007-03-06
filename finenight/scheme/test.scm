(define my-hash (make-hash-table))

(display (hash-table-ref my-hash
			 #\b
			 (lambda () (quote ()))))

(hash-table-set! 
 my-hash
 #\b 
 (append (list ) '(b)))

(display (hash-table-ref my-hash #\b))
