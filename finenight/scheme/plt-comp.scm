(define hash-table-exists? 
  (lambda (table key)
    (hash-table-ref table key #f))) 

(define hash-table-values
  (lambda (table)
    (hash-table-map table (lambda (key value) value))))

(define hash-table-set! hash-table-put!)
(define hash-table-ref  hash-table-get)

(define hash-table-keys
  (lambda (table)
    (hash-table-map table (lambda (key value) key))))

;(define node-final-set! set-node-final!)

