;(define-extension utils-scm)

(define rember
  (lambda (values value)
    (cond ((null? values) '())
	  ((equal? (car values) value)
	   (rember (cdr values) value))
	  (else (cons (car values) 
		      (rember (cdr values) value))))))

(define my-hash-table-get!
  (lambda (hash-table key default-func)
    (if (hash-table-exists? hash-table key)
	(hash-table-ref hash-table key)
	(let ((value (default-func)))
	  (hash-table-set! hash-table key value)
	  value))))

(define my-hash-table-update!
  (lambda (hash-table key default-func func)
    (if (not (hash-table-exists? hash-table key))
	(hash-table-set! hash-table key (default-func)))
    (hash-table-set! hash-table key 
		     (func (hash-table-ref hash-table key)))))


(define reduce
  (lambda (func initial-value values)
    (letrec ((R (lambda (v1 values)
		  (if (null? (cdr values))
		      (func v1 (car values))
		      (R (func v1 (car values)) (cdr values))))))
      (R initial-value values))))
		      
(define every
  (lambda (func list1 list2)
    (cond 
     ((or (null? list1) (null? list2)) #t)
     ((func (car list1) (car list2)) (every func (cdr list1) (cdr list2)))
     (else #f))))

(define some
  (lambda (func lst)
    (if (null? lst)
	#f
	(let ((result (func (car lst))))
	  (if (not result)
	      (some func (cdr lst))
	      result)))))
	    
       
(define (for-each-line-in-file filename proc . mode)
  (with-input-from-file
   filename
   (lambda () (apply for-each-line proc (current-input-port) mode))))

(define (for-each-line proc . port+mode)
  (let while ()
    (let ((line (apply read-line port+mode)))
      (unless (eof-object? line)
        (proc line)
        (while)))))