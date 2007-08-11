;(define-extension utils-scm)

(define my-hash-table-get!
  (lambda (hash-table key default-func)
    (if (hash-table-exists? hash-table key)
	(hash-table-ref hash-table key)
	(let ((value (default-func)))
	  (hash-table-set! hash-table key value)
	  value))))


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

(define vector-walk
  (lambda (vector func)
    (let ([index 0])
      (for-each (lambda (elem)
                  (func index elem)
                  (set! index (+ index 1)))
                (vector->list vector)))))