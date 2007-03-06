(define-extension utils-scm)

(define reduce
  (lambda (func initial-value values)
    (letrec ((R (lambda (v1 values)
		  (if (null? (cdr values))
		      (func v1 (car values))
		      (R (func v1 (car values)) (cdr values))))))
      (R initial-value values))))
		      
