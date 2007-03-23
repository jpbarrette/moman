;(define-extension utils-scm)

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
	    
	