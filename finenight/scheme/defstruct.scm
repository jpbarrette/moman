;;; A Common Lisp-like DEFSTRUCT facility for Scheme.


(define-syntax (def-struct struct-name . specs&methods)
    (receive (slots methods)
	(let* ((meths&specs (reverse specs&methods))
	       (maybe-meths (car meths&specs)))
	      (if (and (pair? maybe-meths)
		       (pair? (car maybe-meths)))
		  (return (reverse! (cdr meths&specs)) maybe-meths)
		  (return specs&methods '#f)))
	(let* ((stype-name (concatenate-symbol struct-name '-stype))
	       (slot-names (for (s in slots)
				(save (if (symbol? s) s (car s)))))
	       (slotac-names (for (s in slot-names)
				  (save (concatenate-symbol struct-name ': s))))
	       (master-name (concatenate-symbol struct-name '-master))
	       (maker-name (concatenate-symbol 'make- struct-name))
	       (newer-name (concatenate-symbol struct-name ':new))
	       (predicate-name (concatenate-symbol struct-name '?))
	       (handler-name (concatenate-symbol struct-name '-handler))
	       (handler-form `(object nil
				      ,@methods
				      ((print self port)
				       ;; Normal default is PRINT-STRUCTURE
				       (print-defstruct self port))
				      (((*value t-implementation-env 'hash) self)
				       (hash-def-struct self)))))
	      `(block
		     ;define the structure type, and place in the var structname-STYPE
		     (define ,stype-name
			     (make-stype ',struct-name ',slot-names ,handler-form))
		     
		     ;assign the slot accessors to structname:slot variables
		     ,@@(loop
			     (for slot in slot-names)
			     (for slot-accessor in slotac-names)
			     
			     (incr i from 2 by 4)
			     (save `(define-constant ,slot-accessor
					(make-structure-accessor ,stype-name
					    ,i ',slot))))
		     
		     ;assign the niladic structure maker to MAKE-structname
		     (define ,maker-name
			     (stype-constructor ,stype-name))
		     ;assign the structure maker to the variable structname:NEW
		     ,@(%def-struct-create-NEW-macros struct-name newer-name maker-name)
		     ;assign the type predicate to the variable structname?
		     (define ,predicate-name
			     (stype-predicator ,stype-name))
		     ;assign the handler to the variable structname-HANDLER
		     (define ,handler-name
			     (stype-handler ,stype-name))
		     ;assign the master to the variable structname-MASTER
		     (define ,master-name (stype-master ,stype-name))
		     ;intialise all the slots in the master
		     ,@(for (slot in slots) (slotaccessor in slotac-names)
			    (when (pair? slot))
			    (save `(set (,slotaccessor ,master-name) ,(cadr slot))))
		     ,stype-name
	       ))))

