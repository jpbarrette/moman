(require-extension utils-scm)
(require-extension check)

(some (lambda (lhs rhs)
	(if (> 5 lhs)
	    rhs
	    #f))
      '(1 2 3 4 5 6 7)
      '(a b c d e f g))