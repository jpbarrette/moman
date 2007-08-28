(require-extension check)

(declare (uses edges))

(load "edges.scm")


(define my-edges (list (cons 'a (list (cons 'b 'b))) (cons 'a (list (cons 'c 'c)))))

(check (transition my-edges 'a 'b) => (list 'b))

