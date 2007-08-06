(require-extension check)
(include "fsa-builder.scm")

;;(declare (uses fsa))
;;(use fsa)

(define myfsa (make-empty-fsa-builder 'a))

(fsa-add-edge! myfsa 'a #\b 'b)
(define a-node (get-node myfsa 'a))
(check (node-label a-node) => 'a)
(define b-node (car (node-transition a-node #\b)))
(check (node-label b-node) => 'b)

(fsa-add-edge! myfsa 'b #\c 'c)
(define c-node (car (node-transition b-node #\c)))
(check (node-label c-node) => 'c)

(fsa-remove-edge! myfsa 'b #\c 'c)
(check (node-transition b-node #\c) => '())

(define myfsa (make-empty-fsa-builder 'a))

(fsa-add-edge! myfsa 'a #\a 'a)
(fsa-add-edge! myfsa 'a #\b 'ab)
(fsa-add-edge! myfsa 'ab #\b 'abb)
(fsa-add-edge! myfsa 'abb #\o 'abbo)

(fsa-add-final! myfsa 'abbo)

(define a-node (get-node myfsa 'a))
(check (node-label (car (node-transition a-node #\a))) => 'a)

(define ab-node (car (node-transition a-node #\b)))
(check (node-label ab-node) => 'ab)

(define abb-node (car (node-transition ab-node #\b)))
(check (node-label abb-node) => 'abb)

(define abbo-node (car (node-transition abb-node #\o)))
(check (node-label abbo-node) => 'abbo)
(check (node-final (get-node myfsa 'abbo)) => #t)

(check (fsa-builder-accept? myfsa (string->list "abbo")) => #t)

; testing deletion of nodes
(define myfsa (make-empty-fsa-builder 'a))

(fsa-add-edge! myfsa 'a #\a 'a)
(fsa-add-edge! myfsa 'a #\c 'c)
(fsa-add-edge! myfsa 'a #\b 'b)
(fsa-add-edge! myfsa 'b #\d 'd)
(fsa-add-edge! myfsa 'd #\c 'c)

(fsa-add-final! myfsa 'c)

(fsa-remove-node! myfsa (get-node myfsa 'b))

(graphviz-export myfsa)






;(define b (list 'z 'y 'x 'w 'v))
;(define myfsa (make-fsa 'a (list 'b 'c) (list (list 'a 'b 'b) (list 'a 'c 'c))))

;(check (node-transition (fsa-initial-state myfsa) 'a 'b) => (list 'b))
;(check (final? myfsa 'a) => #f)
;(check (final? myfsa 'b) => #t)
;(check (final? myfsa 'c) => #t)
;(check (eval (car (transition myfsa 'a 'b))) => (list 'z 'y 'x 'w 'v))
