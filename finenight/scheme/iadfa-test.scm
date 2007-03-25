(require-extension check)
(require-extension iadfa)

;(load "iadfa.scm")
;(require (lib "32.ss" "srfi"))
;(declare (uses iadfa))


(define myfsa (make-empty-fsa 'a))

(fsa-add-edge! myfsa 'start #\a 'a)
(fsa-add-edge! myfsa 'a #\l 'al)
(fsa-add-edge! myfsa 'al #\l 'all)
(fsa-add-edge! myfsa 'all #\o 'allo)
(fsa-add-edge! myfsa 'a #\p 'ap)
(fsa-add-edge! myfsa 'ap #\p 'app)
(fsa-add-edge! myfsa 'app #\e 'appe)
(fsa-add-edge! myfsa 'appe #\n 'appen)
(fsa-add-edge! myfsa 'appen #\d 'append)

(define start-node (get-node myfsa 'start))

;; check for last input
(define a-node (get-node myfsa 'a))
(check (last-input a-node) => #\p)

;; check for last-child
(define ap-node (last-child a-node))
(check (node-label ap-node) => 'ap)

;; check for has-children
(define allo-node (get-node myfsa 'allo))
(check (has-children? allo-node) => #f)
(check (has-children? ap-node) => #t)

;; check for common-prefix
(define append-cp-a (common-prefix (string->list "appendice") start-node (string->list "")))
(define append-node (car append-cp-a))
(check (node-label append-node) => 'append)
(check (string->list "append") => (cdr append-cp-a))

;; check for reduce
(check (reduce (lambda (v1 v2) (+ v1 v2)) 1 (list 2 3 4 5)) => 15)

(define my-fsa (gen-iadfa '("appendice" "bateau")))
;(define my-iadfa (handle-word (build-iadfa) (string->list "appendice")))
(check (accept? my-fsa (string->list "appendice")) => #t)
(check (accept? my-fsa (string->list "bateau")) => #t)
(check (accept? my-fsa (string->list "appendic")) => #f)

(check (node-final (get-node my-fsa 9)) => #t)

(graphviz-export my-fsa)


;; (display (gen-iadfa '("allo"  "bateau")))
;; (define b (list 'z 'y 'x 'w 'v))
;; (define myfsa (make-fsa 'a (list 'b 'c) (list (list 'a 'b 'b) (list 'a 'c 'c))))

;; (check (transition myfsa 'a 'b) => (list 'b))
;; (check (final? myfsa 'a) => #f)
;; (check (final? myfsa 'b) => #t)
;; (check (final? myfsa 'c) => #t)
;; (check (eval (car (transition myfsa 'a 'b))) => (list 'z 'y 'x 'w 'v))
