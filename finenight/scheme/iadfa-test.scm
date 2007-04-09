(require-extension check)
(require-extension iadfa)


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

(define my-iadfa (gen-iadfa '("append" "appendice"
                              "bappend" "bappendice"
                              "bateau"  "batisa" "batise"
                              "brateau" "cracher")))
(define my-fsa (iadfa-fsa my-iadfa))
(check (sort (iadfa-state-ancestrors my-iadfa 9) <) => '(8 22 39))
(check (sort (iadfa-state-ancestrors my-iadfa 8) <) => '(7 24))
(check (sort (iadfa-state-ancestrors my-iadfa 24) <) => '())
(check (accept? my-fsa (string->list "appendice")) => #t)
(check (accept? my-fsa (string->list "bateau")) => #t)
(check (accept? my-fsa (string->list "appendic")) => #f)
(check (node-final (get-node my-fsa 6)) => #t)

(graphviz-export my-fsa)




(define my-iadfa (gen-iadfa '("appendice" "appendicee"
                              "bappendice" "bappendicee")))
(define my-fsa (iadfa-fsa my-iadfa))
;(check (sort (iadfa-state-ancestrors my-iadfa 9) <) => '(8 22 39))
;(check (sort (iadfa-state-ancestrors my-iadfa 8) <) => '(7))
;(check (sort (iadfa-state-ancestrors my-iadfa 24) <) => '())



;(define my-iadfa (handle-word (build-iadfa) (string->list "appendice")))



