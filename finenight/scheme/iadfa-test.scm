(require-extension check)
(require-extension iadfa)

(define my-iadfa (gen-iadfa '("append" "appendice"
                              "bappend" "bappendice"
                              "bateau"  "batisa" "batise"
                              "brateau" "cracher")))
(define my-fsa (iadfa-fsa my-iadfa))

;(check (sort (iadfa-state-ancestrors my-iadfa 9) <) => '(8 22 39))
;(check (sort (iadfa-state-ancestrors my-iadfa 8) <) => '(7 24))
;(check (sort (iadfa-state-ancestrors my-iadfa 24) <) => '())
(check (accept? my-fsa (string->list "appendice")) => #t)
(check (accept? my-fsa (string->list "bateau")) => #t)
(check (accept? my-fsa (string->list "appendic")) => #f)
;(check (node-final (get-node my-fsa 6)) => #t)

;(graphviz-export my-fsa)




(define my-iadfa (gen-iadfa '("appendice" "appendicee"
                              "bappendice" "bappendicee"
                              "batisa" "batise" "batissa"
                              "criba" "cribaa"
                              "crima" "crime")))
(define my-fsa (iadfa-fsa my-iadfa))
;(graphviz-export my-fsa)
;(check (sort (iadfa-state-ancestrors my-iadfa 10) <) => '(9 27 33))
;(check (sort (iadfa-state-ancestrors-for-input my-iadfa 10 #\a) <) => '(27 33))
;(check (sort (iadfa-state-ancestrors my-iadfa 8) <) => '(7))
;(check (sort (iadfa-state-ancestrors my-iadfa 24) <) => '())



;(define my-iadfa (handle-word (build-iadfa) (string->list "appendice")))



