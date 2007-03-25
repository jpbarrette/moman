(require-extension check)
(require-extension iadfa)

(define my-fsa (gen-iadfa '("bateau" "batifoler" "brateau")))
;; "brateau" "cracher" "croteau")))

(graphviz-export my-fsa)

