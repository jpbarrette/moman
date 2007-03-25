(require-extension check)
;(require-extension iadfa)
(load "iadfa")

(define my-fsa (gen-iadfa-from-file "com.zone.sorted.small"))
;; "brateau" "cracher" "croteau")))

(graphviz-export my-fsa)

