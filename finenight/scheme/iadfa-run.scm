;(require-extension check)
;(require-extension iadfa)
(include "iadfa.scm")

(define my-fsa (gen-iadfa-from-file "com.zone.sorted.small"))

(display "done")
(newline)
(display (fsa-start-node my-fsa))


