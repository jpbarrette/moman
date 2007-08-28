;; the node consists of a label and a map a symbol to 
;; a destination object. 
(define-record node 
  label
  input-map)


(define node-add-edge!
  (lambda (node edge)
    (set! (cons (destination-node edge)
		(hash-table-href (node-input-map node)
				 (input-symbol edge) 
				 '())))))


;; will return the list of destination nodes for the
;; given node.
(define node-transition
  (lambda (node symbol)
    (hash-table-ref (node-input-map node) symbol '())))



