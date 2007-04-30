;(define-extension iadfa)

(require-extension srfi-1)
;(require-extension utils-scm)
;(require-extension fsa)
(include "utils-scm.scm")
(include "fsa.scm")
(include "fsa-builder.scm")

(define-record iadfa 
  ancestrors
  index ;; this is used for automatic node name generation
  fsa
  final)


(define has-children?
  (lambda (node)
    (> (node-arity node)
       0)))


(define delete-branch
  (lambda (iadfa stem-start-node stem-start-input stem-end-node)
    (remove-ancestor-to-childs iadfa stem-end-node)
    (node-remove-dsts-for-input! stem-start-node stem-start-input)))

(define common-prefix
  (lambda (iadfa word node)
    (let ([stem '()]
          [stem-start-node node]
          [stem-start-input (car word)]
          [stem-end-node #f]
          [profile '()]
          [found-stem #f])
      (letrec ((c-prefix
                (lambda (word node prefix)
                  (if (not found-stem)
                      (if (< 1 (node-arity node))
                          (begin 
                            (set! stem-start-node node)
                            (set! stem-start-input (car word))
                            (set! profile '()))))
                  (if (eq? (iadfa-final iadfa) node)
                      (begin
                        (delete-branch iadfa stem-start-node stem-start-input stem-end-node)
                        (values stem-start-node (append stem word) (append profile (make-list (length word) #f))))
                      (let ((next-node (node-transition node (car word))))
                        (if (null? next-node)
                            (values node word (make-list (length word) #f))
                            (begin (set! next-node (car next-node))
                                   (if (not found-stem)
                                       (begin 
                                         (set! profile (append profile (list (node-final next-node))))
                                         (set! stem (append stem (list (car word))))
                                         (set! stem-end-node node)
                                         (if (> (node-label node) (node-label next-node))
                                             (set! found-stem #t))))
                                   (c-prefix (cdr word)
                                             next-node
                                             (append prefix
                                                     (list (car word)))))))))))
        (c-prefix word node '())))))
     
             





(define common-suffix
  ;; this function takes a suffix to be consumed
  ;; and a node to start from and the current stem
  (lambda (iadfa current-suffix node profile)
    (letrec ((c-suffix (lambda (iadfa current-suffix node profile)
                         (if (eq? 1 (length current-suffix))
                             (cons node (reverse current-suffix))
                             (let ((next-node (ancestror-transition iadfa node (car current-suffix) (car profile))))
                               (if (or (not next-node) (eq? next-node (fsa-start-node (iadfa-fsa iadfa))))
                                   (cons node (reverse current-suffix))
                                   (c-suffix iadfa
                                             (cdr current-suffix)
                                             next-node
                                             (cdr profile))))))))
      (c-suffix iadfa (reverse current-suffix) node (reverse profile)))))
                                           

(define remove-ancestor-to-childs
  (lambda (iadfa node)
    (if (eq? 1 (node-arity node))
        (node-walk node (lambda (input destination-nodes)
                          (for-each (lambda (dst-node)
                                      (node-remove-ancestror! iadfa dst-node input node))
                                    destination-nodes))))))

(define ancestror-transition
  (lambda (iadfa node input final)
    (let ((ancestrors (vector-ref (iadfa-ancestrors iadfa) (node-label node))))
      (if (not ancestrors)
          #f
          (let ([src-nodes (filter (lambda (node)
                                     (eq? (node-final node) final))
                                   (hash-table-ref/default ancestrors input '()))])
            (if (null? src-nodes)
                #f
                (car src-nodes)))))))

(define node-add-ancestror!
  (lambda (iadfa dst-node input src-node)
    (let ((ancestrors (vector-ref (iadfa-ancestrors iadfa) (node-label dst-node))))
      (if (not ancestrors)
          (begin 
            (set! ancestrors (make-hash-table))
            (vector-set! (iadfa-ancestrors iadfa) (node-label dst-node) ancestrors)))
      (hash-table-update!/default ancestrors
                                  input
                                  (lambda (nodes)
                                    (cons src-node nodes))
                                  '()))))
    
(define node-remove-ancestror!
  (lambda (iadfa dst-node input src-node)
    (let ((ancestrors (vector-ref (iadfa-ancestrors iadfa) (node-label dst-node))))
      (if ancestrors
          (hash-table-update!/default ancestrors
                                      input
                                      (lambda (nodes)
                                        (remove (lambda (node)
                                                  (eq? node src-node))
                                                nodes))
                                      '())))))

(define node-ancestrors
  (lambda (iadfa dst-node input)
    (iadfa-state-ancestrors iadfa (node-label dst-node) input)))
    
(define build-fsa-from-ancestrors
  (lambda (iadfa)
    (let ([fsa (make-empty-fsa-builder 0)])
      (vector-walk
       (iadfa-ancestrors iadfa)
       (lambda (label node-ancestrors)
         (if node-ancestrors
             (hash-table-walk
              node-ancestrors
              (lambda (input nodes)
                (for-each
                 (lambda (node)
                   (fsa-add-edge! fsa label input (node-label node)))
                 nodes))))))
      fsa)))
                                                        

    
(define iadfa-state-ancestrors
  (lambda (iadfa dst-label input)
    (let ((ancestrors (vector-ref (iadfa-ancestrors iadfa) dst-label)))
      (if ancestrors
          (map (lambda (node)
                 (node-label node))
               (hash-table-ref/default ancestrors
                                       input
                                       '()))
          '()))))
    
(define generate-state
  (lambda (iadfa)
    (let ((name (iadfa-index iadfa)))
      (iadfa-index-set! iadfa (+ 1 (iadfa-index iadfa)))
      name)))


(define build-iadfa
  (lambda ()
    (let ((iadfa (make-iadfa (make-vector 100000 #f)
                             2
                             (make-fsa (make-empty-node 0))
                             (make-empty-node 1))))
      (node-final-set! (iadfa-final iadfa) #t)
      iadfa)))

(define gen-iadfa 
  (lambda (words)
    (fold (lambda (word iadfa) 
            (handle-word iadfa (string->list word)))
          (build-iadfa)
          words)))

(define gen-iadfa-from-file 
  (lambda (file)
    (let ((iadfa (build-iadfa)))
      (for-each-line-in-file 
       file
       (lambda (line)
	 (display (format "~A ~%" line))
	 (handle-word iadfa 
		      (string->list line))))
      (iadfa-fsa iadfa))))

(define handle-word
  (lambda (iadfa word)
    (let* ((fsa (iadfa-fsa iadfa)))
      (receive (prefix-node current-suffix profile) (common-prefix iadfa word (fsa-start-node fsa))
        (remove-ancestor-to-childs iadfa prefix-node)
        (if (< 0 (length current-suffix))
            (let* ((suffix (common-suffix iadfa current-suffix (iadfa-final iadfa) profile))
                   (suffix-node (car suffix))
                   (current-stem (cdr suffix)))
              (add-stem iadfa prefix-node suffix-node current-stem profile)))
        iadfa))))


(define iadfa-add-edge!
  (lambda (iadfa src-node input dst-node)
    (node-add-edge! src-node input dst-node)
    (node-add-ancestror! iadfa dst-node input src-node)))

(define add-stem
  (lambda (iadfa prefix-node suffix-node current-stem profile)
    (let ((last-node prefix-node)
          (last-input (last current-stem))
          (processing-stem (take current-stem (- (length current-stem) 1))))
      (fold (lambda (input iadfa)
              (let ((new-node (make-empty-node (generate-state iadfa))))
                (node-final-set! new-node (car profile))
                (set! profile (cdr profile))
                (iadfa-add-edge! iadfa last-node input new-node)
                (set! last-node new-node)
                iadfa))
            iadfa
            processing-stem)
      (iadfa-add-edge! iadfa last-node last-input suffix-node)
      iadfa)))


