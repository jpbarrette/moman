(define-extension iadfa)

(require-extension srfi-1)
;(require-extension utils-scm)
;(require-extension fsa)
(include "utils-scm.scm")
(include "fsa.scm")

(define-record iadfa 
  ancestrors
  index ;; this is used for automatic node name generation
  fsa
  final)


(define has-children?
  (lambda (node)
    (> (hash-table-size (node-symbols-map node))
       0)))

(define common-prefix
  (lambda (word node)
    (if (eq? 0 (length word))
	(cons node word)
	(let ((next-node (node-transition node (car word))))
	  (if (null? next-node)
	      (cons node word)
	      (common-prefix (cdr word)
			     (car next-node)))))))



(define common-suffix
  ;; this function takes a reverted to be consumed
  ;; and a node to start from and the current stem
  (lambda (iadfa current-suffix node)
    (letrec ((c-suffix (lambda (iadfa current-suffix node)
                         (if (eq? 0 (length current-suffix))
                             (cons node (reverse current-suffix))
                             (let ((next-node (ancestror-transition iadfa node (car current-suffix))))
                               (if (not next-node)
                                   (cons node (reverse current-suffix))
                                   (c-suffix iadfa
                                             (cdr current-suffix)
                                             next-node)))))))
      (c-suffix iadfa (reverse current-suffix) node))))
                                           

(define remove-ancestor-to-childs
  (lambda (iadfa node)
    (if (eq? 1 (hash-table-size (node-symbols-map node)))
        (node-walk node (lambda (input destination-nodes)
                          (for-each (lambda (dst-node)
                                      (node-set-ancestror! iadfa dst-node input #f))
                                    destination-nodes))))))

(define ancestror-transition
  (lambda (iadfa node input)
    (let ((ancestrors (vector-ref (iadfa-ancestrors iadfa) (node-label node))))
      (if (not ancestrors)
          #f
          (hash-table-ref ancestrors input #f)))))

(define node-set-ancestror!
  (lambda (iadfa dst-node input src-node)
    (let ((ancestrors (vector-ref (iadfa-ancestrors iadfa) (node-label dst-node))))
      (if (not ancestrors)
          (set! ancestrors (make-hash-table))
          (vector-set! (iadfa-ancestrors iadfa) (node-label dst-node) ancestrors))
      (hash-table-set! ancestrors input src-node))))
    
          
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
    (let* ((fsa (iadfa-fsa iadfa))
	   (common (common-prefix word (fsa-start-node fsa)))
	   (prefix-node (car common))
	   (current-suffix (cdr common)))
      (remove-ancestor-to-childs iadfa prefix-node)
      (if (eq? 0 (length current-suffix))
          (node-final-set! prefix-node #t)
          (let* ((suffix (common-suffix iadfa current-suffix (iadfa-final iadfa)))
                 (suffix-node (car suffix))
                 (current-stem (cdr suffix)))
            (add-stem iadfa prefix-node suffix-node current-stem)))
      iadfa)))


(define iadfa-add-edge!
  (lambda (iadfa src-node input dst-node)
    (node-add-edge! src-node input dst-node)
    (node-set-ancestror! iadfa dst-node input src-node)))

(define add-stem
  (lambda (iadfa prefix-node suffix-node current-stem)
    (let ((last-node prefix-node)
          (last-input (last current-stem))
          (processing-stem (take current-stem (- (length current-stem) 1))))
      (fold (lambda (input iadfa)
              (let ((new-node (make-empty-node (generate-state iadfa))))
                (iadfa-add-edge! iadfa last-node input new-node)
                (set! last-node new-node)
                iadfa))
            iadfa
            processing-stem)
      (iadfa-add-edge! iadfa last-node last-input suffix-node)
      iadfa)))


