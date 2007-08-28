;(define-extension iadfa)

(require-extension srfi-1)
(include "utils-scm.scm")
(include "fsa.scm")

(define-record iadfa 
  register
  index ;; this is used for automatic node name generation
  fsa
  final)

;; This will return the node's last child added.
(define last-child
  (lambda (node)
    (let ((lst-node (node-transition node (last-input node))))
      (car lst-node))))

(define last-child-for-input
  (lambda (node input)
    (let ((lst-node (node-transition node input)))
      (car lst-node))))

;; This returns the last node's symbol (alphabetical order)
;;
;; We rely on the fact that hash-table-keys returns a reversed
;; sorted list of the keys.
(define last-input
  (lambda (node)
    (car (hash-table-keys (node-symbols-map node)))))

(define has-children?
  (lambda (node)
    (> (hash-table-size (node-symbols-map node))
       0)))

(define common-prefix
  (lambda (word node prefix)
    (if (eq? 0 (length word))
	(cons node prefix)
	(let ((next-node (node-transition node (car word))))
	  (if (null? next-node)
	      (cons node prefix)
	      (common-prefix (cdr word)
			     (car next-node)
			     (append prefix (list (car word)))))))))

(define marked-as-registered
  (lambda (iadfa state)
    (hash-table-exists? (iadfa-register iadfa) state)))

(define iadfa-state-ancestrors
  (lambda (iadfa label)
    (let ((register (iadfa-register iadfa)))
      (if (hash-table-exists? register label)
          (hash-table-fold (hash-table-ref register label)
                           (lambda (key nodes ancestrors)
                             (append (map
                                      (lambda (node)
                                        (node-label node)) nodes)
                                     ancestrors))
                           '())
          '()))))

(define iadfa-state-ancestrors-for-input
  (lambda (iadfa label input)
    (let ((register (iadfa-register iadfa)))
      (if (hash-table-exists? register label)
          (map (lambda (node)
                 (node-label node))
               (hash-table-ref/default (hash-table-ref register label)
                                       input
                                       '()))
          '()))))

(define iadfa-node-ancestrors
  (lambda (iadfa label input)
    (let ((register (iadfa-register iadfa)))
      (if (hash-table-exists? register label)
          (hash-table-ref/default (hash-table-ref register label)
                                  input
                                  '())
          '()))))


(define append-parent-to-registered
  (lambda (iadfa parent input child)
    (if (eq? 1 (hash-table-size (node-symbols-map parent)))
        (hash-table-update!/default (iadfa-register iadfa) 
                                    (node-label child)
                                    (lambda (hash)
                                      (hash-table-update!/default hash
                                                                  input
                                                                  (lambda (lst)
                                                                    (cons parent lst))
                                                                  '())
                                      hash)
                                    (make-hash-table)))))


(define delete-parent-to-registered
  (lambda (iadfa parent input child)
    (let ((register (iadfa-register iadfa)))
      (if (hash-table-exists? register (node-label child))
          (hash-table-update! register
                              (node-label child)
                              (lambda (hash)
                                (hash-table-update!/default hash
                                                            input
                                                            (lambda (lst)
                                                              (delete! parent lst eq?))
                                                            '())
                                hash))))))


(define delete-parent-to-registered-childs
  (lambda (iadfa node)
    (let ((symbols-map (node-symbols-map node)))
                                        ;      (if (eq? 1 (hash-table-size symbols-map))
      (hash-table-walk
       symbols-map
       (lambda (symbol destinations)
         (fold (lambda (dst iadfa)
                (delete-parent-to-registered iadfa
                                             node
                                             symbol
                                             dst)
                iadfa)
               iadfa
               destinations))))))

(define mark-as-registered
  (lambda (iadfa parent child)
    (if (eq? (iadfa-final iadfa) #f)
        (iadfa-final-set! iadfa child))
    (append-parent-to-registered iadfa
                                 parent
                                 (last-input parent)
                                 child)))


(define generate-state
  (lambda (iadfa)
    (let ((name (iadfa-index iadfa)))
      (iadfa-index-set! iadfa (+ 1 (iadfa-index iadfa)))
      name)))


(define build-iadfa
  (lambda ()
    (make-iadfa (make-hash-table) 
		1
		(make-fsa (make-empty-node 0))
                #f)))

(define gen-iadfa 
  (lambda (words)
    (let ((iadfa (fold (lambda (word iadfa) 
                         (handle-word iadfa (string->list word)))
                       (build-iadfa)
                       words)))
      (replace-or-register iadfa (fsa-start-node (iadfa-fsa iadfa))))))

(define gen-iadfa-from-file 
  (lambda (file)
    (let ((iadfa (build-iadfa)))
      (for-each-line-in-file 
       file
       (lambda (line)
	 (display (format "~A ~%" line))
	 (handle-word iadfa 
		      (string->list line))))
      (iadfa-fsa (replace-or-register iadfa (fsa-start-node (iadfa-fsa iadfa)))))))


(define handle-word
  (lambda (iadfa word)
    (let* ((fsa (iadfa-fsa iadfa))
	   (common (common-prefix word (fsa-start-node fsa) '()))
	   (common-prefix (cdr common))
	   (last-node (car common))
	   (current-suffix (list-tail word (length common-prefix))))
      (if (has-children? last-node)
	  (replace-or-register iadfa last-node))
      (add-suffix last-node current-suffix iadfa)
      (delete-parent-to-registered-childs iadfa last-node)
      iadfa)))

(define replace-or-register
  (lambda (iadfa node)
    (let* ((fsa (iadfa-fsa iadfa))
	   (child (last-child node)))
      (if (marked-as-registered iadfa child )
	  iadfa
	  (let ()
	    (if (has-children? child)
		(replace-or-register iadfa child))
	    (handle-equivalent-states iadfa node child)
	    iadfa)))))


(define equivalent-registered-states
  (lambda (iadfa node)
    (if (has-children? node)
	(find-equivalent-states iadfa node)
        (find-equivalent-final-states iadfa node))))

(define find-equivalent-final-states
  (lambda (iadfa node)
    (iadfa-final iadfa)))


(define find-equivalent-states
  (lambda (iadfa node)
    (let ((fsa (iadfa-fsa iadfa))
          (input (last-input node)))
      (any (lambda (other)
	     (if (and (not (eq? other node))
		      (eq? (node-final node) (node-final other)))
		 other
		 #f))
           (iadfa-node-ancestrors iadfa
                                  (node-label (last-child-for-input node input))
                                  input)))))


(define handle-equivalent-states
  (lambda (iadfa node child)
    (let* ((fsa (iadfa-fsa iadfa))
	   (equivalent (equivalent-registered-states iadfa child)))
      (if equivalent
	  (begin
	    (replace-last-child node equivalent iadfa)
	    (delete-branch iadfa child))
          (mark-as-registered iadfa node child))
      iadfa)))


;; (define replace-last-child
;;   (lambda (node new-child iadfa)
;;     (let* ((fsa (iadfa-fsa iadfa))
;; 	   (input (last-input node)))
;; 	   (current-child (last-child node)))
;;       (fsa-remove-edge! fsa (node-label node) input (node-label current-child))
;;       (fsa-add-edge! fsa (node-label node) input (node-label new-child))
;;       (append-parent-to-registered iadfa (node-label node) (node-label new-child)))))

(define replace-last-child
  (lambda (node new-child iadfa)
    (let* ((fsa (iadfa-fsa iadfa))
	   (input (last-input node))
           (current-child (last-child-for-input node input)))
      (node-remove-edge! node input current-child)
      (node-add-edge! node input new-child)
      (append-parent-to-registered iadfa node input new-child)
      node)))


(define delete-branch 
  (lambda (iadfa child)
    (let ((fsa (iadfa-fsa iadfa)))
      (if (has-children? child)
          (let ((input (last-input child)))
            (delete-parent-to-registered iadfa 
                                         child
                                         input
                                         (last-child-for-input child input))))
      iadfa)))

(define add-suffix
  (lambda (node current-suffix iadfa)
    (let ((fsa (iadfa-fsa iadfa))
          (last-node node))
      (fold (lambda (input fsa)
              (let ((new-node (make-empty-node (generate-state iadfa))))
                (node-add-edge! last-node input new-node)
                (set! last-node new-node)
                fsa))
            (iadfa-fsa iadfa)
            current-suffix)
      (node-final-set! last-node #t) 
      iadfa)))


