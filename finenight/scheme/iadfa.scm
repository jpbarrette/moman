(define-extension iadfa)
;(require-extension defstruct)
(require-extension utils-scm)
(require-extension fsa)
(require-extension srfi-1)

;(load "fsa.scm")
;(load "sort.scm")
;(require (lib "32.ss" "srfi"))

;(declare (unit iadfa))
;(declare (uses fsa))

(define-record iadfa 
  register
  index ;; this is used for automatic node name generation
  fsa)

;; This will return the node's last child added.
(define last-child
  (lambda (node)
    (let ((lst-node (node-transition node (last-input node))))
      (car lst-node))))


;; This returns the last node's symbol (alphabetical order)
(define last-input
  (lambda (node)
    (car (sort (node-symbols node) char>?))))

(define has-children?
  (lambda (node)
    (not (null? (node-edges node)))))

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
  
(define append-parent-to-registered
  (lambda (iadfa parent child)
    (hash-table-update!/default (iadfa-register iadfa) 
			(node-label child)
			(lambda (lst)
			  (cons parent lst))
			'())))


(define delete-parent-to-registered
  (lambda (iadfa parent child)
    (hash-table-update!/default (iadfa-register iadfa) 
			(node-label child)
			(lambda (lst)
			  (delete! parent lst eq?))
			'())))

(define mark-as-registered
  (lambda (iadfa parent child)
    (append-parent-to-registered iadfa parent child)))


(define generate-state
  (lambda (iadfa)
    (let ((name (iadfa-index iadfa)))
      (iadfa-index-set! iadfa (+ 1 (iadfa-index iadfa)))
      name)))
  

(define build-iadfa
  (lambda ()
    (make-iadfa (make-hash-table) 
		1
		(make-empty-fsa 0))))
  
(define gen-iadfa 
  (lambda (words)
    (let ((iadfa (fold (lambda (word iadfa) 
                         (handle-word iadfa (string->list word)))
                       (build-iadfa)
                       words)))
      (iadfa-fsa (replace-or-register iadfa (fsa-initial-node (iadfa-fsa iadfa)))))))

(define gen-iadfa-from-file 
  (lambda (file)
    (let ((iadfa (build-iadfa)))
      (for-each-line-in-file 
       file
       (lambda (line)
	 (display (format "~A ~%" line))
	 (handle-word iadfa 
		      (string->list line))))
      (iadfa-fsa (replace-or-register iadfa (fsa-initial-node (iadfa-fsa iadfa)))))))


(define handle-word
  (lambda (iadfa word)
    (let* ((fsa (iadfa-fsa iadfa))
	   (common (common-prefix word (fsa-initial-node fsa) '()))
	   (common-prefix (cdr common))
	   (last-node (car common))
	   (current-suffix (list-tail word (length common-prefix))))
      (if (has-children? last-node)
	  (replace-or-register iadfa last-node))
      (add-suffix last-node current-suffix iadfa)
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
    (if (final? node)
	(find-equivalent-final-states iadfa node)
	(find-equivalent-states iadfa node))))

(define find-equivalent-final-states
  (lambda (iadfa node)
    (let ((fsa (iadfa-fsa iadfa)))
      (any (lambda (other)
	      (if (equal? (node-label other) (node-label node)) 
		  #f
		  (if (node-is-equivalent node other)
		      other
		      #f)))
	    (fsa-finals fsa)))))


(define iadfa-node-ancestrors
  (lambda (iadfa label)
    (hash-table-ref/default (iadfa-register iadfa) 
                            label
                            '())))


(define find-equivalent-states
  (lambda (iadfa node)
    (let ((fsa (iadfa-fsa iadfa)))
      (any (lambda (other)
	     (if (and (not (eq? other node))
		      (node-is-equivalent node other))
		 other
		 #f))
           (iadfa-node-ancestrors iadfa (node-label (last-child node)))))))
             

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
           (current-child (last-child node)))
      (node-remove-edge! node input current-child)
      (fsa-add-edge! fsa (node-label node) input (node-label new-child))
      (append-parent-to-registered iadfa node new-child)
      node)))


(define delete-branch 
  (lambda (iadfa child)
    (let ((fsa (iadfa-fsa iadfa)))
      (if (has-children? child)
	  (delete-parent-to-registered iadfa 
				       child
				       (last-child child)))
      (fsa-remove-node! fsa child))
    iadfa))

(define add-suffix
  (lambda (node current-suffix iadfa)
    (let ((fsa (iadfa-fsa iadfa))
	  (last-node node))
      (fold (lambda (input fsa)
              (let ((new-state (generate-state iadfa)))
                (fsa-add-edge! fsa (node-label last-node) input new-state)
                (set! last-node (get-node fsa new-state))
                fsa))
            (iadfa-fsa iadfa)
            current-suffix)
      (fsa-add-final-node! fsa last-node)
      iadfa)))


