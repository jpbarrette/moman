(define-extension iadfa)
(require-extension fsa)
(require-extension defstruct)
(load "utils-scm")

;(load "fsa.scm")
;(load "sort.scm")
;(require (lib "32.ss" "srfi"))

;(declare (unit iadfa))
;(declare (uses fsa))

(defstruct iadfa 
  (register (make-hash-table)) 
  (index 1) ;; this is used for automatic node name generation
  (fsa (make-fsa 0 (make-hash-table))))

;; This will return the node's last child added.
(define last-child
  (lambda (node)
    (let ((lst-node (node-transition node (last-input node))))
      (car lst-node))))


;; This returns the last node's symbol (alphabetical order)
(define last-input
  (lambda (node)
    (car (sort (node-symbols node) char>?))))

(define has-children
  (lambda (node)
    (> (length (node-symbols node)) 0)))

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
  (lambda (iadfa parent-label label)
    (hash-table-set! (iadfa-register iadfa) 
		     label 
		     (cons parent-label (hash-table-ref (iadfa-register iadfa) label (lambda () '()))))))

(define mark-as-registered
  (lambda (iadfa parent-label label)
    (append-parent-to-registered iadfa parent-label label)))


(define generate-state
  (lambda (iadfa)
    (let ((name (iadfa-index iadfa)))
      (iadfa-index-set! iadfa (+ 1 (iadfa-index iadfa)))
      name)))
  

(define build-iadfa
  (lambda ()
    (make-iadfa (make-hash-table) 
		1
		(make-fsa 0 (make-hash-table)))))
  

;; (define gen-iadfa 
;;   (lambda (words)
;;     (let ((iadfa (build-iadfa)))
;;       (letrec ((G (lambda (words)
;; 		    (if (null? words)
;; 			iadfa
;; 			(progn
;; 			 ;;(format t "~%~%~A" word)
;; 			 (time (handle-word iadfa (string->list (car words))))
;; 			 (G (cdr words)))))))
;; 	(G words)))))


;;      (iadfa-fsa (replace-or-register (fsa-start (iadfa-fsa iadfa)) iadfa)))))

(define gen-iadfa 
  (lambda (words)
    (let ((iadfa (reduce (lambda (iadfa word) 
			    (handle-word iadfa (string->list word)))
			 (build-iadfa)
			 words)))
     (iadfa-fsa (replace-or-register iadfa (fsa-initial-node (iadfa-fsa iadfa)))))))
;    (iadfa-fsa iadfa))))

(define handle-word
  (lambda (iadfa word)
    (let* ((fsa (iadfa-fsa iadfa))
	   (common (common-prefix word (fsa-initial-node fsa) '()))
	   (common-prefix (cdr common))
	   (last-node (car common))
	   (current-suffix (list-tail word (length common-prefix))))
      (if (has-children last-node)
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
	    (if (has-children child)
		(replace-or-register iadfa child))
	    (handle-equivalent-states iadfa node child)
	    iadfa)))))


(define equivalent-registered-states
  (lambda (iadfa node)
    (if (final? node)
	(find-equivalent-final-registered-states iadfa node)
	(find-equivalent-registered-states iadfa node))))

(define find-equivalent-final-registered-states
  (lambda (iadfa node)
    (let ((fsa (iadfa-fsa iadfa)))
      (some (lambda (other)
	      (if (equal? other (node-label node)) 
		  #f
		  (node-is-equivalent node (get-node fsa other))))
	    (fsa-finals fsa)))))


(define find-equivalent-registered-states
  (lambda (iadfa node)
    (let ((fsa (iadfa-fsa iadfa)))
      (some (lambda (other)
	      (node-is-equivalent node (get-node fsa other)))
	    (hash-table-ref (iadfa-register iadfa) (node-label (last-child node)))))))

(define handle-equivalent-states
  (lambda (iadfa node child)
    (let* ((fsa (iadfa-fsa iadfa))
	   (equivalent (equivalent-registered-states iadfa child)))
      (if equivalent
	  (begin
	    (delete-branch fsa child)
	    (replace-last-child node equivalent iadfa))
	  (mark-as-registered iadfa (node-label node) (node-label child)))
      iadfa)))


(define replace-last-child
  (lambda (node child new-child iadfa)
    (let* ((fsa (iadfa-fsa iadfa))
	   (input (last-input node))
	   (current-child (last-child node)))
      (fsa-remove-edge! fsa (node-label node) input (node-label current-child))
      (fsa-add-edge! fsa (node-label node) input (node-label new-child))
      (append-parent-to-registered iadfa (node-label node) (node-label new-child)))))

(define delete-branch 
  (lambda (fsa child)
    (let ()
      (if (has-children? child)
	  (delete-branch fsa (last-child child)))
      (fsa-remove-node! fsa child))
    fsa))

(define add-suffix
  (lambda (node current-suffix iadfa)
    (let ((fsa (iadfa-fsa iadfa))
	  (last-node node))
      (reduce (lambda (fsa input)
		(let ((new-state (generate-state iadfa)))
		  (fsa-add-edge! fsa (node-label last-node) input new-state)
		  (set! last-node (get-node fsa new-state))
		  fsa))
	      (iadfa-fsa iadfa)
	      current-suffix)
      (node-final-set! last-node #t)
      iadfa)))




;;(check (eval (car (transition myfsa 'a 'b))) => (list 'z 'y 'x 'w 'v))

