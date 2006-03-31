(require :com.rrette.finenight.fsa "fsa.lisp")

(in-package :com.rrette.finenight)

(setf my-fsa (build-fsa '()
			'((a "b" b)
			  (a nil b)
			  (a "d" d)
			  (a "d" d)
			  (b "c" c)
			  (a "e" e)
			  (b "é" c)
			  (c "d" d)
			  (c nil d)
			  (c nil c))
			'a
			'(d)))


(setf fsa1 (build-fsa '()
		      '((a "a" b)
			(b "b" c)
			(c "c" d)
			(d "d" e))
		      'a
		      '(d)))

(defun my-test ()
  (build-fsa '()
	     '((a "b" b)
	       (a nil b)
	       (a "d" d)
	       (a "d" d)
	       (b "c" c)
	       (a "e" e)
	       (b "é" c)
	       (c "d" d)
	       (c nil d)
	       (c nil c))
	     'a
	     '(d)))
  

;(graphviz-export t 8.5 11 my-fsa)
	     