(require :com.rrette.finenight.fsa "fsa.lisp")

(in-package :com.rrette.finenight)

(setf my-fsa (build-fsa '()
			'((a "b" b)
			  (a nil b)
			  (a "d" d)
			  (a "d" d)
			  (b "c" c)
			  (b "é" c)
			  (c "d" d)
			  (c nil d)
			  (c nil c))
			'a
			'(d)))

;(graphviz-export t 8.5 11 my-fsa)
	     