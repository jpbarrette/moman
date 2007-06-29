(require :com.rrette.finenight.iadfa "iadfa-scm.lisp")
(require :com.rrette.finenight.fsa-builder "fsa-builder-scm.lisp")
(require :org.ancar.CLUnit "CLUnit.lisp")

(in-package :com.rrette.finenight)

(import 'org.ancar.CLUnit::deftest)

(defparameter *my-iadfa* (debug-gen-iadfa '("addendice"
				    "append" "appendice"
				    "bappend" "bappendice"
				    "cappend" "cappendice")))

(defparameter *my-fsa* (iadfa-fsa *my-iadfa*))


(defun test-dest-1 ()
  (eq (sort (iadfa-state-ancestrors *my-iadfa* 1 #\e) #'<) 
      '(8 22 39)))

(defun test-dest-2 ()
  (eq (sort (iadfa-state-ancestrors *my-iadfa* 7 #\a) #'<) 
      '(0)))

(defun test-accept ()
  (and (accept? *my-fsa*  "appendice")
       (accept? *my-fsa* "bateau")
       (not (accept? *my-fsa* "appendic"))
       (accept? *my-fsa* "append")))

(org.ancar.CLUnit::deftest "Destinations 1" 
  :category "Destinations" 
  :test-fn #'test-dest-1)

(org.ancar.CLUnit::deftest "Destinations 2" 
  :category "Destinations" 
	    :test-fn #'test-dest-2)

(org.ancar.CLUnit::deftest "Accepts" 
  :category "Accepts" 
  :test-fn #'test-accept)
(graphviz-export (make-fsa-builder-from-fsa *my-fsa*))
(graphviz-export-to-file (build-fsa-from-ancestrors *my-iadfa*) "ancestrors.dot")





