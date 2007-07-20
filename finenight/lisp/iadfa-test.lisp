(require :org.ancar.CLUnit "CLUnit")
(require :com.rrette.finenight "finenight")

;(import 'org.ancar.CLUnit::deftest)
(in-package :cl-user)

(use-package :com.rrette.finenight.fsa)
(use-package :com.rrette.finenight.fsa-builder)
(use-package :com.rrette.finenight.iadfa)

(defparameter *my-iadfa* (debug-gen-iadfa '("addendice"
					    "append" "appendice"
					    "bappend" "bappendice"
					    "cappend" "cappendice"
					    "mormont")))

(defparameter *my-iadfa* (debug-gen-iadfa '("0-atransfers"
					    "0-adance"
					    "0-babobibobu"
					    "0-balance"
					    "0-balance-transfers")))


(defparameter *my-iadfa* (debug-gen-iadfa '("addendice"
					    "append" "appendice")))

;(defparameter *my-fsa* (gen-iadfa-from-file "../../data/test.dico"))
;(defparameter *my-fsa* (gen-iadfa-from-file "com.zone.sorted.small"))
(defparameter *my-fsa* (gen-iadfa-from-file "com.zone.sorted.small.very"))
(graphviz-export-to-file (make-fsa-builder-from-fsa *my-fsa*) "export.dot")

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





