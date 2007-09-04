;(require :org.ancar.CLUnit "CLUnit")
;(require :com.rrette.finenight "finenight")

(load "CLUnit.lisp")

(load "finenight")

;(in-package :cl-user)

;(import 'org.ancar.CLUnit::deftest)

(in-package :com.rrette.finenight.iadfa)

(defparameter *my-iadfa* (debug-gen-iadfa '("addendice"
					    "append" "appendice"
					    "bappend" "bappendice"
					    "cappend" "cappendice"
					    "mormont")))

(defun iadfa-non-branch-suffix ()
  "This tests that the output of the iadfa isn't screwed up 
by the prefix 0--0 of 0--0--0 because the delete branch 
don't delete any node"
  (let* ((words '("0-----0"
		  "0--0"
		  "0--0--0"))
	 (iadfa (debug-gen-iadfa words)))
    (equal words (extract-words (iadfa-fsa iadfa)))))

(org.ancar.CLUnit::deftest "IADFA list"
    :category "Destinations" 
    :test-fn #'iadfa-non-branch-suffix)

(org.ancar.CLUnit::run-all-tests)

(defparameter *my-iadfa* (gen-iadfa '("0-APR-CREDIT-CARD"
				      "0-APR-CREDIT-CARD-4U"
				      "0-APR-CREDIT-CARD-APPLICATION"
				      "0-APR-CREDIT-CARD-OFFERS"
				      "0-APR-CREDIT-CARD-ONLINE"
				      "0-APR-CREDIT-CARDS"
				      "0-APR-CREDITCARD"
				      "0-APR-CREDITCARDS"
				      "0-APR-CREDITS-CARD")))

(extract-words (iadfa-fsa *my-iadfa*))

;; This situation would cause to have an empty ancestror fsa.
(defparameter *my-iadfa* (gen-iadfa '("0000"
					    "0001")))



;; bad behavior where the common-suffix was going too far.
;; We had a cycle on the W (1 -W> 2 -O> 3 -O> 1).
(defparameter *my-iadfa* (gen-iadfa '("08WL"
					    "08WOOL"
					    "08WOOOL"
					    "08WOOOOL")))

;; bad behavior where the common-suffix was going too far.
;; We had a cycle on the W (1 -W> 2 -O> 3 -O> 1).
(defparameter *my-iadfa* (gen-iadfa '("0060GIRLSXX"
					    "0061TEENSX"
					    "0061TEENSXX")))

;; the stem wasn't long enough).
(defparameter *my-iadfa* (gen-iadfa '("0-atransfers"
					    "0-adance"
					    "0-babobibobu"
					    "0-balance"
					    "0-balance-transfers")))


(defparameter *my-iadfa* (gen-iadfa '("addendice"
				      "append" "appendice")))

;(com.rrette.finenight.fsa::save-fsa (iadfa-fsa *my-iadfa*))

;(defparameter *my-fsa* (gen-iadfa-from-file "../../data/test.dico"))
;(defparameter *my-fsa* (gen-iadfa-from-file "com.zone.sorted.small"))
;(defparameter *my-fsa* (gen-iadfa-from-file "com.zone.sorted.small"))
;(graphviz-export-to-file (make-fsa-builder-from-fsa *my-fsa*) "export.dot")

;(defparameter *my-fsa* (iadfa-fsa *my-iadfa*))


;; (defun test-dest-1 ()
;;   (eq (sort (iadfa-state-ancestrors *my-iadfa* 1 #\e) #'<) 
;;       '(8 22 39)))

;; (defun test-dest-2 ()
;;   (eq (sort (iadfa-state-ancestrors *my-iadfa* 7 #\a) #'<) 
;;       '(0)))

;; (defun test-accept ()
;;   (and (accept? *my-fsa*  "appendice")
;;        (accept? *my-fsa* "bateau")
;;        (not (accept? *my-fsa* "appendic"))
;;        (accept? *my-fsa* "append")))


;; (org.ancar.CLUnit::deftest "Destinations 2" 
;;   :category "Destinations" 
;; 	    :test-fn #'test-dest-2)


;(graphviz-export (make-fsa-builder-from-fsa *my-fsa*))
;(graphviz-export-to-file (build-fsa-from-ancestrors *my-iadfa*) "ancestrors.dot")





