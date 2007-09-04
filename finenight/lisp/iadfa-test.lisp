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

(defmacro test-equivalence (words)
  (with-syms (w iadfa output)
    `(let* ((,w ,words)
            (,iadfa (debug-gen-iadfa ,w))
	    (,output nil))
       (setf ,output (extract-words (iadfa-fsa ,iadfa)))
       (format t "input:~%~S~%output:~%~S~%" ,w ,output)
       (equal ,w ,output))))
       

(defun iadfa-non-branch-suffix ()
  "This tests that the output of the iadfa isn't screwed up 
by the prefix 0--0 of 0--0--0 because the delete branch 
don't delete any node"
  (test-equivalence '("0-----0"
		      "0--0"
		      "0--0--0")))

(org.ancar.CLUnit::deftest "IADFA tests"
    :category "Destinations" 
    :test-fn #'iadfa-non-branch-suffix)

(defun iadfa-test1 ()
  (test-equivalence '("0-APR-CREDIT-CARD"
		      "0-APR-CREDIT-CARD-4U"
		      "0-APR-CREDIT-CARD-APPLICATION"
		      "0-APR-CREDIT-CARD-OFFERS"
		      "0-APR-CREDIT-CARD-ONLINE"
		      "0-APR-CREDIT-CARDS"
		      "0-APR-CREDITCARD"
		      "0-APR-CREDITCARDS"
		      "0-APR-CREDITS-CARD")))
(defun iadfa-test2 ()
 "This situation would cause to have an empty ancestror fsa."
 (test-equivalence '("0000"
		     "0001")))



(defun iadfa-test3 ()
  "bad behavior where the common-suffix was going too far.
We had a cycle on the W (1 -W> 2 -O> 3 -O> 1)."
  (test-equivalence '("08WL"
		      "08WOOL"
		      "08WOOOL"
		      "08WOOOOL")))

(defun iadfa-test4 ()
  "The stem wasn't long enough."
  (test-equivalence '("0-adance"
		      "0-atransfers"
		      "0-babobibobu"
		      "0-balance"
		      "0-balance-transfers")))


(org.ancar.CLUnit::deftest "IADFA Test 1"
    :category "Destinations" 
    :test-fn #'iadfa-test1)

(org.ancar.CLUnit::deftest "IADFA Test 2"
    :category "Destinations" 
    :test-fn #'iadfa-test2)

(org.ancar.CLUnit::deftest "IADFA Test 3"
    :category "Destinations" 
    :test-fn #'iadfa-test3)

(org.ancar.CLUnit::deftest "IADFA Test 4"
    :category "Destinations" 
    :test-fn #'iadfa-test4)

(org.ancar.CLUnit::run-all-tests)








