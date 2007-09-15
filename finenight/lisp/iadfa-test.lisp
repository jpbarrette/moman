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

(defun detect-problems (words)
  (let ((iadfa (build-iadfa))
	(words-to-be-checked nil))
    (dolist (word words)
      (setf words-to-be-checked (nconc words-to-be-checked (list word)))
      (handle-word iadfa (concatenate 'list word))
      (when (not (equal words-to-be-checked 
			(extract-words (iadfa-fsa iadfa))))
	(return)))
    ;; We got the first entry that trigger the problem.
    ;; we need now to see which entry is needed to start
    ;; the problem
    words-to-be-checked))


(defun detect-first-starting-problematic-word (words-to-be-checked)
  (let ((wtbc (cdr words-to-be-checked))
	(last-word (car words-to-be-checked)))
    (do ((iadfa (gen-iadfa wtbc) (gen-iadfa wtbc)))
	((null wtbc))
      (if (equal wtbc
		 (extract-words (iadfa-fsa iadfa)))
	  (return (cons last-word wtbc)))
      (setf last-word (car wtbc))
      (setf wtbc (cdr wtbc)))))

(defun filter-non-problematic-words (words-to-be-checked)
  (let ((problematics-words (list (car words-to-be-checked)))
	(last-word (cadr words-to-be-checked))
	(words-to-be-checked (cddr words-to-be-checked)))
    (do ((iadfa (gen-iadfa (append problematics-words words-to-be-checked))
		(gen-iadfa (append problematics-words words-to-be-checked))))
	((null words-to-be-checked))
      (if (equal (append problematics-words words-to-be-checked)
		 (extract-words (iadfa-fsa iadfa)))
	  (setf problematics-words (nconc problematics-words (list last-word))))
      (setf last-word (car words-to-be-checked))
      (setf words-to-be-checked (cdr words-to-be-checked)))
    (setf problematics-words (nconc problematics-words (list last-word)))
    problematics-words))
	     

(defun detect-problems-from-file (filename)
  (let ((words-to-be-checked nil))
    (let ((iadfa (build-iadfa)))
      (for-each-line-in-file (word filename)
	(setf words-to-be-checked (nconc words-to-be-checked (list word)))
	(handle-word iadfa (concatenate 'list word))
	(when (not (equal words-to-be-checked 
			  (extract-words (iadfa-fsa iadfa))))
	  (return))
	nil))
    ;; We got the first entry that trigger the problem.
    ;; we need now to see which entry is needed to start
    ;; the problem
    (setf words-to-be-checked 
	  (detect-first-starting-problematic-word words-to-be-checked))
    (setf words-to-be-checked
	  (filter-non-problematic-words words-to-be-checked))
    words-to-be-checked))
  


(detect-problems-from-file "../../data/com.zone.sorted.small")
      
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

(defun iadfa-test5 ()
  "When we add 0-SUNGKOREA the stem starts within the common suffixes. 
So we add the stem within the suffixes which create new words."
  (test-equivalence '("0-FORUM" 
		      "0-SSUM"
		      "0-SSUNGKOREA")))


(defun iadfa-test6 ()
  "When we add 0-SUNGKOREA the stem starts within the common suffixes. 
So we add the stem within the suffixes which create new words.
So we need to make sure that the common-suffix won't go further than
the 0-ASUNGKOREA"
  (test-equivalence '("0-ASUNGKOREA"
		      "0-FORUM" 
		      "0-S"
		      "0-SUM"
		      "0-SUNGKOREA")))

(defun iadfa-test8 ()
"This is an example of a test where we had a bad 
update of parents-arities"
  (test-equivalence '("0-1"
		      "0-1-0"
		      "0-1-1")))

(defun iadfa-test9 ()
  (test-equivalence '("0-1-2"
		      "0-1-2-0"
		      "0-1-2-3")))

(defun iadfa-test10 ()
  
  (test-equivalence '("0-1"
		      "0-1-0"
		      "0-1-1"
		      "0-1-100"
		      "0-1-2"
		      "0-1-2-0"
		      "0-1-2-3")))

(defun iadfa-test11 ()
  "This situation was causing problems. The 0-2GENION was 
disapearing after the 0-2GO addition. 0-2GO was subsuming 
the stem of 0-2GENION, so we had GE to add back for the 
subsumed, but the calculated stem to add was only G, 
since we had the entry 0-0OO. 

The cause was that we were completely consuming the profile,
but we shouldn't eat profile when there's one to consume"
  (test-equivalence '("0-0OO"
		      "0-2-GENION"
		      "0-2GENION"
		      "0-2GO")))

(org.ancar.CLUnit::deftest "IADFA Test 11"
    :category "Subsumed previous stems." 
    :test-fn #'iadfa-test11)

(defun iadfa-test12 ()
  "The 0-7-0 was lost. When we were adding the last entry, 
common-prefix was returning a suffix of 7-2 and a previous 
stem of 7-0. However, because of 0-462, we were getting a 
stem of 7 from common-suffix and a previous stem of 7-0.

In fact we shouldn't try to get a common prefix that would 
produce a stem shorter than the previous stem.
"
  (test-equivalence '("0-462"
		      "0-5-0"
		      "0-7-0"
		      "0-7-2")))

(org.ancar.CLUnit::deftest "IADFA Test 12"
    :category "Subsumed previs stems." 
    :test-fn #'iadfa-test12)

(defun iadfa-test13 ()
  "The 0-5000 was created. This was caused by the common
prefix node of 0-3000 and 0-300MPH forgot to remove the 
ancestror of the node created for the subsubed previous 
stem"
  (test-equivalence '("0-1000000" 
		      "0-10000000" 
		      "0-300" 
		      "0-3000" 
		      "0-300MPH" 
		      "0-500"
		      "0-500MPH")))

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

(org.ancar.CLUnit::deftest "IADFA Test 5"
    :category "Destinations" 
    :test-fn #'iadfa-test5)

(org.ancar.CLUnit::deftest "IADFA Test 6"
    :category "Destinations" 
    :test-fn #'iadfa-test6)

(org.ancar.CLUnit::deftest "IADFA Test 8"
    :category "Destinations" 
    :test-fn #'iadfa-test8)

(org.ancar.CLUnit::deftest "IADFA Test 9"
    :category "Destinations" 
    :test-fn #'iadfa-test9)

(org.ancar.CLUnit::deftest "IADFA Test 10"
    :category "Destinations" 
    :test-fn #'iadfa-test10)



(org.ancar.CLUnit::run-all-tests)








