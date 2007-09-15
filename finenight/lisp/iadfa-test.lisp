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
    (detect-first-starting-problematic-word words-to-be-checked)))


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
  "The 0-5000 was created."
  (test-equivalence '("0-1000000"
		      "0-10000000"
		      "0-3"
		      "0-3-0"
		      "0-3-6"
		      "0-30"
		      "0-300"
		      "0-3000"
		      "0-300MPH"
		      "0-50"
		      "0-500"
		      "0-500MPH")))

(defun iadfa-test13 ()
    (test-equivalence '("0-1000000" "0-10000000" "0-10000HIT" "0-1000ADULTTOYSSEXTOYS" "0-1000HIT"
			"0-100ADOLESCENT18" "0-100C" "0-100EDU" "0-100KM" "0-100SPORTS" "0-101"
			"0-101---0-1-2-3-4-5-6-7-8-9-DECLARATION-OF-WAR--MERCURYDOLPHIN"
			"0-10EISAI" "0-11" "0-110" "0-111" "0-117" "0-12" "0-123" "0-123-456-789"
			"0-123456789" "0-12CLUB" "0-12KIDS" "0-12LINEMEN" "0-13" "0-1320FEET"
			"0-14" "0-15" "0-16" "0-160" "0-168" "0-16EDU" "0-173" "0-18" "0-180"
			"0-18SOS" "0-18SUI" "0-18TEENS4U" "0-19BOYS" "0-1AND1-0" "0-1AVSEX" "0-1DU"
			"0-1KISS" "0-1MEDIA" "0-1NET" "0-1SEX" "0-1SHOP" "0-1TECH" "0-1WEB"
			"0-1XXX" "0-2" "0-2-0" "0-2-1" "0-2-60" "0-2-7"
			"0-2-AMATEUR-XXX-GAY-LESBIAN-ADULT-VIDEOS" "0-2-GENION" "0-2-ONLINE" "0-20"
			"0-200" "0-2000" "0-200MPH" "0-21" "0-212" "0-216" "0-21SMARTKIDS" "0-22"
			"0-23" "0-232" "0-24" "0-24-SEX" "0-24AUTO" "0-24BUSINESS" "0-24FLORIST"
			"0-24H" "0-24H-ZARSZERVIZ" "0-24JEWELRY" "0-24SEX" "0-24SHOP"
			"0-24SHOPPING" "0-24UHR" "0-25" "0-255" "0-261" "0-27" "0-273" "0-28"
			"0-29" "0-2FLO-WERS" "0-2GENION" "0-2GO" "0-2K" "0-2TALENT" "0-2TALENTS"
			"0-2U" "0-2VISA" "0-3" "0-3-0" "0-3-6" "0-3-6AIBB" "0-30" "0-300" "0-3000"
			"0-300MPH" "0-312" "0-34" "0-36" "0-360" "0-360C" "0-360CARDS"
			"0-360FINANCIAL" "0-360HOLDINGS" "0-360HOMES" "0-360HOMETOURS" "0-360MLS"
			"0-360PHOTO" "0-360PHOTOS" "0-360PHOTOTOURS" "0-360PODCASTNEWS"
			"0-360REALTY" "0-360RESORTS" "0-360TOUR" "0-360TOURS" "0-360VIEWS"
			"0-360VR" "0-365" "0-371" "0-3BABY" "0-3D" "0-3FORUM" "0-3YEARS" "0-4"
			"0-4-0" "0-40" "0-400KM" "0-400M" "0-400MPH" "0-41" "0-411" "0-45" "0-462"
			"0-48" "0-49" "0-4D" "0-4FACTORY" "0-4VADUZ" "0-5" "0-5-0" "0-5-30" "0-50"
			"0-500" "0-500MPH")))
  
(iadfa-test13)


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








