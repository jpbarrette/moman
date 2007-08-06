(require :com.rrette.finenight.fsc "fsc.lisp")
(require :org.ancar.CLUnit "CLUnit.lisp")

(in-package :com.rrette.finenight)

(import 'org.ancar.CLUnit::deftest)
(import 'org.ancar.CLUnit::run-all-tests)

(defun test-char-vector1 ()
  (let ((myvect "abc")
	(mycvect '(t nil nil)))
    (equal mycvect (generate-characteristic-vector #\a myvect))))

(defun test-char-vector2 ()
  (let ((myvect "aab")
	(mycvect '(t t nil)))
    (equal mycvect (generate-characteristic-vector #\a myvect))))

(defun test-char-vector3 ()
  (let ((myvect "aaa")
	(mycvect '(t t t)))
    (equal mycvect (generate-characteristic-vector #\a myvect))))

(defun test-char-vector4 ()
  (let ((myvect "caa")
	(mycvect '(nil t t)))
    (equal mycvect (generate-characteristic-vector #\a myvect))))

(defun test-char-vector5 ()
  (let ((myvect "bca")
	(mycvect '(nil nil t)))
    (equal mycvect (generate-characteristic-vector #\a myvect))))

(defun test-char-vector6 ()
  (let ((myvect "dbc")
	(mycvect '(nil nil nil)))
    (equal mycvect (generate-characteristic-vector #\a myvect))))

(defun test-accepting1 ()
  (let ((pos1 (make-n-position :i 1 :e 1))
	(pos2 (make-n-position :i 2 :e 1))
	(res '(nil nil)))
    (and 
     (not (is-accepting 1 0 pos1))
     (is-accepting 1 1 pos1)
     (is-accepting 1 2 pos1)
     (not (is-accepting 2 0 pos1))
     (not (is-accepting 2 1 pos1))
     (is-accepting 2 2 pos1)
     (not (is-accepting 2 0 pos2))
     (is-accepting 2 1 pos2)
     (is-accepting 2 2 pos2)
     (not (is-accepting 3 0 pos2))
     (not (is-accepting 3 1 pos2))
     (is-accepting 3 2 pos2)
     (not (is-accepting 4 2 pos2)))))

(defun test-first-occurence ()
  (and 
   (equalp (first-occurence (list (make-n-position :i 0 :e 0)))
	   (list (make-n-position :i 1 :e 0)))
   (equalp (first-occurence (list (make-n-position :i 1 :e 0)))
	   (list (make-n-position :i 2 :e 0)))
   (equalp (first-occurence (list (make-n-position :i 1 :e 1)))
	   (list (make-n-position :i 2 :e 1)))
   (equalp (first-occurence (list (make-n-position :i 1 :e 1)
				  (make-n-position :i 2 :e 1)))
	   (list (make-n-position :i 2 :e 1)
		 (make-n-position :i 3 :e 1)))))


(defun test-occurs-in ()
  t)

(deftest "first-occurence tests"
  :category "n-Beta-Delta tests"
  :test-fn #'test-first-occurence)

	   
(deftest "Characteristic Vector Test 1 (gcv a \"abc\") -> (t nil nil)" 
  :category "Characteristic Vector Tests" 
  :test-fn #'test-char-vector1)
(deftest "Characteristic Vector Test 2 (gcv a \"aab\") -> (t t nil)" 
  :category "Characteristic Vector Tests" 
  :test-fn #'test-char-vector2)
(deftest "Characteristic Vector Test 3 (gcv a \"aaa\") -> (t t t)" 
  :category "Characteristic Vector Tests" 
  :test-fn #'test-char-vector3)
(deftest "Characteristic Vector Test 4 (gcv a \"caa\") -> (nil t t)" 
  :category "Characteristic Vector Tests" 
  :test-fn #'test-char-vector4)
(deftest "Characteristic Vector Test 5 (gcv a \"bca\") -> (nil nil t)" 
  :category "Characteristic Vector Tests" 
  :test-fn #'test-char-vector5)
(deftest "Characteristic Vector Test 6 (gcv a \"dbc\") -> (nil nil nil)" 
  :category "Characteristic Vector Tests" 
  :test-fn #'test-char-vector6)

(deftest "Accepting Positions Test 1"
  :category "Accepting Positions Tests"
  :test-fn #'test-accepting1)


(run-all-tests)