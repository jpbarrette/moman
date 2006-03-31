(require :com.rrette.finenight.utils "utils.lisp")
(require :org.ancar.CLUnit "CLUnit.lisp")

(in-package :com.rrette.finenight)

(import 'org.ancar.CLUnit::deftest)


(defun test-identity-copy-hash ()
  (let* ((hash (make-hash-table
	       :initial-contents '(("a" . 1) ("b" . 2) ("c" . 3))))
	 (hash-copy (copy-hash-table hash)))
    (not (eq hash hash-copy))))

(defun test-equality-copy-hash ()
  (let* ((hash (make-hash-table
		:initial-contents '(("a" . 1) ("b" . 2) ("c" . 3))))
	 (hash-copy (copy-hash-table hash)))
    (equalp hash hash-copy)))

(defun test-inequality ()
  (let* ((hash (make-hash-table
		:initial-contents '(("a" . 1) ("b" . 2) ("c" . 3))))
	 (hash-copy (copy-hash-table hash)))
    (setf (gethash "d" hash-copy) 4)
    (not (equalp hash hash-copy))))

(deftest "Hash copy instance not EQ test" 
  :category "Hash copy" 
  :test-fn #'test-identity-copy-hash)


(deftest "Hash copy equality test"
  :category "Hash copy"
  :test-fn #'test-equality-copy-hash)

(deftest "Hash copy and add inequality test"
  :category "Hash copy"
  :test-fn #'test-inequality)

