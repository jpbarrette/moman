(require :com.rrette.finenight.fsa "fsa.lisp")
(require :org.ancar.CLUnit "CLUnit.lisp")

(in-package :com.rrette.finenight)

(import 'org.ancar.CLUnit::deftest)

(defun test-instance-fsa-copy ()
  (let ((fsa (make-fsa)))
    (not (eq (copy-fsa fsa) fsa))))

(defun test-nodes-not-eq-fsa-copy ()
  (let ((fsa (make-fsa)))
    (not (eq (fsa-nodes (copy-fsa fsa)) (fsa-nodes fsa)))))


(defun test-nodes-equalp-fsa-copy ()
  (let ((fsa (make-fsa)))
    (equalp (fsa-nodes (copy-fsa fsa)) (fsa-nodes fsa))))


(defun test-indentity-add-edge ()
  (let* ((fsa (make-fsa))
	 (fsa2 (copy-fsa fsa)))
    (setf fsa2 (add-edge '(1 #\b 2) fsa2))
    (not (eq (fsa-nodes fsa2) (fsa-nodes fsa)))))

(defun nodes-integrity-add-edge ()
  (let* ((fsa1 (build-fsa '(#\a #\b) '((1 #\a 2) (2 #\b 3)) 1 '(3)))
	(fsa2 (add-edge '(1 #\b 3) fsa1)))
    (null (transition #\b 1 fsa1))))

(defun transition-fsa-without-node ()
    (null (transition #\b 1 my-fsa)))

(defun transition-simple ()
  (and (equal-set (transition #\b 'a my-fsa) '(b))
       (equal-set (transition "b" 'a my-fsa) '(b))
       (equal-set (transition "c" 'a my-fsa) '(d c))))

(defun transition-extended ()
  (and (null (e-transition "a" my-fsa))
       (null (e-transition "da" my-fsa))
       (equal-set (e-transition "bc" my-fsa) '(d c))
       (equal-set (e-transition "d" my-fsa) '(d))))

(defun test-accepts ()
  (and (not (accepts "a" my-fsa))
       (not (accepts "da" my-fsa))
       (accepts "bc" my-fsa)
       (accepts "d" my-fsa)))

(load :fsa-dat)

(deftest "Instance not EQ test" 
  :category "FSA copy" 
  :test-fn #'test-instance-fsa-copy)

(deftest "Nodes not EQ test" 
  :category "FSA copy" 
  :test-fn #'test-nodes-not-eq-fsa-copy)

(deftest "Nodes equalp test" 
  :category "FSA copy" 
  :test-fn #'test-nodes-equalp-fsa-copy)

(deftest "FSA Nodes Hash Identity test" 
  :category "FSA add-edge" 
  :test-fn #'test-indentity-add-edge)

(deftest "FSA Nodes Hash Intregrity check" 
  :category "FSA add-edge" 
  :test-fn #'nodes-integrity-add-edge)


(deftest "FSA Simple Transition" 
  :category "FSA transitions" 
  :test-fn #'transition-simple)

(deftest "FSA Transition without node" 
  :category "FSA transitions" 
  :test-fn #'transition-fsa-without-node)

(deftest "FSA Extended Transition"
  :category "FSA transitions"
  :test-fn #'transition-extended)

(deftest "FSA Accepts"
  :category "FSA transitions"
  :test-fn #'test-accepts)
