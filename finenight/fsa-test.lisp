(load "CLUnit.lisp")
(load "fsa.lisp")

(defun test-instance-fsa-copy ()
  (let ((fsa (make-fsa)))
    (not (eq (copy-fsa fsa) fsa))))

(defun test-nodes-not-eq-fsa-copy ()
  (let ((fsa (make-fsa)))
    (not (eq (fsa-nodes (copy-fsa fsa)) (fsa-nodes fsa)))))


(defun test-nodes-equalp-fsa-copy ()
  (let ((fsa (make-fsa)))
    (equalp (fsa-nodes (copy-fsa fsa)) (fsa-nodes fsa))))


(defun test-indentity-add-transition ()
  (let* ((fsa (make-fsa))
	 (fsa2 (copy-fsa fsa)))
    (setf fsa2 (add-transition '(1 #\b 2) fsa2))
    (not (eq (fsa-nodes fsa2) (fsa-nodes fsa)))))

(defun nodes-integrity-add-transition ()
  (let* ((fsa1 (build-fsa '(#\a #\b) '((1 #\a 2) (2 #\b 3)) 1 '(3)))
	(fsa2 (add-transition '(1 #\b 3) fsa1)))
    (null (transition (fsa-node 1 fsa1) #\b))))

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
  :category "FSA add-transition" 
  :test-fn #'test-indentity-add-transition)

(deftest "FSA Nodes Hash Intregrity check" 
  :category "FSA add-transition" 
  :test-fn #'nodes-integrity-add-transition)

(run-all-tests)


