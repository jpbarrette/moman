(require :org.ancar.CLUnit "CLUnit.lisp")
(require :com.rrette.finenight.edge "edge.lisp")

(in-package :com.rrette.finenight)
(import 'org.ancar.CLUnit::deftest)

(defun test-source ()
  (and (equal (edge-source '(1 "b" 4)) 1)
       (equal (edge-source '(#\b "b" 4)) #\b)
       (equal (edge-source '("b" "c" 4)) "b")))

(defun test-symbol ()
  (and (equal (edge-symbol '(4 1 1)) 1)
       (equal (edge-symbol '(1 #\b 4)) #\b)
       (equal (edge-symbol '(1 "b" 1)) "b")))

(defun test-destination ()
  (and (equal (edge-destination '(1 1 4)) 4)
       (equal (edge-destination '(1 1 #\b)) #\b)
       (equal (edge-destination '(1 1 "b")) "b")))
  

(deftest "edge-source test" 
  :category "Edge base functions tests" 
  :test-fn #'test-source)

(deftest "edge-symbol test" 
  :category "Edge base functions tests" 
  :test-fn #'test-symbol)

(deftest "edge-destination test" 
  :category "Edge base functions tests" 
  :test-fn #'test-destination)




