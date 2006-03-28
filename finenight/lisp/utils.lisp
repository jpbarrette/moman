(defpackage :com.rrette.finenight
  (:use "COMMON-LISP")
  (:nicknames "finenight")
  (:export "copy-hash-table"
	   "equal-set"
	   "uniqueness-set"))

(in-package :com.rrette.finenight)
(provide :com.rrette.finenight.utils)

(defun copy-hash-table (hash &key (test 'eql)) 
  (let ((h (make-hash-table :test test)))
    (maphash (lambda (key x)
	       (setf (gethash key h) x))
	     hash)
    h))


(defun equal-set (rhs lhs)
  (and (eql (list-length lhs)
	    (list-length rhs))
       (reduce (lambda (ok node)
		 (if ok
		     (not (null (member node rhs)))))
		 (cons t lhs))))

(defun uniqueness-set (set)
  (if (null set)
      nil
    (if (member (car set) (cdr set))
	(uniqueness-set (cdr set))
      (cons (car set) (uniqueness-set (cdr set))))))
	    