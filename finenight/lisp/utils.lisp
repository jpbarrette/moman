(defun copy-hash-table (hash &key (test 'eql)) 
  (let ((h (make-hash-table :test test)))
    (maphash (lambda (key x)
	       (setf (gethash key h) x))
	     hash)
    h))


(defun equal-set (rhs lhs)
  (and (eql (list-length lhs)
	    (list-length rhs))
       (tailp lhs rhs)))

