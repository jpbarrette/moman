(declaim (optimize (speed 0) (space 0) (debug 3)))

(defun copy-hash-table (hash &key (test 'eql)) 
  (let ((h (make-hash-table :test test)))
    (maphash (lambda (key x)
	       (setf (gethash key h) x))
	     hash)
    h))

(defun hash-table-update! (func key hash)
  (setf (gethash key hash) 
	(funcall func (gethash key hash))))

(defun hash-table-update!/default (func key hash default)
  (if (not (nth-value 1 (gethash key hash)))
      (setf (gethash key hash) default))
  (setf (gethash key hash) 
	(funcall func (gethash key hash))))

(defun hash-table-ref/default (key hash default)
  (if (not (nth-value 1 (gethash key hash)))
      (setf (gethash key hash) default)
    (gethash key hash)))

(defun hash-values (hash)
  (let ((values nil))
    (with-hash-table-iterator
     (my-iterator hash)
     (loop
      (multiple-value-bind
          (entry-p key value)
          (my-iterator)
        (declare (ignore key))
        (if entry-p
            (setf values (cons value values))
          (return)))))
    values))

(defun hash-keys (hash)
  (let ((keys nil))
    (with-hash-table-iterator
     (my-iterator hash)
     (loop
      (multiple-value-bind 
          (entry-p key value)
          (my-iterator)
        (declare (ignore value))
        (if entry-p
            (setf keys (cons key keys))
          (return)))))
    keys))


(defun equal-set (rhs lhs)
  (and (eql (list-length lhs)
	    (list-length rhs))
       (reduce (lambda (ok node)
		 (if ok
		     (not (null (member node rhs :test 'equal)))))
	       lhs
	       :initial-value t)))

(defun uniqueness-set (set)
  (if (null set)
      nil
    (if (member (car set) (cdr set))
	(uniqueness-set (cdr set))
      (cons (car set) (uniqueness-set (cdr set))))))
	    

(defun generate-name (index)
  (format nil "q~A" index))

(defun for-each-line-in-file (file func)
  (with-open-file (p file :direction :input)
		  (do ((line (read-line p nil 'eof)
			     (read-line p nil 'eof)))
		      ((eql line 'eof))
		      (format t "~A~%" line)
		      (funcall func line))))
		      
(defun vector-walk (v func)
  (dotimes (x (array-dimension v 0) nil)
    (funcall func x (aref v x))))