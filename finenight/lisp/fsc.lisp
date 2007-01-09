(defpackage :com.rrette.finenight
  (:use "COMMON-LISP")
  (:nicknames "finenight")
  (:export "REDUCE-STATE" 
	   "GENERATE-CHARACTERISTIC-VECTOR"
	   "IS-ACCEPTING"
	   "N-POSITION"))
  
(in-package :com.rrette.finenight)
(provide :com.rrette.finenight.fsc)

;;; This is a state. It contains many positions.
;;; This state is not necesseraly reduced.
(defclass fsc-state ()
  ((positions :accessor state-positions
	     :initarg :positions
	     :initform ())))

;;; This is a state. It contains many positions.
;;; This state is necesseraly reduced.
(defclass reduced-state (state)
  ())

;;; This is a standard position.
(defstruct n-position 
  (i 0)
  (e 0))

(defmethod pos-i ((pos n-position))
  (n-position-i pos))

(defmethod pos-i ((pos cons))
  (car pos))

(defmethod pos-e ((pos n-position))
  (n-position-e pos))

(defmethod pos-e ((pos cons))
  (nth 1 pos))

(defun clone-pos (pos)
  (make-n-position :i (pos-i i)
		   :e (pos-e e)))

;;; This will generate a characteristic vector for the
;;; character given in argument, from the character-sequence
(defun generate-characteristic-vector (character character-sequence)
  (map 'cons (lambda (x)
	       (eql x character)) character-sequence))

;;; This function will return true if the position is 
;;; in an accepting state, relatively to the w (word len)
;;; and the n (leveinstein distance). 
;;;
;;; Note: it's an error to have a pos-i greater than w
(defmethod is-accepting (w n (pos n-position))
  (<= (- w (pos-i pos)) 
      (- n (pos-e pos))))

;;; This function will return true if the subsummer is really
;;; subsumming the subsummee.
(defmethod is-subsumming ((subsummer n-position) (subsummee n-position))
  (let ((i (pos-i subsummer))
	(e (pos-e subsummer))
	(j (pos-i subsummee))
	(f (pos-e subsummee)))
    (and (< e f)
	 (<= (abs (- j i)) 
	     (- f e)))))

;;; This function will return true if the subsummee is really
;;; been subsummed by one or more of the subsummers.
(defmethod is-subsummed ((subsummee n-position) subsummers)
  (some (lambda (subsummer)
	  (is-subsumming subsummer subsummee))
	subsummers))

;;; This will return a reduced positions list
;;; made from the positions given in argument.
(defmethod reduce-state (positions)
  (if (null positions)
      ()
    (if (is-subsummed (car positions) (cdr positions))
	(reduce-state (cdr positions))
      (cons (car positions) (reduce-state (cdr positions))))))

;;; This will return a reduced state of M
(defmethod reduce-state ((M fsc-state))
  (make-instance 'reduced-state (reduce-state (fsc-state-positions M))))

(defmethod reduce-state ((M reduced-state))
  M)

;;; This will return a new state that will be the reduced
;;; set representing the state with the posistion given in
;;; argument. 
(defmethod add-position ((M reduced-state) (pos n-position))
  (make-instance 'reduced-state 
		 :positions (reduce-state (cons pos (fsc-state-positions M)))))

(defmethod add-position ((M fsc-state) (pos n-position))
  (make-instance 'fsc-state :positions (cons pos 
					 (fsc-state-positions M))))

(defmethod reduced-union ((M fsc-state) (N fsc-state))
  (make-instance 'reduced-state 
		 :positions (reduce-state (append (fsc-state-positions M)
					    (fsc-state-positions N)))))

(defmethod relevant-subword (input n pos)
  (let* ((w (length input))
	 (i (pos-i pos))
	 (e (pos-e pos))
	 (k (min (1+ (- (* 2 n) e)) (- w i)))) ;we need to verify the 1+
    (butlast (nthcdr i input) (- w k))))




(defun get-right-non-subsuming-pos (n pos base-pos)
  (let ((positions nil))
    (do ((max-distance (+ (pos-i base-pos) n 1))
	 (high (- (pos-e pos) 1) (- high 1))
	 (j (1+ (pos-i pos)) (+ j 1)))
	((>= j max-distance))
       (do* ((f (abs high) (+ f 1)))
	   ((> f n))
	 (let ((new-pos (make-n-position :i j :e f)))
	   (if (and (not (is-subsumming new-pos pos))
		    (not (is-subsumming pos new-pos))
		    (is-subsumming base-pos new-pos))
	       (setf positions (cons new-pos positions))))))
    positions))


(defun powerset (n pos base-pos)
  (let ((positions (get-right-non-subsuming-pos n pos base-pos))
	(set (list (list pos))))
    (mapcar (lambda (p)
	      (setf set (append set (mapcar (lambda (s)
					      (cons pos s))
					    (powerset n p base-pos)))))
	    positions)
    set))
    
  
(defun state-is-smaller (lhs rhs)
  (if (< (pos-i lhs) (pos-i rhs))
      t
    (if (equal (pos-i lhs) (pos-i rhs))
	(if (< (pos-e lhs) (pos-e lhs))
	    t))))


(defun possible-states (n &key (f 0))
  (if (> f n)
      nil
    (append (powerset n 
		      (make-n-position :i 0 :e f) 
		      (make-n-position :i f :e 0))
	    (possible-states n :f (+ f 1)))))
      

    
			     
	
				      
	       