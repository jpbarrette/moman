(defpackage "FINENIGHT"
  (:use "COMMON-LISP")
  (:nicknames "fn")
  (:export "REDUCE-STATE" 
	   "GENERATE-CHARACTERISTIC-VECTOR"
	   "IS-ACCEPTING"
	   "N-POSITION"))
  
(in-package finenight)

;;; This is a state. It contains many positions.
;;; This state is not necesseraly reduced.
(defclass state ()
  ((positions :accessor state-positions
	     :initarg :positions
	     :initform ())))

;;; This is a state. It contains many positions.
;;; This state is necesseraly reduced.
(defclass reduced-state (state)
  ())

;;; This is a standard position.
(defclass n-position ()
  ((i :accessor position-i
      :initarg :i)
   (e :accessor position-e
      :initarg :e)))

;;; This is a tranposition position.
(defstruct t-position (n-position)
  ())

(defstruct s-position (n-position)
  ())

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
  (<= (- w (position-i pos)) 
      (- n (position-e pos))))

;;; This function will return true if the subsummer is really
;;; subsumming the subsummee.
(defmethod is-subsumming ((subsummer n-position) (subsummee n-position))
  (let ((i (position-i subsummer))
	(e (position-e subsummer))
	(j (position-i subsummee))
	(f (position-e subsummee)))
    (and (< e f)
	 (<= (abs (- j i)) 
	     (- f e)))))

;;; This function will return true if the subsummee is really
;;; been subsummed by one or more of the subsummers.
(defmethod is-subsummed ((subsummee n-position) subsummers)
  (some #'(lambda (subsummer)
	    (is-subsumming x subsummee))))

;;; This will return a reduced positions list
;;; made from the positions given in argument.
(defmethod reduce-state (positions)
  (if (null positions)
      ()
    (if (is-subsummed (car positions) (cdr positions))
	(reduce-state (cdr positions))
      (cons (car positions) (reduce-state (cdr positions))))))

;;; This will return a reduced state of M
(defmethod reduce-state ((M state))
  (make-instance 'reduced-state (reduce-state (state-positions M))))

;;; This will return a new state that will be the reduced
;;; set representing the state with the posistion given in
;;; argument. 
(defmethod add-position ((M reduced-state) (pos n-position))
  (make-instance 'reduced-state 
		 :positions (reduce-state (cons pos (state-positions M)))))

(defmethod add-position ((M state) (pos n-position))
  (make-instance 'state :positions (cons pos 
					 (state-positions M))))

(defmethod reduced-union ((M state) (N state))
  (make-instance 'reduced-state 
		 :positions (reduce-state (append (state-positions M)
					    (state-positions N)))))

(defmethod relevant-subword (input n pos)
  (let* ((w (length input))
	 (i (position-i pos))
	 (e (position-e pos))
	 (k (min (1+ (- (* 2 n) e)) (- w i)))) ;we need to verify the 1+
    (butlast (nthcdr i input) (- w k))))


