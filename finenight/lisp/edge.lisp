(defpackage :com.rrette.finenight
  (:use "COMMON-LISP")
  (:nicknames "finenight")
  (:export "edge-source"
	   "edge-symbol"
	   "edge-destination"))

(in-package :com.rrette.finenight)
(provide :com.rrette.finenight.edge)

(defstruct edge
  src
  symb
  dst)

(defstruct fst-edge 
  src
  input-symb
  output-symb
  dst)

;;; This function will build an edge from a 3-tuple, or a fst-edge
;;; if it's a 4-tuple.
(defun build-edge (e)
  (if (equal (length e) 4)
      (make-fst-edge
       :src (edge-source e)
       :input-symb (edge-input e)
       :output-symb (edge-output e))
    (make-edge
     :src (edge-source e) 
     :symb (edge-symbol e) 
     :dst (edge-destination e))))

;;; This function returns the source state identification
;;; of this edge.
(defmethod edge-source (e)
  (nth 0 e))

;;; This function returns the source state identification
;;; of this edge.
(defmethod edge-source ((e edge))
  (edge-src e))

;;; This function returns the symbol of this edge.
(defmethod edge-symbol (e)
  (nth 1 e))

;;; This function returns the symbol of this edge.
(defmethod edge-symbol ((e edge))
  (edge-symb e))

;;; This function returns the symbol (input/output) of the edge
(defmethod edge-symbol ((e fst-edge))
  (format t "~A/~A" (fst-edge-input-symb e) (fst-edge-output-symb e)))

(defmethod edge-input ((edge fst-edge))
  (fst-edge-input-symb edge))

(defmethod edge-input (edge)
  (edge-symbol edge))

(defmethod edge-output ((edge fst-edge))
  (fst-edge-output-symb edge))

(defmethod edge-output (edge)
  (edge-symbol edge))


;;; This function returns the destination state identification
;;; of this edge.
(defmethod edge-destination (e)
  (nth 2 e))


(defun edgify (edge)
  (list (edge-source edge)
	(if (edge-symbol edge)
	    (string (edge-symbol edge)))
	(edge-destination edge)))

;;; This function returns the destination state identification
;;; of this edge.
(defmethod edge-destination ((e edge))
  (edge-dst e))
