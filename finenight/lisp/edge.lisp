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
       :src (nth 0 e)
       :input-symb (nth 1 e)
       :output-symb (nth 2 e)
       :dst (nth 3 e))
    (make-edge
     :src (nth 0 e) 
     :symb (nth 1 e) 
     :dst (nth 2 e))))

;;; This function returns the source state identification
;;; of this edge.
(defmethod edge-source ((e cons))
  (nth 0 e))

;;; This function returns the source state identification
;;; of this edge.
(defmethod edge-source ((e edge))
  (edge-src e))

;;; This function returns the source state identification
;;; of this edge.
(defmethod edge-source ((e fst-edge))
  (fst-edge-src e))

;;; This function returns the symbol of this edge.
(defmethod edge-symbol ((e cons))
  (edge-symbol (build-edge e)))

;;; This function returns the symbol of this edge.
(defmethod edge-symbol ((e edge))
  (edge-symb e))

;;; This function returns the symbol (input/output) of the edge
(defmethod edge-symbol ((e fst-edge))
  (format nil "~A/~A" (fst-edge-input-symb e) (fst-edge-output-symb e)))

(defmethod edge-input ((edge fst-edge))
  (fst-edge-input-symb edge))

(defmethod edge-input ((edge edge))
  (edge-symbol edge))

(defmethod edge-input ((edge cons))
  (edge-input (build-edge edge)))

(defmethod edge-output ((edge fst-edge))
  (fst-edge-output-symb edge))

(defmethod edge-output ((edge edge))
  (edge-symbol edge))

(defmethod edge-output ((edge cons))
  (edge-output (build-edge edge)))


;;; This function returns the destination state identification
;;; of this edge.
(defmethod edge-destination ((e edge))
  (edge-dst e))

;;; This function returns the destination state identification
;;; of this edge.
(defmethod edge-destination ((e fst-edge))
  (fst-edge-dst e))

;;; This function returns the destination state identification
;;; of this edge.
(defmethod edge-destination ((e cons))
  (edge-destination (build-edge e)))


(defun edgify (edge)
  (list (edge-source edge)
	(if (edge-symbol edge)
	    (string (edge-symbol edge)))
	(edge-destination edge)))

