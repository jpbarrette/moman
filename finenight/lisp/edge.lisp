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

;;; This function will build an edge from a 3-tuple
(defmethod build-edge (e)
  (make-edge
   :src (edge-source e) 
   :symb (edge-symbol e) 
   :dst (edge-destination e)))

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

;;; This function returns the destination state identification
;;; of this edge.
(defmethod edge-destination (e)
  (nth 2 e))

;;; This function returns the destination state identification
;;; of this edge.
(defmethod edge-destination ((e edge))
  (edge-dst e))
