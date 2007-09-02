
(load "finenight")

(in-package :com.rrette.finenight.iadfa)

(defun iadfa-run ()
  (gen-iadfa-from-file "../../data/com.zone.sorted.small"))

(gen-iadfa-from-file "../../data/com.zone.sorted.small")
;(sb-ext:save-lisp-and-die "iadfa-run" :executable t :toplevel #'iadfa-run)


(progn
  (setf last-time (get-internal-real-time))
  (sleep 3)
  (setf current-time (get-internal-real-time))
  (float (* 1000 (/ 1 (/ (- current-time last-time) internal-time-units-per-second)))))
