
(load "finenight")

(in-package :com.rrette.finenight.iadfa)

(defun iadfa-run ()
  (gen-iadfa-from-file "../../data/com.zone.sorted.small"))

(gen-iadfa-from-file "../../data/com.zone.sorted.small")
;(sb-ext:save-lisp-and-die "iadfa-run" :executable t :toplevel #'iadfa-run)

(let ((x 1))
  (defun test ()
    (setf x (1+ x))))

