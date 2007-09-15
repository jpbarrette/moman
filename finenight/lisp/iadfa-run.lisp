
(load "finenight")

(in-package :com.rrette.finenight.iadfa)

(defun iadfa-run ()
  (let* ((iadfa (gen-iadfa-from-file "../../data/com.zone.sorted.small"))
	 (fsa (iadfa-fsa iadfa)))
    (with-open-file (str "output.txt" :direction :output :if-exists :supersede)
      (dolist (word (extract-words fsa))
	(format str "~A~%" word)))))

;(detect-problems-from-file "../../data/com.zone.sorted.small")
(iadfa-run)



;(sb-ext:save-lisp-and-die "iadfa-run" :executable t :toplevel #'iadfa-run)


