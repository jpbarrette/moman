(find-all-symbols :com.rrette.finenight.utils)
(find-all-symbols "common-lisp")
(list-all-packages)

(do-symbols (s :com.rrette.finenight.utils)
  (format nil "~A ~%" s))

(let ((syms '()))
  (do-symbols (s :com.rrette.finenight.iadfa)
    (setf syms (cons s syms)))
  syms)
