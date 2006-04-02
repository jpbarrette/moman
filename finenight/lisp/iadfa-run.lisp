(require :com.rrette.finenight.iadfa "iadfa.lisp")
(in-package :com.rrette.finenight)

(setf words nil)

(with-open-file (stream "../../data/test.dico")
    (do ((line (read-line stream nil)
               (read-line stream nil)))
        ((null line))
      (setf words (append words (cons line nil)))))

(graphviz-export (gen-iadfa words) :file "export.dot")