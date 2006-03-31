(require :com.rrette.finenight.iadfa "iadfa.lisp")
(in-package :com.rrette.finenight)


(graphviz-export (gen-iadfa '("absence" "allo" "avance" "ballon")) :file "export.dot")