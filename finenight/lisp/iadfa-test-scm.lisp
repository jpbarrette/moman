(require :com.rrette.finenight.iadfa "iadfa-scm.lisp")
(require :com.rrette.finenight.fsa-builder "fsa-builder-scm.lisp")
(require :org.ancar.CLUnit "CLUnit.lisp")

(in-package :com.rrette.finenight)

(import 'org.ancar.CLUnit::deftest)

(define my-iadfa (debug-gen-iadfa '("addendice"
                                    "append" "appendice"
                                    "bappend" "bappendice"
                                    "cappend" "cappendice")))
(define my-fsa (iadfa-fsa my-iadfa))

(defun test-dest-1 ()
  (eq? (sort (iadfa-state-ancestrors my-iadfa 1 #\e) <) 
       '(8 22 39)))

(defun test-dest-2 ()
  (eq? (sort (iadfa-state-ancestrors my-iadfa 7 #\a) <) 
       '(0)))

;(check (sort (iadfa-state-ancestrors my-iadfa 8) <) => '(7 24))
;(check (sort (iadfa-state-ancestrors my-iadfa 24) <) => '())

(defun test-accept ()
  (and (accept? my-fsa (string->list "appendice"))
       (accept? my-fsa (string->list "bateau"))
       (not (accept? my-fsa (string->list "appendic")))
       (accept? my-fsa (string->list "append"))))

(graphviz-export (make-fsa-builder-from-fsa my-fsa))
(graphviz-export-to-file (build-fsa-from-ancestrors my-iadfa) "ancestrors.dot")


;(define my-iadfa (gen-iadfa '("appendice" "appendicee"
;                              "bappendice" "bappendicee"
;                              "batisa" "batise" "batissa"
;                              "criba" "cribaa"
;                              "crima" "crime")))
;(define my-fsa (iadfa-fsa my-iadfa))
;(graphviz-export my-fsa)
;(check (sort (iadfa-state-ancestrors my-iadfa 10) <) => '(9 27 33))
;(check (sort (iadfa-state-ancestrors-for-input my-iadfa 10 #\a) <) => '(27 33))
;(check (sort (iadfa-state-ancestrors my-iadfa 8) <) => '(7))
;(check (sort (iadfa-state-ancestrors my-iadfa 24) <) => '())



;(define my-iadfa (handle-word (build-iadfa) (string->list "appendice")))



