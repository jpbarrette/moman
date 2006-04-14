(require :com.rrette.finenight.fsa :fsa)

(load "node-test.lisp")
(load "edge-test.lisp")
(load "fsa-test.lisp")
(load "fsc-test.lisp")
(load "utils-test.lisp")
(load :state-test)

(run-all-tests)