(load "finenight")

(in-package :com.rrette.finenight.iadfa)

(defparameter *my-fsa* (gen-iadfa-from-file "com.zone.sorted"))
