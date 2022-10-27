; in sbcl
(load "dump.lisp")
(dump-bytes "test1")
(dump-file "test1")
(dump-file "test1" :width 10)
; default of :width is 0 (infinite length line)


