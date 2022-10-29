; in sbcl
(load "dump.lisp")

(dump-byte "test1")

(dump-file "test1")
(dump-file "test1" :width 10)
; default of :width is 0 (infinite length line)
(dump-file "test1" :width 10)
(dump-file "test1" :pmode :hex :width 10)
(dump-file "test1" :width 10 :pmode :octal)
(dump-file "test1" :pmode :octal)



