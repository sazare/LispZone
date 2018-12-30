(load "testmtp.lisp")
(load "test.lisp")

(test-set "changemark"
  (test "no specials" "this is that or it" (changemark "this is that or it"))
  (test "dot and comma" "this is that or it DOT" (changemark "this is that, or it."))
  (test "question" "who are you ?" (changemark "who are you?"))
)

