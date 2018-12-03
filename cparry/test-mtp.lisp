
(test-set "changemark"
  (test "dot and comma" "this is that or it ." (changemark "this is that, or it."))
  (test "question" "who are you ?" (changemark "who are you?"))
)

