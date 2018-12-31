;(load "test.lisp")
;(load "pmem5.lisp")

(test-set "test for measuere"
	  (test "number gt" t (measuere 5 2))
	  (test "number lt" nil (measuere 1 2))
	  (test "number eq" nil (measuere 5 5))
	  (test "number gt float" t (measuere 5.2 4.8))
	  (test "atom eq" t (measuere 'ab 'ab))
	  (test "atom neq" nil (measuere 'ab 'cd))
	  (test "atom num" nil (measuere 3 'cd))
	  (test "atom num" nil (measuere 'ab 9.2))
)


(test-set "test for aplology"
;; negative
  (setf mistrust 8)
  (setf ajump 0.5)
  (setf anger 5.0)
  (setf (get 'DDKNOW 'truth) nil)
  (apology)
  (test "after not ajump " t (= ajump 0.5))
  (test "after not anger" t (= anger 4.0))
  (test "after not chosen" 'ACCUSE chosen)

;; negative
  (setf mistrust 8)
  (setf ajump 0.5)
  (setf anger 5.0)
  (setf (get 'DDKNOW 'truth) T)
  (apology)
  (test "after not ajump " t (= ajump 0.5))
  (test "after not anger" t (= anger 4.0))
  (test "after not chosen" 'SORRY chosen)

;; active
  (setf mistrust 10)
  (setf ajump 0.5)
  (setf anger 5.0)
  (setf (get 'DDKNOW 'truth) nil)
  (apology)
  (test "after not ajump " t (= ajump 0.2))
  (test "after not anger" t (= anger 5.0))
  (test "after not chosen" 'ACCUSE chosen)


;; active
  (setf mistrust 10)
  (setf ajump 0.5)
  (setf anger 5.0)
  (setf (get 'DDKNOW 'truth) T)
  (apology)
  (test "after not ajump " t (= ajump 0.2))
  (test "after not anger" t (= anger 5.0))
  (test "after not chosen" 'SORRY chosen)


  (setf (get 'DDKNOW 'truth) nil)
)
 
;; helper
(test-set "test for helper"
;; dhostile T
  (setf chosen nil)
  (setf (get 'dhostile 'truth) t)
  (setf (get '*dhelpful 'truth) nil)
  (setf (get 'ddharm 'truth) nil)
  (helper)
  (test "after helper dhostile" 'caution chosen)

;; ?*dhelpful T
  (setf chosen nil)
  (setf (get 'dhostile 'truth) nil)
  (setf (get '*dhelpful 'truth) t)
  (setf (get 'ddharm 'truth) nil)
  (helper)
  (test "after helper *dhelpful" 'caution chosen)

;; ddharm T
  (setf chosen nil)
  (setf (get 'dhostile 'truth) nil)
  (setf (get '*dhelpful 'truth) nil)
  (setf (get 'ddharm 'truth) t)
  (helper)
  (test "after helper ddharm" 'caution chosen)

;; all nil
  (setf chosen nil)
  (setf (get 'dhostile 'truth) nil)
  (setf (get '*dhelpful 'truth) nil)
  (setf (get 'ddharm 'truth) nil)
  (helper)
  (test "after helper all nil" nil chosen)


)

;; knower
(test-set "test for knower"
;;
  (format t "skip test~%")
)

