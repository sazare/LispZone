;; 前提
;; 元のコードのディレクトリでcleandb.shを実行する。bel0とinf0が作成される
(load "parry.lisp")

;1. sbcl起動
(defvar bels (read-bel))
(make-bels bels)
(defvar infs (read-inf))
(make-infs infs)

;; いまここ

