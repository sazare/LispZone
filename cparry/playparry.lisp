;; 前提
;; 元のコードのディレクトリでcleandb.shを実行する。bel0とinf0が作成される
(load "parry.lisp")

;1. sbcl起動
(load "loadall.lisp")

;2. データ読み込み
(defvar bels (read-bel))
(make-bels bels)
(defvar infs (read-inf))
(make-infs infs)

;; いまここ

