;; a script for the dump after initialization phase

(load "loadall.lisp")
(load "utils.lisp")

; show all list
 (showtree 'flarelist '(words sets set) ())
 (showtree 'sensitivelist '(words sets set) ())
 (showtree 'setlist '(words sets set) ())

; show mafia delusion transition
 (showlist (get 'flarelist 'sets) '(next) Nil)

; show story
 (showlist (mapcar (lambda (x) (car x)) stl) '(story) () )


(readbel)
*intlist*

(readinf)

(defvar paop (union *aoputplist* *aoputplist*))
(defvar daop (union *aodefplist* *aodefplist*))

(print paop)
(print daop)




