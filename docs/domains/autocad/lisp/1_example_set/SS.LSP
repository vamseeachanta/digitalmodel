
(defun c:ss ( / a b)
   (setq a (cdr (assoc 1 (entget (car (entsel "Select TEXT to
transfer:"))))))
   (while (setq b (car (entsel "Select TEXT item to replace:")))
     (setq c (entget b))
     (entmod (subst (cons 1 a)(assoc 1 c)c))
   (if (or (= (cdr (assoc 0 c)) "POLYLINE")(= (cdr (assoc 0 c))
"INSERT"))
   (entupd b)))
(princ)
)

   (setq cnt 1 ss (ssadd))
   (while pt1 (setq pt2 (getpoint "\nEnter next point:"))
   (if pt2 (progn (command "arc" pt1 "e" pt2 "r" (/ (distance pt1 
 pt2) 1.75))
   (if (= cnt 1) (setq arc1 (entlast) cnt 2) (ssadd (entlast) ss))
))
   (setq pt1 pt2) ) (setvar "HIGHLIGHT" 0) (if (> (sslength ss) 0)
   (command "pedit" arc1 "Y" "J" ss "" "X")
   (if arc1 (command "pedit" arc1 "Y" "X")) ) (setvar "HIGHLIGHT"
1)
   (princ)
)
