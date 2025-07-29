(defun c:barea ( / a b opp tekst)

(setq a (getpoint "\nSelect Internal Point: "))

(command "-Boundary" a "")

(setq b (entlast))

(redraw b 3)

(command "area" "O" "L")

(setq opp (rtos (getvar "area") 2 3))

(setq tekst (strcat "\nArea = " opp))

(alert tekst)

(redraw b 4)

(command "Erase" b "")

(princ)

);defun

(princ)

;coding ends here 
