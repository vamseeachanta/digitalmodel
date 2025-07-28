
; DOUBLE OFFSET DOF.lsp vs 1.0 March 28 1996 
; Function creates an offset to either side of the selected entity
; and erases the original.

(defun c:DOF (/ a b b1 p1 d pckbox)
(setq d 20 p1 nil pee (/ pi 2)e 3 pckbox (getvar "pickbox")line "                               ")
(if(= #parof nil)(setq #parof(/(getvar "viewsize")10)))
(princ "\nCurrent offset <")(princ #parof)
(setq answer(getstring ">: "))
(if (/= (atof answer) 0.0)(setq #parof(atof answer)))

	(princ line)
	(setvar "pickbox" pckbox)
	(setq ent(entsel "\nPick the line: "))
	(setvar "pickbox" 2)
	(setq point(cadr ent))
	(princ line)
        (setq side(getpoint "\nPick an offset side: "))
	(setq dist(distance side point))(setq ang(angle side point))
	(if(or(or(< ang 0.78)(> ang 5.5))(and(> ang 2.35)(< ang 3.92)))
		(setq ang(- 0 ang))(setq ang(- pi ang))
	);end if
	(setq other(polar point ang dist))
	(command "offset" #parof ent side ent other "")
        (entdel(car ent))

(setvar "pickbox" pckbox)
(prin1)
)
