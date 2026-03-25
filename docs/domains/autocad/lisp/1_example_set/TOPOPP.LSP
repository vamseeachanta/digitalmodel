;*************************************************************************
;From the desk of Paul Standing
;topopp.lsp v1 12/04/96
;This routine will allow the user to pick two points on a map and
;create a profile of the drawing using pickpoints and keyboard input
;***********************************************************************
(defun c:pp ()
(setvar "cmdecho" 0)
(setq pt_list nil)
(setq porig nil)
(setq index 0)
(setq po_list nil)
(setq pt1 (getpoint"\nPick the first point along target line: "))
(setq pt2 (getpoint pt1 "\nPick the end point of target line: "))
(setq d (distance pt1 pt2))
(setq pt3 (getpoint "\nEnter the startpoint of the profile: "))
(setq pt4 (list (+ (car pt3)d) (cadr pt3)))
(command "line" pt3 pt4 "")
(setq ht (getreal "\nEnter text height: "))
(command "text" "j" "r" pt3 ht 0 "SEA LEVEL" )
(setq stop 0)
       (setq p (getpoint "\nSelect point along target line: "))
       (while (= stop 0)
       
       (setq el (getreal "\nEnter the elevation of this point: "))
       (setq x (distance pt1 p))
       (setq pickpt (polar pt3 0 x))
       (command "point" pickpt )
       (setq pt (list (+ (car pt3)x) (+ (cadr pt3)el)))
       (if (= porig nil)(setq porig pt))
       (setq pt_list (append pt_list (list pt)))
       (setq p (getpoint "\nSelect point along target line:")) 
       (if (= p nil)(setq stop nil)
       )
)
(command "pline" porig(foreach p pt_list (command p)))
(setvar "pdmode" 34)
(command "regen")
(princ)
(princ)





)
(prompt "\nType PP to envoke the command")
