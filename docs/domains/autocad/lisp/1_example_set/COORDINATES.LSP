(defun c:COORDINATES (/  p x y ptcoord textloc)
(while ;start while
(setq p (getpoint "\nPick Point to Label: "))
(setq textloc (getpoint p "\nPick Text Location"))
(setq x (rtos (car p)))
(setq y (rtos (cadr p)))
(setq ptcoord (strcat "x=" x ", " "y=" y))
(command "_leader" p textloc "" ptcoord "")
(princ)
) ;end while
)