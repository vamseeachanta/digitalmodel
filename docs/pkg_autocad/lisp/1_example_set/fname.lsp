(DEFUN C:FN (/ pt txt )
	(setq pt (GETPOINT "\nPick start point of dwg name: "))
	(setq txt (getvar "savename"))
	(command "text" pt "500" "0" txt)
)
