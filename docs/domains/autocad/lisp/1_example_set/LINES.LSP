(defun C:ADDL ()
(defun *error* (msg)
	(princ "error: ")
	(princ msg)
	(terpri)
)
	(graphscr)
	(prompt "\n CHOOSE TWO POINTS TO DRAW A LINE BETWEEN")
	(command "osnap" "node"
		"line" pause pause
	)
)
