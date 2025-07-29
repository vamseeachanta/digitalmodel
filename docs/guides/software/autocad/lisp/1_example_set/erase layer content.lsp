(defun c:elay()
	(setq ref (entget(car(entsel)))
			lay_name (cdr (assoc 8 ref))
			sel (ssget"X" (list (cons 8 lay_name)))
	)
	(command"erase" sel "")
	(princ)
)