;This lisp is developed as part of Survey Drawing project, which aims to help people prepare survey drawings easily.
;You may find more such lisps at www.surveydrawing.net and www.esurveying.net


(defun c:EXyz()
	(setq entset(ssget (list (cons 0 "Text"))))
	(setq len(sslength entset))
	(setq ctr 0)
	(setq fileName(getfiled "Specify CSV File Name" "C:\\XYZCode" "CSV" 1))
	(setq fileO(open fileName "w"))
	(repeat len
		(setq ent(ssname entset ctr))
		(setq txt(cdr (assoc 1 (entget ent))))
		(setq insPO(exins ent))
		(setq insX(car insPO))
		(setq insY(cadr insPO))
		(if (= (length insPO) 3)
			(setq insZ(caddr insPO))
			(setq insZ 0)
		)
		(write-line (strcat (rtos insX) "," (rtos insY) "," (rtos insZ) "," txt) fileO)
		(setq ctr(1+ ctr))
	)
	(princ (strcat "\nData exported to '" fileName "'. Open in Excel or Notepad."))
	(close fileO)
	(princ)
)

;Function to extract the insertion point of a text.
(defun exins(tent)
	(setq tdetails(entget tent))
	(setq check(assoc 11 tdetails))
	(if (= check nil)
		(setq check(cdr (assoc 10 tdetails)))
		(progn
			(setq check(cdr check))
			(if (and (= (car check) 0) (= (cadr check) 0))
				(setq check(cdr (assoc 10 tdetails)))
				(setq check(cdr (assoc 11 tdetails)))
			)
		)
	)
	(setq check(list (car check) (cadr check)))
	(setq return check)
)

(princ "\nType \"EXyz\" to Export X,Y,Z and Content to File.") (princ)
