(defun c:number (/ ans ent1 nmax ent num ent2 nmin ent3 num2 ss ssl index tents tot ent4 num3 ave)
(princ "\nInitializing...")
(initget 1 "Max Min Average")
(setq ans (getkword "\nSpecify Numerical Requirement [MAx/MIn/Ave]: "))
(cond
((= ans "Max")
(if
(setq ent1 (car (entsel "\nSelect Numerical Text: ")))
(progn
(setq nmax (atof (cdr (assoc 1 (entget ent1)))))
(while
(/= (setq ent (car (entsel "\nSelect Numerical Text: "))) nil)
(if (= (cdr (assoc 0 (entget ent))) "TEXT")
(progn
(setq num (atof (cdr (assoc 1 (entget ent)))))
(if (> num nmax)
(setq nmax num)
) ;_ end if
) ;_ end progn
(alert "Selected Entity must be Text.")
) ;_ end if
) ;_ end while
(alert (strcat "Maximum Number is: " (rtos nmax)))
) ;_ end progn
(alert "Text Required.")
) ;_ end if
)
((= ans "Min")
(if
(setq ent2 (car (entsel "\nSelect Numerical Text: ")))
(progn
(setq nmin (atof (cdr (assoc 1 (entget ent2)))))
(while
(/= (setq ent3 (car (entsel "\nSelect Numerical Text: "))) nil)
(if (= (cdr (assoc 0 (entget ent3))) "TEXT")
(progn
(setq num2 (atof (cdr (assoc 1 (entget ent3)))))
(if (< num2 nmin)
(setq nmin num2)
) ;_ end if
) ;_ end progn
(alert "Selected Entity must be Text.")
) ;_ end if
) ;_ end while
(alert (strcat "Minimum Number is: " (rtos nmin)))
) ;_ end progn
(alert "Text Required.")
) ;_ end if
)
((= ans "Average")
(setq ss (ssget)
ssl (sslength ss)
index 0
tents 0
tot 0
) ;_ end setq
(repeat ssl
(setq ent4 (entget (ssname ss index)))
(if (= (cdr (assoc 0 ent4)) "TEXT")
(progn
(setq num3 (atof (cdr (assoc 1 ent4))))
(setq tot (+ num3 tot))
(setq tents (1+ tents))
) ;_ end progn
) ;_ end if
(setq index (1+ index))
) ;_ end repeat
(if (/= tents 0)
(progn
(setq ave (/ tot tents))
(alert (strcat "Average of " (rtos tents) " Numbers is: " (rtos ave)))
) ;_ end progn
(alert "No Text Entities Selected.")
) ;_ end if
)
) ;_ end cond
(princ)
) ;_ end defun