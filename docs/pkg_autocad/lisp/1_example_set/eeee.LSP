\\To Change Precision of number
\\ ie if value is 12 or 13 or 7...it will cange to 12.00, 13.00, 7.00 etc
(defun c:chpr()
  (setq ss(ssget))
  (setq len(sslength ss))
  (setq co 0)
  (repeat len
    (setq ent(ssname ss co))
    (setq lis(entget ent))
    (setq txt(atof(cdr(assoc 1 lis))))
    (setq ntxt(cons 1 (rtos txt 3 3)))
    (setq co (+ co 1))
    (setq otxt(assoc 1 lis))
    (setq lis(subst ntxt otxt lis))
    (entmod lis)
    (entupd ent)
    )
  )
   
    