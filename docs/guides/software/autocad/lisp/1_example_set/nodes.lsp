(defun c:nodes()
(setvar "cmdecho" 0)
(setq of(open (getfiled "" "" "xls" 1) "w"))
(setq count 0)
(setq ss(ssget))
(setq sl(sslength ss))
(write-line "line" of)
(repeat sl

(setq ent(cdr( assoc 10 (entget (ssname ss count)))))

(setq wtf(strcat (rtos (car ent)) 
","
(rtos (cadr ent))
","
(rtos(caddr ent))))

(write-line wtf of)
(setq count (1+ count))
)
(close of)

(setq prom(strcat  (itoa sl)" coordinates" " were prosessed "))
(prompt "\n" ) 

(prompt prom)
(princ)
)
(prompt "\nwritten by IMRAN and company")
(prompt "\nEnter Nodes to start")