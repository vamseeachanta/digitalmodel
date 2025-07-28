(defun c:sg ()

(setvar "cmdecho" 0)

(prompt "\nIMRAN AutoLISP Products.... contact at ihalims@hotmail.com\n")

(setq of(open (getfiled "" "" "" 2) "r"))

(setq r1l(read-line of))
(setq loop(atoi r1l))
(setq dummy loop)

(setq nf(open (strcat (rtos loop)".scr") "w"))
(write-line "Multiple point" nf)

(progn
(while (/= r1l nil)
(setq nrl(substr r1l (+ 2(strlen (rtos (atof r1l)))) (strlen r1l)))
;(setq nrl (strcat (rtos (atof nrl))"," (substr nrl (+ 2(strlen (rtos(atof nrl)))) (strlen nrl))))
(if (= loop dummy)
(write-line nrl nf)
(pron)
);if
(setq r1l(read-line of))
(setq loop(atoi r1l))

);progn

)(exit)
(princ)
);dfun

(defun pron()
(close nf)
(setq nf(open (strcat (rtos loop)".scr") "w"))
(write-line "Multiple point" nf)
(setq nrl(substr r1l (+ 2(strlen (rtos (atof r1l)))) (strlen r1l)))
;(setq nrl (strcat (rtos (atof nrl))"," (substr nrl (+ 2(strlen (rtos(atof nrl)))) (strlen nrl))))
(write-line nrl nf)(setq dummy loop))
