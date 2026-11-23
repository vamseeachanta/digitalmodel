(defun c:ctim (/ ctim)
        (setq ctim (menucmd "M=$(edtime,$(getvar,date),H:MMam/pm)"))
        (princ (strcat "\nThe current time is " ctim))
        (princ))
