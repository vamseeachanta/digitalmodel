(defun C:ZEE (/ PNT1  P1z  PTXT )
    (setq PNT1 (getpoint "\nPick coordinate point: "))
    (setq P1z (caddr pnt1)) ;z coord
    (setq STDz (rtos P1z ))
    ;(setq COORD (strcat "\nEL=" STDZ)
    (setq PTXT (getpoint "\nPick text location: "))
    (command "leader" PNT1 PTXT ""(strcat "EL=" STDz) "")
    (princ)) ; end			 
