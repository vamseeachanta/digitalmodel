
(defun C:CR (/ PNT1 P1X P1Y STDY DY COORD PTXT)
    (setq PNT1 (getpoint
      "\nPick coordinate point: "))
    (setq P1X (car pnt1)) ;x coord
    (setq P1Y (cadr pnt1)) ;y coord
    (setq STDX (rtos P1X 2 3))
    (setq STDY (rtos P1Y 2 3))
    (setq COORD (strcat "E"  STDX)
          COORD1(strcat "N" STDY))
    (setq PTXT (getpoint PNT1
      "\nPick text location: "))
    (command "LEADER" PNT1 PTXT ""
      COORD COORD1 "" "")
    (princ)) ; end
 
