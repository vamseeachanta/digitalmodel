(defun C:CC (/ PNT1 P1X P1Y STDY DY PTXT )
    (setq PNT1 (getpoint "\nPick coordinate point: "))
    (setq P1X (car pnt1)) ;x coord
    (setq P1Y (cadr pnt1)) ;y coord
    (setq STDX (rtos P1X 2 3))
    (setq STDY (rtos P1Y 2 3))
    ;(setq COORD (strcat  STDX "\n" STDY))
    (setq PTXT (getpoint "\nPick text location: "))
    (command "leader" PNT1 PTXT "" (strcat  STDX)
      (strcat  STDY) "")
    (princ)) ; end			 
