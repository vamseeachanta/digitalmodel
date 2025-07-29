(defun C:CR (/ PNT1 P1X P1Y P1z STDY DY PTXT )
    (setq PNT1 (getpoint "\nPick coordinate point: "))
    (setq P1X (car pnt1)) ;x coord
    (setq P1Y (cadr pnt1)) ;y coord
    (setq P1z (cadr pnt1)) ;z coord
    (setq STDX (rtos P1X 2 3))
    (setq STDY (rtos P1Y 2 3))
    (setq STDz (rtos P1z 2 3))
    ;(setq COORD (strcat "E=" STDX "\nN=" STDY "\nEL=" STDZ)))
    (setq PTXT (getpoint "\nPick text location: "))
    (command "leader" PNT1 PTXT "" (strcat "E=" STDX)
      (strcat "N=" STDY) "") (strcat "EL=" STDz) ""))
    (princ)) ; end			 
