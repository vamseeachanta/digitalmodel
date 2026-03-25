;;This program is written by Ummer Syed Mohamed, Thrissur, India.
;;Modify Murali 28-06-2010
;;--------------------------------------------------------------


(DEFUN C:SLP(/ PT1 PT2 DY DX SLOPE TEXT)

(while

(SETQ PT1(GETPOINT "\nSelect first point: "))
(SETQ PT2(GETPOINT PT1 "\nSelect other point: "))
(SETQ DY (abs (float (- (CADR PT1) (CADR PT2)))))
(SETQ DX (ABS (float (- (CAR PT1) (CAR PT2)))))
(SETQ SLOPE (RTOS (* (/ (/ DY 1.0) DX) 100.) 2 3))
;;(setq Slope_per(STRCAT "Slope =" SLOPE"%"))

(setq text(STRCAT SLOPE"%"))
;(princ)
;(ALERT text)
;(princ)


(setq Txt_Pnt (getpoint "\nPick Text Point:"))
         (setq y_val (cadr Txt_Pnt))
         (setq x_val (car Txt_Pnt)) 
         (setq txt_pnt (list x_val y_val))

	 ;(setq dpth (- (atof(cdr G_val)) (atof(cdr I_val))))
	 ;(setq dpth (float dpth))
         ;(setq val1 (rtos dpth 2 3))
          (command "text" txt_pnt "" 0 text)

)


)
