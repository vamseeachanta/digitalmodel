(defun c:dp()

(setvar "osmode" 191)
 (while
       
	(setq GL (entsel "\nGround Value :"))
       
         (setq GL1 (entget (car GL)))
        (if (> (length GL1) 22) 
                (setq G_val (nth 12 GL1))
       
         (setq G_Val (nth 11 GL1))
        )

       (setq IL (entsel "\nInvert Value :"))
         
         (setq IL1 (entget (car IL)))
       (if (> (length IL1) 22) 
          (setq I_val (nth 12 IL1))
       
         (setq I_Val (nth 11 IL1))
        )
     
	 (setq Txt_Pnt (getpoint "\nPick Text Point:"))
         (setq y_val (cadr Txt_Pnt))
         (setq x_val (car Txt_Pnt)) 
         (setq txt_pnt (list x_val y_val))

	 (setq dpth (- (atof(cdr G_val)) (atof(cdr I_val))))
	 (setq dpth (float dpth))
         (setq val1 (rtos dpth 2 3))
          (command "text" txt_pnt "" 90 val1)  
    )
(setvar "osmode" 191)
)


;==============================================

;Addding a prefix for Chainage
;Murali- 28-02-07

(defun c:pc()

(setq txt (entsel "\nEnter Prefix Text :"))

(while
	(setq cv (entsel "\nSelect Text to Add Prefix Value :"))
	(setq cv1 (entget (car cv)))
        (if (> (length cv1) 22) 
                (setq c_val (nth 12 cv1))
       
         (setq c_val (nth 11 GL1))
	)
	 	
	 (setq new_chn (strcat cv1 c_val))
         (setq new_chn1 (rtos new_chn 2 2))  
         (command "change" cv1 "" "" "" "" "" "" new_chn1 "" )  

)
)

;==============================================

(defun c:SL()

(setq SL (getreal "\nEnter Slop % :"))
   (setq SL (/ sl 100))
(while
(setq Lv (entsel "\nReference Level:"))
   (setq lv1 (entget (car Lv)))
   (setq Lv2 (cdr(nth 13 lv1)))
      
(setq p1 (getpoint "\n Slop From :"))
(setq p2 (getpoint "\n Slop to :"))
(setq d (distance p1 p2))
   (setq slop (+ (atof Lv2) (* SL d)))
(setq slop (rtos slop 2 3))
(command "text" p2 "" "" slop)
)
)

;==============================================

(defun c:SL1()

(setq SL (getreal "\nEnter Slop % :"))
   (setq SL (/ sl 100))

(setq IL (getreal "\nRefernce IL:"))
   
(setq p1 (getpoint "\n Slop From :"))
(setq p2 (getpoint "\n Slop to :"))
(setq d (distance p1 p2))
   (setq slop (-  IL (* SL d)))
(setq slop (rtos slop 2 3))
(setq text(STRCAT "Slope =" SLOPE "%"))
(princ)
(ALERT text)
(princ)
)





