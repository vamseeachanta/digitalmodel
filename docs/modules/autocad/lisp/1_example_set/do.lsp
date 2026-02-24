;***************************
;dom lisp prog. for xyz point 
;****************************
; By Dominic TA  , Dutco Balfour Beatty Dubai     10th Oct 2005
; DOM DOM DOM DOM _____

(defun read1 ()
(setq z (read-line 1st))
(IF Z
(PROGN
(SETQ MAXS (STRLEN Z)
COUN 1 
CHRCT 1
)
(WHILE (< COUN MAXS)
(IF (/= " " (SUBSTR Z COUN 1))
(SETQ CHRCT (1+ CHRCT))
(PROGN
(if (= 1 chrct);
(prompt ".");
(progn
(SETQ NUMB (ATOF (SUBSTR Z (1+ (- COUN chrct))))) ;CHRCT)))
(SETQ DDLIST (APPEND DDLIST (LIST NUMB)))
(SETQ CHRCT 1)
));
))
(SETQ COUN (1+ COUN))
)
(SETQ NUMB (ATOF (SUBSTR Z (1+ (- COUN CHRCT)))))
(cond ((= 0 numb)
(setq numb (substr z (1+ (- coun chrct))))
(setq Dlist (append Dlist (list numb)))
(prompt "1"))
);
(setq DDLIST (APPEND DDLIST (LIST NUMB)))
)
)
)
(defun c:3DP ()
(SETVAR "CMDECHO" 0)
(setq ddlist nil)
(setq j (getstring "\n input file to read:"))
(setq 1st (open j "r"))
(READ1)
(while (/= z NIL)
(setq k (cadr DDLIST))
(setq m (car DDLIST))
(setq n (caddr DDLIST))
(setq fu (last ddlist))
(setq y (list m k n))
;(SETQ Y (LIST M K))
(command "point" y)
     (setq n (RTOS n 2 3))
    ;(setq COORD (strcat "E=" DOMX "\nN=" "\NZ=" DOMY))
    ;(setq COORD (strcat "E=" DOMX "\nN=" "\NZ=" DOMY))
    ;(setq PTXT (LIST DOMZ)) 
    ;(princ DOMz)
    (command "Text" y "2" "0" n)
    
    
         			 
(SETQ DDLIST NIL)
(read1)
)
(close 1st)
)
