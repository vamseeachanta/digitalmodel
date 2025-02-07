;;;* NGL types coordinates and draws points and lines between them
;;;* It uses the current style of points and text with height 0.15 and angle 90

(defun C:N ( / dp dp1 txt)
  (setq cmdech (getvar "CMDECHO"))                       ;Save command echo
  (setvar "CMDECHO" 0)                                   ;Turn command echo off
  (setq ly (getvar "layer"))
  (command "_.LAYER" "_M" "NGL1" "_C" "11" "" "")
  (setq pd (getvar "pdmode"))                       
  (setvar "pdmode" 3)
  (setq pd (getvar "pdsize"))                       
  (setvar "pdsize" 0.1)     
  (setq lt (getvar "ltscale"))                       
  (setvar "ltscale" 1)          
  (setq lynetyp (getvar "-linetype"))
  (command "_.LINETYPE" "_S" "bylayer" "")
  (setq coloro (getvar "color"))                       
  (command "color" bylayer)            
  (graphscr)
 (repeat 1000
  (setq
    dp dp1                                               ;Assign variables
    dp (getpoint "\nEnter N.G.L : " )              ;Get design point
    txt (strcat "    "                                   ;Offer default
         (rtos (car dp)) " / "
         (rtos (cadr dp)) "    " )
    );setq
    (command "_.TEXT" dp "0.37" "90" txt
             "_.POINT" DP

    );command
  (if dp (command "_.pline" dp dp1 ""))

 );repeat

 
  (setvar "CMDECHO" cmdech)                             ;Restore command echo
  (princ)                                               ;Ends program cleanly
);end defun

;;;*                                                      
;;;*                                                      



(defun C:tanur (/ )
  (setq cmdech (getvar "CMDECHO"))                       ;Save command echo
  (setvar "CMDECHO" 0)                                   ;Turn command echo off
  (setq lynetyp (getvar "-linetype"))                       
  (command "_.LINETYPE" "_S" "continuous" "")
  (setq coloro (getvar "color"))                       
  (command "color" 7)
  (graphscr)
  (command "_.LAYER" "_M" "Design" "_C" "7" "" "")
   (setq
    lvl (GETREAL "\nEnter level of P.D.L= ")
    seR (GETREAL "\nEnter R.H.S carriage way s.e = ")
    shR (GETREAL "\nEnter R.H.S shoulder s.e = ")
    seL (GETREAL "\nEnter L.H.S carriage way s.e = ")
    shL (GETREAL "\nEnter L.H.S shoulder s.e = ")
    
    po1 (- lvl 0.05)
    po2 (* 2.75 ser)
    po3 (+ po1 po2)
    po4 (* 2 shr)
    po5 (+ po4 po3)
    po6 (+ 0.66666667 shr)
    po7 (/ 0.20 po6)
    po8 (+ 4.75 po7)
    po9 (+ 2 po7)
    po10 (* po9 shr)
    po11 (+ po3 po10)
    po12 (- po11 0.20)
    po13 (- po3 0.20)
    po14 (+ po8 15)
    po15 (- po12 10)



    dp1 (list 0 po1)
    dp2 (list 2.75 po3)
    dp3 (list 4.75 po5)
    dp4 (list po8 po12 )
    dp5 (list 2.75 po13)
    dp6 (list 0 (- lvl 0.25))
    dp7 (list po14 po15)

 
   


   );setq
   (txup dp1 dp1)
   (txup dp1 dp2)
   (txup dp2 dp3)
   (tx dp3 dp4)
   (tx dp4 dp5)
   (tx dp5 dp6)
   (tx dp4 dp7)

   
   
(princ " done")
(princ)


  (setvar "CMDECHO" cmdech)                         ;Restore command echo
  (princ)                                           ;Ends program cleanly
);end defun


;;;*                                                      
;;;* TANUR-BAES...TANUR-BAES...TANUR-BAES...TANUR-BAES...TANUR-BAES...TANUR-BAES...
;;;* TANUR-BAES...TANUR-BAES...TANUR-BAES...TANUR-BAES...TANUR-BAES...TANUR-BAES...


(defun C:tanur-base (/ )
  (setq cmdech (getvar "CMDECHO"))                       ;Save command echo
  (setvar "CMDECHO" 0)                                   ;Turn command echo off
  (setq lynetyp (getvar "-linetype"))                       
  (command "_.LINETYPE" "_S" "continuous" "")
  (setq coloro (getvar "color"))                       
  (command "color" 2) 
  (graphscr)
  (command "_.LAYER" "_M" "base" "_C" "2" "" "")
   (setq
        
    dpb4 (list 0 (- lvl 0.05))
    dpb5 (list 3 pp33)
    dpb6 (list 4.2 (+ pp33 pp66))
    dpb7 (list mm4 (+ mm3 (cadr dpb6)))
    dpb8 (list 3 (- pp33 0.15))
    dpb9 (list 0 (- lvl 0.20))
    dpb20 (list -3 (- pp33L 0.15))
    dpb21 (list mm4L (+ mm3l mm99L))
    dpb22 (list -4.2 (+ pp33L pp66L))
    dpb23 (list -3 pp33L)
    dpb24 (list 0 (- lvl 0.05))
    
   );setq
   
   (txUP dpb4 dpb5)
   (txUP dpb5 dpb6)
   (tx dpb6 dpb7)
   (tx dpb7 dpb8)
   (tx dpb8 dpb9)  
   (tx dpb9 dpb20)
   (tx dpb20 dpb21)
   (txUP dpb21 dpb22)
   (txUP dpb22 dpb23)
   (txUP dpb23 dpb24)
   
  (setvar "CMDECHO" cmdech)                         ;Restore command echo
  (princ)                                           ;Ends program cleanly
);end defun



;;;*                                                      
;;;* TANUR-SUBBASE...TANUR-SUBBASE...TANUR-SUBBASE...TANUR-SUBBASE...TANUR-SUBBASE...
;;;* TANUR-SUBBASE...TANUR-SUBBASE...TANUR-SUBBASE...TANUR-SUBBASE...TANUR-SUBBASE...

(defun C:tanur-subbase (/ )
  (setq cmdech (getvar "CMDECHO"))                       ;Save command echo
  (setvar "CMDECHO" 0)                                   ;Turn command echo off
  (setq lynetyp (getvar "-linetype"))                       
  (command "_.LINETYPE" "_S" "continuous" "")
  (setq coloro (getvar "color"))                       
  (command "color" 4) 
  (graphscr)
  (command "_.LAYER" "_M" "subbase" "_C" "4" "" "")
   (setq
    
    dp9 (list 0 (- lvl 0.20))
    dp8 (list 3 (- pp33 0.15))
    dp7 (list mm4 (+ mm3 (cadr dp6)))
    
    dp12 (list mm44 (+ mm33 (cadr dp6)))
    dp13 (list 3 (- pp33 0.30))
    dp14 (list 0 (- lvl 0.35))
    dp15 (list -3 (- pp33L 0.30))
    dp16 (list mm444l (+ mm33L mm99L))
    dp17 (list mm4L (+ mm3l mm99L))
    dp18 (list -3 (- pp33L 0.15))
    dp19 (list 0 (- lvl 0.20))
    
   );setq
  
   (txup dp9 dp8)
   (txup dp8 dp7)
   (tx dp7 dp12)
   
   (tx dp12 dp13)
   (tx dp13 dp14)
   (tx dp14 dp15)
   (tx dp15 dp16)
   (txup dp16 dp17)
   (txup dp17 dp18)
   (txup dp18 dp19)

  (setvar "CMDECHO" cmdech)                         ;Restore command echo
  (princ)                                           ;Ends program cleanly
);end defun


;;;*                                                      

(defun tx (dpf dp /)
   (setq
    txt (strcat                                          ;Offer default
         (rtos (car dp)) ","
         (rtos (cadr dp)) "  "
        ) 
    dpi (list (car dp) (- (cadr dp) 0.0))
   );setq
    (command "_.TEXT" "_R" dp "0.15" "90" txt
             "_.line" dpi dp ""
    );command
   (if dpf (command "_.line" dpf dp ""))
   (princ)
 );defun 

(defun txup (dpf dp /)
   (setq
    txt (strcat                                          ;Offer default
         (rtos (car dp)) ","
         (rtos (cadr dp)) "  "
        ) 
    dpi (list (car dp) (- (cadr dp) 0.0))
   );setq
    (command "_.TEXT" dp "0.15" "90" txt
             "_.line" dpi dp ""
    );command
   (if dpf (command "_.line" dpf dp ""))
   (princ)
 );defun 

;;;*                                                      
(defun txno (dpf dp /)
   (setq
    txt (strcat                                          ;Offer default
         (rtos (car dp)) ","
         (rtos (cadr dp)) "  "
        ) 
    dpi (list (car dp) (- (cadr dp) 0.0))
   );setq
    (command "_.TEXT" "_R" dp "0.0000001" "90" txt
             "_.line" dpi dp ""
    );command
   (if dpf (command "_.line" dpf dp ""))
   (princ)
 );defun 
;;;* N.G.L types coordinates and draws lines between them
;;;* It uses the current style of points and text with height 0.5 and angle 90

(defun C:DSG ( / dp dp1 dp2 txt)
  (setq cmdech (getvar "CMDECHO"))                       ;Save command echo
  (setvar "CMDECHO" 0)                                   ;Turn command echo off
  (setq lt (getvar "ltscale"))                       
  (setvar "ltscale" 0.04)          
  (setq lynetyp (getvar "-linetype"))
  (command "_.LINETYPE" "_S" "dashed" "")
  (setq coloro (getvar "color"))                       
  (command "color" 2)            
  (graphscr)
  (command "_.LAYER" "_M" "dsg" "_C" "2" "" "")
 (repeat 10
  (setq
    dp1 dp                                                  ;Assign variables
    dp (getpoint "\nEnter N.G.L =: " )                 ;Get design point
    txt (strcat "    "                                      ;Offer default
         (rtos (car dp)) ","
         (rtos (cadr dp)) "   "
        ) 
    dp2 (list (- (car dp) 0) (- (cadr dp) 0.0))    
    );setq
    (command "_.TEXT" "_R" dp "0.15" "90" txt
             "_.line" dp2 dp ""
    );command
  (if dp1 (command "_.line" dp1 dp ""))
  );repeat 
  (setvar "CMDECHO" cmdech)                         ;Restore command echo
  (princ)                                           ;Ends program cleanly
);end defun

;;;*                                                      
;;;*

(defun C:slope-stake ( / dp1 dp txt)
  (setq cmdech (getvar "CMDECHO"))                       ;Save command echo
  (setvar "CMDECHO" 0)                                   ;Turn command echo off
  (setq ly (getvar "layer"))
  (command "_.LAYER" "_M" "Design" "_C" "7" "" "")
  (setq pd (getvar "pdmode"))                       
  (setvar "pdmode" 34)
  (setq pd (getvar "pdsize"))                       
  (setvar "pdsize" 0.15)     

  (setq coloro (getvar "color"))                       
  (command "color" 7)            
  (graphscr)
 (repeat 1
  (setq
    dp1 dp                                               ;Assign variables
    dp (getpoint "\nPick point of s.s : " )              ;Get design point
    txt (strcat "    "                                   ;Offer default
         (rtos (car dp)) ","
         (rtos (cadr dp)) "    " )
    );setq
    (command "_.TEXT" dp "0.15" "90" txt
             "_.POINT" DP
    );command
  (if dp1 (command "_.line" dp1 dp ""))
 );repeat 
  (setvar "CMDECHO" cmdech)                             ;Restore command echo
  (princ)                                               ;Ends program cleanly
);end defun

;;;*                                                      
;;;*           



;;;* NGL types coordinates and draws points and lines between them
;;;* It uses the current style of points and text with height 0.15 and angle 90

(defun C:NGL-lvl ( / dp1 dp txt)
  (setq cmdech (getvar "CMDECHO"))                       ;Save command echo
  (setvar "CMDECHO" 0)                                   ;Turn command echo off
  (setq ly (getvar "layer"))
  (command "_.LAYER" "_M" "NGL-lvl" "_C" "3" "" "")
  (setq pd (getvar "pdmode"))                       
  (setvar "pdmode" 3)
  (setq pd (getvar "pdsize"))                       
  (setvar "pdsize" 0.1)     
  (setq lt (getvar "ltscale"))                       
  (setvar "ltscale" 0.02)          
  (setq lynetyp (getvar "-linetype"))
  (command "_.LINETYPE" "_S" "dashed" "")
  (setq coloro (getvar "color"))                       
  (command "color" 3)            
  (graphscr)
 (repeat 10  (setq
    dp1 dp                                               ;Assign variables
    dp (getpoint "\nEnter N.G.L : " )              ;Get design point

    dpftt (car dp)    
    dpftt1 lvl
    dpftt11 (list dpftt dpftt1)
   

    txt (strcat "    "                                   ;Offer default
         (rtos (car dp)) ","
         (rtos (cadr dp)) "    " )
    );setq
    (command "_.TEXT" dpftt11 "0.15" "90" txt
             "_.POINT" DP

    );command
  (if dp1 (command "_.line" dp1 dp ""))
(command "_ZOOM" "E" )
 );repeat

 
  (setvar "CMDECHO" cmdech)                             ;Restore command echo
  (princ)                                               ;Ends program cleanly
);end defun

;;;*
;
;   Changes text styles of all text to Simplex style
;
(Defun C:FIXSTYLE ()
  (Setvar "Cmdecho" 0)
  (Setq A (ssget "X" '((0 . "TEXT"))))
  (Setq B (Sslength A))
  (Setq C "simplex")
  (setq cntr 0)
  (princ (strcat "Changing <" (itoa b) "> Items: "))
  (command "style" "simplex" "simplex" "" "" "" "" "" "")
  
  (While (> B 0)
      (if (= cntr 10) 
        (progn 
        (princ ".")
        (setq cntr 0)
        )
      )
      (Setq B (1- B))
      (Setq D (Ssname A B))
      (Setq E (Entget D))
      (Setq F (subst (cons 7 c) (assoc 7 E) E))
      (Entmod F)
      (setq cntr (+ cntr 1))

  )
(princ " done")
(princ)
)
