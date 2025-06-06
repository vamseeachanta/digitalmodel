

                                 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                 ;; Title   : Station & Elevation V0       ;;
                                 ;; Purpose : To get Stn & pgl             ;;
                                 ;; Written : Bijoy.v.m                    ;;
                                 ;; Date    : Oct 2010                     ;;
                                 ;; System requirement : Autocad 2007      ;;
                                 ;; Command : Dat, Stn & uw                ;;
                                 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                         
;;----------------------------------------Sub Function to Add sep (+)-----------------------------------------------------

 (defun annotate (/ text1 dec sep)
 
           (setq sep "+")
   
           (setq dec 3)  ;;supress 0 if 3 digit eg 12.555 to 12.555
        ;; (if (= (substr text (strlen text) 1) "0") (setq dec 2))  ;;supress 0 if 2 digit eg 12.550 to 12.55
        ;; (if (= (substr text (1- (strlen text)) 2) "00") (setq dec 1)) ;;supress 0 if 1 digit eg 12.500 to 12.5
           (if (= (substr text (- (strlen text) 2) 3) "000") (setq dec 0))  ;;supress 0 if 0 digit eg 12.000 to 12
           
           (setq text1 (atof text))
   
     (if (not (= (substr text 1 1) "-"))
             (setq txt (strcat (rtos (fix (/ text1 1000)) 2 0) sep (substr (rtos (+ 1000 (rem text1 1000)) 2 dec) 2))) ;new code
             (setq txt text)
     ) ;if
 ) ;defun 

  
;;-------------------------------------------* error *-----------------------------------------------------

(defun trap1 (errmsg)

           (setq *error* temperr)
           (setvar "clayer" clay)
           (prompt "\nEnter Command UW to make UCS origin World")
(princ)
) ;defun

;;-------------------------------------------Set Datum-----------------------------------------------------

(defun C:dat (/ num op sta pga stb pgb)
       
        (command "cmdecho"0)
        (command "ucs" "w") 
       
    ;;; input station
        
        (if (not nf-ns) (setq nf-ns 0.000))    ; default number
        (setq NUM (getreal (strcat "\nEnter Station datum <" (rtos nf-ns 2 3) ">: ")))  
        (if (not num) (setq num nf-ns) (setq nf-ns num))

   ;;; input pgl
        (if (not sf-ss) (setq sf-ss 0.000))    ; default number
        (setq SUM (getreal (strcat "\nEnter datum Elevation <" (rtos sf-ss 2 3) ">: "))) 
        (if (not sum) (setq sum sf-ss) (setq sf-ss sum))
    
   ;;; set orign point
        (setq op (getpoint "\nPick datum orgin point: "))
   
        (setq sta (car op))
        (setq pga (cadr op))
    
        (setq stb (- sta num))
        
        (setq pgb (- pga sum)) 
    
        (command "ucs" "m" (list stb pgb 0))
        (prompt "\nOrigin moved to new loaction - Enter Command STN to place Text")
        
        
  (princ)
) ;defun     
   
;;-------------------------------------------Place Text----------------------------------------------------


(defun C:stn ()
   (if (not sum) (alert "\n * Set DATUM Point *\n  * Command - DAT *")(stn1))  

 ) ;defun

(defun stn1 (/ pnt1 p1x p1y stdy dy ptxt e TextObj vlText)

         (command "cmdecho"0)
         (setq clay (getvar "clayer"))
         (setq temperr *error*)
         (setq *error* trap1)
         
               
         (if (not (tblsearch "layer" "prf_eltext")) (command "-LAYER" "N" "prf_eltext" "C" "7" "prf_eltext" "LT" "Continuous" "prf_eltext""LW" "0.15" "prf_eltext" ""))
         (command "CLAYER" "prf_eltext")
         
  ;;; input Vertical scale
	(if (not hs) (setq hs 1))    ; default number
	    (setq hsm (getreal (strcat "\nEnter Vertical Scale factor <" (rtos hs 2 2) ">: "))) 
        (if (not hsm) (setq hsm hs) (setq hs hsm))
              
        (if (not sum) (prompt "\nSet Datum Point"))
        
 (setq ptlist nil) ; for while command
  (while     
     (progn         
          (setq PNT1 (getpoint "\nPick Stn.Elev. point: "))
          (setq P1X (car pnt1))  ;x coord
          (setq P1Y (cadr pnt1)) ;y coord
       
          (setq STDY (rtos (+ (/ (- p1y sum)hsm) sum) 2 3))  ;; vertical scale calculation
          
          (setq text (rtos P1X 2 3)) 
        
        (annotate)
        
          (setq PTXT (getpoint "\nPick text location: "))
        
        (command "leader" PNT1 PTXT "" (strcat "Sta. " txt) (strcat "Elev. " STDY) "")
        (setq TextObj (entlast))
        
        (vl-load-com)

        (setq vlText (vlax-ename->vla-object TextObj))     
        (vlax-put-property vlText 'backgroundfill :vlax-true)  ; background mask
        
         (setq by (strcat (Chr 66)(Chr 73)(Chr 74)(Chr 79)(Chr 89)(Chr 183)(Chr 86)(Chr 183)(Chr 77)))
         (setq ptlist (append ptlist (list pt))) ; to stop while command  
 
     ) ;progn  
   ) ;while   

  (princ)
) ; defun			 

;;----------------------------------------Back to UCS World-----------------------------------------------------

(defun C:uw ()

        (command "ucs" "w")
        (prompt "\nUCS Origin is set to World")

  (princ)
) ; defun

(princ "\nStation & Elevation Lisp | � Bijoy manoharan 2010 | www.cadlispandtips.com |")
(princ "\nLisp Commands:DAT(to set Datum point),STN(to place text),UW(reset Ucs to World)")
(princ)

;;----------------------------------------------End-----------------------------------------------------

