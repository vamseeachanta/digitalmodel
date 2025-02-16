

                                 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                 ;; Title   : Easting & Northing           ;;
                                 ;; Purpose : To get Coordinate            ;;
                                 ;; Written : Bijoy.v.m                    ;;
                                 ;; Date    : Nov 2010                     ;;
                                 ;; System requirement : Autocad 2007      ;;
                                 ;; Command : DAT, EN & UW                 ;;
                                 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                         
;;--------------------------------------sub funaction for error-----------------------------------------------------
    
(defun trap1 (errmsg)

           (setq *error* temperr)
           (setvar "clayer" clay)
           (prompt "\n © Bijoy manoharan 2010 www.cadlispandtips.com")
(princ)
) ;defun

;;-------------------------------------------Set Datum-----------------------------------------------------

(defun C:dat (/ num op sta pga stb pgb)
       
        (command "cmdecho"0)
        (command "ucs" "w") 
       
    ;;; input station
        
        (if (not nf-ns) (setq nf-ns 0.000))    ; default number
        (setq NUM (getreal (strcat "\nEnter Eastward datum <" (rtos nf-ns 2 3) ">: ")))  
        (if (not num) (setq num nf-ns) (setq nf-ns num))

   ;;; input pgl
        (if (not sf-ss) (setq sf-ss 0.000))    ; default number
        (setq SUM (getreal (strcat "\nEnter Northward datum <" (rtos sf-ss 2 3) ">: "))) 
        (if (not sum) (setq sum sf-ss) (setq sf-ss sum))
    
   ;;; set orign point
        (setq op (getpoint "\nPick datum orgin point: "))
   
        (setq sta (car op))
        (setq pga (cadr op))
    
        (setq stb (- sta num))
        
        (setq pgb (- pga sum)) 
    
        (command "ucs" "m" (list stb pgb 0))
        (prompt "\nOrigin moved to new loaction - Enter Command EN to place Text")
        
        
  (princ)
) ;defun     
   
;;-------------------------------------------Place Text----------------------------------------------------


(defun C:EN (/ enp1 ex ey dy ptl e TextObj vlText)

         (command "cmdecho"0)
         (setq clay (getvar "clayer"))
         (setq temperr *error*)
         (setq *error* trap1)
         
               
         (if (not (tblsearch "layer" "Text Coordinate")) (command "-LAYER" "N" "Text Coordinate" "C" "7" "Text Coordinate" "LT" "Continuous" "Text Coordinate""LW" "0.15" "Text Coordinate" ""))
         (command "CLAYER" "Text Coordinate")
       
       
 (setq ptlist nil) ; for while command
  (while     
     (progn         
          (setq enp1 (getpoint "\nPick Coordinate point: "))
          (setq ex (car enp1))  ;x coord
          (setq ey (cadr enp1)) ;y coord
          (setq enx (rtos ex 2 3))
          (setq eny (rtos ey 2 3))      

        
          (setq ptl (getpoint "\nPick text location: "))
        
        (command "leader" enp1 ptl "" (strcat "E " enx) (strcat "N " eny) "")
        (setq TextObj (entlast))
           
   (vl-load-com)

        (setq vlText (vlax-ename->vla-object TextObj))     
        (vlax-put-property vlText 'backgroundfill :vlax-true)  ; background mask
        
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

(princ "\nEasting & Northing Lisp | © Bijoy manoharan 2010 | www.cadlispandtips.com |")
(princ "\nLisp Commands:DAT(to set Datum point),UW(Ucs World),EN(to Coordinate text)")
(princ)

;;----------------------------------------------End-----------------------------------------------------

