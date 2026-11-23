

                             ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                             ;; Title    : Barricades                ;;   
                             ;; Purpose  : To Draw Barricades        ;;
                             ;; Written  : Bijoy manoharan           ;;       
                             ;; Web page : www.cadlispandtips.com    ;;
                             ;; Command  : WB                        ;;       
                             ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;---------------------------------------sub function error-------------------------------------------
 
 (defun trap1 (errmsg)
    (setvar "clayer" clay)
    (setvar "OSMODE" osm)
    (command "undo" "end")
    (setq *error* temperr)
    (prompt "\n© Bijoy manoharan 2012 www.cadlispandtips.com")
    (princ)
   ) ;defun

;;--------------------------------------Create New layer----------------------------------------------
    (defun LASE()

           (if (not (tblsearch "layer" "RD_Barricades"))
           (command "-LAYER" "N" "RD_Barricades" "C" "7" "RD_Barricades" "LT" "Continuous" "RD_Barricades""LW" "0.00" "RD_Barricades" "")):if   

    ) ;defun

;;---------------------Main Function for Water Barricades---------------------------------------------


(defun c:WB (/ osm clay e dee lsb pt2) 


         (command "cmdecho"0)
         (command "undo" "group")
         (setq clay (getvar "clayer"))
         (command "-SCALELISTEDIT" "R" "Y" "E")
         (command "-PURGE" "LA" "*" "N")
	 (setq osm (getvar "OSMODE"))
         (setvar "OSMODE" 0)
         (setq temperr *error*)
         (setq *error* trap1)



   (LASE)                    ;; go to create layer subfunction


  (if (= (tblsearch "ltype" "acad_iso03w100") nil)  
       (command "-linetype" "l" "acad_iso03w100" "acadiso.lin" "")
  )

 ; set Barricades width
         (if (not efb)(setq efb 0.60))                  
  	 	     (setq eeb (getreal (strcat "\nSpecify Barricades Width <" (rtos efb 2 2) ">:")))
	 (if (not eeb)(setq eeb efb)(setq efb eeb))	 
	             (setq dee (/ eeb 2))

(setq ptlist nil) ; for while command  

  (while 
     
      (progn 
        (setq e (car (entsel "\nSelect Road Edge Line to offset <exit>:")))
        (setq en (entget e))
         
         (setq pt2 (getpoint "\nSpecify point on side to offset:"))
         
    
      (if (and (not (= pt2 nil))(= (cdr (assoc 0 en)) "LWPOLYLINE")) 
	 
      (progn  

         (setq lsb 0.3) ;;; CHANGE LINETYPE SCALE HERE  ;;;

         (command "_change" e "" "_P" "_LA" "RD_Barricades" "")
	 (command "_change" e "" "_P" "_S" "1" "")
         (command "_change" e "" "_P" "_C" "BYLAYER" "")
         (command "_change" e "" "_P" "_LT" "BYLAYER" "")
         (command "_change" e "" "_P" "_LW" "BYLAYER" "")
	 (command "_offset" eeb e "_non" pt2 "")	
         (command "_change" (entlast) "" "_P" "_LA" "RD_Barricades" "")
        
         (command "_offset" dee e "_non" pt2 "")
 	 (command "_change" (entlast) "" "_P" "_LT" "acad_iso03w100" "")
         (command "_change" (entlast) "" "_P" "_S" lsb "")
         (command "_.pedit" (entlast) "_W" eeb "")
         (command "_.pedit" (entlast) "_L" "ON" "")
         (command "_change" (entlast) "" "_P" "_LA" "RD_Barricades" "")
         (command "_change" (entlast) "" "_P" "_C" "red" "")

         (command "_offset" dee e "_non" pt2 "")
         (command "_change" (entlast) "" "_P" "_LT" "Continuous" "")
         (command "_.pedit" (entlast) "_W" eeb "")
         (command "_change" (entlast) "" "_P" "_C" "255" "")

         (command "_DRAWORDER" e "" "_F")

         
   (setq ptlist (append ptlist (list pt2))) ; to stop while command

       ) ;progn

  (alert "\n    The Selected Object is Not a Polyline, Change Objects to Polyline and Try Again")
     ) ;if  
       ) ;progn  
  ) ;while


          (setvar "OSMODE" osm)
          (setvar "clayer" clay)
          (command "undo" "end")
 (princ)
) ;defun

;;---------------------Main Function for Concrete Barricades---------------------------


(defun c:CB (/ osm clay e dee pt2) 


         (command "cmdecho"0)
         (command "undo" "group")
         (setq clay (getvar "clayer"))
         (command "-SCALELISTEDIT" "R" "Y" "E")
         (command "-PURGE" "LA" "*" "N")
	 (setq osm (getvar "OSMODE"))
         (setvar "OSMODE" 0)
         (setq temperr *error*)
         (setq *error* trap1)



   (LASE)                    ;; go to create layer subfunction


 ; set Barricades width
         (if (not efb)(setq efb 0.60))                  
  	 	     (setq eeb (getreal (strcat "\nSpecify Barricades Width <" (rtos efb 2 2) ">:")))
	 (if (not eeb)(setq eeb efb)(setq efb eeb))	 
	             (setq dee (/ eeb 2))

(setq ptlist nil) ; for while command  

  (while 
     
      (progn 
        (setq e (car (entsel "\nSelect Road Edge Line to offset <exit>:")))
        (setq en (entget e))
         
         (setq pt2 (getpoint "\nSpecify point on side to offset:"))
         
    
      (if (and (not (= pt2 nil))(= (cdr (assoc 0 en)) "LWPOLYLINE")) 
	 
      (progn  

         (command "_change" e "" "_P" "_LA" "RD_Barricades" "")
	 (command "_change" e "" "_P" "_S" "1" "")
         (command "_change" e "" "_P" "_C" "BYLAYER" "")
         (command "_change" e "" "_P" "_LT" "BYLAYER" "")
         (command "_change" e "" "_P" "_LW" "BYLAYER" "")
	 (command "_offset" eeb e "_non" pt2 "")	
         (command "_change" (entlast) "" "_P" "_LA" "RD_Barricades" "")
        
         (command "_offset" dee e "_non" pt2 "")
         (command "_change" (entlast) "" "_P" "_LT" "Continuous" "")
         (command "_.pedit" (entlast) "_W" eeb "")
         (command "_change" (entlast) "" "_P" "_C" "253" "")

         (command "_DRAWORDER" e "" "_F")

         
   (setq ptlist (append ptlist (list pt2))) ; to stop while command

       ) ;progn

  (alert "\n    The Selected Object is Not a Polyline, Change Objects to Polyline and Try Again")
     ) ;if  
       
       ) ;progn  
  ) ;while


          (setvar "OSMODE" osm)
          (setvar "clayer" clay)
          (command "undo" "end")
 (princ)
) ;defun

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;--------------------------------------Create New layer----------------------------------------------

    (defun LASEC()

           (if (not (tblsearch "layer" "RD_Cones"))
           (command "-LAYER" "N" "RD_Cones" "C" "1" "RD_Cones" "LT" "Center" "RD_Cones""LW" "0.00" "RD_Cones" "")):if   

    ) ;defun

;;------------------------------Create Block-------------------------------  
 
 (defun BLKC()

   (if (not (tblsearch "block" "CONES"))
      (progn

         (command "CLAYER" "0")
         (command "DONUT" "0" "0.50" (list 0 0 0)"")
         (command "-BLOCK" "CONES" (list 0 0 0) (entlast) "")

      ) ;progn
    ) ;if

  ) ;defun 

;;---------------------Main Function for Water Barricades---------------------------------------------


(defun c:TC (/ osm clay e eeb lsc ) 


         (command "cmdecho"0)
         (command "undo" "group")
         (setq clay (getvar "clayer"))
         (command "-SCALELISTEDIT" "R" "Y" "E")
         (command "-PURGE" "LA" "*" "N")
	 (setq osm (getvar "OSMODE"))
         (setvar "OSMODE" 0)
         (setq temperr *error*)
         (setq *error* trap1)

   

 ; set distance between cones
         (if (not efc)(setq efc 2.00))                  
  	 	     (setq eeb (getreal (strcat "\nSpecify Distance Between Cones <" (rtos efc 2 2) ">:")))
	 (if (not eeb)(setq eeb efc)(setq efc eeb))	 
	             

(setq ptlist nil) ; for while command  

  (while 
     
      (progn 
        (setq e (car (entsel "\nSelect Road Edge Line to offset <exit>:")))
        (setq en (entget e))
         
                 
      (if (= (cdr (assoc 0 en)) "LWPOLYLINE")
     
   
      (progn  

         (setq lsc 0.5) ;;; CHANGE LINETYPE SCALE HERE  ;;;

          (LASEC)                     ;; go to create layer subfunction
          (BLKC)                      ;; subfunction create block

          (command "CLAYER" "RD_Cones")

          (command "_measure" e "_B" "CONES" "Y" eeb)
          (command "_change" e "" "_P" "_LA" "RD_Cones" "")
	  (command "_change" e "" "_P" "_S" lsc "")
          (command "_change" e "" "_P" "_C" "BYLAYER" "")
          (command "_change" e "" "_P" "_LT" "BYLAYER" "")
          (command "_change" e "" "_P" "_LW" "BYLAYER" "")

         
   (setq ptlist (append ptlist (list e))) ; to stop while command

       ) ;progn

  (alert "\n    The Selected Object is Not a Polyline, Change Objects to Polyline and Try Again")
     ) ;if  
       ) ;progn  
  ) ;while

          (setvar "OSMODE" osm)
          (setvar "clayer" clay)
          (command "undo" "end")
 (princ)
) ;defun



(princ "\nLisp Commands, Water Barricades:WB | Concrete Barricades:CB | Cones:TC |\n© Bijoy manoharan 2012|www.cadlispandtips.com|")
(princ)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


