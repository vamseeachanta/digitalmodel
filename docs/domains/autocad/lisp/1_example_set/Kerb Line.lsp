

         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
         ;; Title    : Kerb Line              ;;   
         ;; Purpose  : To Draw kerb Marking   ;;
         ;; Written  : Bijoy manoharan        ;;       
         ;; Web page : www.cadlispandtips.com ;;
         ;; Command  : KB                     ;;       
         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;------------------sub function error------------------------
 
 (defun trap1 (errmsg)
    (setvar "clayer" clay)
    (setvar "OSMODE" osm)
    (command "undo" "end")
    (setq *error* temperr)
    (prompt "\n© Bijoy manoharan 2012 www.cadlispandtips.com")
    (princ)
   ) ;defun

;;---------------------Main Function---------------------------


(defun c:KB (/ osm clay e dee ls pt2) 


         (command "cmdecho"0)
         (command "undo" "group")
         (setq clay (getvar "clayer"))
         (command "-SCALELISTEDIT" "R" "Y" "E")
         (command "-PURGE" "LA" "*" "N")
	 (setq osm (getvar "OSMODE"))
         (setvar "OSMODE" 0)
         (setq temperr *error*)
         (setq *error* trap1)



   (if (not (tblsearch "layer" "Kerb Line"))
         (command "-LAYER" "N" "Kerb Line" "C" "7" "Kerb Line" "LT" "Continuous" "Kerb Line""LW" "0.00" "Kerb Line" "")
   ):if


  (if (= (tblsearch "ltype" "acad_iso03w100") nil)  
       (command "-linetype" "l" "acad_iso03w100" "acadiso.lin" "")
  )

 ; set kerb width
         (if (not ef)(setq ef 0.30))                  
  	 	     (setq ee (getreal (strcat "\nSpecify Kerb Width <" (rtos ef 2 2) ">:")))
	 (if (not ee)(setq ee ef)(setq ef ee))	 
	             (setq dee (/ ee 2))

(setq ptlist nil) ; for while command  

  (while 
     
      (progn 
        (setq e (car (entsel "\nSelect Kerb Line to offset <exit>:")))
        (setq en (entget e))
         
         (setq pt2 (getpoint "\nSpecify point on side to offset:"))
         
  ;  (if  (not (cdr (assoc 0 en)) "LWPOLYLINE")) (alert "\nPlease Switch to Layout"))
    
    (if (and (not (= pt2 nil))(= (cdr (assoc 0 en)) "LWPOLYLINE")) 
	 
      (progn  

         (setq ls 0.2) ;;; CHANGE LINETYPE SCALE HERE  ;;;

         (command "_change" e "" "_P" "_LA" "Kerb Line" "")
	 (command "_change" e "" "_P" "_S" "1" "")
         (command "_change" e "" "_P" "_C" "BYLAYER" "")
         (command "_change" e "" "_P" "_LT" "BYLAYER" "")
         (command "_change" e "" "_P" "_LW" "BYLAYER" "")
	 (command "_offset" ee e "_non" pt2 "")	
         (command "_change" (entlast) "" "_P" "_LA" "Kerb Line" "")
         (command "_offset" dee e "_non" pt2 "")
 	 (command "_change" (entlast) "" "_P" "_LT" "acad_iso03w100" "")
         (command "_change" (entlast) "" "_P" "_S" ls "")
         (command "_.pedit" (entlast) "_W" ee "")
         (command "_.pedit" (entlast) "_L" "ON" "")
         (command "_change" (entlast) "" "_P" "_LA" "Kerb Line" "")
         
   (setq ptlist (append ptlist (list pt2))) ; to stop while command

       ) ;progn

  (alert "\n   The Selected Object is Not a Polyline, Change Objects to Polyline and Try Again")
     ) ;if  
       ) ;progn  
  ) ;while


          (setvar "OSMODE" osm)
          (setvar "clayer" clay)
          (command "undo" "end")
 (princ)
) ;defun

;;---------------------Pedit---------------------------

(defun c:pp (/ pa ssj)
  (setq pa (getvar "peditaccept"))
  (setvar "peditaccept" 1)
    (setq ssj (ssget ))
    (command "pedit" "m" ssj ""  "j" "0.01" "")
  (setvar "peditaccept" pa)

(princ "\n  ||||| Objects Jointed |||||")


(princ)
)

;;------------------------------------------------

(princ "\nKerb Line Lisp Command : KB |PEDIT Command : PP|")
(princ "\n| © Bijoy manoharan 2012|www.cadlispandtips.com |")
(princ)




