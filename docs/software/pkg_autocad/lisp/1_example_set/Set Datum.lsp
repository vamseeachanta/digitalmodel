

                                 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                 ;;                                        ;;
                                 ;; Title   : Set Datum Coordinate         ;;
                                 ;; Purpose : To change Datum coordinate   ;;
                                 ;; Written : Bijoy manoharan              ;;
                                 ;; Date    : Nov 2010                     ;;
                                 ;; Command : DAT                          ;;
                                 ;;                                        ;;
                                 ;; Website: www.cadlispandtips.com        ;;
                                 ;;                                        ;;
                                 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                         

;;                     Note : 1. Rotated Objects will not give accurate coordinate, 
;;                               If you are  changing datum point
;;                               please check atleast two coordinate.


;;-----------------------------------------------Set Datum-----------------------------------------------

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
        (prompt "\nOrigin moved to new loaction - Enter Command UW to change UCS World")        
        
  (princ)
) ;defun     
   
;;-------------------------------------------Back to UCS World-------------------------------------------

(defun C:uw ()

        (command "ucs" "w")
        (prompt "\nUCS Origin is set to World")

  (princ)
) ; defun


(princ "\nSet Datum Coordinate Lisp | © Bijoy manoharan 2012 | www.cadlispandtips.com |")
(princ "\nLisp Commands:DAT(to set Datum point),UW(Ucs World)")
(princ)
;;-------------------------------------------------End-----------------------------------------------------

