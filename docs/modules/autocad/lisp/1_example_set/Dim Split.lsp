 
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Title  :Dim Split                             ;;
  ;; Purpose: Add Primary & Secondary dimension    ;;
  ;; Written: Bijoy Manoharan                      ;;
  ;; Command: D1 & D2                              ;;
  ;; Date   : May-2011                             ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;------------------sub function error------------------------
 
 (defun trap1 (errmsg)
    (setq *error* temperr)
    (prompt "\n© Bijoy Manoharan 2011 www.cadlispandtips.com")
    (princ)
   ) ;defun

;-------------------------------Main Function Primary-------------------------

(defun c:D1 (/ puf e en dt dtxt txt ptlist pt)

     (setq temperr *error*)
     (setq *error* trap1)
     
 ;;; input text suf  
        
     (if (not pufx) (setq pufx ""))
     (setq puf (getstring (strcat "\nEnter Primary dimension text <" pufx ">: ")))
     (if (= puf "") (setq puf pufx) (setq pufx puf)) 
     
;;; select object

     (setq ptlist nil) ; for while command
 (while
  (progn
     (setq e (entsel "\nSelect Dimension: "))   
     (setq en (entget (car e)))
     (setq dt (cdr (assoc 1 en)))
     

     (if (/= dt "") (setq dtxt dt))
     (if (= dt "") (setq dtxt "<>"))

     (setq txt (strcat puf "\\X" dtxt))
     
     (command"dimedit" "n" txt e "")

     (setq pt e)
     (setq ptlist (append ptlist (list pt))) ; to stop while command
  
  ) ;progn
 ) ;while


(princ)
) ;defun 

;-------------------------------Main Function Secondary-------------------------

(defun c:D2 (/ suf e en dt dtxt txt ptlist pt)

     (setq temperr *error*)
     (setq *error* trap1)
     
 ;;; input text suf  
        
     (if (not sufx) (setq sufx "(TYP)"))
     (setq suf (getstring (strcat "\nEnter Secondary dimension text <" sufx ">: ")))
     (if (= suf "") (setq suf sufx) (setq sufx suf)) 
     
;;; select object

     (setq ptlist nil) ; for while command
 (while
  (progn
     (setq e (entsel "\nSelect Dimension: "))   
     (setq en (entget (car e)))
     (setq dt (cdr (assoc 1 en)))
     

     (if (/= dt "") (setq dtxt dt))
     (if (= dt "") (setq dtxt "<>"))

     (setq txt (strcat dtxt "\\X" suf))
     
     (command"dimedit" "n" txt e "")

     (setq pt e)
     (setq ptlist (append ptlist (list pt))) ; to stop while command
  
  ) ;progn
 ) ;while


(princ)
) ;defun



(princ "\nDim Split Lisp | © Bijoy manoharan 2011 | www.cadlispandtips.com |")
(princ "\nLisp Commands:D1(to add Primary dim text),D2(to add Secondary dim text)")
(princ)

;-------------------------------------End-----------------------------------
