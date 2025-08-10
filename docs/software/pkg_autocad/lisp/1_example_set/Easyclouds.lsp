
                           ;;------------------------------------------;;
                          ;; Title    : Easy Clouds                  
                          ;; Purpose  : To draw Revision Cloud     
                          ;; Copyright: Bijoy.v.m Nov-2010              
                          ;; Web page : www.cadlispandtips.com 
                          ;; Command  : CD, CDB, CDR                              
                           ;;------------------------------------------;;  

;;-------------------------------Create Block-----------------------------------------------------    

  (defun REV0()

	(setq osm (getvar "OSMODE"))  ; store current snap mode system variable
        (setvar "OSMODE" 0)
      (progn     
        (setq ssblk (ssadd))
        (command "PLINE" (list 0.00 0.00 0.00)(list 8.00 0.00 0.00)(list 4.00 6.9282 0.00)"c")
        (setq ssblk (ssadd (entlast) ssblk))
     
        (if (not (tblsearch "style" "Gen-Text")) 
            (command "-style" "Gen-Text" "Arial.ttf" "2.5" "1" 0 "n" "n"))
        
        (Command "-attdef" "" "00" "Rev-00" "00" "S" "Gen-Text" "J" "MC" (list 4.00 2.30 0.00) "0")
        (setq ssblk (ssadd (entlast) ssblk))  
        (Command "CHPROP" ssblk "" "LA" "0" "C" "bylayer" "LW" "0" "LT" "continuous" "")
        
  ;to create Block      
        (command "-BLOCK" "Rev-00" (list 0 0 0) ssblk "")
      ) ;progn 
   (vl-load-com)   
     (setq BLOCKS
       (vla-get-Blocks
         (vla-get-activedocument
           (vlax-get-acad-object)
         )
       )
         BLK (vla-Item BLOCKS "Rev-00")
     )
  (vla-put-explodable (vla-Item BLOCKS "Rev-00") :vlax-false)
      (command "REDRAW")
      (setvar "OSMODE" osm) ; store current snap mode system variable      
  ) ;defun
  
 ;;-------------------------------Sub Function Error-------------------------------------------------   
 
(defun trap1 (errmsg)

           (command "undo" "b")
           (setvar "attdia" ad)
	   (setvar "attreq" aq)
           (setq *error* temperr)
           (prompt "\n © Bijoy.v.m 2010 www.cadlispandtips.com")
(princ)
) ;defun

 ;;-------------------------------End Error-------------------------------    
  
(defun c:cd (/ model)
  (setq model (getvar "tilemode"))
  (defun Cloud(/ ad aq e d nd en etype p1 p2 pa pb vlist pc ra b0 b1 df cdal larc larb ptlist)

   (command "cmdecho"0)
   (command "undo" "group")
   (setq ad (getvar "attdia"))
   (setq aq (getvar "attreq"))
   (setq temperr *error*)
   (setq *error* trap1)
   (setvar "attdia" 0)
   (setvar "attreq" 1)
   
   (command "-LAYER" "N" "Rev_cloud" "C" "7" "Rev_cloud" "LT" "Continuous" "Rev_cloud""LW" "0.00" "Rev_cloud" "")
  
   (if (not (tblsearch "block" "Rev-00")) (REV0)) ; create new block  
   (setq by (strcat (Chr 66)(Chr 73)(Chr 74)(Chr 79)(Chr 89)(Chr 183)(Chr 86)(Chr 183)(Chr 77)))
    ;;; input Revision number   
       (if (not namef) (setq namef "01"))
       (setq name (getstring (strcat "\nType Revision Number <" namef ">: ")))
       (if (= name "") (setq name namef) (setq namef name))
       
    ;;; input revcloud arc length
         (if (not cd+d) (setq cd+d 5.00))   
         (setq cdal (getreal (strcat "\nEnter Length of Arc :<" (rtos cd+d 2 2) ">: ")))
         (if (not cdal) (setq cdal cd+d) (setq cd+d cdal))
       
   ; loop to continue selecting ogjects - pressing a null [Enter] will exit the loop
   
  (setq ptlist nil) ; for while command
  
  (while 
   (progn 
      (command "undo" "M") ; undo mark
      (command "_.pspace")
      (setq e (entsel "\nSelect Object: "))
  
     (if (not (= e nil))
      (progn  
       (setq e (car e))
       
       ; get end points
       (setq en (entget e))
       (setq etype (cdr (assoc 0 en)))
       (cond
          ((= etype "LINE") (setq p1 (cdr (assoc 11 en)))) ; set last points p2
          ((or (= etype "LWPOLYLINE") (= etype "POLYLINE"))
             (progn
                (get-vertice en) ; goto sub-function which creates a list of vertice data stored in VLIST
                (setq pa (nth 0 vlist) pb (last vlist)) ; set pa and pb to first and last entries of VLIST
                (setq p1 (list (car pa) (cadr pa)) p2 (list (car pb) (cadr pb)))  
             )
          )
          (( = etype "ARC")
             (setq pc (cdr (assoc 10 en)) ; set centre of arc point pc
                   ra (cdr (assoc 40 en)) ; set radius of arc ra
                   b0 (cdr (assoc 50 en)) ; set first arc bearing b0
                   b1 (cdr (assoc 51 en)) ; set second arc bearing b1
                   p1 (polar pc b0 ra)    ; calculate first arc point p1
                   p2 (polar pc b1 ra)    ; calculate last arc point p2
             )
          )
          (T (setq p1 nil p2 nil))
       )
     (if (or(= etype "LWPOLYLINE") (= etype "POLYLINE") (= etype "LINE") ( = etype "ARC") ( = etype "CIRCLE"))
      (progn
      ; insert block at end points
       (if p1 (command "insert" "Rev-00" p1 1.0 1.0 0.0 name))
       (setq larb (entlast))
       (Command "CHPROP" larb "" "LA" "Rev_cloud" "C" "bylayer" "LW" "bylayer" "LT" "bylayer" "")
       (command "_revcloud" "S" "N" "A" cdal cdal "O" e "N")
       (setq larc (entlast))
       (Command "CHPROP" larc "" "LA" "Rev_cloud" "C" "bylayer" "LW" "bylayer" "LT" "bylayer" "")  
      ) ;progn 
    ) ;if
    
      ) ;progn
     ) ;if
    (setq ptlist (append ptlist (list e))) ; to stop while command
    ) ;progn 
   ) ; end while loop
   ) ;defun
   
 (cond 
      ((= model 0) (Cloud))
      ((= model 1) (alert "\nPlease Switch to Layout"))
 ) ;cond

  (princ) 
) ;defun 
;;------------------------------Sub Function-----------------------------

;;; sub function to construct a list of POLYLINE vertice data (x,y,z,bulge)
(defun get-vertice (et / exd ename nv z xyz)
   (setq vlist nil exd (cdr (assoc 210 et)) ename (cdr (assoc -1 et)))
   ;;; store POLYLINE vertice data
   (if (= (cdr (assoc 0 et)) "POLYLINE")
      (progn
         (setq et (entget (entnext (cdar et))))
         (while (eq (cdr (assoc 0 et)) "VERTEX")
            (setq vlist (append vlist (list (append (cdr (assoc 10 et)) (list (cdr (assoc 42 et)))))))
            (setq et (entget (entnext (cdar et))))
         )
      )
   )
   ;;; store LWPOLYLINE vertice data
   (if (= (cdr (assoc 0 et)) "LWPOLYLINE")
      (progn
         (setq nv 0)
         (setq z (cdr (assoc 38 et)))
         (repeat (length et)
            (if (= (car (nth nv et)) 10) (setq xyz (append (cdr (nth nv et))(list z))))
            (if (= (car (nth nv et)) 42)
               (setq vlist (append vlist (list (append xyz (list (cdr (nth nv et)))))))
            )
            (setq nv (1+ nv))
         )
      )
   )
)

;;------------------------------EndSub Function-----------------------------

;;---------------------------Function Remove Cloud--------------------------


(defun c:cdr (/ model) 

  (setvar 'clayer "0")
  (setq model (getvar "tilemode"))

(defun remove (/ ss num int int1)  
     (progn
        (command "_.pspace")
        (setq ss (ssget "X" (list (cons 8 "Rev_cloud"))))
  
        (setq num (sslength ss))
     (if(> num 1) (setq int (/ num 2)))
     (if(= num 1) (setq int num)) 
        (setq int1 (itoa int))   
   
          (if(> num 0) 
             (progn
                (command "erase" ss "")
                (command "_.purge" "LA" "Rev_cloud" "N") 
                (command "_.purge" "B" "Rev-00" "N")
                (command "REDRAW")
                (princ (strcat "\n"int1" Revision Clouds Removed")) 
                (setq num 0)
             ) ;progn 
          ) ;if
      ) ;progn
 ) ;defun 
 
 (cond 
      ((and (= model 0) (tblsearch "layer" "Rev_cloud")) (remove))
      ((= model 1) (alert "\nPlease Switch to Layout"))
      ((and (= model 0) (not (tblsearch "layer" "Rev_cloud"))) (alert "\nNo Revision Cloud Found"))  
 ) ;cond

  (princ) 
) ;defun 


(princ "\nEasyclouds Lisp | © Bijoy manoharan 2010 | www.cadlispandtips.com |")
(princ "\nLisp Commands:CD & CDB(to Place Cloud),CDR(to Remove Cloud)")
(princ)

;;------------------------------End Function-----------------------------
