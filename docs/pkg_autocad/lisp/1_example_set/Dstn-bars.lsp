
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Title    : Bars Distribution            
;; Purpose  : To draw a Distribution bars  
;; Copyright: Bijoy Manoharan Dec-2010  
;; Web page : www.cadlispandtips.com 
;; Command  : DB                          
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; ------ sub function error -------
 
(defun trap1 (errmsg)
           (setvar "clayer" clay)
           (command "undo" "end")
           (setq *error* temperr)           
      
           (prompt "\n Distribution bars placed\n © Bijoy Manoharan 2010 www.cadlispandtips.com")
(princ)
) ;defun

;; ---- End sub function error -----
;;------------------------------Create New layer-----------------------------
  (defun LASE()   
        (if (not (tblsearch "layer" "br_sec_rft dstn bar"))(command "-LAYER" "N" "br_sec_rft dstn bar"
                 "C" "4" "br_sec_rft dstn bar" "LT" "Continuous" "br_sec_rft dstn bar""LW" "0.00" "br_sec_rft dstn bar" ""))
        (if (not (tblsearch "layer" "Defpoints"))(command "-LAYER" "N" "Defpoints" ""))
            (command "-LAYER" "C" "1" "Defpoints" "LT" "Continuous" "Defpoints""LW" "0.00" "Defpoints" "")
  ) ;defun  
  
;;------------------------------Create Block-------------------------------   
 (defun BLK()
   (if (not (tblsearch "block" "T16"))
      (progn
         (setq osm (getvar "OSMODE"))
         (setvar "OSMODE" 0)
         (command "CLAYER" "0")
         (command "DONUT" "0" "0.04" (list 0 0 0)"")
         (command "-BLOCK" "T16" (list 0 0 0) (entlast) "")
         (setvar "OSMODE" osm)
      ) ;progn
    ) ;if
  ) ;defun  
   
;;------------------------------Subfunctions-------------------------------   

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

; sub function to get length of selected object (entity name required)
(defun getlen(ename / ent etype)
   (setq ent (entget ename))
   (setq etype (cdr (assoc 0 ent)))
   (if (or (= etype "ARC") (= etype "LINE"))
      (progn
         (command "pedit" ename "y" "")
         ; set ename and e = to entity name
         (setq ename (entlast) e (entlast))
         ; set pe-flag = 'T' to indicate that entity has been PEDITed
         (setq pe-flag T)
      )
      ; set pe-flag = 'nil' to indicate that entity has not been PEDITed
      (setq pe-flag nil)
   )
   ; command area will store the length (perimeter) in system variable "perimeter"
   (command "area" "o" ename)
   (getvar "perimeter")
 )


;;------------------------------Function Main-------------------------------   

(defun c:DB(/ e d barlen nd en etype p1 p2 pa pb vlist pc ra b0 b1 clay pe-flag osm)

   (command "cmdecho"0)
   (command "undo" "group")
   (setq clay (getvar "clayer"))
   (setq temperr *error*)
   (setq *error* trap1)

    (LASE) ; subfunction create layer
    (BLK)  ; subfunction create block    
   
      (command "CLAYER" "br_sec_rft dstn bar")
      
   ; Enter Distance between bars
          (if (not df) (setq df 0.15))
          (setq d (getreal (strcat "\nSpecify Distance Between Bars <" (rtos df 2 2) ">:")))
       (if (not d) (setq d df)(setq df d))
       
       
   (setq ptlist nil) ; for while command
   (setq by (strcat (Chr 66)(Chr 73)(Chr 74)(Chr 79)(Chr 89)(Chr 183)(Chr 86)(Chr 183)(Chr 77)))
   ; loop to continue selecting ogjects - pressing a null [Enter] will exit the loop
   (while 
     (progn
       (setq e (entsel "\nSelect Object: "))
   
     (if (not (= e nil))
      (progn 
   
       (setq e (car e))
       (setq barlen (getlen e)) ; goto sub-function to get length of bar

       ; set number of bars (rounded value)
       (setq nd (fix (+ (/ barlen d) 0.5)))

       (command "DIVIDE" e "B" "T16" "Y" nd)
       (Command "CHPROP" e "" "LA" "Defpoints" "")


       ; get end points
       (setq en (entget e))
       (setq etype (cdr (assoc 0 en)))
       (cond
          ((= etype "LINE") (setq p1 (cdr (assoc 10 en)) p2 (cdr (assoc 11 en)))) ; set first and last points p1 & p2
          ((or (= etype "LWPOLYLINE") (= etype "POLYLINE"))
             (progn
                (get-vertice en) ; goto sub-function which creates a list of vertice data stored in VLIST
                (setq pa (nth 0 vlist) pb (last vlist)) ; set pa and pb to first and last entries of VLIST
                (setq p1 (list (car pa) (cadr pa)) p2 (list (car pb) (cadr pb))) ; create first and last points p1 & p2 from pa & pb
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

       ; insert block at end points
       (if p1 (command "insert" "T16" p1 1.0 1.0 0.0))
       (if p2 (command "insert" "T16" p2 1.0 1.0 0.0))

       ; if selected entity has been PEDIT then explode back to original enity type
       (if pe-flag (command "explode" e)) ;if
           
      ) ;progn 
    ) ;if
    
       (setq ptlist (append ptlist (list e))) ; to stop while command
      ) ;progn
    ) ; end while loop

    (princ)
 ) ;defun

(princ "\nDstn-bars Lisp | © Bijoy manoharan 2010 | www.cadlispandtips.com |")
(princ "\nLisp Command:DB")
(princ)
;;------------------------------End-------------------------------   
