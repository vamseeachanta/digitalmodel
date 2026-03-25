
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Title    : Parking Bay            
;; Copyright: Bijoy Manoharan Oct-2014  
;; Web page : www.cadlispandtips.com 
;; Command  : PB for Parallel,
;; Command  : PPB for Perpendicular Parking,
;; Command  : PBB for Angular Parking
;; Command  : RD for change direction of line                          
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; ------ sub function error -------
 
(defun trap1 (errmsg)
           (setvar "clayer" clay)
           (command "undo" "end")
           (setq *error* temperr)           
      
           (prompt "\n Parking Bay Placed\n © Bijoy Manoharan 2014 www.cadlispandtips.com")
(princ)
) ;defun

;; ---- End sub function error -----
;;------------------------------Create New layer-----------------------------
  (defun LASE()   
        (if (not (tblsearch "layer" "RD_PR_PBAY"))(command "-LAYER" "N" "RD_PR_PBAY"
                 "C" "90" "RD_PR_PBAY" "LT" "Continuous" "RD_PR_PBAY" "LW" "0.18" "RD_PR_PBAY" "D" "Parking Bay" "RD_PR_PBAY" ""))
  ) ;defun  
  
;;------------------------------Create Block-------------------------------   
 (defun BLK()
   (if (not (tblsearch "block" "PAR"))
      (progn
         (setq osm (getvar "OSMODE"))
         (setvar "OSMODE" 0)

   ; Enter parking bay Block Length
          (if (not h1) (setq h1 2.50))

          (setq h (getreal (strcat "\nSpecify Parking Bay Height <" (rtos h1 2 2) ">:")))
       (if (not h) (setq h h1)(setq h1 h))

         (command "CLAYER" "0")
         (command "pline" (list 0 0 0) (list 0 h1 0) (list -0.5 h1 0) (list 0.5 h1 0) "")
         (command "-BLOCK" "PAR" (list 0 0 0) (entlast) "")
         (setvar "OSMODE" osm)
      ) ;progn
    ) ;if
  ) ;defun  

;;------------------------------Create Block-------------------------------   
 (defun BLKP()
   (if (not (tblsearch "block" "PARP"))
      (progn
         (setq osm (getvar "OSMODE"))
         (setvar "OSMODE" 0)

   ; Enter parking bay Block Length
          (if (not h2) (setq h2 6.00))

          (setq h (getreal (strcat "\nSpecify Parking Bay Height <" (rtos h2 2 2) ">:")))
       (if (not h) (setq h h2)(setq h2 h))

         (command "CLAYER" "0")
         (command "pline" (list 0 0 0) (list 0 h2 0) (list -0.5 h2 0) (list 0.5 h2 0) "")
         (command "-BLOCK" "PARP" (list 0 0 0) (entlast) "")
         (setvar "OSMODE" osm)
      ) ;progn
    ) ;if
  ) ;defun  
   
;;------------------------------Create Block------------------------------- 
 (defun BLKA()
   (if (not (tblsearch "block" "PARA"))
      (progn
         (setq osm (getvar "OSMODE"))
         (setvar "OSMODE" 0)
         (command "CLAYER" "0")
         (command "pline" (list 0 0 0) (list -3.866 6.696 0) (list -4.366 6.696 0) (list -3.366 6.696 0) "")
         (command "-BLOCK" "PARA" (list 0 0 0) (entlast) "")
         (setvar "OSMODE" osm)
      ) ;progn
    ) ;if
  ) ;defun 
;;------------------------------Function Main-------------------------------   

(defun c:PB(/ e d clay osm)

   (command "cmdecho"0)
   (setq clay (getvar "clayer"))
   (setq temperr *error*)
   (setq *error* trap1)

    (LASE) ; subfunction create layer

   (if (not (tblsearch "block" "PAR")) (BLK))  ; subfunction create block    
   
      (command "CLAYER" "RD_PR_PBAY")     

  
 ; Enter Distance of parking bay
          (if (not df) (setq df 6.00))

          (setq d (getreal (strcat "\nSpecify Parking Width <" (rtos df 2 2) ">:")))
       (if (not d) (setq d df)(setq df d))
       
       
   (setq ptlist nil) ; for while command

   ; loop to continue selecting ogjects - pressing a null [Enter] will exit the loop
   (while 
     (progn
       (setq e (entsel "\nSelect Object: "))
   
     (if (not (= e nil))
      (progn 
   
       (command "MEASURE" e "B" "PAR" "Y" df)
          
      ) ;progn 
    ) ;if
    
       (setq ptlist (append ptlist (list e))) ; to stop while command
      ) ;progn
    ) ; end while loop

    (princ)
 ) ;defun

;;------------------------------Function Main-------------------------------   

(defun c:PPB(/ e d clay osm)

   (command "cmdecho"0)
   (setq clay (getvar "clayer"))
   (setq temperr *error*)
   (setq *error* trap1)

    (LASE) ; subfunction create layer

   (if (not (tblsearch "block" "PARP")) (BLKP))  ; subfunction create block    
   
      (command "CLAYER" "RD_PR_PBAY")     

  
 ; Enter Distance of parking bay
          (if (not dfp) (setq dfp 3.00))

          (setq d (getreal (strcat "\nSpecify Parking Width <" (rtos dfp 2 2) ">:")))
       (if (not d) (setq d dfp)(setq dfp d))
       
       
   (setq ptlist nil) ; for while command

   ; loop to continue selecting ogjects - pressing a null [Enter] will exit the loop
   (while 
     (progn
       (setq e (entsel "\nSelect Object: "))
   
     (if (not (= e nil))
      (progn 
   
       (command "MEASURE" e "B" "PARP" "Y" dfp)
          
      ) ;progn 
    ) ;if
    
       (setq ptlist (append ptlist (list e))) ; to stop while command
      ) ;progn
    ) ; end while loop

    (princ)
 ) ;defun
;;------------------------------Function Main-------------------------------   

(defun c:PBB(/ e d clay osm)

   (command "cmdecho"0)
   (setq clay (getvar "clayer"))
   (setq temperr *error*)
   (setq *error* trap1)

    (LASE) ; subfunction create layer
    (BLKA)  ; subfunction create block    
   
      (command "CLAYER" "RD_PR_PBAY")
      
   ; Enter Distance of parking bay
          (if (not dfa) (setq dfa 3.00))

          (setq d (getreal (strcat "\nSpecify Parking Width <" (rtos dfa 2 2) ">:")))
       (if (not d) (setq d dfa)(setq dfa d))
       
       (setq dfb (* dfa 1.1547))
       
   (setq ptlist nil) ; for while command

   ; loop to continue selecting ogjects - pressing a null [Enter] will exit the loop
   (while 
     (progn
       (setq e (entsel "\nSelect Object: "))
   
     (if (not (= e nil))
      (progn 
   
       (command "MEASURE" e "B" "PARA" "Y" dfb)
          
      ) ;progn 
    ) ;if
    
       (setq ptlist (append ptlist (list e))) ; to stop while command
      ) ;progn
    ) ; end while loop

    (princ)
 ) ;defun


;;------------------------------End-------------------------------   

;;;;Reverse Polyline ;;;;;

; reverse the polyline direction

(defun getcodelist (code lista / item lout)
   (while (setq item (assoc code lista))
      (setq lista (cdr (member item lista)) lout (cons item lout)))
   lout)

(defun dxfn (code name)
   (cdr (assoc code (entget name))))

(defun listrotate (n lista)
   (repeat n (setq lista (cdr (append lista (list (car lista)))))))

(defun c:RD (/ poly vt lvt lvt2 new old ed closed polygen arcgen
                  l_10 l_40 l_41 l_42 i n item ss a e )

   (command "undo" "be")
   (princ "\nRD - Reverses the direction of a PLINE, ARC or LINE")

   (sssetfirst nil nil)
   (princ "\nSelect ARCs, LINEs or POLYLINEs to reverse")
   (setq ss (ssget '((0 . "ARC,LINE,POLYLINE,LWPOLYLINE"))))

   (setq a -1 polygen nil arcgen 0)
   (while (and ss (< (setq a (1+ a)) (sslength ss)))
      (setq poly (ssname ss a) arc_check nil)
      (if (= (dxfn 0 poly) "ARC") (progn
         (setq arc_check T)
         (command "pedit" poly "" "")
         (if (or (entget poly) (and (/= "LWPOLYLINE" (dxfn 0 (entlast))) (/= "POLYLINE" (dxfn 0 (entlast)))))
            (princ "\nCouldn't convert ARC to PLINE")
            (setq poly (entlast) arcgen (1+ arcgen)))))
      (cond ((= (dxfn 0 poly) "POLYLINE")
               (if (/= 128 (logand 128 (dxfn 70 poly))) (progn
                  (if (not arc_check) (setq polygen T))
                  (setq ed (entget poly))
                  (setq ed (subst (cons 70 (logior 128 (cdr (assoc 70 ed)))) (assoc 70 ed) ed))
                  (entmod ed) (entupd poly)))
               (setq closed (= (dxfn 70 poly) 1) vt (entnext poly))
               (while (/= (dxfn 0 vt) "SEQEND")
                  (if (/= 16 (dxfn 70 vt))
                     (setq lvt (cons (list (dxfn 10 vt) (dxfn 42 vt)) lvt)))
                  (setq vt (entnext vt)))
               (if closed
                  (setq lvt (cons (last lvt) lvt)
                        lvt (reverse (cdr (reverse lvt)))))
               (setq lvt2 (cdr (append lvt (list (car lvt))))
                     lvt  (mapcar '(lambda (a b) (list (car a) (- (cadr b)))) lvt lvt2)
                     lvt2 nil)
               (setq vt (entnext poly))
               (while (/= (dxfn 0 vt) "SEQEND")
               (if (/= 16 (dxfn 70 vt)) (progn
                  (setq ed  (entget vt)
                        old (assoc 10 ed)
                        new (cons 10 (caar lvt))
                        ed  (subst new old ed)
                        old (assoc 42 ed)
                        new (cons 42 (cadr (car lvt)))
                        ed  (subst new old ed)
                        lvt (cdr lvt))
                  (entmod ed)))
               (setq vt (entnext vt)))
               (entupd poly))
            ((= (dxfn 0 poly) "LWPOLYLINE")
               (if (/= 128 (logand 128 (dxfn 70 poly))) (progn
                  (if (not arc_check) (setq polygen T))
                  (setq ed (entget poly))
                  (setq ed (subst (cons 70 (logior 128 (cdr (assoc 70 ed)))) (assoc 70 ed) ed))
                  (entmod ed) (entupd poly)))
               (setq ed  (entget poly)
                     lvt (member (assoc 10 ed) ed)
                     lvt (reverse (cdr (reverse lvt)))
                     l_10 (getcodelist 10 lvt)
                     l_41 (getcodelist 41 lvt)
                     l_41 (mapcar '(lambda (a) (cons 40 (cdr a))) l_41)
                     l_41 (listrotate 1 l_41)
                     l_40 (getcodelist 40 lvt)
                     l_40 (mapcar '(lambda (a) (cons 41 (cdr a))) l_40)
                     l_40 (listrotate 1 l_40)
                     l_42 (getcodelist 42 lvt)
                     l_42 (mapcar '(lambda (a) (cons 42 (- (cdr a)))) l_42)
                     l_42 (listrotate 1 l_42)
                     n    (length l_10)
                     i    -1)
               (while (< (setq i (1+ i)) n)
                  (setq lvt2 (append lvt2 (list (nth i l_10) (nth i l_41)
                                                (nth i l_40) (nth i l_42)))))
               (setq lvt (reverse ed))
               (while (setq item (assoc 10 lvt))
                  (setq lvt (cdr (member item lvt))))
               (setq lvt (reverse lvt) lvt2 (append lvt lvt2 (list (assoc 210 ed))))
               (entmod lvt2)
               (entupd poly))
            ((= (dxfn 0 poly) "LINE")
               (setq ed (entget poly) e (cdr (assoc 10 ed)))
               (setq ed (subst (cons 10 (cdr (assoc 11 ed))) (assoc 10 ed) ed))
               (setq ed (subst (cons 11 e) (assoc 11 ed) ed))
               (entmod ed)
               (entupd poly))
      ))
   (if (< 0 arcgen) (princ (strcat "\n" (itoa arcgen) "ARC" (if (= 1 arcgen) " was" "s were")
                                   "converted to " (if (= 1 arcgen) "a PLINE" "PLINEs"))))
	
   (command "undo" "e")
   (princ))

(princ)


(alert "-------------------------- Parking bay ---------------------------

\n Commands                                           
\n    Command   PB   ( For Parallel Parking )
\n    Command   PPB  ( For Perpendicular parking )
\n    Command   PBB  ( For Angular parking )
\n    Command   RD   ( For Reverse Direction)
\nBijoy manoharan\nOct 2014\nwww.cadlispandtips.com")

(Princ "\nParking Bay Lisp | © Bijoy manoharan 2014 | www.cadlispandtips.com |")
(princ "\nLisp Command:PB for Parallel, PPB for Perpendicular, PBB for angular Parking Bay")
(princ "\nLisp Command:RD for Reverse direction ")
(princ)

------------------End------------------

