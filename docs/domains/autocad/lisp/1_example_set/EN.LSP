

                                 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                 ;; Title   : Easting & Northing           ;;
                                 ;; Purpose : To get Coordinate            ;;
                                 ;; Written : Bijoy.v.m                    ;;
                                 ;; Date    : Nov 2010                     ;;
                                 ;; System requirement : Autocad 2007      ;;
                                 ;; Command : DAT, EN & UW                ;;
                                 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                         
;;--------------------------------------Sub Function to Create Styles-----------------------------------------------------
    
    (defun Styles()
       
    ;create text Style   
       
    (if (not (tblsearch "style" "Gen-Text")) (command "-style" "Gen-Text" "Arial.ttf" "A""yes" "No" 2.5 "1" 0 "n" "n"))
    
    ;create dimension style
    
    (if (not (tblsearch "DImstyle" "Dim Arrow Ann"))
        (progn
           (command "dim" "style" "Gen-Text"
            "DIMADEC"     0
            "DIMALT"      0
            "DIMALTD"     2
            "DIMALTF"     1.000
            "DIMALTRND"   0.0000
            "DIMALTTD"    2
            "DIMALTTZ"    0
            "DIMALTU"     2
            "DIMALTZ"     0
            "DIMASZ"      3
            "DIMATFIT"    3
            "DIMAUNIT"    0
            "DIMAZIN"     0
            "DIMBLK"      ""
            "DIMBLK1"     ""
            "DIMBLK2"     ""
            "DIMLDRBLK"   ""
            "DIMCEN"      0
            "DIMCLRD"     7
            "DIMCLRE"     7
            "DIMCLRT"     7
            "DIMDEC"      0
            "DIMDLE"      0.0000
            "DIMDLI"      1.0000
            "DIMEXE"      1.5000
            "DIMEXO"      1.5000
            "DIMFRAC"     0
            "DIMGAP"      1.0000
            "DIMJUST"     0
            "DIMLFAC"     1000.0000
            "DIMLIM"      0
            "DIMLUNIT"    2
            "DIMLWD"      0
            "DIMLWE"      0 
            "DIMRND"      0.0000
            "DIMSAH"      0
            "DIMSCALE"    1.0000
            "DIMSD1"      0
            "DIMSD2"      0
            "DIMSE1"      0
            "DIMSE2"      0
            "DIMSOXD"     0
            "DIMTAD"      1     
            "DIMTDEC"     0
            "DIMTIH"      0
            "DIMTIX"      0
            "DIMTM"       0.0000
            "DIMTMOVE"    0
            "DIMTOFL"     0
            "DIMTOH"      0
            "DIMTSZ"      0.0000
            "DIMTVP"      0.0000
            "DIMTXSTY"    "Gen-Text"
            "DIMTXT"      2.5000
            "DIMZIN"      0
            "DIMFIT"      5 /e)        
          
          (command "dimstyle" "An" "y" "Dim Arrow Ann" "S" "")
        ) ;progn
      ) ;if
      
 ) ;defun

;;-------------------------------------------* error *-----------------------------------------------------

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
         (Styles)
         (command "CLAYER" "Text Coordinate")
         (command "-DIMSTYLE" "r" "Dim Arrow Ann")  
         
       
 (setq ptlist nil) ; for while command
  (while     
     (progn         
          (setq enp1 (getpoint "\nPick Coordinate point: "))
          (setq ex (car enp1))  ;x coord
          (setq ey (cadr enp1)) ;y coord
          (setq enx (rtos ex 2 3))
          (setq eny (rtos ey 2 3))      

        
          (setq ptl (getpoint "\nPick text location: "))
        
          (SETVAR 'DIMTAD 0) ; Justification centered
          (SETVAR 'DIMLDRBLK "_ORIGIN") ;; leader arrow
        (command "leader" enp1 ptl "" (strcat "E " enx) (strcat "N " eny) "")
        (setq TextObj (entlast))
           
   (vl-load-com)

        (setq vlText (vlax-ename->vla-object TextObj))     
        (vlax-put-property vlText 'backgroundfill :vlax-true)  ; background mask
        
         (SETVAR 'DIMTAD 1 ) ; Justification above
         (setvar "DIMLDRBLK" ".") ;;leader arrow
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

