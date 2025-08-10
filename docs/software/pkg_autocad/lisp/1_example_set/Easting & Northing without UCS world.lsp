

                                 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                 ;; Title   : Easting & Northing                    ;;
                                 ;; Purpose : To place Coordinate without Ucs World ;;
                                 ;; Written : Bijoy.v.m                             ;;
                                 ;; Date    : Sep 2011                              ;;
                                 ;; System requirement : Autocad 2007               ;;
                                 ;; Command : EN UV                                   ;;
                                 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                         
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
           (command "ucs" "p")
           (prompt "\n © Bijoy.v.m 2011 www.cadlispandtips.blogspot.com")
(princ)
) ;defun

;;-------------------------------------------Place Text----------------------------------------------------


(defun C:EN (/ enp1 enp2 ex ey dy ptl e TextObj vlText)

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
          (command "ucs" "w")
          (setq enp1 (getpoint "\nPick Coordinate point: "))
          (setq ex (car enp1))  ;x coord
          (setq ey (cadr enp1)) ;y coord
          (setq enx (rtos ex 2 3))
          (setq eny (rtos ey 2 3))      

          (command "ucs" "p")
          (setq enp2 (getpoint "\nPick again the same point: "))
          (setq ptl (getpoint "\nPick text location: "))
        
          (SETVAR 'DIMTAD 0) ; Justification centered
          (SETVAR 'DIMLDRBLK "_ORIGIN") ;; leader arrow
        (command "leader" enp2 ptl "" (strcat "E " enx) (strcat "N " eny) "")
        (setq TextObj (entlast))
           
   (vl-load-com)

        (setq vlText (vlax-ename->vla-object TextObj))     
        (vlax-put-property vlText 'backgroundfill :vlax-true)  ; background mask
        
         (SETVAR 'DIMTAD 1 ) ; Justification above
         (setvar "DIMLDRBLK" ".") ;;leader arrow
         (setq by (strcat (Chr 66)(Chr 73)(Chr 74)(Chr 79)(Chr 89)(Chr 183)(Chr 86)(Chr 183)(Chr 77)))
         (setq ptlist (append ptlist (list pt))) ; to stop while command
         
 
     ) ;progn  
   ) ;while   

  (princ)
) ; defun			 

;;----------------------------------------Back to UCS World-----------------------------------------------------

(defun C:uv ()

        (command "ucs" "V")
        (prompt "\nUCS Origin is set to View")

  (princ)
) ; defun


(princ "\nEasting & Northing Lisp | © Bijoy manoharan 2011 | www.cadlispandtips.com |")
(princ "\nLisp Commands:EN(to place Coordinate text without using UCS world),UV(Ucs View)")
(princ)

;;----------------------------------------------End-----------------------------------------------------

