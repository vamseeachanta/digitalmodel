; Next available MSG number is    76 
; MODULE_ID LSP_3D_LSP_
;;;
;;;    3d.lsp
;;;    
;;;    Copyright 1988,1990,1992,1994,1996-1998 by Autodesk, Inc.
;;;
;;;    Permission to use, copy, modify, and distribute this software
;;;    for any purpose and without fee is hereby granted, provided
;;;    that the above copyright notice appears in all copies and
;;;    that both that copyright notice and the limited warranty and
;;;    restricted rights notice below appear in all supporting
;;;    documentation.
;;;
;;;    AUTODESK PROVIDES THIS PROGRAM "AS IS" AND WITH ALL FAULTS.
;;;    AUTODESK SPECIFICALLY DISCLAIMS ANY IMPLIED WARRANTY OF
;;;    MERCHANTABILITY OR FITNESS FOR A PARTICULAR USE.  AUTODESK, INC.
;;;    DOES NOT WARRANT THAT THE OPERATION OF THE PROGRAM WILL BE
;;;    UNINTERRUPTED OR ERROR FREE.
;;;
;;;    Use, duplication, or disclosure by the U.S. Government is subject to
;;;    restrictions set forth in FAR 52.227-19 (Commercial Computer
;;;    Software - Restricted Rights) and DFAR 252.227-7013(c)(1)(ii) 
;;;    (Rights in Technical Data and Computer Software), as applicable.
;;;
;;;============================================================================
;;;
;;; Nine 3d objects can be drawn: box, cone, dish, dome, mesh, pyramid,
;;; sphere, torus, and wedge.
;;;
;;; When constructing a pyramid with the "ridge" option, enter the ridge
;;; points in the same direction as the base points, ridge point one being
;;; closest to base point one.  This will prevent the "bowtie" effect.
;;; Note that this is also true for the pyramid's "top" option.


;;; ===================== load-time error checking ============================

  (defun ai_abort (app msg)
     (defun *error* (s)
        (if old_error (setq *error* old_error))
        (princ)
     )
     (if msg
       (alert (strcat " Application error: "
                      app
                      " \n\n  "
                      msg
                      "  \n"
              )
       )
     )
     (exit)
  )

;;; Check to see if AI_UTILS is loaded, If not, try to find it,
;;; and then try to load it.
;;;
;;; If it can't be found or it can't be loaded, then abort the
;;; loading of this file immediately, preserving the (autoload)
;;; stub function.

  (cond
    ; it's already loaded.
    ((and ai_dcl (listp ai_dcl)))
    ; find it
    ((not (findfile "ai_utils.lsp"))
      (ai_abort "3D"
                  (strcat "Can't locate file AI_UTILS.LSP."
                          "\n Check support directory." ) ) )
    ; load it
    ((eq "failed" (load "ai_utils" "failed"))
        (ai_abort "3D" "Can't load file AI_UTILS.LSP") )
  )

  ; defined in AI_UTILS.LSP
  (if (not (ai_acadapp))
    ; a Nil <msg> supresses ai_abort's alert box dialog.
    (ai_abort "3D" nil)
  )

;;; ==================== end load-time operations ===========================



;;;--------------------------------------------------------------------------
;;; Allow easier reloads

(setq boxwed     nil  
      cone       nil
      mesh       nil
      pyramid    nil
      spheres    nil
      torus      nil
      3derr      nil
      C:3D       nil
)

;;;--------------------------------------------------------------------------
;;; System variable save

(defun modes (a)
  (setq MLST nil)
  (repeat (length a)
    (setq MLST (append MLST (list (list (car a) (getvar (car a))))))
    (setq a (cdr a))
  )
)

;;;--------------------------------------------------------------------------
;;; System variable restore

(defun moder ()
  (repeat (length MLST)
    (setvar (caar MLST) (cadar MLST))
    (setq MLST (cdr MLST))
  )
)

;;;--------------------------------------------------------------------------
;;; Draw a cone

(defun cone (/ elev cen1 rad top h numseg cen2 oldelev e1 e2)
  (setq numseg 0)
  ;3D point can't be null
  (initget 17)
  (setq elev (caddr (setq cen1 (getpoint "\nSpecify center point for base of cone: "))))
  ;Base radius can't be 0, neg, or null
  (initget 7 "Diameter")
  (setq rad (getdist cen1 "\nSpecify radius for base of cone or [Diameter]: "))
  (if (= rad "Diameter")
    (progn
      ;Base diameter can't be 0, neg, or null
      (initget 7)
      (setq rad (/ (getdist cen1 "\nSpecify diameter for base of cone: ") 2.0))
    )
  )

  ;Top radius can't be neg
  (initget 4 "Diameter")
  (setq top (getdist cen1 "\nSpecify radius for top of cone or [Diameter] <0>: "))
  (if (= top "Diameter")
    (progn
      ;Top diameter can't be neg
      (initget 4)
      (setq top (getdist cen1 "\nSpecify diameter for top of cone <0>: "))
      (if top
        (setq top (/ top 2.0))
      )
    )
  )
  (if (null top)
    (setq top 0.0)
  )

  ;Height can't be 0, neg, or null
  (initget 7 "Height")
  (setq h (getdist cen1 "\nSpecify height of cone: "))

  ;SURFTAB1 can't be less than 2
  (while (< numseg 2)
    (initget 6)
    (setq numseg (getint "\nEnter number of segments for surface of cone <16>: "))
    (if (null numseg)
      (setq numseg 16)
    )  
    (if (< numseg 2)
      (princ "\nNumber of segments must be greater than 1.")
    )
  )
  (setvar "SURFTAB1" numseg)

  ; Draw base circle
  (command "_.CIRCLE" cen1 rad)
  (setq undoit T)
  (setq e1 (entlast))
  (setq cen2 (list (car cen1) (cadr cen1) (+ (caddr cen1) h)))
  (setq oldelev (getvar "ELEVATION"))
  (command "_.ELEV" (+ elev h) "")
  (cond 
    ; Draw top point or circle
    ((= top 0.0) (command "_.POINT" cen2))  
    (t (command "_.CIRCLE" cen2 top))
  )
  (setq e2 (entlast))
  (setvar "ELEVATION" oldelev)

  ; Draw cone
  (command "_.RULESURF" (list e1 cen1) (list e2 cen2))
  (entdel e1) 
  (entdel e2)
)

;;;--------------------------------------------------------------------------
;;; Draw a sphere, dome, or dish

(defun spheres (typ / cen r numseg ax ax1 e1 e2)
  (setq numseg 0)
  (initget 17)                        ;3D point can't be null
  (setq cen (getpoint (strcat "\nSpecify center point of " typ": ")))
  (initget 7 "Diameter")              ;Radius can't be 0, neg, or null
  (cond 
    ((= typ "sphere") (princ "\nSpecify radius of sphere or [Diameter]: "))
    ((= typ "dome"  ) (princ "\nSpecify radius of dome or [Diameter]: "))
    ((= typ "dish"  ) (princ "\nSpecify radius of dish or [Diameter]: "))
  )
  (setq r (getdist cen )) 
  (if (= r "Diameter")
    (progn
      (initget 7)                     ;Diameter can't be 0, neg, or null
      (setq r (/ (getdist cen (strcat "\nSpecify diameter of "typ": ")) 2.0))
    )
  )
  (setq cen (trans cen 1 0))          ;Translate from UCS to WCS
        
  (while (< numseg 2)                 ;SURFTAB1 can't be less than 2
    (initget 6)
	(princ (strcat "\nEnter number of longitudinal segments for surface of "typ" <16>: "))
    (setq numseg (getint))
    (if (null numseg)
      (setq numseg 16)
    )
    (if (< numseg 2)
      (princ "\nNumber of segments must be greater than 1.")
    )
  )
  (setvar "SURFTAB1" numseg)
   
  (setq numseg 0)
  (while (< numseg 2)                 ;SURFTAB2 can't be less than 2
    (initget 6)
    (princ (strcat "\nEnter number of latitudinal segments for surface of "typ" "))
    (if (= typ "sphere")
      (princ "<16>: ")                ;Set default to 16 for a sphere
      (princ "<8>: ")                 ;Set default to 8 for a dome or dish
    )
    (setq numseg (getint))
    (if (null numseg)
      (if (= typ "sphere")
        (setq numseg 16)
        (setq numseg 8)
      )
    )
    (if (< numseg 2)
      (princ "\nNumber of segments must be greater than 1.")
    )
  )
  (setvar "SURFTAB2" numseg)

  (command "_.UCS" "_x" "90")
  (setq undoit T)

  (setq cen (trans cen 0 1))          ;Translate from WCS to UCS
  (cond
    ((= typ "sphere")
      (setq ax (list (car cen) (+ (cadr cen) r) (caddr cen)))
      (setq ax1 (list (car cen) (- (cadr cen) r) (caddr cen)))
      (command "_.LINE" ax ax1 "")      ;Draw axis of revolution
      (setq e1 (entlast))
      ;;Draw path curve
      (command "_.ARC" ax "_e" ax1 "_a" "180.0") 
      (setq e2 (entlast))
    )
    (t
      (if (= typ "dome")
        (setq ax (list (car cen) (+ (cadr cen) r) (caddr cen)))
        (setq ax (list (car cen) (- (cadr cen) r) (caddr cen)))
      )
      (command "_.LINE" cen ax "")      ;Draw axis of revolution
      (setq e1 (entlast))
      ;;Draw path curve
      (command "_.ARC" "_c" cen ax "_a" "90.0") 
      (setq e2 (entlast))
    )
  )

  ;;Draw dome or dish
  (command "_.REVSURF" (list e2 ax) (list e1 cen) "" "") 
  (entdel e1)                 
  (entdel e2)
  (command "_.UCS" "_prev")
)

;;;--------------------------------------------------------------------------
;;; Draw a torus

(defun torus (/ cen l trad numseg hrad tcen ax e1 e2)
  (setq numseg 0)
  (initget 17)                        ;3D point can't be null
  (setq cen (getpoint "\nSpecify center point of torus: "))
  (setq trad 0 l -1)
  (while (> trad (/ l 2.0))
    (initget 7 "Diameter")            ;Radius can't be 0, neg, or null
    (setq l (getdist cen "\nSpecify radius of torus or [Diameter]: "))
    (if (= l "Diameter")
      (progn
        (initget 7)                   ;Diameter can't be 0, neg, or null
        (setq l (/ (getdist cen "\nSpecify diameter of torus: ") 2.0))
      )
    )
    (initget 7 "Diameter")            ;Radius can't be 0, neg, or null
    (setq trad (getdist cen "\nSpecify radius of tube or [Diameter]: "))
    (if (= trad "Diameter")
      (progn
        (initget 7)
        (setq trad (/ (getdist cen "\nSpecify diameter of tube: ") 2.0))
      )
    )
    (if (> trad (/ l 2.0))
      (prompt "\nTube diameter cannot exceed torus radius.")
    )
  )
  (setq cen (trans cen 1 0))          ;Translate from UCS to WCS

  (while (< numseg 2)
    (initget 6)                       ;SURFTAB1 can't be 0 or neg
    (setq numseg (getint "\nEnter number of segments around tube circumference <16>: "))
    (if (null numseg)
      (setq numseg 16)
    )
    (if (< numseg 2)
      (princ "\nNumber of segments must be greater than 1.")
    )
  )
  (setvar "SURFTAB1" numseg)

  (setq numseg 0)
  (while (< numseg 2)
    (initget 6)                       ;SURFTAB2 can't be 0 or neg
    (setq numseg (getint "\nEnter number of segments around torus circumference <16>: "))
    (if (null numseg)
      (setq numseg 16)
    )
    (if (< numseg 2)
      (princ "\nNumber of segments must be greater than 1.")
    )
  )
  (setvar "SURFTAB2" numseg)

  (command "_.UCS" "_x" "90")
  (setq undoit T)

  (setq cen (trans cen 0 1))          ;Translate from WCS to UCS
  (setq hrad (- l (* trad 2.0)))
  (setq tcen (list (+ (+ (car cen) trad) hrad) (cadr cen) (caddr cen)))
  (setq ax (list (car cen) (+ (cadr cen) 2.0) (caddr cen)))

  (command "_.CIRCLE" tcen trad)        ;Draw path curve
  (setq e1 (entlast))
  (command "_.LINE" cen ax "")          ;Draw axis of revolution
  (setq e2 (entlast))
  (command "_.REVSURF" (list e1 tcen) (list e2 ax) "" "") ;Draw torus
  (entdel e1)            
  (entdel e2)
  (command "_.UCS" "_prev")
)

;;;--------------------------------------------------------------------------
;;; Draw a box or wedge

(defun boxwed (typ / pt1 l w h1 h2 a ang pt2 pt3 pt4 pt5 pt6 pt7 pt8 lockflag)
  (initget 17)                        ;3D point can't be null
  (setq pt1 (getpoint (strcat "\nSpecify corner point of "typ": ")))
  (setvar "ORTHOMODE" 1)
  (initget 7)                         ;Length can't be 0, neg, or null
  (setq l (getdist pt1 (strcat "\nSpecify length of "typ": ")))
  (setq pt3 (list (+ (car pt1) l) (cadr pt1) (caddr pt1)))
  (grdraw pt1 pt3 2)
  (cond 
    ((= typ "wedge")
      (initget 7)                     ;Width can't be 0, neg, or null
      (setq w (getdist pt1 "\nSpecify width of wedge: "))
    )
    (t 
      (initget 7 "Cube")              ;Width can't be 0, neg, or null
      (setq w (getdist pt1 "\nSpecify width of box or [Cube]: "))
      (if (= w "Cube") 
         (setq w l h1 l h2 l)
      )
    )
  )
  (setq pt2 (list (car pt1) (+ (cadr pt1) w) (caddr pt1)))
  (setq pt4 (list (car pt3) (+ (cadr pt3) w) (caddr pt3)))
  (grdraw pt3 pt4 2)
  (grdraw pt4 pt2 2)
  (grdraw pt2 pt1 2)
  (setvar "ORTHOMODE" 0)
  (cond 
    ((= typ "wedge")
      (initget 7)                     ;Height can't be 0, neg, or null
      (setq h1 (getdist pt1 (strcat "\nSpecify height of "typ": ")))
      (setq h2 0.0)
    )
    (t  
      (if (/= h1 l) 
        (progn
          (initget 7)                 ;Height can't be 0, neg, or null
          (setq h1 (getdist pt1 (strcat "\nSpecify height of "typ": ")))
          (setq h2 h1)
        )
      )
    )
  )

  (setq pt5 (list (car pt3) (cadr pt3) (+ (caddr pt3) h2)))
  (setq pt6 (list (car pt4) (cadr pt4) (+ (caddr pt4) h2)))
  (setq pt7 (list (car pt1) (cadr pt1) (+ (caddr pt1) h1)))
  (setq pt8 (list (car pt2) (cadr pt2) (+ (caddr pt2) h1)))
  (command "_.3DMESH" "6" "3" pt5 pt3 pt3 pt7 pt1 pt1 pt8 pt2
            pt1 pt6 pt4 pt3 pt6 pt6 pt5 pt8 pt8 pt7
  )            

  (setq undoit T)
  
  ;; Post special prompt.
  (if (= typ "box")
    (prompt "\nSpecify rotation angle of box about the Z axis or [Reference]: ")
    (prompt "\nSpecify rotation angle of wedge about the Z axis: ")
  )
  ;; Cannot ROTATE on locked layer. Temporarily unlock layer, if need be.
  (if (= 4 (logand 4 (cdr (assoc 70 (tblsearch "layer" (getvar "clayer"))))))
    (progn 
      (command "_.LAYER" "_UNLOCK" (getvar "clayer") "")
      (setq lockflag 1)
    )
  )  
  
  ;; Issue command.
  (command "_.ROTATE" (entlast) "" pt1)
  
  ;; Allow regular prompting.
  (setvar "cmdecho" 1)
  
  ;; There are a variable number of pauses possible, so keep
  ;; pausing until the ROTATE command completes. 
  (while (= 1 (logand (getvar "cmdactive") 1)) 
    (command pause)
  )
  
  ;; Reset cmdecho.
  (setvar "cmdecho" 0)
  
  ;; ReLock if need be.
  (if (= 1 lockflag)
    (command "._LAYER" "_LOCK" (getvar "clayer") "")
  )
)

;;;--------------------------------------------------------------------------
;;; Draw a pyramid

(defun pyramid (/ pt1 pt2 pt3 pt4 pt5 tp1 tp2 tp3 tp4)
  (initget 17)                        ;3D point can't be null
  (setq pt1 (getpoint "\nSpecify first corner point for base of pyramid: "))
  (initget 17)
  (setq pt2 (getpoint pt1 "\nSpecify second corner point for base of pyramid: "))
  (grdraw pt1 pt2 2)
  (initget 17)
  (setq pt3 (getpoint pt2 "\nSpecify third corner point for base of pyramid: ")) 
  (grdraw pt2 pt3 2)
  (initget 17 "Tetrahedron _Tetrahedron")          ;Choose 3 or 4 point base
  (setq pt4 (getpoint pt3 "\nSpecify fourth corner point for base of pyramid or [Tetrahedron]: ")) 
  (if (= pt4 "Tetrahedron")
    (grdraw pt3 pt1 2)
    (progn
      (grdraw pt3 pt4 2)
      (grdraw pt4 pt1 2)
    )
  )
  (cond 
    ((= pt4 "Tetrahedron")            ;3 point may have top or apex
      (initget 17 "Top _Top")
      (setq pt5 (getpoint "\nSpecify apex point of tetrahedron or [Top]: "))
    )
    (t                                ;4 point may have ridge, top, or apex
      (initget 17 "Top Ridge _Top Ridge") 
      (setq pt5 (getpoint "\nSpecify apex point of pyramid or [Ridge/Top]: "))
    )
  )
  (cond 
    ((= pt5 "Top")                    ;Prompt for top points
      (initget 17)
	  (if (= pt4 "Tetrahedron")
		(setq temp "tetrahedron")
		(setq temp "pyramid")
	  )
      (setq tp1 (getpoint pt1 (strcat "\nSpecify first corner point for top of "temp": ")))
      (grdraw pt1 tp1 2)
      (initget 17)
      (setq tp2 (getpoint pt2 (strcat "\nSpecify second corner point for top of "temp": ")))
      (grdraw tp1 tp2 2)
      (grdraw pt2 tp2 2)
      (initget 17)
      (setq tp3 (getpoint pt3 (strcat "\nSpecify third corner point for top of "temp": ")))
      (grdraw tp2 tp3 2)
      (grdraw pt3 tp3 2)
      (if (/= pt4 "Tetrahedron")
        (progn
          (initget 17)
          (setq tp4 (getpoint pt4 "\nSpecify fourth corner point for top of pyramid: "))
          (grdraw tp3 tp4 2)
          (grdraw pt4 tp4 2)
        )
      )
    )
    ((= pt5 "Ridge")                  ;Prompt for ridge points
      (grdraw pt4 pt1 2 -1)
      (initget 17)                
      (setq tp1 (getpoint "\nSpecify first ridge end point of pyramid: "))
      (grdraw pt4 pt1 2)
      (grdraw pt1 tp1 2)
      (grdraw pt4 tp1 2)
      (grdraw pt3 pt2 2 -1)
      (initget 17)                
      (setq tp2 (getpoint tp1 "\nSpecify second ridge end point of pyramid: "))
      (grdraw pt2 tp2 2)
      (grdraw pt3 tp2 2)
    )
    (t 
      (setq tp1 pt5)                  ;Must be apex
      (setq tp2 tp1)
    )
  )

  (cond 
    ((and (/= pt4 "Tetrahedron")(/= pt5 "Top"))
      (command "_.3DMESH" "4" "4" tp1 tp1 tp2 tp2 tp1 pt4 pt3 tp2 
                tp1 pt1 pt2 tp2 tp1 tp1 tp2 tp2
      )
    )
    ((and (/= pt4 "Tetrahedron")(= pt5 "Top"))
      (command "_.3DMESH" "5" "4" tp1 tp1 tp2 tp2 tp4 tp4 tp3 tp3
                tp4 pt4 pt3 tp3 tp1 pt1 pt2 tp2 tp1 tp1 tp2 tp2
      )
    )
    ((and (= pt4 "Tetrahedron")(/= pt5 "Top"))
      (command "_.3DMESH" "5" "2" tp1 pt2 pt3 pt2 pt3 pt1 tp1 pt1 
                tp1 pt2
      )
    )
    (t 
      (command "_.3DMESH" "4" "4" pt3 pt1 tp1 tp3 pt2 pt2 tp2 tp2
                pt3 pt3 tp3 tp3 pt3 pt1 tp1 tp3
      )
    )
  )     
)

;;;------------------------------------------------------------------------
;;; Draw a mesh
;;;
;;; Given a starting and an ending point, this function finds the next
;;; set of points in the N direction.

(defun next-n (pt1 pt2 / xinc yinc zinc loop pt)
  (setq xinc (/ (- (car pt2) (car pt1)) (1- n)))
  (setq yinc (/ (- (cadr pt2) (cadr pt1)) (1- n)))
  (setq zinc (/ (- (caddr pt2) (caddr pt1)) (1- n)))
  (setq loop (1- n))
  (setq pt pt1)
  (while (> loop 0)
    (setq pt (list (+ (car pt) xinc) (+ (cadr pt) yinc) (+ (caddr pt) zinc)))
    (command pt)
    (setq loop (1- loop))
  )
)

;;; This function finds the next point in the M direction.

(defun next-m (pt1 pt2 loop / xinc yinc zinc)
  (if (/= m loop)
    (progn
      (setq xinc (/ (- (car pt2) (car pt1)) (- m loop)))
      (setq yinc (/ (- (cadr pt2) (cadr pt1)) (- m loop)))
      (setq zinc (/ (- (caddr pt2) (caddr pt1)) (- m loop)))
    )
    (progn
      (setq xinc 0)
      (setq yinc 0)
      (setq zinc 0)
    )
  )
  (setq pt1 (list (+ (car pt1) xinc) (+ (cadr pt1) yinc) (+ (caddr pt1) zinc)))
)

(defun mesh (/ c1 c2 c3 c4 m n loop)
  (setq m 0 n 0)                      ;Initialize variables
  (initget 17)                     
  (setq c1 (getpoint "\nSpecify first corner point of mesh: "))
  (initget 17)                     
  (setq c2 (getpoint c1 "\nSpecify second corner point of mesh: "))
  (grdraw c1 c2 2)
  (initget 17)                     
  (setq c3 (getpoint c2 "\nSpecify third corner point of mesh: "))
  (grdraw c2 c3 2)
  (initget 17)                     
  (setq c4 (getpoint c3 "\nSpecify fourth corner point of mesh: "))
  (grdraw c3 c4 2)
  (grdraw c4 c1 2 1)
  (while (or (< m 2) (> m 256))
    (initget 7)                     
    (setq m (getint "\nEnter mesh size in the M direction: "))
    (if (or (< m 2) (> m 256)) 
      (princ "\nValue must be between 2 and 256.")
    )
  )
  (grdraw c4 c1 2)
  (grdraw c1 c2 2 1)
  (while (or (< n 2) (> n 256))
    (initget 7)                     
    (setq n (getint "\nEnter mesh size in the N direction: "))
    (if (or (< n 2) (> n 256)) 
      (princ "\nValue must be between 2 and 256.")
    )
  )
  (setvar "osmode" 0)                 ;Turn OSMODE off
  (setvar "blipmode" 0)               ;Turn BLIPMODE off
  (command "_.3DMESH" m n)
  (command c1)
  (setq loop 1)
  (next-n c1 c2)
  (while (< loop m)
    (setq c1 (next-m c1 c4 loop)) 
    (setq c2 (next-m c2 c3 loop))
    (command c1)
    (next-n c1 c2)
    (setq loop (1+ loop))
  )
)

;;;--------------------------------------------------------------------------
;;; Internal error handler

(defun 3derr (s)                      ;If an error (such as CTRL-C) occurs
                                      ;while this command is active...
  (setvar "cmdecho" 0)
  (if (/= s "Function cancelled")
    (princ (strcat "\nError: " s))
  )
  (if undoit
    (progn
      (command)
      (command "_.UNDO" "_e")            ;Terminate undo group
      (princ "\nundoing...") 
      (command "_.U")                   ;Erase partially drawn shape
    )
    (command "_.UNDO" "_e")               
  )
  (moder)                             ;Restore saved modes
  (if ofl
    (setvar "FLATLAND" ofl)
  )
  (command "_.REDRAWALL")
  (ai_undo_off)
  (setvar "CMDECHO" oce)              ;Restore saved cmdecho value
  (setq *error* olderr)               ;Restore old *error* handler
  (princ)
)

;;;--------------------------------------------------------------------------
;;;
;;; Main program.  Draws 3D object specified by "key" argument.
;;; If "key" is nil, asks which object is desired.

(defun 3d (key / olderr undo_setting)
  (if m:err                           ;If called from the menu
    (setq olderr m:err *error* 3derr) ;save the menus trapped *error*
    (setq olderr *error* *error* 3derr)
  )
  (setq undoit nil ofl nil)
  (setq oce (getvar "cmdecho"))
  (setvar "CMDECHO" 0)

  (ai_undo_on)                       ; Turn UNDO on

  (modes '("BLIPMODE" "ORTHOMODE" "OSMODE"
           "SURFTAB1" "SURFTAB2" "UCSFOLLOW"))
  ;Test for FLATLAND and FLATLAND's value.
  (if (/= (setq ofl (getvar "FLATLAND")) 0) 
    (setvar "FLATLAND" 0)             ;Set FLATLAND for duration
  )                                   ;of the function.
  (command "_.UNDO" "_group")
  (setvar "UCSFOLLOW" 0)
  (setvar "OSMODE" 0)
  (if (null key)
    (progn
      (initget "Box Cone DIsh DOme Mesh Pyramid Sphere Torus Wedge")
      (setq key (getkword 
        "\nEnter an option\n[Box/Cone/DIsh/DOme/Mesh/Pyramid/Sphere/Torus/Wedge]: "))
    )
  )
  (cond 
    ((= key "Box")     (boxwed  "box")   ) 
    ((= key "Cone")    (cone)            )
    ((= key "DIsh")    (spheres "dish")  )
    ((= key "DOme")    (spheres "dome")  )
    ((= key "Mesh")    (mesh)            )
    ((= key "Pyramid") (pyramid)         )
    ((= key "Sphere")  (spheres "sphere"))
    ((= key "Torus")   (torus)           )
    ((= key "Wedge")   (boxwed  "wedge") )
    (T nil)                           ;Null reply?  Just exit
  )
  (moder)                             ;Restore saved modes
  (if ofl
    (setvar "FLATLAND" ofl)
  )
  (command "_.REDRAWALL")
  (command "_.UNDO" "_E")             ;Terminate undo group

  (ai_undo_off)                       ; Return UNDO to initial state.

  (setvar "CMDECHO" oce)              ;Restore saved cmdecho value
  (setq *error* olderr)               ;Restore old *error* handler
  (princ)
)

;;;--------------------------------------------------------------------------
;;; C: function definitions

(defun C:AI_BOX ()     (3d "Box"))
(defun C:AI_CONE ()    (3d "Cone"))
(defun C:AI_DISH ()    (3d "DIsh"))
(defun C:AI_DOME ()    (3d "DOme"))
(defun C:AI_MESH ()    (3d "Mesh"))
(defun C:AI_PYRAMID () (3d "Pyramid"))
(defun C:AI_SPHERE ()  (3d "Sphere"))
(defun C:AI_TORUS ()   (3d "Torus"))
(defun C:AI_WEDGE ()   (3d "Wedge"))
(defun C:3D ()         (3d nil))

(princ "  3D Objects loaded.")
(princ)
