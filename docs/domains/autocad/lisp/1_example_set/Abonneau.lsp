;; Function créé par Patrick Morel pour Trial Design

;^C^C_break \_f \@ 
(defun C:breakat (/)
 (princ "\nBreak Object at selected point.")
 (setq ent (car (entsel "\nSelect object: "))) 
 (setq p (getpoint "\nSpecify break point? "))
 (command "circle" p "5")
 (setq ent2 (entlast))
 (princ "\n") 
 (command "_break" ent p p)
 (command "erase" ent2 "") 
 ;(command "_break" ent "f" p "@")
 )

;---------- racourci ------------------

;(defun C:E9 (/)
; (command "CLAYER" "FRAME")
;  )

(defun C:mv (/)  
 (command "CLAYER" "FRAME" "mview" pause pause "_layerp")
  )

(defun C:ol (/)  
 (command "CLAYER" "FRAME" "offset" "L" "C" ".55" pause pause "exit" "_layerp")
  )

(defun C:gt (/)  
 (c:GATTE)
  )

(defun C:ttr (/)
 (command "circle" "ttr")
  )

(defun C:c3t (/)  
 (command "circle" "3p" "TAN" pause "TAN" pause "NEAR" pause)
 (princ)
  )

(defun C:c3 (/)
 (command "circle" "3p")
  )

(defun C:c2 (/)
 (command "circle" "2p")
  )

(defun C:dw (/)
 (command "dwgprops")
  )

(defun C:dd (/)
 (command "dimscale")
  )

(defun C:XH (/)
 (command "XLINE" "H")
  )

(defun C:XV (/)
 (command "XLINE" "V")
  )

(defun C:D1 (/)
 (command "DIMLINEAR")
  )

(defun C:D2 (/)
 (command "DIMALIGNED")
  )

(defun C:D3 (/)
 (command "DIMRADIUS")
  )

(defun C:D4 (/)
 (command "DIMDIAMETER")
  )

(defun C:D5 (/)
 (command "DIMANGULAR")
  )

(defun C:D6 (/)
 (command "QLEADER")
  )

(defun C:D7 (/)
 (command "DIMARC")
  )

(defun C:VT (/)
 (command "VIEW" "TOP")
  )

(defun C:VB (/)
 (command "VIEW" "BOTTOM")
  )

(defun C:VD (/)
 (command "VIEW" "RIGHT")
  )

(defun C:VG (/)
 (command "VIEW" "LEFT")
  )

(defun C:VF (/)
 (command "VIEW" "FRONT")
  )

(defun C:VA (/)
 (command "VIEW" "BACK")
  )

(defun C:V1 (/)
 (command "VIEW" "SWISO")
  )

(defun C:V2 (/)
 (command "VIEW" "SEISO")
  )

(defun C:V3 (/)
 (command "VIEW" "NEISO")
  )

(defun C:V4 (/)
 (command "VIEW" "NWISO")
  )

(defun C:zz (/)
 (command "ZOOM" "O")
  )

(defun C:CU (/)
 (command "LAYMCUR")
  )

(defun C:LC (/)
 (command "LAYMCUR")
  )

(defun C:LI (/)
 (command "LAYISO")
  )

(defun C:LO (/)
 (command "LAYON")
  )

(defun C:LF (/)
 (command "LAYOFF")
  )

(defun C:EL (/)
 (command "ELLIPSE" "C")
  )

(defun C:LZ (/)
 (command "LAYFRZ")
  )

(defun C:LT (/)
 (command "LAYTHW")
  )

(defun C:E1 (/)
 (command "CLAYER" "LIGNE")
  )

(defun C:E2 (/)
 (command "CLAYER" "LIGNE 1")
  )

(defun C:E3 (/)
 (command "CLAYER" "HID")
  )

(defun C:E4 (/)
 (command "CLAYER" "HATCH")
  )

(defun C:E5 (/)
 (command "CLAYER" "HATCH 1")
  )

(defun C:E6 (/)
 (command "CLAYER" "HATCH 2")
  )

(defun C:E7 (/)
 (command "CLAYER" "DIM")
  )

(defun C:E8 (/)
 (command "CLAYER" "DIM DÉTAIL")
  )

(defun C:E9 (/)
 (command "CLAYER" "FRAME")
  )

(defun C:E0 (/)
 (command "CLAYER" "SHADE")
  )

;----------------------------

(defun C:lk (/)
  (command "_.PSPACE" "mview" "l" "on" "all" "")
)

(defun C:uk (/)
  (command "_.PSPACE" "mview" "l" "off" "all" "")
)


(defun C:ToF (DecVal /)
  (setq tempStr (rtos (fix (ABS DecVal))))
  (princ tempStr)
)



;----------------------------


(defun C:zf (/)
  (command "model" "ZOOM" "W" "0,0" "2.125,5.156")
)


(defun C:tsc (/)
  ;(princ "point 1?")

  ;(if (paper)
    ;(princ "\npaper" )
   ; (princ "\nmodel")
  ;)
  ;(princ (strcat "\n --> " (rtos(c:vsc)) " : " (rtos(getvar "dimscale"))))

  (if (= (getvar "dimscale") 0)
    (princ (strcat "\n ViewPort Scale Selected --> " (rtos(c:vsc))))
    (princ (strcat "\n DimScale Selected --> " (rtos(getvar "dimscale"))))
  )
  
  (setq StarPt (getpoint "\nSelect start point? "))
  ;(setq EndPt (getpoint "Point 2?"))
  ;(command "mtext" StarPt "H" (* (getvar "dimscale") 0.0875))
  ;(princ (rtos (c:vsc)))

  ;(setq Lastlayer (getvar "clayer"))
  ;(command "-layer" "set" "Dim" "")

  (if (= (getvar "dimscale") 0)
    (command "mtext" StarPt "H" (* (c:vsc) 0.0875))
    (command "mtext" StarPt "H" (* (getvar "dimscale") 0.0875))
  )

  ;di(command "-layer" "set" Lastlayer "")

  ;(command "mtext" StarPt "H" (* (c:vsc) 0.0875))
  ;(command "-_mtext")
)




(defun C:Fina  (/)
(if (= (getvar "dimscale") 0)
    (princ (strcat "\n ViewPort Scale Selected --> " (rtos(c:vsc))))
    (princ (strcat "\n DimScale Selected --> " (rtos(getvar "dimscale"))))
  )
  
(if (= (getvar "dimscale") 0)
    (setq ScaleVal (* (c:vsc) 1.0))
    (setq ScaleVal (* (getvar "dimscale") 1.0))
  )

  (setq FiniStr (getstring  "\nQuel est le FINI? "))
  (setq StarPt (getpoint "\nSelect start point? "))
  (setq EndPt (getpoint "\nSelect end point? "))
  (setq pt2 (list (/(+ (car StarPt) (car EndPt)) 2) (cadr EndPt)))
  (if (< (car EndPt) (car StarPt))
  	(setq pt3 (list (+ (car EndPt) (* ScaleVal 0.15625)) (cadr EndPt)))
  	(setq pt3 (list (- (car EndPt) (* ScaleVal 0.15625)) (cadr EndPt)))
  )
  (command "-layer" "set" "Dim1" "")
  (command "-insert" "fini" "S" (* ScaleVal 1) EndPt 0 FiniStr)
  (command "_qleader" StarPt pt2 pt3 "" (* ScaleVal 0.0875) " " "")


  ;(command "_qleader" StarPt pt2 pt3 "" "fini" "S" ScaleVal EndPt 0 FiniStr)

  
  ;(if (= (getvar "dimscale") 0)
     ;(command "_qleader" StarPt pt2 EndPt "" "fini" "S" (* (c:vsc) 1.0) EndPt 0 FiniStr)
     ;(command "_qleader" StarPt pt2 EndPt "" "fini" "S"  (* (getvar "dimscale") 1.0) EndPt 0 FiniStr)
  ;)
  
)

(defun C:Fini  (/)

  (if (= (getvar "dimscale") 0)
    (setq ScaleVal (* (c:vsc) 1.0))
    (setq ScaleVal (* (getvar "dimscale") 1.0))
  )

  (if (= (getvar "dimscale") 0)
    (princ (strcat "\n ViewPort Scale Selected --> " (rtos(c:vsc))))
    (princ (strcat "\n DimScale Selected --> " (rtos(getvar "dimscale"))))
  )
  

  (setq FiniStr (getstring  "\nQuel est le FINI? "))
  (setq StarPt (getpoint "\nSelect start point? "))
  (setq EndPt (getpoint "\nSelect end point? "))
  (setq pt2 (list (/(+ (car StarPt) (car EndPt)) 2) (cadr EndPt)))
  (if (< (car EndPt) (car StarPt))
  	(setq pt3 (list (+ (car EndPt) (* ScaleVal 0.15625)) (cadr EndPt)))
  	(setq pt3 (list (- (car EndPt) (* ScaleVal 0.15625)) (cadr EndPt)))
  )
  (setq Lastlayer (getvar "clayer"))
  (setq aosmo (getvar "OSMODE"))
  (setvar "OSMODE" 0)
  ;(command "-layer" "set" "Dim" "")
  (command "leader" StarPt pt2 pt3 "" "" "block" "fini" EndPt ScaleVal ScaleVal 0 FiniStr) ;(* ScaleVal 0.0875) " " "")
  (command "-layer" "set" Lastlayer "")
  (setvar "OSMODE" aosmo)


  ;(command "_qleader" StarPt pt2 pt3 "" "fini" "S" ScaleVal EndPt 0 FiniStr)

  
  ;(if (= (getvar "dimscale") 0)
     ;(command "_qleader" StarPt pt2 EndPt "" "fini" "S" (* (c:vsc) 1.0) EndPt 0 FiniStr)
     ;(command "_qleader" StarPt pt2 EndPt "" "fini" "S"  (* (getvar "dimscale") 1.0) EndPt 0 FiniStr)
  ;)
  
)





;
;--- paper -------------------------------------------------
; returns T if in paper space
(defun paper ()
(> 2 (getvar "cvport") (getvar "tilemode")) ; port=1 & tile=0
)
;
;--- getx --------------------------------------------------
; return <nth> dotted pair of the extended entity data
; from an entity association list <data>
;
(defun getx (n data)
(nth n (cdadr (assoc -3 data)))
)
;
;
;--- sslist -----------------------------------------------
; convert selection-set <SS> into a list of entities <P>
; result is the list <P>
; Example: (sslist <selection-set>)
;
(defun sslist (SS / N P)
	(repeat (setq N (sslength SS)) ;seed N
			(setq N (1- N) ;index number
				P (cons (ssname SS N) P)
			) ;setq
		) ;repeat
	) ; sslist
;
;
;--- findvp ------------------------------------------------
; find viewport data of current viewport
;

(defun findvp (vp# / vplist)
(setq vplist (ssget "X" '((0 . "viewport")))
	vplist (sslist vplist)
)
  
(while
	(and (/= (dxf 69) vp#)
		(setq ent (car vplist))
	)
  
	(setq data (entget ent)
	      
	vplist (cdr vplist)

	)
) ;while
ent
)
;


;--- c:vscale ----------------------------------------------
; get the xp scale factor of a pspace viewport
;
(defun c:vscale (/ dxf ent data cvsize cvhgt)
(defun dxf (code) (cdr (assoc code data)))
(if (paper)
(setq ent (car (entsel "\nSelect edge of viewport: ")))
(setq ent (findvp (getvar "cvport")))
)
(cond
((and
ent
(setq data (entget ent '("ACAD")))
(= "VIEWPORT" (dxf 0))
) ;and
(setq cvhgt (dxf 41) ; viewport height
cvsize (cdr (getx 6 data)) ; viewsize from extended data
)
(prompt "\nPS:MS == ")
(cond
((< cvsize cvhgt)
(princ (rtos (/ cvhgt cvsize)))
(princ (strcat ":-" (rtos 1)))
)
(T
(princ (strcat (rtos 1) ":"))
(princ (rtos (/ cvsize cvhgt)))
)
) ;cond
)
(T (prompt " no viewport found."))
) ;cond
(princ)
) ;c:vscale
;




(defun c:vsc (/ dxf ent data cvsize cvhgt)
	(defun dxf (code) (cdr (assoc code data)))
	(if (paper)
		(setq ent (car (entsel "\nSelect edge of viewport: ")))
		(setq ent (findvp (getvar "cvport")))
	)
  
(cond
	(
	 	(and
			ent
			(setq data (entget ent '("ACAD")))
			(= "VIEWPORT" (dxf 0))
		) ;and
		(setq cvhgt (dxf 41) ; viewport height
			cvsize (cdr (getx 6 data)) ; viewsize from extended data
		)
			;(prompt "\nPS:MS == ")
		(cond
			((< cvsize cvhgt)
				;(princ (rtos (/ cvhgt cvsize)))
				(SETQ Vsq  (/ cvhgt cvsize))
			)

			(t
				;(princ (strcat (rtos 1) ":"))
				;(princ (rtos (/ cvsize cvhgt)))
				(setq vsq(/ cvsize cvhgt))
			)
		) ;cond
 
	)
	(T
	 ;(prompt " no viewport found.")
	 (setq vsq (getvar "dimscale"))
	)
) ;cond
  ;(princ)
  ;(princ (RTOS vsq))
  vsq
  
)

  ;c:vscale
;



;Tip1687:   VPLIM.LSP     Pspace Limits in Mspace (c)2001, Murray Clack
 
 ;VPLIM.lsp draws the limits of a Paperspace Viewport Boundary in MODELSPACE
 ;VP - Get ViewPort object
 ;HT - Get Outside HeighT of Viewport
 ;WD - Get Oustide WiDth of Viewport
 ;VN - Get Viewport Number
 ;CTR - Get CenTeR of Viewport
 ;CTRX - Caluculate X of Viewport CenTeR
 ;CTRY - Caluculate Y of Viewport CenTeR
 ;VS - Get View Size of viewport
 ;XP - Calculate XP factor of viewport
 ;IW - Calculate Width of viewport
 ;BL - Calculate Bottom Left corner of viewport
 ;BR - Calculate Bottom Right corner of viewport
 ;TR - Calculate Top Right corner of viewport
 ;TL - Calculate Top Left corner of viewport
 ;PW - Save PlineWid
 ;OS - Save OSmode

 ;load statement
(prompt "\nVPLIM.lsp loaded.  Enter VPL to execute ")



 ;start function and define variables
(defun
     C:VPL (/ VP HT WD VN CTR CTRX CTRY VS XP IW BL BR TR TL PW OS
           )

 ;turn off command echoing
  (setvar "cmdecho" 0)

 ;enter pspace
  (command ".pspace")

 ;select viewport boundary
  (setq VP
         (entget
           (car (entsel
                  "\nSelect Viewport to Draw Boundary 
             in "
                ) ;_ end of entsel
           ) ;_ end of car
         ) ;_ end of entget
  ) ;_ end of setq

 ;Get Viewport height with 
  (setq HT (cdr (assoc 41 VP)))

 ;Get Viewport width with 
  (setq WD (cdr (assoc 40 VP)))

 ;Get Viewport Number
  (setq VN (cdr (assoc 69 VP)))

 ;enter mspace
  (command ".mspace")

 ;set correct viewport
  (setvar "cvport" VN)

 ;set UCS to View
  (command ".ucs" "v")

 ;Get VIEWCTR store as CTR
  (setq CTR (getvar "viewctr"))

 ;Get X of CTR 
  (setq CTRX (car CTR))

 ;Get Y of CTR 
  (setq CTRY (cadr CTR))

 ;Get inside Viewport height
  (setq VS (getvar "viewsize"))

 ;Get XP Factor with HeighT / View Size
  (setq XP (/ HT VS))

 ;Get inside width of Viewport by 
  (setq IW (* (/ VS HT) WD))

 ;Find four corners of Viewport
  (setq BL (list (- CTRX (/ IW 2)) (- CTRY (/ VS 2))))

  (setq BR (list (+ CTRX (/ IW 2)) (- CTRY (/ VS 2))))

  (setq TR (list (+ CTRX (/ IW 2)) (+ CTRY (/ VS 2))))

  (setq TL (list (- CTRX (/ IW 2)) (+ CTRY (/ VS 2))))

 ;Save current pline width
  (setq PW (getvar "plinewid"))

 ;Set Pline width to zero
  (setvar "plinewid" 0)

 ;Save current osmode
  (setq OS (getvar "osmode"))
 ; Save current Layer 
  (setq curlayer (getvar "clayer"))

 ;SET osmode
  (setvar "osmode" 0)



(if  (not (tblsearch "LAYER" "frame"))
      (command ".LAYER" "M" "frame" "C" 1 "" "")
      (command ".LAYER" "Set" "frame" ""))


 ;Draw pline inside border
  (command ".pline" BL BR TR TL "c")

 ;Restore pline width back
  (setvar "plinewid" PW)

 ;Restore UCS back
  (command ".ucs" "p")

 ;Restore osmode
  (setvar "osmode" OS)
  (setvar "clayer" curlayer)

 ;Clean up command prompt
  (princ)

 ;Go Back To Papserspace
  (command ".pspace")

) ;The End!

;FROM THE DESK OF PAUL STANDING
; MOVE LAYER 1995 (WILL MOVE A SINGLE LAYER THAT THE USER SUPPLIES)
(DEFUN C:MLAYER ()
(SETVAR "CMDECHO" 0)
(SETQ LAY (GETSTRING "\nENTER LAYER TO MOVE: "))
(SETQ SS (SSGET "X" (LIST (CONS 8 LAY))))
(prompt "Base point or displacement: ")
(COMMAND "MOVE" SS ""  PAUSE)
(SETVAR "CMDECHO" 1)
(setq ss nil)
(PRINC)
)

;;; PlineOrg (2.0) -Gilles Chanteau- 15/09/07
;;; Change le point de départ de la polyligne fermée
;;; Le nouveau point de départ peut être un situé n'importe où sur la polyligne

(defun c:plineorg (/ erreur os pt pl plst norm nb n blst pa d1 d2 d3)

  (vl-load-com)

  (defun erreur	(msg)
    (if	(= msg "Fonction annulée")
      (princ)
      (princ (strcat "\nErreur: " msg))
    )
    (setvar "OSMODE" os)
    (setq *error* m:err
	  m:err	nil
    )
  )

  (setq	m:err	*error*
	*error*	erreur
	os	(getvar "OSMODE")
  )
  (setvar "OSMODE" 515)
  (if (and
	(setq pt
	       (getpoint
		 "\nSélectionnez le nouveau point de départ sur la polyligne: "
	       )
	)
	(setq pl (car (nentselp pt)))
	(setq pl (vlax-ename->vla-object pl))
	(= (vla-get-ObjectName pl) "AcDbPolyline")
	(= (vla-get-Closed pl) :vlax-true)
      )
    (progn
      (vla-StartUndoMark (vla-get-ActiveDocument (vlax-get-acad-object)))
      (setq plst (vlax-get pl 'Coordinates)
	    norm (vlax-get pl 'Normal)
	    pt   (trans pt 1 0)
	    pa	 (vlax-curve-getParamAtPoint pl pt)
	    nb	 (/ (length plst) 2)
	    n	 nb
      )
      (repeat n
	(setq blst (cons (vla-getBulge pl (setq n (1- n))) blst))
      )
      (if (= pa (fix pa))
	(setq n	   (fix pa)
	      plst (append (sublist plst (* 2 n) nil)
			   (sublist plst 0 (* 2 n))
		   )
	      blst (append (sublist blst n nil) (sublist blst 0 n))
	)
	(setq n	   (1+ (fix pa))
	      d3 (vlax-curve-getDistAtParam pl n)
	      d2 (- d3 (vlax-curve-getDistAtPoint pl pt))
	      d3 (- d3 (vlax-curve-getDistAtParam pl (1- n)))
	      d1 (- d3 d2)
	      pt   (trans pt 0 (vlax-get pl 'Normal))
	      plst (append (list (car pt) (cadr pt))
			   (sublist plst (* 2 n) nil)
			   (sublist plst 0 (* 2 n))
		   )
	      blst (append (list (k*bulge (nth (1- n) blst) (/ d2 d3)))
			   (sublist blst n nil)
			   (sublist blst 0 (1- n))
			   (list (k*bulge (nth (1- n) blst) (/ d1 d3)))
		   )
	)
      )
      (vlax-put pl 'coordinates plst)
      (repeat (setq n (length blst))
	(vla-setBulge pl (setq n (1- n)) (nth n blst))
      )
      (vla-EndUndoMark (vla-get-ActiveDocument (vlax-get-acad-object)))
    )
    (prompt "\nEntité non valide.")
  )
  (princ)
)

;;; SUBLIST Retourne une sous-liste
;;;
;;; Arguments
;;; lst : une liste
;;; start : l'index de départ de la sous liste (premier élément = 0)
;;; leng : la longueur (nombre d'éléments) de la sous-liste (ou nil)
;;;
;;; Exemples :
;;; (sublist '(1 2 3 4 5 6) 2 2) -> (3 4)
;;; (sublist '(1 2 3 4 5 6) 2 nil) -> (3 4 5 6)

(defun sublist (lst start leng / n r)
  (if (or (not leng) (< (- (length lst) start) leng))
    (setq leng (- (length lst) start))
  )
  (setq n (+ start leng))
  (repeat leng
      (setq r (cons (nth (setq n (1- n)) lst) r))
    )
)

;; K*BULGE
;; Retourne le bulge proportionnel au bulge de référence
;; Arguments :
;; b : le bulge
;; k : le rapport de proportion (entre les angles ou les longueurs d'arcs)

(defun k*bulge (b k / a)
  (setq a (atan b))
  (/ (sin (* k a)) (cos (* k a)))
)

