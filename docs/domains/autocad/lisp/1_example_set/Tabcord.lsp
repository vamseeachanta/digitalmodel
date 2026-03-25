;; ==================================================================== ;;
;;                                                                      ;;
;;  TABCORD.LSP - Fills the table in co-ordinates of LwPolyline         ;;
;;                vertexes, and also the centres and radiuses           ;;
;;                of arc segments. Marks vertexes of LwPolyline         ;;
;;                accordingly data in the table by digits or            ;;
;;                letters. Look section 'ADJUSTMENT' for                ;;
;;                acquaintance with options.                            ;;
;;                                                                      ;;
;; ==================================================================== ;;
;;                                                                      ;;
;;  Command(s) to call: TABCORD                                         ;;
;;                                                                      ;;
;;  Select LwPolyline and after the table will be generated             ;;
;;  insert it into the necessary place. After that vertexes of          ;;
;;  polylines will be marked by figures or letters.                     ;;
;;                                                                      ;;
;; ==================================================================== ;;
;;                                                                      ;;
;;  THIS PROGRAM AND PARTS OF IT MAY REPRODUCED BY ANY METHOD ON ANY    ;;
;;  MEDIUM FOR ANY REASON. YOU CAN USE OR MODIFY THIS PROGRAM OR        ;;
;;  PARTS OF IT ABSOLUTELY FREE.                                        ;;
;;                                                                      ;;
;;  THIS PROGRAM PROVIDES 'AS IS' WITH ALL FAULTS AND SPECIFICALLY      ;;
;;  DISCLAIMS ANY IMPLIED WARRANTY OF MERCHANTABILITY OR FITNESS        ;;
;;  FOR A PARTICULAR USE.                                               ;;
;;                                                                      ;;
;; ==================================================================== ;;
;;                                                                      ;;
;;  V1.3, 14th Aug 2008, Riga, Latvia                                   ;;
;;  © Aleksandr Smirnov (ASMI)                                          ;;
;;  For AutoCAD 2005 - 2008 (isn't tested in a next versions)           ;;
;;                                                                      ;;
;;                                   http://www.asmitools.com           ;;
;;                                                                      ;;
;; ==================================================================== ;;


(defun c:tabcord(/ aCen cAng cCen cPl cRad cReg
		fDr it lCnt lLst mSp pCen pT1
		pT2 ptLst R tHt tLst vlaPl vlaTab
		vLst cTxt oldCol nPl clFlg actDoc
		tPt1 tPt2 cAng tiPt oSnp *error*
		mType mHt oZin cAcu dHead hStr
		hHt w1 w2 w3 isPer isAre pMul aMul
		lWrt aVal xVal yVal)
  

;;;  ****************************************************************
;;;  *************************** ADJUSTMENT *************************
;;;  ****************************************************************

  (setq mType nil) 	; Markups mode. T - digits, NIL - letters
  
  (setq tHt -1.0)    	; Table text size. Positive - absolute,
                        ; negative multiplayer to TEXTSIZE variable
  
  (setq mHt -2.0)	; Markups text size. Positive - absolute,
                        ; negative - multiplayer to TEXTSIZE variable
  
  (setq cAcu 4)    	; Precision of coordinates (from 0 to 8)

  (setq dHead nil)   	; If T delete table header, if NIL not delete

  (setq hStr "Land # ") ; Standard header (if dHead not equal T)

  (setq hHt -1.25)      ; Header text size. Positive - absolute,
                        ; negative - multiplayer to TEXTSIZE variable
  
  (setq w1 -10.0)       ; 'Point' column width. Positive - absolute,
                        ; negative - multiplayer to TEXTSIZE variable

  (setq w2 -20.0)       ; 'X' and 'Y' colums width. Positive - absolute,
                        ; negative - multiplayer to TEXTSIZE variable

  (setq w3 -12.0)       ; 'Radius' column width. Positive - absolute,
                        ; negative - multiplayer to TEXTSIZE variable
  
  (setq isPer T)	; if T adds perimeter row

  (setq isAre T)        ; if T adds area row

  (setq isGCen T)       ; if T adds center of gravity row

  (setq pMul 0.001)     ; perimeter multiplayer

  (setq aMul 0.000001)  ; area  multiplayer

;;;  ****************************************************************
;;;  ************************* END ADJUSTMENT ***********************
;;;  ****************************************************************
  
  (if(minusp tHt)
    (setq tHt(getvar "TEXTSIZE"))
    ); end if

  (if(minusp mHt)
    (setq mHt(*(abs mHt)(getvar "TEXTSIZE")))
    ); end if

    (if(minusp hHt)
    (setq hHt(*(abs hHt)(getvar "TEXTSIZE")))
    ); end if

  (if(minusp w1)
    (setq w1(*(abs w1)(getvar "TEXTSIZE")))
    ); end if

  (if(minusp w2)
    (setq w2(*(abs w2)(getvar "TEXTSIZE")))
    ); end if

  (if(minusp w3)
    (setq w3(*(abs w3)(getvar "TEXTSIZE")))
    ); end if

  (vl-load-com)

  (defun Get_Acad_Ver(Gen_Only)
    (if Gen_Only
     (substr(getvar "ACADVER") 1 2)
     (substr(getvar "ACADVER") 1 4)
    ); end if
  ); and of Get_Acad_Ver
  
  (defun Extract_DXF_Values(Ent Code)
    (mapcar 'cdr
     (vl-remove-if-not
      '(lambda(a)(=(car a)Code))
	 (entget Ent)))
    ); end of


  (defun *error*(msg)
    (setvar "CMDECHO" 1)
    (if oSnp(setvar "OSMODE" oSnp))
    (if oZin(setvar "DIMZIN" oZin))
    (if mSp(vla-EndUndoMark actDoc))
    (princ)
    ); end of *error*

  (defun Alph_Num(Counter / lLst cRes)
  (setq lLst '("A" "B" "C" "D" "E" "F" "G" "H" "I" "J"
	       "K" "L" "M" "N" "O" "P" "Q" "R" "S" "T"
	       "U" "V" "W" "X" "Y" "Z"))
  (if(<= 1.0(setq cRes(/ Counter 26.0)))
     (strcat(itoa(fix cRes))
	   (nth(- Counter(* 26(fix cRes)))lLst))
     (nth Counter lLst)
    ); end if
  ); end of Alph_Num


(if(<= 16.1(atof(Get_Acad_Ver nil)))
  (progn
  (if
    (and
      (setq cPl(entsel "\nSelect LwPoliline > "))
      (= "LWPOLYLINE"(car(Extract_DXF_Values(car cPl)0)))
      ); end and
	(progn
	  (princ "\nPlease Wait... \n")
	  (setq vlaPl(vlax-ename->vla-object(car cPl))
		ptLst(mapcar 'append
			       (setq vLst(Extract_DXF_Values(car cPl)10))
			       (mapcar 'list(Extract_DXF_Values(car cPl)42)))
		r 2 lCnt 0
		tLst '((1 0 "Point")(1 1 "X")(1 2 "Y")(1 3 "Radius"))
		actDoc(vla-get-ActiveDocument
		       (vlax-get-acad-object))
		mSp(vla-get-ModelSpace actDoc)
		    ); end setq
	    (setvar "CMDECHO" 0)
	    (setq oSnp(getvar "OSMODE"))
	    (setq oZin(getvar "DIMZIN"))
	    (setvar "DIMZIN" 0)
	    (vla-StartUndoMark actDoc)
	    (foreach vert ptLst
	      (setq vert(trans vert 0 1)
		    tLst(append tLst
			  (list(list r 0(if mType
					  (itoa(1+ lCnt))
					    (Alph_Num lCnt)))
			  (list r 1(rtos(car vert)2 cAcu))
			  (list r 2(rtos(cadr vert)2 cAcu))
			  (list r 3 ""))))
	      (if(and
		   (/= 0.0(last vert))
		    (setq pt1(vlax-curve-GetPointAtParam vlaPl lCnt))
		    (setq pt2(vlax-curve-GetPointAtParam vlaPl(1+ lCnt)))
		   ); end and
		(setq r(1+ r)
		      cRad(abs(/(distance pt1 pt2)
			  2(sin(/(* 4(atan(abs(last vert))))2))))
		      aCen(vlax-curve-GetPointAtParam vlaPl(+ 0.5 lCnt))
		      fDr(vlax-curve-getFirstDeriv vlaPl
			   (vlax-curve-getParamAtPoint vlaPl aCen))
		      pCen(trans
			    (polar aCen(-(if(minusp(last vert)) pi(* 2 pi))
			      (atan(/(car fDr)(cadr fDr))))cRad)0 1)
		      tLst(append tLst(list
			    (list r 0 "center")
			    (list r 1(rtos(car pCen)2 cAcu))
			    (list r 2(rtos(cadr pCen)2 cAcu))
			    (list r 3(rtos cRad 2 cAcu))))
		      ); end setq
		); end if
	      (setq r(1+ r) lCnt(1+ lCnt))
	      ); end foreach
	  (setq vlaTab(vla-AddTable mSp (vlax-3D-point '(0 0 0))
			(+ 1(/(length tLst)4)) 4 (* 3 tHt)w2))
	  (foreach i tLst
	    (vl-catch-all-apply 'vla-SetText(cons vlaTab i))  
	    (vla-SetCellTextHeight vlaTab(car i)(cadr i)tHt)
	    (vla-SetCellAlignment vlaTab(car i)(cadr i)acMiddleCenter)
	    ); end foreach
	  (if(or isPer isAre)
	    (progn
	   (vla-InsertRows vlaTab r(* 0.05 tHt)1)
	   (vla-SetCellTextHeight vlaTab r 0(* 0.05 tHt))
	   (setq r(1+ r))
	      ); end progn
	    ); end if
	  (if isPer
	    (progn
	      (if(= :vlax-true(vla-get-Closed vlaPl))
	        (setq lWrt "Perimeter")
		(setq lWrt "Length")
	       ); end if
	      (vla-InsertRows vlaTab r tHt 1)
	      (vla-SetText vlaTab r 0 lWrt)
	      (vla-SetText vlaTab r 1
		(rtos(*(vla-get-Length vlaPl)pMul)2 cAcu))
	      (vla-SetCellTextHeight vlaTab r 0 tHt)
	      (vla-SetCellTextHeight vlaTab r 1 tHt)
	      (setq r(1+ r))
	      ); end progn
	    ); end if
	  (if isAre
	    (progn
	      (if(= :vlax-true(vla-get-Closed vlaPl))
	        (setq aVal (rtos(*(vla-get-Area vlaPl)aMul)2 cAcu))
		(setq aVal "Not closed contour")
	       ); end if
	      (vla-InsertRows vlaTab r tHt 1)
	      (vla-SetText vlaTab r 0 "Area")
	      (vla-SetText vlaTab r 1 aVal)
	      (vla-SetCellTextHeight vlaTab r 0 tHt)
	      (vla-SetCellTextHeight vlaTab r 1 tHt)
	      (setq r(1+ r))
	      ); end progn
	    ); end if
	  (if(= :vlax-true(vla-get-Closed vlaPl))
	    (progn
	     (setq nPl(vla-Copy vlaPl))
	     (command "_.region" (entlast) "")
	     (setq cCen(vlax-get(setq cReg
		 (vlax-ename->vla-object(entlast)))'Centroid))
	      (vla-Delete cReg)
	      (setq clFlg T)
	     ); end progn
	    ); end if
	  (if isAre
	    (progn
	      (if cCen
	        (setq xVal(rtos(car cCen)2 cAcu)
		      yVal (rtos(cadr cCen)2 cAcu))
		(setq xVal "-"
		      yVal "-")
	       ); end if
	      (vla-InsertRows vlaTab r tHt 1)
	      (vla-SetText vlaTab r 0 "Gravity Center")
	      (vla-SetText vlaTab r 1 xVal)
	      (vla-SetText vlaTab r 2 yVal)
	      (vla-SetCellTextHeight vlaTab r 0 tHt)
	      (vla-SetCellTextHeight vlaTab r 1 tHt)
	      (vla-SetCellTextHeight vlaTab r 2 tHt)
	      (setq r(1+ r))
	      ); end progn
	    ); end if
	  (vla-put-VertCellMargin vlaTab (* 0.75 tHt))
	  (vla-SetColumnWidth vlaTab 0 w1)
	  (vla-SetColumnWidth vlaTab 3 w3)
	  (if(vlax-property-available-p vlaTab 'RepeatTopLabels)
	    (vla-put-RepeatTopLabels vlaTab :vlax-true)
	    ); end if
	  (if(vlax-property-available-p vlaTab 'BreakSpacing)
	    (vla-put-BreakSpacing vlaTab (* 3 tHt))
	    ); end if
	   (if dHead
	     (vla-DeleteRows  vlaTab 0 1)
	     (progn
	       (vla-SetText vlaTab 0 0 hStr)
	       (vla-SetCellTextHeight vlaTab 0 0 hHt)
	      ); end progn
	    ); end if
	  (vla-put-Height vlaTab(* 1.75(/(length tLst)4)))
	  (princ "\n<<< Place Table >>> ")
	  (command "_.copybase" (trans '(0 0 0)0 1)(entlast) "")
	  (command "_.erase" (entlast) "")
	  (command "_.pasteclip" pause)
	  (setq lCnt 0)
	  (foreach v vLst
	    (if clFlg
	      (setq cAng(angle cCen(trans v 0 1))
	            iPt(polar v cAng (* 2 mHt)))
	      (setq tPt1(vlax-curve-GetPointAtParam vlaPl
			  (- lCnt 0.0000001))
		    tPt2(vlax-curve-GetPointAtParam vlaPl
			  (+ lCnt 0.0000001))
		    iPt(polar v(+(* pi 0.5)(if(minusp
			(setq cAng(angle tPt1(if tPt2 tPt2
			   (polar tPt1(* 0.5 pi)0.0000001)))))
			cAng(- cAng)))(* 2 mHt))
		    ); end setq
	      ); end if
	    (setvar "OSMODE" 0)
	    (setq cTxt(vla-AddText mSp
		      (if mType(itoa(1+ lCnt))(Alph_Num lCnt))
		       (vlax-3d-point iPt) mHt)
		  tiPt(vla-get-InsertionPoint cTxt)
		  lCnt(1+ lCnt)
		  ); end setq
	    (vla-put-Alignment cTxt 10)
	    (vla-put-TextAlignmentPoint cTxt tiPt)
	    (setq oldCol(getvar "CECOLOR"))
	    (setvar "CECOLOR" "1")
	    (command "_.circle"(trans v 0 1) (/ mHt 4))
	    (setvar "CECOLOR" oldCol)
	    ); end foreach
	  (setvar "DIMZIN" oZin)
	  (setvar "OSMODE" oSnp)
	  (setvar "CMDECHO" 1)
	  (vla-EndUndoMark actDoc)
	  ); end progn
     (princ "\n<!> It isn't LwPolyline! Quit. <!> ")
    ); end if
   ); end progn
  (princ "\n<!> This program works in AutoCAD 2005+ only! <!> " )
  );end if
    (gc)
  (princ)
 ); end of c:tabcord

(princ "\n[Info] http:\\\\www.AsmiTools.com [Info]")
(princ "\n[Info] Type TABCORD to fill table of LwPolyline coordinates [Info]")









