;; ==================================================================== ;;
;;                                                                      ;;
;;  FHAND.LSP - Converts objects to 'freehand' style. For LINES         ;;
;;              POLYLINES, CIRCLES, ARCS, ELLIPSES or SPLINES.          ;;
;;                                                                      ;;
;; ==================================================================== ;;
;;                                                                      ;;
;;  Command(s) to call: FHAND                                           ;;
;;                                                                      ;;
;;  Specify averange segment length, curves offset and select           ;;
;;  objects.                                                            ;;
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
;;  V1.0,  1st Feb 2007, Riga, Latvia                                   ;;
;;  © Aleksandr Smirnov (ASMI)                                          ;;
;;  For AutoCAD 2000 - 2008 (isn't tested in a next versions)           ;;
;;                                                                      ;;
;;                             http://www.asmitools.com                 ;;
;;                                                                      ;;
;; ==================================================================== ;;

(defun c:fhand(/ cLen curAng curvLst curvSet drLst enPt newPl newPt
	       nextDis nextPt oldDis oldTol pdMax pdMin stPt sumDis
	       oldOsn actDoc unlLst *error*)

  (vl-load-com)

; Credits for randnum.lsp: Paul Furman, 1996,
; and Kenny Ramage, http://afralisp.hypermart.net/tips.htm

(defun RandNum (/ modulus multiplier increment random)
  (if (not seed)
    (setq seed (getvar "DATE"))
  )
  (setq modulus    65536
        multiplier 25173
        increment  13849
        seed (rem (+ (* multiplier seed) increment) modulus)
        random (/ seed modulus)
  )
)

; Credits for randnum.lsp: Paul Furman, 1996,
; and Kenny Ramage, http://afralisp.hypermart.net/tips.htm
  
(defun GetRandNum (minNum maxNum / tmp)
  (if (not (< minNum maxNum))
    (progn
      (setq tmp    minNum
            minNum maxNum
            maxNum tmp
      )
    )
  )
  (setq random (+(*(randnum)(- maxNum minNum))minNum))
); end of GetRandNum
  
(defun asmi_GetPerpAtCurvePoint(Curve Point / curDer curAng)
   (setq curDer(vlax-curve-GetFirstDeriv Curve
	         (vlax-curve-GetParamAtPoint Curve Point))
             ); end setq
      (if(=(cadr curDer) 0.0)
        (setq curAng(/ pi 2))
        (setq curAng(- pi(atan(/(car curDer)(cadr curDer)))))
      ); end if
  curAng
  ); end of asmi_GetPerpAtCurvePoint

  (defun asmi-LayersUnlock(/ restLst)
  (setq restLst '())
  (vlax-for lay
    (vla-get-Layers
             (vla-get-ActiveDocument
               (vlax-get-acad-object)))
    (setq restLst
     (append restLst
       (list
         (list
          lay
           (vla-get-Lock lay)
   (vla-get-Freeze lay)
          ); end list
         ); end list
       ); end append
    ); end setq
    (vla-put-Lock lay :vlax-false)
    (if
      (vl-catch-all-error-p
(vl-catch-all-apply
  'vla-put-Freeze(list lay :vlax-false)))
      t)
    ); end vlax-for
  restLst
  ); end of asmi-LayersUnlock

 (defun asmi-LayersStateRestore(StateList)
  (foreach lay StateList
    (vla-put-Lock(car lay)(cadr lay))
     (if
      (vl-catch-all-error-p
(vl-catch-all-apply
  'vla-put-Freeze(list(car lay)(nth 2 lay))))
      t)
    ); end foreach
  (princ)
     ); end of asmi-LayersStateRestore

  (defun *error*(msg)
     (if unlLst
       (progn
        (vla-EndUndoMark actDoc)
        (setvar "OSMODE" oldOsn)
        (asmi-LayersStateRestore unlLst)
      ); end progn
       ); end if
    (princ)
    ); end of *error*
  
    
  (if(not fnand:dis)(setq fnand:dis 1.0))
  (if(not fnand:tol)(setq fnand:tol 10.0))
  (if(minusp fnand:tol)(setq fnand:tol(- fnand:tol)))
  (setq oldDis fnand:dis
	oldTol fnand:tol)
  (setq fnand:dis
	 (getdist
	   (strcat
	     "\nSpecify averange segments length (+/- 25%) <"
	     (rtos fnand:dis) ">: "))
	fnand:tol
	 (getreal
	   (strcat
	     "\nSpecify +/- curves offset <"
	     (rtos fnand:tol) ">: "))
	); end setq
  (if(null fnand:dis)(setq fnand:dis oldDis))
  (if(null fnand:tol)(setq fnand:tol oldTol))
  (princ "\n<<< Select curves to make it freehand >>> ")
  (if
    (setq curvSet
	   (ssget '((-4 . "<OR")(0 . "*LINE")
      		   (0 . "CIRCLE")(0 . "ARC")
      		   (0 . "ELLIPSE")(-4 . "OR>")
      		   (-4 . "<NOT")(0 . "MLINE")
                   (-4 . "NOT>"))
		 ); end ssget
	  ); end setq
    (progn
     (princ "\nPlease wait... ")
     (setq curvLst
	   (mapcar 'vlax-ename->vla-object
                    (vl-remove-if 'listp
                     (mapcar 'cadr(ssnamex curvSet))))
	   pdMax(* 0.25 fnand:dis)
	   pdMin(- pdMax)
	   unlLst(asmi-LayersUnlock)
	   oldOsn(getvar "OSMODE")
	   ); end setq
     (setvar "OSMODE" 0)
     (vla-StartUndoMark
       (setq actDoc
	      (vla-get-ActiveDocument
		(vlax-get-acad-object))))
     (foreach ent curvLst
       (setq stPt
	      (vlax-curve-GetStartPoint ent)
	     enPt
	      (vlax-curve-GetEndPoint ent)
	     cLen
	        (vlax-curve-getDistAtParam ent
                    (vlax-curve-getEndParam ent))
	     ); end setq
       (if
	 (> cLen (* 1.25 fnand:dis))
	 (progn
	   (setq drLst
		    (list stPt)
		 sumDis 0.0
		 ); end setq
	   (while
	     (and
	       (setq nextPt
		    (vlax-curve-GetPointAtDist ent
		      (+ sumDis
			 (if
	                   (minusp
			     (setq nextDis
				    (+ fnand:dis
				       (GetRandNum pdMax pdMin)))
			     ); end minusp
	                     (setq nextDis(- nextDis)) nextDis
	                   ); end if
			 ); end +
		      ); end vlax-curve-GetPointAtDist
		   ); end setq
	       (> cLen(+(* 1.25 fnand:dis)sumDis))
	       ); end and
		 (setq curAng
		      (asmi_GetPerpAtCurvePoint ent nextPt)
		   newPt
		    (polar nextPt curAng fnand:tol)
		   fnand:tol(- fnand:tol)
		   drLst
		    (append drLst
			      (list newPt)
			    ); end append
		   sumDis(+ sumDis nextDis)
		   ); end setq
	     ); end while
	   (setq drLst
		  (mapcar
		    '(lambda(x)(trans x 0 1))
		  (append drLst
			      (list enPt)
			    ); end append
		    ); end mapcar
		 ); end setq
	   (setvar "CMDECHO" 0)
	   (command "_.pline")
	   (mapcar 'command drLst)
	   (command)
	   (setvar "CMDECHO" 1)
	   (setq newPl
		  (vlax-ename->vla-object
		    (entlast))); end setq
	   (vla-put-color newPl
	     (vla-get-color ent))
	   (vla-put-Lineweight newPl
	     (vla-get-Lineweight ent))
	   (vla-put-Layer newPl
	     (vla-get-Layer ent))
	   (vla-put-Linetype newPl
	     (vla-get-Linetype ent))
	   (vla-Delete ent)
	   ); end progn
	 ); end if
       ); end foreach
     (if unlLst
       (progn
        (vla-EndUndoMark actDoc)
        (setvar "OSMODE" oldOsn)
        (asmi-LayersStateRestore unlLst)
      ); end progn
       ); end if
     (princ "\nDone... ")
     ); end progn
    ); end if
  (princ)
  ); end of c:fhand

(princ "\n[Info] http:\\\\www.AsmiTools.com [Info]")
(princ "\n[Info] Type FHAND to convert objects to 'Freehand' style. [Info]")