;; ==================================================================== ;;
;;                                                                      ;;
;;  BRI.LSP - Break one of intersected objects                          ;;
;;                                                                      ;;
;; ==================================================================== ;;
;;                                                                      ;;
;;  Command(s) to call: BRI                                             ;;
;;                                                                      ;;
;;  Specify width of break, select breaking and breakable               ;;
;;  objects in loop until Right Click or Esc pressed. Works             ;;
;;  for LINES, POLYLINES, LWPOLYLINES, CIRCLES, ELLIPSES, ARCS          ;;
;;  and SPLINES.                                                        ;;
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
;;  V1.0, 19th June 2008, Riga, Latvia                                  ;;
;;  © Aleksandr Smirnov (ASMI)                                          ;;
;;  For AutoCAD 2000 - 2008 (isn't tested in a next versions)           ;;
;;                                                                      ;;
;;                             http://www.asmitools.com                 ;;
;;                                                                      ;;
;; ==================================================================== ;;

(defun c:bri(/ oldDist stFlag boObj brObj cPt brArr diArr dDim
	      brLst ptLst vObj cDist pt1 pt2 bLay oldSnp *error*)
  
  (vl-load-com)

  (defun Is_Curve(Entity)
    (if
      (and
	Entity
	(member(cdr(assoc 0(entget Entity)))
	       '("LINE" "POLYLINE" "LWPOLYLINE" "SPLINE"
		 "CIRCLE" "ELLIPSE" "ARC"))
	 ); end and
       T nil
      ); end if
    ); end of is curve

  (defun *error*(msg)
    (if oldSnp
      (setvar "OSMODE" oldSnp)
      ); end if
    (setvar "CMDECHO" 1)
    (princ "\nApplication console break. Exit. ")
    (princ)
    ); end of *error*

 (if(not bi:bDist)(setq bi:bDist 10.0))
  (setq oldDist bi:bDist
	bi:bDist(getdist
		  (strcat"\nSpecify distance of break <"
			 (rtos bi:bDist) ">: "))
	); end setq
 (if(null bi:bDist)(setq bi:bDist oldDist))
  (while(not stFlag)
    (if
      (setq brObj(entsel "\nSpecify breaking object or Right Click to Exit > "))
      (if
         (and
	  (setq boObj(entsel "\nSpecify breakable object or Right Click to Exit > "))
	  (setq cPt(trans(cadr boObj)1 0))
        ); and and
      (if
	(and
	  (Is_Curve(car boObj))(Is_Curve(car brObj))
	  ); end and
	(if
	  (and
	    (setq brArr
	       (vla-IntersectWith
	          (vlax-ename->vla-object(car boObj))
	          (vlax-ename->vla-object(car brObj))
	           acExtendNone); end vla-IntersectWith
		  );end setq
		 (setq dDim(vlax-safearray-get-dim
		       (setq diArr(vlax-variant-value brArr)))
	        ); end setq
	        (>
	          (vlax-safearray-get-u-bound diArr dDim)
	          (vlax-safearray-get-l-bound diArr dDim)
	         ); end >
	    ); end and
	  (progn
	    (setq brLst(vlax-safearray->list diArr))
	    (while brLst
	      (setq ptLst(cons
			   (list
			     (car brLst)
			     (cadr brLst)
			     (nth 2 brLst)
			     ); end list
			   ptLst); end cons
		    ); end setq
	      (repeat 3(setq brLst(cdr brLst)))
	      ); end while
	    (setq ptLst
		   (vl-sort ptLst
			    '(lambda(a b)(<
					   (distance a cPt)
					   (distance b cPt)
					   ); end <
			       ); lambda
			    ); end vl-sort
	          vObj(vlax-ename->vla-object(car boObj))
	          cDist(vlax-curve-GetDistAtPoint vObj
		         (vlax-curve-getClosestPointTo vObj
			   (car ptLst)))
	          pt1(vlax-curve-GetPointAtParam vObj
		      (vlax-curve-GetParamAtDist vObj
		       (- cDist(/ bi:bDist 2))))
	          pt2(vlax-curve-GetPointAtParam vObj
		      (vlax-curve-GetParamAtDist vObj
		       (+ cDist(/ bi:bDist 2))))
		  bLay(vla-Item
			(vla-get-Layers
			  (vla-get-ActiveDocument
			    (vlax-get-acad-object)))
			(cdr(assoc 8(entget(car boObj)))))
		  ); end setq
	    (if(= :vlax-true(vla-get-Lock bLay))
	      (princ "\n<!> Can't break. Entity on locked layer. <!> ")
	      (if(and pt1 pt2)
		(progn
		  (setq oldSnp(getvar "OSMODE"))
		  (setvar "OSMODE" 0)
		  (setvar "CMDECHO" 0)
	          (vl-cmdf "_.break"(list(car boObj)
					 (trans pt1 0 1))
			                 (trans pt2 0 1))
		  (setvar "CMDECHO" 1)
		  (setvar "OSMODE" oldSnp)
		 ); end progn
		(princ "\n<!> Can't break. Breaking distance too long. <!> ")
		); end if
	      ); end if
	    ); end progn
	  (princ "\n<!> Can't break. Non intersection detected. <!> ")
	  ); and if
	(princ "\n<!> Can't break. Invalid object type. <!> ")
	); end if
	(progn
	  (setq stFlag T)
	  (princ "\nExit BRI.lsp. ")
	); end progn
      ); end if
      (progn
	(setq stFlag T)
	(princ "\nExit BRI.lsp. ")
	); end progn
     ); end if
    ); end while
  (princ)
  ); end of c:bri

(princ "\n[Info] http:\\\\www.AsmiTools.com [Info]")
(princ "\n[Info] Type BRI to break intersections. [Info]")