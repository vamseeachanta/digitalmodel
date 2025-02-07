;; ==================================================================== ;;
;;                                                                      ;;
;;  CURSE.LSP - This program will transform various types of curves to  ;;
;;              LwPolylines with specified long segments. It works      ;;
;;              with LwPolylines, Circles, Splines, Ellipses and Arcs.  ;;
;;              In LwPolylines arc segments will be transformed only.   ;;
;;                                                                      ;;
;; ==================================================================== ;;
;;                                                                      ;;
;;  Command(s) to call: CURSE                                           ;;
;;                                                                      ;;
;;  Select curves and specify length of segments. New polylines will    ;;
;;  be created over old curves. Specify to remove old curves or not.    ;;
;;                                                               	;;
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
;;  V1.1, 20th Feb 2009, Riga, Latvia                                   ;;
;;  © Aleksandr Smirnov (ASMI)                                          ;;
;;  For AutoCAD 2000 - 2008 (isn't tested in a next versions)           ;;
;;                                                                      ;;
;;                                      http://www.asmitools.com        ;;
;;                                                                      ;;
;; ==================================================================== ;;


(defun c:curse(/ cSet sLen cLen sDis ptLst aDoc oOsn eLst cAns vCnt lPt
	       *error*)
  
  (vl-load-com)

  (defun *error*(msg)
    (if aDoc(vla-StartUndoMark aDoc))
    (if oOsn(setvar "OSMODE" oOsn))
    (setvar "CMDECHO" 0)
    (princ)
    ); end of *error*

  (princ "<<< Select curves to transform >>>")
  (if
    (setq cSet
	   (ssget
		  '((0 . "*POLYLINE,SPLINE,ARC,CIRCLE,ELLIPSE"))))
    (if(setq sLen(getdist "\nSpecify segment lentgth: "))
      (progn
	(setq oOsn(getvar "OSMODE"))
	(setvar "CMDECHO" 0)(setvar "OSMODE" 0)
        (foreach itm(setq eLst(vl-remove-if 'listp
		                (mapcar 'cadr(ssnamex cSet))))
	(setq sDis 0.0
	      cLen(-(vlax-curve-getDistAtParam itm
		      (vlax-curve-getEndParam itm))
		    (vlax-curve-getDistAtParam itm
		      (vlax-curve-getStartParam itm)))
		    ); end setq
	(if(= "LWPOLYLINE"(cdr(assoc 0(entget itm))))
	(progn
	  (setq vCnt 1
		ptLst(list(vlax-curve-GetPointAtDist itm sDis))
		aDoc(vla-get-ActiveDocument
		      (vlax-get-acad-object)))
	  (vla-StartUndoMark aDoc)
	   (while(and
		   (<= sDis cLen)
		   (vlax-curve-GetPointAtParam itm vCnt)
		   (setq cPt(vlax-curve-GetPointAtDist itm sDis))
		   ); end and
	      (if(= 0.0(vla-GetBulge(vlax-ename->vla-object itm)(1- vCnt)))
		(setq ptLst(append ptLst
		          (list(vlax-curve-GetPointAtParam itm vCnt)))
		      sDis(vlax-curve-GetDistAtParam itm vCnt)
		      ); end setq
		(progn
		  (while(<= sDis(vlax-curve-GetDistAtParam itm vCnt))
		    (setq cPt(vlax-curve-GetPointAtDist itm sDis)
			  ptLst(append ptLst(list cPt))
			  sDis(+ sDis sLen))
		    ); end while
		  (setq ptLst(append ptLst
		          (list(vlax-curve-GetPointAtParam itm vCnt))))
		  ); end progn
		); end if
	       (setq vCnt(1+ vCnt))
	      ); end while
	    ); end progn
	   (while(and
		   (<= sDis cLen)
		   (setq cPt(vlax-curve-GetPointAtDist itm sDis))
		   ); end and
	     (setq ptLst(append ptLst(list cPt))
		   sDis(+ sDis sLen))
	   ); end while
	  ); end if
	  (if(not(equal(setq lPt(vlax-curve-getEndPoint itm))(last ptLst)))
	    (setq ptLst(append ptLst(list lPt)))
	   ); end if
	(command "_.pline")(mapcar 'command ptLst)(command "")
	(setq ptLst nil)
       ); end foreach
	(initget "Yes No")
	(setq cAns(getkword "\nRemove old curves [Yes/No] <Yes>: "))
	(if(or(null cAns)(= cAns "Yes"))
	  (mapcar 'entdel eLst)
	  ); end foreach
       (setvar "CMDECHO" 1)
       (setvar "OSMODE" oOsn)
       (vla-EndUndoMark aDoc)
      ); end progn
      (princ "\n<!> Nothing selected <!> ")
     ); end if
    ); end if
  (princ)
  ); end of c:curse

(princ "\n[Info] http:\\\\www.AsmiTools.com [Info]")
(princ "\n[Info] Type CURSE for transform curves to linear LwPolylies [Info]")