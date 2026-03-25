;; ==================================================================== ;;
;;                                                                      ;;
;;  MULLION.LSP - Draws window mullions                                 ;;
;;                                                                      ;;
;; ==================================================================== ;;
;;                                                                      ;;
;;  Command(s) to call: MULLION                                         ;;
;;                                                                      ;;
;;  Specify vertical and horizontal mullion quantity and                ;;
;;  draw boundary rectangle.                                            ;;
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
;;  V1.0, 21st Feb 2007, Riga, Latvia                                   ;;
;;  © Aleksandr Smirnov (ASMI)                                          ;;
;;  For AutoCAD 2000 - 2008 (isn't tested in a next versions)           ;;
;;                                                                      ;;
;;                             http://www.asmitools.com                 ;;
;;                                                                      ;;
;; ==================================================================== ;;


(defun c:mullion(/ bRec Count hDel hDis lEnt
	       maxPt minPt oldHor oldVer
	       vDel vDis oldPlw *error*)
  
  (vl-load-com)

  (defun *error*(msg)
    (setvar "CMDECHO" 1)
    (princ)
    ); end of *error*
  
  (if(not mul:ver)
    (setq mul:ver 4 mul:hor 4))
  (initget 4)
  (setq oldVer mul:ver
	oldHor mul:hor
	mul:hor (getint
		  (strcat "\nHor <"(itoa mul:hor)">: ")))
  (initget 4)
  (setq mul:ver (getint
		  (strcat "\nVer <"(itoa mul:ver) ">: "))
	lEnt(entlast))
  (if(null mul:ver)(setq mul:ver oldVer))
  (if(null mul:hor)(setq mul:hor oldHor))
  (setvar "CMDECHO" 0)
  (princ "\nDraw boubdary ")
  (command "_.rectang" pause pause)
  (if(or(not(entlast))(not(equal lEnt(entlast))))
    (progn
      (setq bRec(vlax-ename->vla-object(entlast)))
      (vla-GetBoundingBox bRec 'minPt 'maxPt)
      (setq minPt
	     (trans(vlax-safearray->list minPt)0 1)
	    maxPt
	     (trans(vlax-safearray->list maxPt)0 1)
	    vDis(-(car maxPt)(car minPt))
	    hDis(-(cadr maxPt)(cadr minPt))
	    oldPlw(getvar "PLINEWID")
	    ); end setq
      (setvar "PLINEWID" 1.5)
      (if(/= 0 mul:hor)
	(progn
		  (setq hDel(/ hDis(1+ mul:hor))
			Count 1)
		  (repeat mul:hor
		    (vl-cmdf "_.pline" "_non" (list(car minPt)
					    (+(cadr minPt)(* hDel Count)))
			               "_non" (list(car maxPt)
					    (+(cadr minPt)(* hDel Count)))
			     ""); end vl-cmdf
		    (setq Count(1+ Count))
		    ); end repeat
	  ); end progn
	); end if
      (if(/= 0 mul:ver)
	(progn
		(setq vDel(/ vDis(1+ mul:ver))
		      Count 1)
		(repeat mul:ver
		  (vl-cmdf "_.pline" "_non" (list
					      (+(car minPt)(* vDel Count))
					      (cadr minPt))
			             "_non" (list
					      (+(car minPt)(* vDel Count))
					      (cadr maxPt))
			   ""); end vl-cmdf
		  (setq Count(1+ Count))
	  ); end repat
	 ); progn
	); end if
      (setvar "PLINEWID" oldPlw)
      (if bRec(vla-Delete bRec))
      ); end progn
    ); end if
  (setvar "CMDECHO" 1)
  (princ)
  ); end of c:mullion

(princ "\n[Info] http:\\\\www.AsmiTools.com [Info]")
(princ "\n[Info] Type MULLION to draw window mullions. [Info]")