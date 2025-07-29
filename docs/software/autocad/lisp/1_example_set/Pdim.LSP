;; ==================================================================== ;;
;;                                                                      ;;
;;  PDIM.LSP - This lisp for dimensioning of several                    ;;
;;             LwPolylines simultaneously. The program works            ;;
;;             with current dimensional style. The distance of          ;;
;;             the dimensional text from a polyline is equal            ;;
;;             to multiplication of height of the dimensional           ;;
;;             text (DIMTEXT system variable) on a variable             ;;
;;             'tOff'. You can change value of 'tOff' in the            ;;
;;             program beginning, after note.                           ;;
;;                                                                      ;;
;; ==================================================================== ;;
;;                                                                      ;;
;;  Command(s) to call: PDIM                                            ;;
;;                                                                      ;;
;;  Select LwPolylines and press Enter.                                 ;;
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
;;  V1.3, 30th Jan 2009, Riga, Latvia                                   ;;
;;  © Aleksandr Smirnov (ASMI)                                          ;;
;;  For AutoCAD 2000 - 2008 (isn't tested in a next versions)           ;;
;;                                                                      ;;
;;                                 http://www.asmitools.com             ;;
;;                                                                      ;;
;; ==================================================================== ;;

(defun c:pdim(/ tOff plSet pLlst vLst oldDss cAng cDis cPt)

; *** NOTE ***
;                                                                 
;  The distance of the text from a LwPolyline line is equal       
;  of multiplication of system variable 'DIMTXT' (height of       
;  the dimensional text) on a variable 'tOff'. Change this        
;  variable for change this distance.                             

  (setq tOff 1.0)
  
  (princ "\n<<< Select LwPolyline for dimensioning >>> ")
  (if(setq plSet(ssget '((0 . "LWPOLYLINE"))))
    (progn
      (setq pLlst(vl-remove-if 'listp
                         (mapcar 'cadr(ssnamex plSet)))
	    oldDss(getvar "DIMASSOC")
	    ); end if
      (setvar "CMDECHO" 0)
      (command "_.undo" "_be")
      (setvar "DIMASSOC" 2)
      (foreach pl pLlst
	(setq vLst(mapcar '(lambda(x)
		   (trans x 0 1))(mapcar 'cdr
		     (vl-remove-if-not
		       '(lambda(x)(= 10(car x)))(entget pl))))
	      ); end setq
	(if(equal '(70 . 1)(assoc 70(entget pl)))
	  (setq vLst(append vLst(list(car vLst))))
	  ); end if
	(while(< 1(length vLst))
	  (setq cAng(angle(car vLst)(cadr vLst))
	        cDis(/(distance(car vLst)(cadr vLst))2))
	  (if(>=(caar vLst)(caadr vLst))
	    (setq cAng(- cAng pi))
	    ); end if
	  (setq cPt(polar
		     (polar(car vLst)cAng cDis)
		     (+ cAng(* 0.5 pi))(* tOff
					  (*(getvar "DIMSCALE")
					      (getvar "DIMTXT"))))
		); end setq
	  (command "_.dimaligned" "_end" (car vLst)
		   "_end" (cadr vLst) "_none" cPt)
	  (setq vLst(cdr vLst))
	  ); end while
	); end foreach
      (setvar "DIMASSOC" oldDss)
      (command "_.undo" "_e")
      (setvar "CMDECHO" 1)
      ); end progn
    ); end if
  (princ)
  ); end of c:pdim

(princ "\n[Info] http:\\\\www.AsmiTools.com [Info]")
(princ "\n[Info] Type PDIM for multiple LwPolyline dimensioning. [Info]")