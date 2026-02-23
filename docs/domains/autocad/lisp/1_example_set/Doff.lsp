;; ==================================================================== ;;
;;                                                                      ;;
;;  DOFF.LSP - Multiple doubleside offset with possibility              ;;
;;             of delition of source objects.                           ;;
;;                                                                      ;;
;; ==================================================================== ;;
;;                                                                      ;;
;;  Command(s) to call: DOFF                                            ;;
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
;;  V1.0, 3rd Jul 2006, Riga, Latvia                                    ;;
;;  © Aleksandr Smirnov (ASMI)                                          ;;
;;  For AutoCAD 2000 - 2008 (isn't tested in a next versions)           ;;
;;                                                                      ;;
;;                             http://www.asmitools.com                 ;;
;;                                                                      ;;
;; ==================================================================== ;;


(defun c:doff(/ oldDis plSet plLst delFlg)

  (vl-load-com)

  (if(not doffdis)(setq doffdis 10.0))
  (setq oldDis doffdis
	doffdis(getdist
		    (strcat "\nSpecify offset distance <"
			    (rtos doffdis
				  (getvar "LUNITS")
				  (getvar "LUPREC")) ">: "))
	); end setq
  (if
    (not doffdis)
     (setq doffdis oldDis)
    ); end setq
  (if
    (setq plSet
      (ssget
	'((0 . "LWPOLYLINE,LINE,ARC,CIRCLE,ELLIPSE,SPLINE"))))
    (progn
      (setq plLst
	     (mapcar 'vlax-ename->vla-object 
                    (vl-remove-if 'listp 
                     (mapcar 'cadr(ssnamex plSet)))))
      (foreach pl plLst
	(vla-Offset pl doffdis)
	(vla-Offset pl(- doffdis))
	); end foreach
      (initget "Yes No")
      (setq delFlg(getkword "\nDelete source objects [Yes/No]? <Yes>: "))
      (if(null delFlg)(setq delFlg "Yes"))
        (if(= "Yes" delFlg)
         (mapcar 'vla-Delete plLst)
	); end if
      ); end progn
    ); end if
  (princ)
  ); end of c:outline

(princ "\n[Info] http:\\\\www.AsmiTools.com [Info]")
(princ "\n[Info] Type DOFF for doubleside offset. [Info]")