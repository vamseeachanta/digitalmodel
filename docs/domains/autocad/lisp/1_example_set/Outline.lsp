;; ==================================================================== ;;
;;                                                                      ;;
;;  OUTLINE.LSP - Transforms width polylines to it boundaries           ;;
;;            (outline).                                                ;;
;;                                                                      ;;
;; ==================================================================== ;;
;;                                                                      ;;
;;  Command(s) to call: OUTLINE                                         ;;
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
;;  V1.0, 20th Dec 2006, Riga, Latvia                                   ;;
;;  © Aleksandr Smirnov (ASMI)                                          ;;
;;  For AutoCAD 2000 - 2008 (isn't tested in a next versions)           ;;
;;                                                                      ;;
;;                                      http://www.asmitools.com        ;;
;;                                                                      ;;
;; ==================================================================== ;;


(defun c:outline(/ plSet plLst offDis posLst negLst)
  (vl-load-com)
  (princ "\n<<< Select wide polylines >>> ")
  (if
    (setq plSet
      (ssget
	'((0 . "LWPOLYLINE")(-4 . "<NOT")(43 . 0.0)(-4 . "NOT>"))))
    (progn
      (setq plLst
	     (mapcar 'vlax-ename->vla-object 
                    (vl-remove-if 'listp 
                     (mapcar 'cadr(ssnamex plSet)))))
      (foreach pl plLst
	(setq offDis(/(vla-get-ConstantWidth pl)2)
	      posLst(vlax-safearray->list
		       (vlax-variant-value
			 (vla-Offset pl offDis)))
	      negLst(vlax-safearray->list
		       (vlax-variant-value
			 (vla-Offset pl (- offDis))))
	      ); end setq
    (mapcar '(lambda(x)(vla-put-ConstantWidth x 0.0))posLst)
    (mapcar '(lambda(x)(vla-put-ConstantWidth x 0.0))negLst)
	); end foreach
      (mapcar 'vla-Delete plLst)
      ); end progn
    ); end if
  (princ)
  ); end of c:outline

(princ "\n[Info] http:\\\\www.AsmiTools.com [Info]")
(princ "\n[Info] Type OUTLINE to outline width polylines. [Info]")