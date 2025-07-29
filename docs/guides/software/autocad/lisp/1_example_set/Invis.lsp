;; ==================================================================== ;;
;;                                                                      ;;
;;  INVIS.LSP - Makes objects temporarily invisible and                 ;;
;;              returns visibility.                                     ;;
;;                                                                      ;;
;; ==================================================================== ;;
;;                                                                      ;;
;;  Command(s) to call: INVIS                                           ;;
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
;;  V1.1, 11th Apr 2005, Riga, Latvia                                   ;;
;;  © Aleksandr Smirnov (ASMI)                                          ;;
;;  For AutoCAD 2000 - 2008 (isn't tested in a next versions)           ;;
;;                                                                      ;;
;;                             http://www.asmitools.com                 ;;
;; ==================================================================== ;;
;;                                                                      ;;

(defun c:invis(/ errCount wMode objSet actDoc *error*)
  
  (vl-load-com)

  (defun put_Visible_Prop(Object Flag)
    (if
      (vl-catch-all-error-p
	(vl-catch-all-apply
	  'vla-put-visible (list Object Flag)))
          (setq errCount(1+ errCount))
    ); end if
  (princ)
  ); end of put_Visible_Prop

  (defun Set_to_List(SelSet)
    (mapcar 'vlax-ename->vla-object
                    (vl-remove-if 'listp
                     (mapcar 'cadr(ssnamex SelSet))))
  ); end of Set_to_List

  (defun errMsg()
    (if(/= 0 errCount)
  (princ(strcat ", " (itoa errCount)
		" were on locked layer."))
      "."
  ); end if
    ); end of errMsg
  
(setq actDoc(vla-get-ActiveDocument
	      (vlax-get-Acad-object))
      errCount 0); end setq
  (vla-StartUndoMark actDoc)
(initget "Visible Invisible" 1)
  (setq wMode
    (getkword "\nMake objects [Visible/Invisible]: "))
  (if(and
       (= wMode "Visible")
       (setq objSet(ssget "_X" '((60 . 1))))
       ); end and
    (progn
      (setq objSet(Set_to_List objSet))
   (mapcar
    '(lambda(x)(put_Visible_Prop x :vlax-true))objSet)
    (princ
      (strcat "\n<< "
	      (itoa(-(length objSet)errCount))
		   " now visible" (errMsg) " >>"))
      ); end progn
    (progn
      (if(not(setq objSet(ssget "_I")))
	(setq objSet(ssget))
	); end if
      (if objSet
	(progn
	  (setq objSet(Set_to_List objSet))
  (mapcar
    '(lambda(x)(put_Visible_Prop x :vlax-false))objSet)
    (princ
      (strcat "\n<< "
	      (itoa(-(length objSet)errCount))
		   " now invisible" (errMsg) " >>"))
	  ; end if
	 ); end progn
	); end if
       ); end progn
      ); end if
  (vla-EndUndoMark actDoc)
(princ)
); end of c:invis

(princ "\n[Info] http:\\\\www.AsmiTools.com [Info]")
(princ "\n[Info] Type INVIS to make objects invisible or visible. [Info]")