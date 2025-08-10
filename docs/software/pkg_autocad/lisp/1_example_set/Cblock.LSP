;; ==================================================================== ;;
;;                                                                      ;;
;;  CBLOCK.LSP - This routine replaces (or adds) blocks to              ;;
;;               others blocks.                                         ;;
;;                                                                      ;;
;; ==================================================================== ;;
;;                                                                      ;;
;;  Command(s) to call: CBLOCK                                          ;;
;;                                                                      ;;
;;  Specify the sample of replacing block, select replaced              ;;
;;  blocks and press Enter. Answer remove old blocks or not.            ;;
;;  Replacing block references inherit scale and the rotation angle.    ;;
;;  All other properties (as Layer, Color etc.) is current.             ;;
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
;;  V1.0, 6th Feb 2007, Riga, Latvia                                    ;;
;;  © Aleksandr Smirnov (ASMI)                                          ;;
;;  For AutoCAD 2000 - 2008 (isn't tested in a next versions)           ;;
;;                                                                      ;;
;;                                 http://www.asmitools.com             ;;
;;                                                                      ;;
;; ==================================================================== ;;

(defun c:cblock(/ BLROT BLSET CURVSET DXFLST1
	          DXFLST2 FILLST INSPT NNAME SABBL
	          SAMBL TARBL XSCAL YSCAL ZSCAL
	          ACTDOC RLST DELFLG)

  (vl-load-com)

  (defun asmi_GetActiveSpace(/ actDoc)
    (if
      (= 1(vla-get-ActiveSpace
	    (setq actDoc
		   (vla-get-ActiveDocument
	      (vlax-get-acad-object)
	      ); end vla-get ActiveSpace
	     ); end setq
	    ); end vla-get-ActiveDocument
	 ); end =
      (vla-get-ModelSpace actDoc)
      (vla-get-PaperSpace actDoc)
      ); end if
    ); end of asmi_GetActiveSpace

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
  
  (if
    (and
      (setq samBl
	     (entsel "\nSelect sample block you want to change > "))
      (= "INSERT" (cdr(assoc 0
		   (setq dxfLst1
			  (entget(car samBl))))))
      ); end and
    (progn
      (setq filLst
	     (vl-remove-if-not
	       '(lambda(x)(member(car x)'(0 2)))dxfLst1)
	      ); end setq
      (princ "\n<<< Select blocks to change >>> ")
      (if
	(setq blSet
	  (ssget filLst))
	(progn
	  (if
	    (and
	      (setq tarBl
		     (entsel "\nSelect distination block > "))
	      (= "INSERT" (cdr(assoc 0
		   (setq dxfLst2
			  (entget(car tarBl))))))
	      (setq nName
		     (cdr(assoc 2 dxfLst2)))
	      ); end and
	    (progn
	      (initget 1 "Yes No")
	      (setq blSet
		     (mapcar 'vlax-ename->vla-object
                       (vl-remove-if 'listp
                         (mapcar 'cadr(ssnamex blSet))))
		    delFlg
	               (getkword
	                  "\nDo you want to delete old blocks? [Yes/No]: ")
		    ); end setq
	      (vla-StartUndoMark
                (setq actDoc
	         (vla-get-ActiveDocument
		   (vlax-get-acad-object))))
	      (setq rLst(asmi-LayersUnlock))
	      (foreach bl BlSet
		(setq insPt
		       (vla-get-InsertionPoint bl)
		      xScal
		       (vla-get-XScaleFactor bl)
		      yScal
		       (vla-get-YScaleFactor bl)
		      zScal
		       (vla-get-ZScaleFactor bl)
		      blRot
		       (vla-get-Rotation bl)
		      ); end setq
		(vla-InsertBlock
		  (asmi_GetActiveSpace)
		  insPt nName xScal yScal zScal blRot)
		(if(= "Yes" delFlg)
		(vla-Delete bl)
		  ); end if
		); end foreach
	      (asmi-LayersStateRestore rLst)
	      (vla-EndUndoMark actDoc)
	      ); end progn
	    (princ "\n>>> This isn't block or empty selection! <<< ")
	    ); end if
	  ); end progn
	(princ "\n>>> Empty selection set! <<< ")
	); end if
      ); end progn
    (princ "\n>>> This isn't block or empty selection! <<< ")
    ); end if
  (princ)
  ); end of c:cblock

(princ "\n[Info] http:\\\\www.AsmiTools.com [Info]")
(princ "\n[Info] Type CBLOCK to replace or add blocks [Info]")
