;; ==================================================================== ;;
;;                                                                      ;;
;;  CHATT.LSP - The program for change attributes with the chosen       ;;
;;              value in dynamic and ordinary blocks.                   ;;
;;                                                                      ;;
;; ==================================================================== ;;
;;                                                                      ;;
;;  Command(s) to call: CHATT                                           ;;
;;                                                                      ;;
;;  Pick sample attribute for filter creation, and after that select    ;;
;;  blocks containing this attribute. The program will request to enter ;;
;;  replaced value (the specified attribute by default) and if          ;;
;;  attributes are found will highlight blocks and will request         ;;
;;  new value.                                                          ;;
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
;;  V1.2, 28th Jan 2009, Riga, Latvia                                  	;;
;;  © Aleksandr Smirnov (ASMI)                                          ;;
;;  For AutoCAD 2000 - 2008 (isn't tested in a next versions)           ;;
;;                                                                      ;;
;;                                      http://www.asmitools.com        ;;
;;                                                                      ;;
;; ==================================================================== ;;


(defun c:chatt(/ cAtt cBl cTag efNm sStr nLst fSet oVal fLst exLst
	       fStr actDoc atLst pLst cFrom cTo sucCnt errCnt wSet
	       nStr)

  (vl-load-com)

  (if
    (and
      (setq cAtt(nentsel "\nPick sample attribute > "))
      (= "ATTRIB"(cdr(assoc 0(entget(car cAtt)))))
      ); end and
    (progn
      (setq actDoc(vla-get-ActiveDocument
		    (vlax-get-acad-object))
	    cBl(vla-ObjectIDtoObject actDoc
		 (vla-get-OwnerID
		   (setq cAtt
		     (vlax-ename->vla-object(car cAtt)))))
	    cTag(vla-get-TagString cAtt)
	    sucCnt 0 errCnt 0
	    wSet(ssadd)
	    ); end setq
      (if(vlax-property-available-p cBl 'EffectiveName)
	(progn
	  (setq fStr(vla-get-EffectiveName cBl)
		nStr(cons 2(strcat "`*U*," fStr))
		nLst(mapcar 'vla-get-Name
		       (vl-remove-if-not
		         (function(lambda(x)
			   (equal fStr(vla-get-EffectiveName x))))
		             (mapcar 'vlax-ename->vla-object
		               (vl-remove-if 'listp 
                                 (mapcar 'cadr(ssnamex
			           (ssget "_X" (list '(0 . "INSERT")
				     '(66 . 1) nStr))))))))
		); end setq
	   (foreach n nLst
	     (if(not(member n exLst))
	         (setq fStr(strcat "`" n "*," fStr)
		       exLst(cons n exLst)
		       ); end setq
	       ); end if
	     ); end foreach
	  (setq fLst(list '(0 . "INSERT")(cons 2 fStr)))
	  ); end progn
	(setq fLst(list '(0 . "INSERT")(cons 2(vla-getName cBl))))
	); end if
      (princ "\n<<< Select blocks >>> ")
      (if(setq fSet(ssget fLst))
	(progn
	  (princ(strcat "\n" (itoa(sslength fSet)) " block(s) found. "))
	    (setq cFrom(getstring T
			 (strcat "\nChange from <"
				 (setq oVal(vla-get-TextString cAtt)) ">: ")))
	  (if(= "" cFrom)(setq cFrom oVal))
	  (foreach b(mapcar 'vlax-ename->vla-object
		               (vl-remove-if 'listp 
                                 (mapcar 'cadr(ssnamex fSet))))
	    (setq atLst(vlax-safearray->list
			 (vlax-variant-value
			   (vla-GetAttributes b))))
	    (foreach at atLst
	         (if(and
		       (equal(vla-get-TagString at)cTag)
		       (equal(vla-get-TextString at)cFrom)
		      ); end and
		     (progn
		       (setq pLst(cons at pLst))
		       (ssadd(vlax-vla-object->ename b)wSet)
		     ); end progn
		   ); end if
	      ); end foreacn
	    ); end foreach
	  (if(/= 0(length pLst))
	    (progn
	      (princ
		(strcat "\n" (itoa(length pLst)) " attribute(s) found. ")
		); end princ
	      (sssetfirst nil wSet)
	      (if
	        (and
	          (setq cTo(getstring T "\nChange to: "))
	          (/= "" cTo)
	          ); end and
	        (progn
		 (sssetfirst nil nil)
	         (vla-StartUndoMark actDoc)
	          (foreach a pLst
	             (if(vl-catch-all-error-p
		          (vl-catch-all-apply 'vla-put-TextString
		              (list a cTo)))
		           (setq errCnt(1+ errCnt))
		           (setq sucCnt(1+ sucCnt))
	               ); end if
	            ); end foreach
	         (princ
	           (strcat "\n" (itoa sucCnt) " of "
			   (itoa(length pLst))" attributes changed. ")
	       ); end princ
	     (if(/= 0 errCnt)
	       (princ(strcat(itoa errCnt) " were on locked layer! "))
	       ); end if
	     (vla-EndUndoMark actDoc)
	     ); end progn
	    ); end if
	  ); end progn
	    (princ
	      (strcat "\nCan't to find attributes with '"
		      cFrom "' value!"))
	    ); end if
	  ); end progn
	); end if
      ); end progn
    (princ "\n<!> It isn't attribute <!> ")
    ); end if
  (princ)
  ); end of c:chatt

(princ "\n[Info] http:\\\\www.AsmiTools.com [Info]")
(princ "\n[Info] Type CHATT for change multiple attributes value. [Info]")

