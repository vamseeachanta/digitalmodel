;; ==================================================================== ;;
;;                                                                      ;;
;;  MPL.LSP - This lisp draws 2 - 16 polylines simultaneously           ;;
;;                                                                      ;;
;; ==================================================================== ;;
;;                                                                      ;;
;;  Command(s) to call: MPL                                             ;;
;;                                                                      ;;
;;  Instead standard options  instead you can use the short             ;;
;;  options consisting of quantity of polylines (from 2 to 16)          ;;
;;  and justification (Top, Zero, Bottom). For example:                 ;;
;;                                                                      ;;
;;  Command: mpl                                                        ;;
;;  <<< Quantity = 12, Justification = Top, Offset = 40 >>>             ;;
;;  Specify first point or [Quantity/Justification/Offset]: 6           ;;
;;  <<< Quantity = 6, Justification = Top, Offset = 40 >>>              ;;
;;                                                                      ;;
;;  Command: mpl                                                        ;;
;;  <<< Quantity = 6, Justification = Top, Offset = 40 >>>              ;;
;;  Specify first point or [Quantity/Justification/Offset]: B           ;;
;;  <<< Quantity = 6, Justification = Bottom, Offset = 40 >>>           ;;
;;                                                                      ;;
;;  Command: mpl                                                        ;;
;;  <<< Quantity = 6, Justification = Top, Offset = 40 >>>              ;;
;;  Specify first point or [Quantity/Justification/Offset]: 16z         ;;
;;  <<< Quantity = 16, Justification = Zero, Offset = 40 >>>            ;;
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
;;  V1.3, 12 May, 2005, Riga, Latvia                                    ;;
;;  © Aleksandr Smirnov (ASMI)                                          ;;
;;  For AutoCAD 2000 - 2008 (isn't tested in a next versions)           ;;
;;                                                                      ;;
;;                                        http://www.asmitools.com      ;;
;;                                                                      ;;
;; ==================================================================== ;;

(defun c:mpl(/ ptOpt oldQuont oldJust oldOff stPt mlName lastEnt
               firEnt lnSet oldEcho rLst *error*)
  
(vl-load-com)
  
(defun asmi-mlStyleCreate(Quont / dxfLst topOrd Count mlDict)
  (setq dxfLst
   (list'(0 . "MLINESTYLE")'(102 . "{ACAD_REACTORS")'(102 . "}")
    '(100 . "AcDbMlineStyle")(cons 2(strcat(itoa Quont)"_PLINES"))
    '(70 . 0)'(3 . "")'(62 . 256)'(51 . 1.5708)'(52 . 1.5708)
     (cons 71 Quont))
        Count 0.0
        topOrd(-(/ Quont 2.0) 0.5)
       ); end setq
  (repeat Quont
     (setq dxfLst(append dxfLst
                   (list(cons 49(- topOrd Count))
                     '(62 . 256) '(6 . "BYLAYER")))
           Count(1+ Count)
	   );end setq
    ); end repeat
    (if
     (null
       (member
        (assoc 2 dxfLst)(dictsearch(namedobjdict)"ACAD_MLINESTYLE")))
          (progn
             (setq mlDict
		    (cdr
		      (assoc -1
			(dictsearch(namedobjdict)"ACAD_MLINESTYLE"))))
              (dictadd mlDict(cdr(assoc 2 dxfLst))(entmakex dxfLst))
	    ); end progn
     ); end if
      (strcat(itoa Quont)"_PLINES")
    ); end of asmi-mlStyleCreate

  (defun asmi-LayersUnlock(/ restLst)
    (setq restLst '())
    (vlax-for lay(vla-get-Layers
        (vla-get-ActiveDocument
          (vlax-get-acad-object)))
            (setq restLst
              (append restLst
		      (list
			(list lay (vla-get-Lock lay)
			      ); end list
			); end list
		      ); end append
		  ); end setq
       (vla-put-Lock lay :vlax-false)
     ); end vlax-for
   restLst
  ); end of asmi-LayersUnlock

  (defun asmi-LayersStateRestore(StateList)
    (foreach lay StateList
       (vla-put-Lock(car lay)(cadr lay))
      ); end foreach
    (princ)
   ); end of asmi-LayersStateRestore

 (defun ExplodeMline()
   (setvar "CMDECHO" 0)
   (if(or(not lastEnt)(not(equal lastEnt(entlast))))
       (setq lastEnt(entlast))
       (setq lastEnt nil)
     ); end if
  (if lastEnt
    (progn
      (setq rLst(asmi-LayersUnlock))
      (command "_.explode" lastEnt)
      (setq lnSet(ssadd))
      (ssadd
        (setq lastEnt
          (entnext lastEnt))lnSet); end setq
    (while
      (setq lastEnt(entnext lastEnt))
       (if lastEnt(ssadd lastEnt lnSet))
     ); end while
  (cond
    ((or
       (and lnSet(not(getvar "PEDITACCEPT")))
       (and lnSet(=(getvar "PEDITACCEPT")0))
       ); end or
    (command "_.pedit" "_m" lnSet "" "_y" "_j" "0.0" "")
     ); end condition #1
    ((and lnSet(=(getvar "PEDITACCEPT")1))
     (command "_.pedit" "_m" lnSet "" "_j" "0.0" "")
      ); end condition #2
    ); end cond
    (asmi-LayersStateRestore rLst)
    (setvar "CMDECHO" oldEcho)
    ); end progn
   ); end if
  ); end of ExplodeMline

  
  (defun *error*(msg)
    (ExplodeMline)
    (if rLst
      (asmi-LayersStateRestore rLst)
      ); end if
      (if odlEcho
	 (setvar "CMDECHO" odlEcho)
	); end if
    (princ)
    ); end of *error*

  
  (if(not mpl:quont)(setq mpl:quont 2))
  (if(not mpl:just)(setq mpl:just "Zero"))
  (if(not mpl:off)(setq mpl:off 40.0))
  (setq ptOpt T
        oldQuont mpl:quont
        oldJust mpl:just
        oldOff mpl:Off
        oldEcho(getvar "CMDECHO")
	); end setq
  (while(and ptOpt(/= 'LIST (type ptOpt)))
   (princ
     (strcat "\n<<< Quantity = " (itoa mpl:quont)
             ", Justification = " mpl:just
             ", Offset = " (rtos mpl:off) " >>> "
	     ); end strcat
     ); end princ
   (initget 128)
    (setq ptOpt
     (getpoint
       (strcat "\nSpecify first point or [Quantity/Justification/Offset]: ")))
         (if(=(type ptOpt) 'STR)
            (setq ptOpt(strcase ptOpt))
           ); end if
    (cond
       ((= 'LIST(type ptOpt))
       (setq stPt ptOpt)
       (princ "\nSpecify next point or [Undo]: ")
       ); end condition #1
      ((= ptOpt "Q")
       (setq mpl:quont
       (getint
          (strcat "\nSpecify quantity from 2 to 16 <"(itoa mpl:quont)">: ")))
           (if(null mpl:quont)(setq mpl:quont oldQuont))
           (if(or(< mpl:quont 2)(> mpl:quont 16))
              (progn
                (setq mpl:quont oldQuont)
                (princ "\n<!> Only from 2 to 16 polylines are available <!> ")
              ); end progn
            ); end if
       ); end condition #2
      ((= ptOpt "J")
        (initget "Zero Top Bottom")
         (setq mpl:just
             (getkword
                (strcat "\nSpecify justification [Zero/Top/Bottom] <" mpl:just ">: ")))
                (if(null mpl:just)(setq mpl:just oldJust))
        ); end condition #4
      ((= ptOpt "O")
       (initget 2)
       (setq mpl:off
        (getdist
          (strcat "\nSpecify offset distance <" (rtos mpl:off) ">: ")))
            (if(null mpl:off)(setq mpl:off oldOff))
       ); end condition #5
      ((if(member ptOpt
             '("2" "3" "4" "5" "6" "7" "8" "9" "10" "11" "12" "13" "14" "15" "16"))
           (setq mpl:quont(atoi ptOpt))
        ); end if
       ); end condition #6
      ((if(member ptOpt
             '("Z" "T" "B"))
           (setq mpl:just(cadr
                (assoc(strcase ptOpt)
                '(("Z" "Zero")("T" "Top")("B" "Bottom"))))
                ); end setq
        ); end if
       ); end condition #7
      ((if(member ptOpt
        '("2Z" "2T" "2B" "3Z" "3T" "3B" "4Z" "4T" "4B" "5Z" "5T" "5B"
          "6Z" "6T" "6B" "7Z" "7T" "7B" "8Z" "8T" "8B" "9Z" "9T" "9B"
          "10Z" "10T" "10B" "11Z" "11T" "11B" "12Z" "12T" "12B"
          "13Z" "13T" "13B" "14Z" "14T" "14B" "15Z" "15T" "15B"
          "16Z" "16T" "16B"))
      (setq mpl:quont
             (atoi(substr ptOpt 1(1-(strlen ptOpt))))
            mpl:just(cadr
             (assoc(substr(strcase ptOpt)(strlen ptOpt)1)
             '(("Z" "Zero")("T" "Top")("B" "Bottom"))))
            ); end setq
         ); end if
       ); end condition #8
         ((if ptOpt(princ "\n<!> Invalid option keyword <!> "))
          ); end condition #9
         ); end cond
       ); end while
    (if ptOpt
       (progn
        (setq mlName(asmi-mlStyleCreate mpl:quont))
   (if(entlast)
       (setq lastEnt(entlast))
    ); end if
 (setvar "cmdecho" 0)
  (command "_.mline"
           "_ST" mlName
           "_S" mpl:off
           "_J" (strcat "_" mpl:just)
         stPt)
  (setvar "CMDECHO" 1) 
       (while(= 1(getvar "CMDACTIVE"))
          (command pause)
        ); end while
  (setvar "CMDECHO" 0)
  (ExplodeMline)
  ); end progn
 ); end if
  (princ)
  ); end of c:mpl


(princ "\n[Info] http:\\\\www.AsmiTools.com [Info]")
(princ "\n[Info] To draw 2 - 16 polylines type MPLINE. [Info]")
