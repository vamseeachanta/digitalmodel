;; ====================================================================	;;
;;                                                               	;;
;;  ATNUM.LSP - This program for fast dynamic numbering by a block      ;;
;;              with single attribute. To number a suffix and a prefix  ;;
;;              can be added.     			                ;;
;;                                                               	;;
;; ==================================================================== ;;
;;                                                               	;;
;;  Command(s) to call: ATNUM                                           ;;
;;                                                               	;;
;;  Specify a suffix, a prefix, the starting number, specify block      ;;
;;  scale and select block to number.                                   ;;
;;  (for erase the old suffix or prefix you should press Spacebar).	;;
;;  Insert a blocks or press Esc to quit. The program remembers old	;;
;;  properties and it is possible to confirm it pressing of Spacebar	;;
;;  key.								;;
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
;;                                                               	;;
;;  V1.2, 16th Jan 2007, Riga, Latvia                                  	;;
;;  © Aleksandr Smirnov (ASMI)                          	   	;;
;;  For AutoCAD 2000 - 2008 (isn't tested in a next versions)		;;
;;                                                               	;;
;;                                      http://www.asmitools.com   	;;
;;								 	;;
;; ====================================================================	;;


(defun c:atnum (/ oldStart oldPref oldSuf oldEcho
        oldSize oldBlock temBl oldAtr oldAtd *error*)

  (defun *error* (msg)
    (setvar "ATTREQ" oldAtr)
    (setvar "CMDECHO" oldEcho)
    (setvar "ATTDIA" oldAtd)
    (princ) 
    ); end *error* 
 
  (if(not atnum:Size)(setq atnum:Size 1.0)) 
  (if(not atnum:Num)(setq atnum:Num 1))
  (if(not atnum:Pref)(setq atnum:Pref "")) 
  (if(not atnum:Suf)(setq atnum:Suf "")) 
  (setq  oldStart atnum:Num 
         oldSize atnum:Size
         oldPref atnum:Pref 
         oldSuf atnum:Suf 
         oldEcho(getvar "CMDECHO")
         oldAtr(getvar "ATTREQ")
	 oldAtd(getvar "ATTDIA")
   ); end setq 
  (setvar "CMDECHO" 0)
  (setvar "ATTREQ" 1)
  (setvar "ATTDIA" 0)
    (setq atnum:Pref 
    (getstring T 
      (strcat "\nType prefix: <"atnum:Pref">: "))) 
  (if(= "" atnum:Pref)(setq atnum:Pref oldPref)) 
  (if(= " " atnum:Pref)(setq atnum:Pref "")) 
  (setq atnum:Suf 
    (getstring T 
      (strcat "\nType suffix: <"atnum:Suf">: "))) 
  (if(= "" atnum:Suf)(setq atnum:Suf oldSuf)) 
  (if(= " " atnum:Suf)(setq atnum:Suf "")) 
  (setq atnum:Num 
    (getint 
      (strcat "\nSpecify start number <"(itoa atnum:Num)">: "))) 
  (if(null atnum:Num)(setq atnum:Num oldStart))
  (setq atnum:Size 
    (getreal 
      (strcat "\nSpecify block scale <"(rtos atnum:Size)">: "))) 
  (if(null atnum:Size)(setq atnum:Size oldSize))
  (if atnum:Block(setq oldBlock atnum:Block))
  (setq temBl
     (entsel(strcat "\nSelect block <"
         (if atnum:Block atnum:Block "not difined") "> > "))); end setq
  (cond
    ((and atnum:Block(not temBl)(tblsearch "BLOCK" atnum:Block))
    (setq atnum:Block oldBlock)
     ); end condition #1
    ((= 1(cdr(assoc 66(entget(car temBl)))))
    (setq atnum:Block(cdr(assoc 2(entget(car temBl)))))
    ); end condition #2
    (t
     (princ "\nBlock not contains attribute! ")
     (setq atnum:Block nil)
     ); end condition #3
    ); end cond
  (if atnum:Block
    (progn
      (princ "\n>>> Pick insertion point or press Esc to quit <<<\n ")
(while T  
  (command "_.-insert" atnum:Block "_s" atnum:Size pause "0"
       (strcat atnum:Pref(itoa atnum:Num)atnum:Suf)); end command
    (setq atnum:Num (1+ atnum:Num)) 
  ); end while
); end progn
    ); end if
  (princ) 
  ); end of c:atnum

(princ "\n[Info] http:\\\\www.AsmiTools.com [Info]")
(princ "\n[Info] Numbering by attributed blocks. Type ATNUM to run. [Info]")