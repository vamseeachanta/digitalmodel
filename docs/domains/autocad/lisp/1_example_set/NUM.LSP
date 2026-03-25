;; ====================================================================	;;
;;                                                               	;;
;;  NUM.LSP - This program for fast dynamic numbering. To number a 	;;
;;            suffix and a prefix can be added.     			;;
;;                                                               	;;
;; ==================================================================== ;;
;;                                                               	;;
;;  Command(s) to call: NUM                                    		;;
;;                                                               	;;
;;  Specify the text size, a suffix, a prefix and starting number   	;;
;;  (for erase the old suffix or prefix you should press Spacebar).	;;
;;  Insert a numbers or press Esc to quit. The program remembers old	;;
;;  properties and it is possible to confirm it pressing of Spacebar	;;
;;  key.								;;
;;                                                               	;;
;; ====================================================================	;;
;;                                                               	;;
;;  THIS PROGRAM AND PARTS OF IT MAY REPRODUCED BY ANY METHOD ON ANY   	;;
;;  MEDIUM FOR ANY REASON. YOU CAN USE OR MODIFY THIS PROGRAM OR  	;;
;;  PARTS OF IT ABSOLUTELY FREE.                 			;;
;;                                                               	;;
;;  THIS PROGRAM PROVIDES 'AS IS' WITH ALL FAULTS AND SPECIFICALLY 	;;
;;  DISCLAIMS ANY IMPLIED WARRANTY OF MERCHANTABILITY OR FITNESS    	;;
;;  FOR A PARTICULAR USE.             			                ;;
;;                                                               	;;
;; ====================================================================	;;
;;                                                               	;;
;;  V1.3, 12 May, 2005, Riga, Latvia                                  	;;
;;  © Aleksandr Smirnov (ASMI)                          	   	;;
;;  For AutoCAD 2000 - 2008 (isn't tested in a next versions)		;;
;;                                                               	;;
;;                                      http://www.asmitools.com   	;;
;;								 	;;
;; ====================================================================	;;


(defun c:num (/ oldPref oldSuf oldStart curStr newNum 
                actDoc actSp oldEcho oldSize *error*) 

  (defun *error* (msg) 
    (setvar "CMDECHO" oldEcho) 
    (princ) 
    ); end *error* 
  
  (vl-load-com) 
  (if(not num:Size)(setq num:Size(getvar "DIMTXT"))) 
  (if(not num:Pref)(setq num:Pref "")) 
  (if(not num:Suf)(setq num:Suf "")) 
  (if(not num:Num)(setq num:Num 1)) 
  (setq oldPref num:Pref 
        oldSuf num:Suf 
        oldStart num:Num 
        oldSize num:Size 
        actDoc(vla-get-ActiveDocument 
                (vlax-get-acad-object)) 
        oldEcho(getvar "CMDECHO") 
   ); end setq 
  (setvar "CMDECHO" 0) 
  (if(=(vla-get-ActiveSpace actDoc)1) 
         (setq actSp(vla-get-ModelSpace actDoc)) 
         (setq actSp(vla-get-PaperSpace actDoc)) 
    ); end if
  (setq num:Size 
    (getreal 
      (strcat "\nText size <"(rtos num:Size)">: "))) 
  (if(null num:Size)(setq num:Size oldSize)) 
  (setq num:Pref 
    (getstring T 
      (strcat "\nPrefix: <"num:Pref">: "))) 
  (if(= "" num:Pref)(setq num:Pref oldPref)) 
  (if(= " " num:Pref)(setq num:Pref "")) 
  (setq num:Suf 
    (getstring T 
      (strcat "\nSuffix: <"num:Suf">: "))) 
  (if(= "" num:Suf)(setq num:Suf oldSuf)) 
  (if(= " " num:Suf)(setq num:Suf "")) 
  (setq num:Num 
    (getint 
      (strcat "\nStarting number <"(itoa num:Num)">: "))) 
  (if(null num:Num)(setq num:Num oldStart))
  (princ "\n<<< Insert numbers or press Esc to quit >>> ")
      (while T 
        (setq curStr(strcat num:Pref(itoa num:Num)num:Suf) 
              newNum(vla-AddText actSp 
              curStr (vlax-3d-point '(0.0 0.0 0.0)) num:Size)) 
        (vla-put-Alignment newNum acAlignmentMiddleCenter) 
        (command "_.copybase"(trans '(0.0 0.0 0.0)0 1)(entlast)"") 
        (command "_.erase" (entlast) "") 
        (command "_.pasteclip" pause) 
        (setq num:Num(1+ num:Num)) 
       ); end while 
  (princ) 
 ); end of c:num

(princ "\n<INFO> http:\\www.asmitools.com <INFO>")
(princ "\n<INFO> Dynamic numbering tool. Type NUM to run. <INFO>")