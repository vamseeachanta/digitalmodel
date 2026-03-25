;; ==================================================================== ;;
;;                                                                      ;;
;;  ORDY.LSP - This lisp for labeling X,Y coordinates and point         ;;
;;	       numbers with standard _DIMORDINATE dimension.            ;;
;;	       All coordinates and point numbers keeps in the           ;;
;;	       text file "Drawing_Name.csv" in a format:                ;;
;;             "point_number, X-coordinate, Y-coordinate, 0".           ;;
;;             The dimension properties is defined by current           ;;
;;             dimension style and variables DIMSCALE, DIMLFAC          ;;
;;             and DIMDEC.                                              ;;
;;                                                                      ;;
;; ==================================================================== ;;
;;                                                                      ;;
;;  Command(s) to call: ORDY                                            ;;
;;                                                                      ;;
;;  Specify point number and insert coordinates labels.                 ;;
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
;;  V1.1, 8th Apr 2008, Riga, Latvia                                    ;;
;;  © Aleksandr Smirnov (ASMI)                                          ;;
;;  For AutoCAD 2000 - 2008 (isn't tested in a next versions)           ;;
;;                                                                      ;;
;;                                 http://www.asmitools.com             ;;
;;                                                                      ;;
;; ==================================================================== ;;


(defun c:ordy(/ fPt oldEcho oldNum dFlc dDec fVar cX cY cNum *error*) 

  (defun *error*(msg) 
    (setvar "CMDECHO" 1)
    (if fVar(close fVar))
    (princ) 
    ); end of *error* 

  (princ(strcat "DIMSCALE="(rtos(getvar "DIMSCALE"))" "
		"DIMLFAC="(rtos(setq dFlc(getvar "DIMLFAC")))" "
		"DIMDEC="(rtos(setq dDec(getvar "DIMDEC")))" "
	 ); end strcat
	); end princ	
  (setvar "CMDECHO" 0)
  (if(= 0(getvar "USERI3"))(setvar "USERI3" 1)
    ); end if
  (setq cNum(getint(strcat "\nSpecify first point number <"
				(itoa(getvar "USERI3")) ">: ")))
  (if cNum (setvar "USERI3" cNum))
  (setq fVar(open(strcase(strcat(getvar "DWGPREFIX")
				 (getvar "DWGNAME") ".csv")) "a"))
  (while
    (setq fPt
	   (getpoint
             (strcat "\nSpecify point or Right-Click to Quit <"
		     (itoa(getvar "USERI3"))">: ")))
      (if(vl-cmdf "_.dimordinate" fPt "_t" 
          (strcat
	"["(itoa(getvar "USERI3"))"]" "\\P"
       (setq cX(rtos(* dFlc(car fPt))2 dDec)) "mE"  
       "\\X" 
       (setq cY(rtos(* dFlc(cadr fPt))2 dDec)) "mN"
       ); end strcat 
          pause 
          ); end vl-cmdf
	(progn
	  (write-line
	    (strcat
	      (itoa(getvar "USERI3")) "," cX "," cY "," "0")fVar)
	  (setvar "USERI3"(1+(getvar "USERI3")))
	  ); end progn
	); end if
    ); end while
    (close fVar)
    (setvar "CMDECHO" 1) 
  (princ) 
  ); end of c:ordy

(princ "\n[Info] http:\\\\www.AsmiTools.com [Info]")
(princ "\n[Info] Type ORDY to tag coordinates [Info]")