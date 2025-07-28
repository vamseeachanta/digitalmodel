;; ==================================================================== ;;
;;                                                                      ;;
;;  ORDI.LSP - This lisp for labeling X,Y coordinates with              ;;
;;             standard _DIMORDINATE dimension. The dimension           ;;
;;             properties is defined by current dimension               ;;
;;             style and variables DIMSCALE, DIMLFAC                    ;;
;;             and DIMDEC.                                              ;;
;;                                                                      ;;
;; ==================================================================== ;;
;;                                                                      ;;
;;  Command(s) to call: ORDI                                            ;;
;;                                                                      ;;
;;  ... and pick, pick, pick... till Esc is pressed.                    ;;
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
;;                             http://www.asmitools.com                 ;;
;;                                                                      ;;
;; ==================================================================== ;;


(defun c:ordi(/ fPt oldEcho dFlc dDec *error*) 

  (defun *error*(msg) 
      (setvar "CMDECHO" oldEcho) 
    (princ) 
    ); end of *error* 

  (princ(strcat "DIMSCALE="(rtos
			     (getvar "DIMSCALE"))" "
		"DIMLFAC="(rtos
			    (setq dFlc
			     (getvar "DIMLFAC")))" "
		"DIMDEC="(rtos
			   (setq dDec
			    (getvar "DIMDEC")))" "
	 ); end strcat
	); end princ	
  (setq oldEcho(getvar "CMDECHO")) 
  (setvar "CMDECHO" 0)
  (princ "\n<<< Specify point or Esc to Quit >>> ")
  (while T
  (if 
    (setq fPt(getpoint)) 
    (progn 
      (command "_.dimordinate" fPt "_t" 
          (strcat 
       "X=" (rtos(* dFlc(car fPt))2 dDec) 
       "\\X" 
       "Y=" (rtos(* dFlc(cadr fPt))2 dDec) 
       ); end strcat 
          pause 
          ); end command 
      ); end progn 
    ); end if
    ); end while
    (setvar "CMDECHO" oldEcho) 
  (princ) 
  ); end of c:ordi

(princ "\n[Info] http:\\\\www.AsmiTools.com [Info]")
(princ "\n[Info] X,Y coordinates labeling. Type ORDI to run. [Info]")