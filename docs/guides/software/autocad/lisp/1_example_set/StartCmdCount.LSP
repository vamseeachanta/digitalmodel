(defun c:StartCmdCount()
  (vl-load-com)
  (if(not cmdcount:cmdreactor)
    (progn
     (setq cmdcount:cmdreactor
	   (vlr-Command-Reactor nil
	     '((:vlr-CommandEnded . CmdCountReaction))
	     ); end vlr-Command-Reactor
	   cmdcount:list nil
	   cmdcount:start (getvar "DATE")
	  ); end setq
     (princ "\n<<< Command Counter now started >>> ")
      ); end progn
    (princ "\n Command Counter already started  ")
    ); end if
  (princ)
  ); end of c:StartCmdCount

(defun c:StopCmdCount(/ cTot eTime)
  (if cmdcount:list
    (progn
      (setq cmdcount:list
	  (vl-sort cmdcount:list
		   '(lambda(a b)(>(cdr a)(cdr b))))
	    cTot(apply '+(mapcar 'cdr cmdcount:list))
	    eTime(getvar "DATE")
	    ); end setq
      (princ "\n====== Command Statistic ======\n")
      (foreach i cmdcount:list
	(princ(strcat "\n "(car i)" "))
	(repeat(- 26(strlen(car i)))(princ "."))
	(princ " ")(princ(itoa(cdr i)))
	); end foreach
      (princ(strcat "\n\n Commands total: "(itoa cTot)))
      (princ(strcat "\n Time total:   " (TotalTime cmdcount:start eTime)))
      (princ "\n\n========== End Report =========")
      (textscr)
      (if cmdcount:cmdreactor
	(progn
	  (vlr-remove cmdcount:cmdreactor)
	  (setq cmdcount:cmdreactor nil)
	 ); end progn
	); end if
      ); end progn
     (princ "\n No command history found  ")
    ); end if
  (princ)
  ); end of c:StopCmdCount


(defun TotalTime(sTime eTime / r days hours minutes seconds)
  (setq r (* 86400 (- eTime sTime))
        days(fix (/ r 86400))
        r(- r(* days 86400))      
        hours(fix (/ r 3600))
        r(- r (* hours 3600))
        minutes(fix (/ r 60))
        r(- r (* minutes 60))
        seconds  (fix r))
    (strcat(rtos hours 2 0) ":" (if(> 10.0 minutes) "0" "")
	   (rtos minutes 2 0) ":" (if(> 10.0 seconds) "0" "")
	   (rtos seconds 2 0))
  ); end TotalTime

  
(defun CmdCountReaction(Reac Args / cLst)
  (if
    (not
      (setq cLst(assoc(car Args)cmdcount:list)))
        (setq cmdcount:list(cons(cons(car Args) 1)cmdcount:list))
        (setq cmdcount:list(subst(cons(car Args)(1+(cdr cLst)))
			                     cLst cmdcount:list))
    ); end if
  (princ)
 ); end of CmdCountReaction

(princ "\n[Info] http:\\\\www.AsmiTools.com [Info]")
(princ "\n[Info] Type STARTCMDCOUNT to command count STOPCMDCOUNT for report. [Info]")
