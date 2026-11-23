;;; TKKT.lsp
;;; Ve ky thuat - ket cau.
;;; Ve moc thep
(defun c:moc(/ ve_moc ph w l ss pd pn et f)
 (defun ve_moc()
  (setvar "UCSICON" 2)
  (command "_.UCS" "E" "L")
  (if (setq ph(getpoint "\nHuong moc:")) (progn
   (setq ph (fix(/ (cadr ph)(abs(cadr ph)))))
   (command "_.LINE" (list 0 (* ph w)) (list l (* w ph))"")
   (ssadd(entlast) ss) 
   (if (= ph 1)  
     (command "_.ARC" "C" (list 0 (/ w 2.0))  (list 0 w)"0,0")
     (command "_.ARC" "C" (list 0 (/ (* -1 w) 2.0)) "0,0"(list 0 (* -1 w)) )
   )
   (ssadd(entlast) ss) 
  ))
  (command "_.UCS" "")
  (setvar "UCSICON" 1)
 )
;;;;
 (setq pd "t" i 1 ss(ssadd)
       f(open"c:\\acad\\tkxd\\data\\mocthep.txt" "r") l(atof(read-line f)) w(atof(read-line f)) )(close f)
 (while (not (listp pd))
  (initget "D  R")
  (setq pd(getpoint (strcat"\nDai moc<"(rtos l 2 3)">/Rong moc<"(rtos w 2 3)">/<diem dau>:")))
  (if (= pd "D") (progn
   (setq l(getreal"\Dai moc<0.05>:"))
   (if (not l) (setq l 0.05))
  ))
  (if (= pd "R") (progn
   (setq w(getreal"\Rong moc<0.05>:"))
   (if (not w) (setq w 0.05))
  ))
 );end while
 (if (listp pd) (progn
  (initget 1)
  (setq pn(getpoint pd"\ndiem tiep theo:") pds pd)
  (command "_.LINE" pd pn"")
  (ssadd (entlast) ss)
  (ve_moc)
  (while pn
   (setq i(1+ i)pd pn pn(getpoint pd"\ndiem tiep theo:"))  
   (if pn (progn  (command"_.LINE" pn pd"")(ssadd (entlast) ss)  ))
  );End while
  (if (= i 2) (progn
   (command "_.LINE" pd pds"")  
   (setq et(entlast))
  ))
  (ve_moc)
  (if (= i 2) (progn (command "_.ERASE" et"") (redraw)))
  (command "_.PEDIT" ss"" "J" ss"""")
 ))
 (setq f(open "c:\\acad\\tkxd\\data\\mocthep.txt" "w"))
 (write-line (rtos l 2 3) f) (write-line (rtos w 2 3)f) (close f)
(princ)
)
ÿ