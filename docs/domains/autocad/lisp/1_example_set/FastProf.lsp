(defun FASTPROFStartUp ()
  (princ "\nStarting FastProf load")
  (if (not c:fphelp)
    (progn
    (cond
      ((= 15 (atoi (substr (getvar "acadver") 1 2)))
        (load "FP31-A2k.vlx")
      )
      ((= 14 (atoi (substr (getvar "acadver") 1 2)))
        (foreach LoadedARX (arx)
          (if (= "FP31-R14.ARX" (strcase LoadedARX))
            (setq #FASTPROFLoaded T)
          )
        )
; If it isn't loaded ...
        (if (not #FASTPROFLoaded)
          (progn
      ;; Load it
            (arxload "FP31-R14.ARX")
          )
        )
      )
    )
  )
  (PRINC "\nFastProf already loaded")
  )
  (princ)
)   
 
(DEFUN LOAD_FPMENU ()
  (if ( not (menugroup "fastprof31"))
  (if (findfile "FP31.MNU")
	(progn
          (command "menuload" "FP31")
          (princ "\nFP31 menu loaded.")
        )
        (princ "\nCouldn't find FP31.MNU")
     ))
  (princ)
)


(LOAD_FPMENU)
(FASTPROFStartUp)
(princ)
