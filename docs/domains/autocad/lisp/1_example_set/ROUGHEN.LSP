; ROUGHEN.LSP: Routine to "roughen" a polyline

;  copyright 1993 by Mark Middlebrook, Daedalus Consulting
;  prepared for AutoCAD Power Tools,  12 Sept 93
;------------------------------------------------------------------------

(defun C:ROUGHEN (/ olderr ocmd oplt osmd basicziglen zigtemp roughness
                    roughnesstemp en etype en1 en2 vtx1 vtx2
                    seglen segang inclen zignum ziglen zigdist
                    wanderdist wanderang newvtx seed)

   (lisp_setup)

   ;set desired length and roughness of "zigzags" here:  
   (setq basicziglen (* 0.25 (getvar "DIMSCALE")) ;larger is longer segment
         roughness (* 0.05 (getvar "DIMSCALE"))  ;larger is rougher amplitude
   )

   (setq zigtemp (getreal (strcat "Max zigzag frequecy <" (rtos basicziglen) ">: "))
         roughnesstemp (getreal (strcat "Max Amplitude <" (rtos roughness) ">: "))
   ) ;end setq
   (if zigtemp (setq basicziglen zigtemp))
   (if roughnesstemp (setq roughness roughnesstemp))
   (princ (strcat "\nFrequency: " (rtos basicziglen) "   Amplitude: " (rtos roughness)))

   (setq ocmd (getvar "CMDECHO")
         oplt (getvar "PLINETYPE")
         osmd (getvar "OSMODE")
   )
   (setvar "CMDECHO" 0)
   (setvar "PLINETYPE" 0)
   (setvar "OSMODE" 0)

   (setq en (car (entsel "\nSelect a Polyline or Line: ")))
   (terpri)
   (if en
      (progn
         (setq etype (cdr (assoc 0 (entget en))))

         (cond
            ((equal etype "LINE")                     ;is entity a Line?
               (command "._PEDIT" en "_Yes" "_eXit")  ;make it a Pline
               (setq en (entlast)                     ;reset en
                  etype (cdr (assoc 0 (entget en)))
               )
            )
         )

         (cond
            ((or (equal etype "POLYLINE") (equal etype "LWPOLYLINE"))  ;is entity a Pline?
               (setq en1 (entnext en)                 ;first vertex
                     en2 (entnext en1)                ;second vertex
               )
               (command "._PEDIT" en "_Edit")         ;edit vertex

               (while (/= "SEQEND" (cdr (assoc 0 (entget en2))))
                  (princ (strcat "\r Roughening ... " (setq SBAR (progress SBAR)) "    "))
                  (setq vtx1 (cdr (assoc 10 (entget en1)))
                        vtx2 (cdr (assoc 10 (entget en2)))
                        seglen (distance vtx1 vtx2)
                        segang (angle vtx1 vtx2)
                        zignum (fix (/ seglen basicziglen)) ;# of "zigzags"...
                        zignum (max zignum 2)               ; but not less than 2
                        ziglen (/ seglen zignum)            ;zigzag length
                        zigdist ziglen
                        i 2
                  )

                  (repeat (1- zignum)
                     (princ (strcat "\r Roughening ... " (setq SBAR (progress SBAR)) "    "))
                     (setq newvtx (polar vtx1 segang zigdist)  ;vertex init. loc.
                           wanderdist (* roughness (randnum))
                           wanderang (if (= (rem i 2) 1)       ;wander back & forth
                                          (+ segang (/ pi 4))
                                          (- segang (/ pi 4))
                                     );if
                     );setq
                     (command "_Insert" newvtx "_Next"
                              "_Move" (polar newvtx wanderang wanderdist))
                     (setq zigdist (+ ziglen zigdist)
                           i (1+ i)
                     )
                  );repeat

                  (command "_Next")
                  (setq en1 en2
                        en2 (entnext en1)
                  )
               );while

               (command "_eXit" "_eXit")     ;exit PEDIT
               (redraw en)
               (princ "\r Roughening ... done!")
            );entity is a Pline

            (T (prompt "\nEntity is not a Polyline or Line."))
         );cond
      ) ;progn
   ) ;end if

   (setvar "CMDECHO" ocmd)
   (setvar "PLINETYPE" oplt)
   (setvar "OSMODE" osmd)
   (lisp_unsetup)
   (princ)
);defun

; -- Spinning Bar to indicate progress --
	(defun progress (sbar)
		(cond
			((= sbar "\\") "|")
			((= sbar "|") "/")
			((= sbar "/") "-")
			(t "\\")
		)
	) ;end defun spinbar

; Random number generation function - based on the linear
; congruential method as presented in Doug Cooper's book
; Condensed Pascal, pp. 116-117.
; Returns a random number between 0 and 1.
(defun randnum ()
   (if (not seed) (setq seed (getvar "DATE")))
   (setq modulus 65536
         multiplier 25173
         increment 13849
         seed (rem (+ (* multiplier seed) increment) modulus)
         random (/ seed modulus)
   )
)

; custom error handling ... will undo if variable "undoit" is set to "T" (true)
(defun myerror (msg)
	(if	(not
			(member
				msg
				'("Function cancelled" "console break")
			)
		)
			(prompt msg)
	)
	(command ".undo" "end")
	(if undoit
		(progn
			(Prompt "\nUndoing...")
			(command ".undo" "1")
			(setq undoit nil)
		)
	)
	(princ)
)

; standard lisp setup ... groups lisp function into 1 undo and sets custom error handler
(defun lisp_setup ()
	(setq old_error *error*
		*error* myerror
                old_cmdecho (getvar "cmdecho")
	)
        (setvar "cmdecho" 0)
	(command ".undo" "begin")
)

; standard lisp finish ... closes the undo group and resets the standard error handler
(defun lisp_unsetup ()
	(setq *error* old_error
		undoit Nil
	)
        (setvar "cmdecho" old_cmdecho)
	(command ".undo" "end")
)


