;TIP1203.LSP:    ARCLEADER.LSP    Arc Leader    (c)1996, Douglas C. Barense
 
(defun c:lea (/ STARTPNT ANGLEPNT ENDPNT1 ENDPNT2)
  (graphscr)
  (initget 15)
  (setq STARTPNT (getpoint"\nStarting point for arrow: "))
  (initget 15)
  (setq ANGLEPNT
    (getpoint STARTPNT "\nSelect angle definition point: ")
  )
  (setq ENDPNT1
    (polar
      STARTPNT
      (angle STARTPNT ANGLEPNT)
      (* 0.09375 (getvar"LTSCALE"))
    )
  )
  (setq ENDPNT2 (* 0.03125 (getvar "LTSCALE")))
  (command
    "pline" STARTPNT "w" "0.0" ENDPNT2 ENDPNT1 "w" "0.0" "0.0" "arc"
  )
  (princ) 
); end arcleadr.lsp

