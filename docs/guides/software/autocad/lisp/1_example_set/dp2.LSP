; ************************* DP2.LSP *****************************
; Program to select required text (on a layer called LEVELS only)
; and change the numerical part of each text item by adding or
; subtracting a user-entered value. 20 May 1998
; Version 3 Modified to convert negative number values
; Written by Colin Browning
; ===============================================================
(Defun *ERROR* (MSG)
(Princ "Error: ")
(Princ MSG)
(Princ)
)

; *** SUB-ROUTINE TO UPDATE NUMBER VALUE:-
(Defun Processit ()
(Setq NUM1 nil)
(Setq TLa (Strlen T2a)) ;calc string length
(Setq KNT 1)
(Setq PRE "") ;initialise prefix store as an empty string
(Repeat TLa
(Setq C1 (Substr T2a KNT 1)) ;check each char
(Setq C2 (Ascii C1)) ;get ascii value
(If (Or (< C2 45)(> C2 57)(= C2 46)(= C2 47));if not a number
(Progn ;char or a minus sign then...
(Setq PRE (Strcat PRE C1)) ;build prefix string
(Setq KNT (1+ KNT))
) ;end of Progn
(Setq NUM1 (Substr T2a KNT)) ;else get remaining number
) ;end of IF
) ;end of Repeat
(If NUM1 ;check if number element exists
(Progn
(Setq TLb (Strlen NUM1)) ;calc no. of chars in number string
(Setq NUM2 (Atof NUM1)) ;convert string number to a real
(Setq TLc (Strlen (Itoa (Fix NUM2))));calc no. of chars in integer part
(If (= TLb TLc) ;calc no. dec places
(Setq TLd 0)
(Setq TLd (- TLb TLc 1))
) ;end of 1st IF
(Setq NUM3 (+ NUM2 V2)) ;update number value as a real
(Setq NUM4 (Rtos NUM3 2 TLd)) ;convert back to string
(Setq T2b (Strcat PRE NUM4)) ;add back prefix code if any
) ;end of Progn
(Setq T2b PRE) ;Else re-use prefix if no no. element
) ;end of outer IF
(Princ)
)

; *** MAIN FUNCTION:-
(Defun C:DP2 ()
(Graphscr)
(Initget "Global Manual")
(Setq S1 (Getkword "\nSelect text Globally or Manually (G/M) <Globally>: "))
(If (Null S1)(Setq S1 "Global"))
(If (= S1 "Global")
(Setq S2 (Ssget "X" (List '(0 . "TEXT")'(8 . "LEVELS"))))
)
(If (= S1 "Manual")
(Setq S2 (Ssget (List '(0 . "TEXT")'(8 . "LEVELS"))))
)
(If S2 ;if valid selection...
(Progn ;then do this...
(Initget 1)
(Setq V2 (Getreal "\nEnter value to change levels by: "))
(Setq L2 (Sslength S2)) ;calcs how many items selected
(Setq CNT 0) ;initialise counter
(Repeat L2 ;loop thru each
(Setq N2 (Ssname S2 CNT)) ;gets entity name
(Setq A2 (Entget N2)) ;gets assoc data listing
(Setq T2 (Assoc 1 A2)) ;extracts text sub-list
(Setq T2a (Cdr T2)) ;extracts actual text item
(Processit) ;sub-routine to change text values
(Setq A3 (Subst (Cons 1 T2b) T2 A2)) ;subst modified text value
(Entmod A3) ;update drawing
(Setq CNT (1+ CNT)) ;increment counter
) ;end of Repeat loop
) ;end of Progn
(Prompt "\nNo valid selection. Try again!");Else option if no selection
) ;end of IF function
(Princ) ;exit cleanly
) 