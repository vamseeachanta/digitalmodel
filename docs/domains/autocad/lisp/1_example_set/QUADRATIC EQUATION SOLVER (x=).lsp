;Tip1709:   QUADEQ.LSP      Quadratic equation       (c)2001, V.H.Vinerts

 ; ===== QUADEQ.LSP by VHV, 29Mar99; QUADratic EQuation solver
(defun C:x=  (/ A B C RADCND RADCAL X1 X2)
  (prompt
    "\nTo find the roots of a quadratic equation, which ")
  (prompt "is expressed as ")
  (prompt
    "\n[ax-squared plus bx plus c =0], input the values of 
             a, b, and c: ")
  (setq A (getreal "\nThe value of a is: ")
        B (getreal "\nThe value of b is: ")
        C (getreal "\nThe value of c is: "))
 ; === The calculations follow:
  (setq RADCND (- (* B B) (* 4.0 A C)))
  (if (< RADCND 0)
    (prompt "\nInput again, we get imaginary results!")
    ) ; end IF
  (setq RADCAL (sqrt RADCND))
  (setq X1 (/ (- RADCAL B) (* 2 A)))
  (setq X2 (/ (- (- RADCAL) B) (* 2 A)))
 ; === and the answers are:
  (princ "\nThe roots are: ")
  (prompt (rtos X1 2 6))
  (prompt " and ")
  (prompt (rtos X2 2 6))
  (princ)
  ) ; end of QUADEQ.LSP


