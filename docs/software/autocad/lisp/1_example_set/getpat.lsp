;|

GETPAT.LSP (c) 2001 Tee Square Graphics
    Version 1.01b - 1/22/2002
    
This routine may be used to extract hatch pattern data
from existing drawings when the .pat file containing
the original information is not available.

After loading the file in the usual manner, type the
command GETPAT at the AutoCAD Command: prompt, select
any (non-SOLID) hatch object, and the pattern information
will be written to a .pat file having the same name as
the pattern (e.g., pattern information for the hatch
pattern WOODS will be written to WOODS.PAT.

Ver. 1.01b includes two small fixex to eliminate "Bad 
Argument" LISP errors when run with certain installations
of AutoCAD 2000+.

|;

(defun C:GETPAT (/ cmde hat elst rotn hnam temp xofs yofs what
                   temp outf flin angl tmp1 tmp2 xvec yvec)
  (setq cmde (getvar "cmdecho"))
  (setvar "cmdecho" 0)
  (while (not (setq hat (entsel "\nSelect hatch: "))))
  (setq elst (entget (car hat)))
  (if (= (cdr (assoc 0 elst)) "HATCH")
    (progn
      (setq rotn (* 180 (/ (cdr (assoc 52 elst)) pi))
            hnam (cdr (assoc 2 elst))
            hscl (cdr (assoc 41 elst))
      )

;; The following nine lines may optionally be omitted.
;; Their purpose is to create a temporary "clone" of the
;; selected hatch with a 0 deg. rotation angle, in case
;; the hatch object specified a rotation angle. If these
;; lines are omitted, the current rotation of the selected
;; hatch will become the "0" deg. rotation for the extracted
;; pattern definition.
      (if (not (zerop rotn))
        (progn
          (setq temp elst)
          (entmake temp)
          (command "_.rotate" (entlast) "" (cdr (assoc 10 temp))(- rotn))
          (setq elst (entget (entlast)))
          (entdel (entlast))
        )
      )
;; End of optional code.

      (setq xofs (cdr (assoc 43 elst))
            yofs (cdr (assoc 44 elst))
            elst (member (assoc 53 elst) elst)
      )
      (setq outf (strcat hnam ".pat"))
      (if (findfile outf)
        (progn
          (initget "Overwrite Append")
          (setq what (getkword (strcat "\n" outf " already exists; Overwrite/Append? ")))
        )
      )
      (setq outf (open outf (if (= what "Append") "a" "w"))
            flin (strcat "*" hnam)
      )
      (foreach x elst
        (cond
          ((= (car x) 53)
            (write-line flin outf)
            (setq angl (cdr x)
                  flin (trim (angtos angl 0 7))
            )
          )
          ((= (car x) 43)
            (setq flin (strcat flin ", " (trim (rtos (/ (- (cdr x) xofs) hscl) 2 7))))
          )
          ((= (car x) 44)
            (setq flin (strcat flin "," (trim (rtos (/ (- (cdr x) yofs) hscl) 2 7))))
          )
          ((= (car x) 45)
            (setq tmp1 (cdr x))
          )
          ((= (car x) 46)
            (setq tmp2 (cdr x)
                  xvec (/ (+ (* tmp1 (cos angl))(* tmp2 (sin angl))) hscl)
                  yvec (/ (- (* tmp2 (cos angl))(* tmp1 (sin angl))) hscl)
                  flin (strcat flin ", " (trim (rtos xvec 2 7)) "," (trim (rtos yvec 2 7)))
            )
          )
          ((= (car x) 49)
            (setq flin (strcat flin ", " (trim (rtos (/ (cdr x) hscl) 2 7))))
          )
          ((= (car x) 98)
            (write-line flin outf)
          )
          (T nil)
        )
      )
      (write-line "" outf)
      (close outf)
      (alert (strcat hnam " pattern definition written to " hnam ".PAT"))
    )
    (alert "Selected object not a HATCH.")
  )
  (setvar "cmdecho" cmde)
  (princ)
)
(defun trim (x / n)
  (setq n (strlen x))
  (while (= (substr x n 1) "0")
    (setq n (1- n)
          x (substr x 1 n)
    )
  )
  (if (= (substr x n 1) ".")
    (setq x (substr x 1 (1- n)))
  )
  x
)
(alert
  (strcat "GETPAT.LSP (c) 2003 Tee Square Graphics\n"
          "          Type GETPAT to start"
  )
)
(princ)