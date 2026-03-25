;;; 
;;; TK_EXP Version 1.0, Copyright (c) (1997) by Pham Hoang Anh, HUCE.
;;;

(defun dxf(code elist)
  (cdr (assoc code elist))   ;finds the association pair, strips 1st element
);defun

;--------------------------- INTERNAL ERROR HANDLER --------------------------|

(defun ssx-err (s)                    ; If an error (such as CTRL-C) occurs
                                      ; while this command is active...
(if (/= s "Function cancelled")
    (princ (strcat "\nError: " s))
)
(setq *error* olderr)                 ; Restore old *error* handler
(princ)
)

;-------------------------- ADD AND REMOVE FUNCTIONS -------------------------|

(defun r() (command "r") (ssx))
(defun a() (command "a") (ssx))

;-------------------------------- MAIN PROGRAM -------------------------------|

(defun ssxA (/ olderr t1 t2 t3 f1 f2)
  (setq olderr *error* *error* ssx-err t1 T f2 'f1)
  (setq t1 E)
  (setq t2 0)
  (initget 1)
  (setq t3 "ATTDEF")
  (if t3 (setq f1 (cons (cons t2 t3) f1)))
  (setq f2 (ssget "x" (eval f2)))
  (if (/= nil f2)(command "erase" "P" ""))
  (setq *error* olderr)
)
;
;
;
(DEFUN LST (/ ii en ed)
;
 (if (= "INSERT"                        ; check entity type
     (cdr (assoc 0 (setq e (entget (ssname g n))))))
  (progn                         ; if OK update
   ; SUBENTS  
   (SETQ en (DXF -1 e))          ; get first subentity 
   (setq ii 1)
   (while (and (setq en (entnext en)) ; loop thu subent
               (<= ii 10) )
    (SETQ ED (ENTGET EN))        ; get subent data
    (IF	(/= "SEQEND" (dxf 0 ed)) ; if seqend continue
     (IF (= "ATTRIB" (dxf 0 ed)) ; if not attrib finish
      (progn
        (SETQ ANG (* 180 (/ (CDR (ASSOC 50 ED))PI)))
        (COMMAND "TEXT" (CDR(ASSOC 10 ED)) (CDR (ASSOC 40 ED)) ANG (CDR(ASSOC 1 ED)))
	(ENTUPD (CDR (ASSOC -1 (ENTMOD (SUBST (CONS 70 1)(ASSOC 70 ED)ED)))))

        (setq ii (+ ii 1))
      );PROGN
     );if attrib
    );if sequend
   );while 
   (if (= (dxf 41 e)(dxf 42 e))(COMMAND "EXPLODE" (DXF -1 E)))
   (SSXA)
  );PROGN
 );if
);DEFUN
;
;
(DEFUN C:TK_EXP (/ o_osmode
                   g lg n e )

  (command "cmdecho" 0)
  (setq o_osmode (getvar "OSMODE"))
  (setvar "OSMODE" 0)

  (command "UNDO" "_GROUP")

  (setq g (ssget))                               ; get entities
  (if g
   (progn                                 ; check if any
      (prompt " ")
      (setq n 0 lg (sslength g))                 ; set counter & No of entities
      (while (< n lg)
        (if (= "INSERT"                          ; check entity type
            (cdr (assoc 0 (setq e (entget (ssname g n))))))
          (progn                               ; if OK update
           (LST)
          );progn
        );if
        (prompt "\10\10\10\10\10\10\10\10\10\10\10\10\10\10\10\10\10\10ENTITY No.   ")
        (princ n)
        (princ)
        (setq n (1+ n))                      ; get next entity
      );while
   );progn
  );if
  (command "redraw")
  (setvar "OSMODE" o_osmode)

  (command "UNDO" "_END")

  (princ)
);defun
;;; =====================================================================
(princ "  TK_EXP loaded.")
(princ)