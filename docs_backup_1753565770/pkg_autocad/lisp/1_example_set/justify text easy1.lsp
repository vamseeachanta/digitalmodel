;TIP919: CHJUST.LSP (C)1993, JAY LUND
; Change text justification without changing its location.
; Not for aligned or fit text.
; Developed from CHTXJUST.LSP by William Jones - CADalyst tip #840
; Modified by Jay Lund - 7/22/93

(defun JT (s)                        ; error handler
   (if (/= s "Function cancelled")
      (princ (strcat "\nError: " s))
   )
   (command ".undo" "end")
   (chjres)
   (setvar "aunits" au)
   (setq ol nil th nil sty nil index nil tmpt nil ang nil ss nil num nil
   rel nil *error* olderr)
   (princ)
)

(defun chjres ()                         ; reset variables
   (setvar "angbase" ab)
   (setvar "angdir" ad)
   (setvar "auprec" aup)
   (setvar "cmdecho" ce)
   (setvar "snapmode" sm)
   (setvar "textsize" th)
   (princ)
)

(defun C:Jt (/ olderr index tmpt ang ss num rel txj en ed et vt tj
   ga es ts el ei eh ea fvh rstyle)
	(setq rk nil)
	(setq rk (getvar "clayer"))
   (command ".undo" "group")
   (setq olderr *error*
      *error* chjerr
      ab (getvar "angbase")
      ad (getvar "angdir")
      au (getvar "aunits")
      aup (getvar "auprec")
      ce (getvar "cmdecho")
      ol (getvar "clayer")
      sm (getvar "snapmode")
      th (getvar "textsize")
      sty (getvar "textstyle")
      index 0
   )
   ; temporary text
   (if (> (cdr (assoc 40 (tblsearch "style" sty))) 0)
      (command ".text" "0,0" "" ".")       ; if fixed height
      (command ".text" "0,0" "" "" ".")    ; if variable height
   )
   (setq tmpt (entlast))
   (setq ang (/ (* (cdr (assoc 50 (entget tmpt))) 180.0) pi))
   (entdel tmpt)
   (setvar "angbase" 0)
   (setvar "angdir" 0)
   (setvar "aunits" 3)
   (setvar "auprec" 8)
   (setvar "cmdecho" 0)
   (setvar "snapmode" 0)
   (prompt "\nSelect text to change justification:")
   (setq ss (ssget) ; "x" '((0 . "text"))
      num (sslength SS)
   )
   (if (getvar "filedia")
      (progn  ; if Release > = 11
         (setq rel "cl")
         (prompt (strcat "\nIf text is vertical, select Left, Center, Middle or "
         "Right justification..."))
         (initget 1 "L C M R TL TC TR ML MC MR BL BC BR")
         (setq txj (getkword (strcat "\nSelect justification - Left/Center/"
         "Middle/Right/TL/TC/TR/ML/MC/MR/BL/BC/BR: ")))
      )
      (progn  ; if Release < = 10
         (setq rel "pl")
         (initget 1 "L C M R")
         (setq txj (getkword (strcat "\nSelect justification - Left/Center/"
         "Middle/Right: ")))
      )
   )
   (repeat num
      (setq en (ssname ss index)          ; entity name
         ed (entget en)                ; entity data
         et (cdr (assoc 0 ed))         ; entity type
         vt nil
         tj nil
         ga t
      )
      (if (= et "TEXT")
         (setq es (cdr (assoc 7 ed)))      ; style
      )
      ;if text is vertical 
      (if (= (logand (cdr (assoc 70 (tblsearch "style" es))) 4) 4)
         (setq vt t)
      )
      ; if justification is left, center, middle or right
      (if (or (= txj "L") (= txj "C") (= txj "M") (= txj "R"))
         (setq tj t)
      )
      (if (and (/= vt nil) (= tj nil))
         (setq ga nil)
      )
      ; if entity is text, but not aligned or fit and variable ga not nil
      (if (and (= et "TEXT") (/= (cdr (assoc 72 ed)) 3) (/= (cdr (assoc 72
         ed)) 5) ga)
         (progn 
            (setq ts (cdr (assoc 1 ed))     ; string
               el (cdr (assoc 8 ed))     ; layer
               ei (cdr (assoc 10 ed))    ; insertion point
               eh (cdr (assoc 40 ed))    ; height
            )
            (if vt ; if text is vertical
               (setq ea "")
               (setq ea (cdr (assoc 50 ed))) ; rotation angle
            )
            (command ".erase" en "")
            (command ".layer" "set" el "")
            (setq fvh (cdr (assoc 40 (tblsearch "style" es))))
            ; if fixed height text has been scaled, temporarily change the 
            ; height in style
            (if (and (> fvh 0) (/= fvh eh)) 
               (progn
                  (command ".style" es "" eh "" "" "" "" "")
                  (setq rstyle t)
               )
            )
            (cond
               ; if not left justified and Release > = 11 and fixed height
               ((and (/= txj "L") (= rel "cl") (> fvh 0))
               (command ".text" "style" es "justify" txj ei ea ts))
               ; if not left justified and Release > = 11 and variable height
               ((and (/= txj "L") (= rel "cl") (= fvh 0))
               (command ".text" "style" es "justify" txj ei eh ea ts))
               ; if left justified and fixed height
               ((and (= txj "L") (> fvh 0))
               (command ".text" "style" es ei ea ts))
               ; if left justified and variable height
               ((and (= txj "L") (= fvh 0))
               (command ".text" "style" es ei eh ea ts))
               ; if not left justified and Release < = 10 and fixed height
               ((and (/= txj "L") (= rel "pl") (> fvh 0))
               (command ".text" "style" es txj ei ea ts))
               ; if not left justified and Release < = 10 and variable height
               ((and (/= txj "L") (= rel "pl") (= fvh 0))
               (command ".text" "style" es txj ei eh ea ts))
            ) ; cond
            (if (/= txj "L") ; if not left justified
               (command ".move" (entlast) "" (cdr (assoc 10 (entget
               (entlast)))) ei)
            )
            (if rstyle ; reset height in style, if it was changed
               (command ".style" es "" fvh "" "" "" "" "")
            )
         ) ; progn
      ) ; if
      (setq index (+ index 1))
   ) ; repeat
   (setvar "aunits" au)
   ; restore style, height (if variable) & rotation angle
   (if (> (cdr (assoc 40 (tblsearch "style" sty))) 0) 
      (command ".text" "style" sty "0,0" ang ".")     
      (command ".text" "style" sty "0,0" th ang ".")
   )
   (entdel (entlast))
   (chjres)
   (command ".undo" "end")
   (setvar "clayer" rk)
   (princ)
) ; end chjust.lsp
