; blk-lst.lsp - block list
;
;     Copyright (C) 1992 by Autodesk, Inc.
;
;     Permission to use, copy, modify, and distribute this software 
;     for any purpose and without fee is hereby granted, provided 
;     that the above copyright notice appears in all copies and that 
;     both that copyright notice and this permission notice appear in 
;     all supporting documentation.
;
;     THIS SOFTWARE IS PROVIDED "AS IS" WITHOUT EXPRESS OR IMPLIED
;     WARRANTY.  ALL IMPLIED WARRANTIES OF FITNESS FOR ANY PARTICULAR
;     PURPOSE AND OF MERCHANTABILITY ARE HEREBY DISCLAIMED.
;     ****************************************************************
;
; I have repeatedly had requests for listing attributes and their
; values, both constant and variable. So here is a set of different
; routines for doing so. Copy them, save them, read them, use them, 
; distribute them, and have lots of fun!
;
; Jeremy Tammik, Autodesk AG, 91-05-23
; Modified & added c:cattl and c:attlst for Siro, 91-07-15
;
; Global: bnam
;
; Commands defined:
;
; c:blktbl lists the block table, showing what block definitions exist.
; c:blklst lists one block definition (all entities).
; c:cattl  lists all constant attributes in a block definition
; c:attlst lists all attributes in a block insertion, reading the
;          constant ones from the block definition and the variable ones
;          from the insertion entity.


(defun dxf (n ed) (cdr (assoc n ed)))


;==================================================================
; c:blktbl - block table list:
;
; List all the blocks defined in this drawing.

(defun c:blktbl (/ b)
    (while (setq b (tblnext "block" (not b)))
           (print b))
    (princ)
)


;==================================================================
; c:blklst - block list:
;
; List the definition of a user-selected block.

(defun c:blklst (/ nam b en)
    (if (not bnam) (setq bnam ""))
    (setq nam (getstring (strcat "Block name <" bnam ">: ")))
    (if (/= "" nam) (setq bnam nam))
    (if (setq b (tblsearch "block" bnam))
        (if (setq en (dxf -2 b))
            (progn
             (print (dxf 0 (entget en)))
             (while (setq en (entnext en))
                    (print (dxf 0 (entget en)))))
            (prompt (strcat "\nNo entities in block " bnam)))
        (prompt (strcat "\nBlock " bnam " not found.")))
    (princ)
)

;==================================================================
; pratt - print attribute ins point, tag, and value & incr counter:

(defun pratt (en type pcnt)
    (setq ed (entget en))
    (if (= type (dxf 0 ed))
        (if (or (/= "ATTDEF" (dxf 0 ed))       ; variable attribute: ATTRIB
                (/= 0 (logand 2 (dxf 70 ed)))) ; const: ATTDEF & const flag
            (progn 
             (set pcnt (1+ (eval pcnt)))
             (print (dxf 10 ed))
             (prin1 (dxf 2 ed))
             (princ ": ")
             (prin1 (dxf 1 ed)))))
)

;==================================================================
; c:cattl - list constant attributes in block definition:
;
; List all the attributes of a user-selected block, both constant
; and variable:

(defun c:cattl (/ nam b en constcnt)
    (if (not bnam) (setq bnam ""))
    (setq nam (getstring (strcat "Block name <" bnam ">: "))
          constcnt 0)
    (if (/= "" nam) (setq bnam nam))
    (if (setq b (tblsearch "block" bnam))
        (if (setq en (dxf -2 b))
            (progn
             (pratt en "ATTDEF" 'constcnt)
             (while (setq en (entnext en))
                    (pratt en "ATTDEF" 'constcnt)))
            (prompt (strcat "\nNo entities in block " bnam)))
        (prompt (strcat "\nBlock " bnam " not found.")))
    (prompt (strcat "\nBlock " bnam " has " (itoa constcnt) " constant attributes."))
    (princ)
)

;==================================================================
; c:attlst - list all attributes of block definition and insertion:
;
; List all the attributes of a user-selected block, both constant
; and variable:

(defun c:attlst (/ nam b en1 en constcnt varcnt)
    (setq en (car (entsel)))
    (if (not en) (progn (prompt "\nNothing selected.")(quit)))
    (if (/= "INSERT" (dxf 0 (entget en))) (progn (prompt "\nNot a block.")(quit)))
    (setq bnam (dxf 2 (entget en))
          constcnt 0 varcnt 0 en1 en)

    ; list constant attributes:

    (if (setq b (tblsearch "block" bnam))
        (if (setq en (dxf -2 b))
            (progn
             (pratt en "ATTDEF" 'constcnt)
             (while (setq en (entnext en))
                    (pratt en "ATTDEF" 'constcnt)))
            (prompt (strcat "\nNo entities in block " bnam)))
        (prompt (strcat "\nBlock " bnam " not found.")))

    ; list variable attributes:

    (while (/= "SEQEND" (dxf 0 (entget (setq en1 (entnext en1)))))
           (pratt en1 "ATTRIB" 'varcnt))

    (prompt (strcat "\nBlock " bnam " has " (itoa constcnt) " constant attributes."))
    (prompt (strcat "\nBlock " bnam " has " (itoa varcnt) " variable attributes."))
    (princ)
)
