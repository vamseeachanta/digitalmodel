;;;     BURST.LSP
;;;     Copyright (C) 1997 by Autodesk, Inc.
;;;
;;;     Permission to use, copy, modify, and distribute this software
;;;     for any purpose and without fee is hereby granted, provided
;;;     that the above copyright notice appears in all copies and 
;;;     that both that copyright notice and the limited warranty and 
;;;     restricted rights notice below appear in all supporting 
;;;     documentation.
;;;
;;;     AUTODESK PROVIDES THIS PROGRAM "AS IS" AND WITH ALL FAULTS.  
;;;     AUTODESK SPECIFICALLY DISCLAIMS ANY IMPLIED WARRANTY OF 
;;;     MERCHANTABILITY OR FITNESS FOR A PARTICULAR USE.  AUTODESK, INC. 
;;;     DOES NOT WARRANT THAT THE OPERATION OF THE PROGRAM WILL BE 
;;;     UNINTERRUPTED OR ERROR FREE.
;;;
;;;     Use, duplication, or disclosure by the U.S. Government is subject to 
;;;     restrictions set forth in FAR 52.227-19 (Commercial Computer 
;;;     Software - Restricted Rights) and DFAR 252.227-7013(c)(1)(ii) 
;;;     (Rights in Technical Data and Computer Software), as applicable. 
;;;
;;;     Last Revision 3/10/97 7:29 AM
;;;     Credits: Phil Kreiker
;;;           
;;;
;;;
;;;

(Defun C:BURST (/ item bitset bump att-text lastent burst-one burst
                  BCNT BLAYER BCOLOR ELAST BLTYPE ETYPE PSFLAG ENAME )

   ;-----------------------------------------------------
   ; Item from association list
   ;-----------------------------------------------------
   (Defun ITEM (N E) (CDR (Assoc N E)))
   ;-----------------------------------------------------
   ; Error Handler
   ;-----------------------------------------------------

  (init_bonus_error 
    (list
      (list "cmdecho" 0
            "highlight" 1
      )
      T     ;flag. True means use undo for error clean up.  
    );list  
  );init_bonus_error


   ;-----------------------------------------------------
   ; BIT SET
   ;-----------------------------------------------------

   (Defun BITSET (A B) (= (Boole 1 A B) B))

   ;-----------------------------------------------------
   ; BUMP
   ;-----------------------------------------------------

   (Setq bcnt 0)
   (Defun bump (prmpt)
      (Princ
         (Nth bcnt '("\r-" "\r\\" "\r|" "\r/")) 
      )
      (Setq bcnt (Rem (1+ bcnt) 4))
   )

   ;-----------------------------------------------------
   ; Convert Attribute Entity to Text Entity
   ;-----------------------------------------------------

   (Defun ATT-TEXT (AENT / TENT ILIST INUM)
      (Setq TENT '((0 . "TEXT")))
      (ForEach INUM '(8
            6
            38
            39
            62
            67
            210
            10
            40
            1
            50
            41
            51
            7
            71
            72
            73
            11
         )
         (If (Setq ILIST (Assoc INUM AENT))
            (Setq TENT (Cons ILIST TENT))
         )
      )
      (Setq
         tent (Subst
                 (Cons 73 (item 74 aent))
                 (Assoc 72 tent)
                 tent
              )
      )
      (EntMake (Reverse TENT))
   )

   ;-----------------------------------------------------
   ; Find True last entity
   ;-----------------------------------------------------

   (Defun LASTENT (/ E0 EN)
      (Setq E0 (EntLast))
      (While (Setq EN (EntNext E0))
         (Setq E0 EN)
      )
      E0
   )

   ;-----------------------------------------------------
   ; Burst one entity
   ;-----------------------------------------------------

   (Defun BURST-ONE (BNAME / BENT ANAME ENT ATYPE AENT AGAIN ENAME
                     ENT SS-COLOR SS-LAYER SS-LTYPE mirror ss-mirror
                     mlast)
      (Setq
         BENT   (EntGet BNAME)
         BLAYER (ITEM 8 BENT)
         BCOLOR (ITEM 62 BENT)
         BCOLOR (Cond
                   ((> BCOLOR 0) BCOLOR)
                   ((= BCOLOR 0) "BYBLOCK")
                   ("BYLAYER")
                )
         BLTYPE (Cond ((ITEM 6 BENT)) ("BYLAYER"))
      )
      (Setq ELAST (LASTENT))
      (If (= 1 (ITEM 66 BENT))
         (Progn
            (Setq ANAME BNAME)
            (While (Setq
                      ANAME (EntNext ANAME)
                      AENT  (EntGet ANAME)
                      ATYPE (ITEM 0 AENT)
                      AGAIN (= "ATTRIB" ATYPE)
                   )
               (bump "Converting attributes")
               (ATT-TEXT AENT)
            )
         )
      )
         (Progn
            (bump "Exploding block")
            (Command "_.explode" BNAME)
         )
      (Setq
         SS-LAYER (SsAdd)
         SS-COLOR (SsAdd)
         SS-LTYPE (SsAdd)
         ENAME    ELAST
      )
      (While (Setq ENAME (EntNext ENAME))
         (bump "Gathering pieces")
         (Setq
            ENT   (EntGet ENAME)
            ETYPE (ITEM 0 ENT)
         )
         (If (= "ATTDEF" ETYPE)
            (Progn
               (If (BITSET (ITEM 70 ENT) 2)
                  (ATT-TEXT ENT)
               )
               (EntDel ENAME)
            )
            (Progn
               (If (= "0" (ITEM 8 ENT))
                  (SsAdd ENAME SS-LAYER)
               )
               (If (= 0 (ITEM 62 ENT))
                  (SsAdd ENAME SS-COLOR)
               )
               (If (= "BYBLOCK" (ITEM 6 ENT))
                  (SsAdd ENAME SS-LTYPE)
               )
            )
         )
      )
      (If (> (SsLength SS-LAYER) 0)
         (Progn
            (bump "Fixing layers")
            (Command
               "_.chprop" SS-LAYER "" "_LA" BLAYER ""
            )
         )
      )
      (If (> (SsLength SS-COLOR) 0)
         (Progn
            (bump "Fixing colors")
            (Command
               "_.chprop" SS-COLOR "" "_C" BCOLOR ""
            )
         )
      )
      (If (> (SsLength SS-LTYPE) 0)
         (Progn
            (bump "Fixing linetypes")
            (Command
               "_.chprop" SS-LTYPE "" "_LT" BLTYPE ""
            )
         )
      )
   )

   ;-----------------------------------------------------
   ; BURST MAIN ROUTINE
   ;-----------------------------------------------------

   (Defun BURST (/ SS1)
      (setq PSFLAG (if (= 1 (caar (vports)))
                       1 0
                   )
      )
      (Setq SS1 (SsGet (list (cons 0 "INSERT")(cons 67 PSFLAG))))
      (If SS1
         (Progn
            (Setvar "highlight" 0)
            (terpri)
            (Repeat
               (SsLength SS1)
               (Setq ENAME (SsName SS1 0))
               (SsDel ENAME SS1)
               (BURST-ONE ENAME)
            )
            (princ "\n")
         )
      )
   )

   ;-----------------------------------------------------
   ; BURST COMMAND
   ;-----------------------------------------------------

   (BURST)

  (restore_old_error)

);end defun

(Princ "\nBURST loaded, Type BURST to explode blocked attributes into Dtext entities")

(Princ)
