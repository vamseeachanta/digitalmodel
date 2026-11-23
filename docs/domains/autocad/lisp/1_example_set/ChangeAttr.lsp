;;; Modified by Erik Murray 8-13-98
;;; for the purpose of automating attribute
;;; changes through a script.

;;; Usage:  (ChangeAttr "Block Name" "Tag Name" "New Text")

;;;$Header: /VIP/Bonus/Cadtools/gatte.lsp 3     2/13/98 1:01p Kintzlr $
;;;     GATTE.LSP
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
;;; Credits: Source from Compuserve cira 1992 Author unknown
;;;          Rewritten Greg Robinson last rev 3/15/97 4:25 PM
;;;
;;;     Credits: Bill Kramer, Q.C.
;;;              Phil Kreiker Q.C.
;;;              Dominic Panholzer, Q.C.
;;;              Randy Kintzley, Error Control
;;;              Greg Robinson

(Defun ChangeAttr (BN TG NA /
     N      ;selection set counter
     CC     ;changed counter
;     BN     ;block name
;     TG     ;tag name
     ESel   ;entity pick/name/list
     EL     ;entity list
     EN     ;entity name
     PASS   ;loop pass flag
     TAGL   ;list of valid tags for a block
     TAGS   ;String of valid tags for a block
     TAGT   ;Temp tag list
     TAG    ;tag name in loop
     TMP    ;temporary variable
     SS1    ;selection set of insert objects
     XX X   ;flag and counter
     OLDCC  ;previous count of changes for update test
     A      ;entity information in change loop
     FL LA  ;frozen layer check variables
     )
"BONUS_TOOLS_VER_1.2.7"

   (bns_init_error
      (List
         (List "cmdecho" 0)
         T     ;flag. True means use undo for error clean up.  
      ) ;list  
   ) ;init_bonus_error

   ;;
   (Setq n 0
         cc 0
   )
;   (while (null Pass)
;      (initget "Block")
;      (setq ESel (entsel "\nBlock name/<select block or attribute>: "))
;      (cond
;        ((null ESel) (setq Pass 'T BN nil))
;        ((= (type ESel) 'LIST) ;;pick selection
;           (setq EL (entget (car ESel)))
;           (if (= (cdr (assoc 0 EL)) "INSERT")
;               (setq BN (cdr (assoc 2 EL))
;                     Pass 'T
;                     ESel (nentselp (cadr ESel))
;                     EL (entget (car Esel))
;                     TG (if (= (cdr (assoc 0 EL)) "ATTRIB")
;                            (cdr (assoc 2 EL))
;                            nil
;                        )
;               )
;               (prompt "\nSelected item not an INSERT.")
;           );end if
;        );end second conditional for picking attrib
;        ((and (= (type ESel) 'STR) (= ESel "Block"))
;           (setq BN (getstring "\nEnter block name: "))
;           (if (tblsearch "BLOCK" BN)
;              (setq Pass 'T)
;              (prompt "\nInvalid block name.")
;           )
;        );end third conditional
;      );the conditional statement ends
;   ) ;;end of Block Name entry.

   (if BN (progn
     (setq Pass nil
           EN (cdr (assoc -2 (tblsearch "BLOCK" BN)))
     )
     (while EN
        (setq EL (entget EN))
        (if (= (cdr (assoc 0 EL)) "ATTDEF")
           (setq TAGL (cons (cdr (assoc 2 EL)) TAGL)))
        (setq EN (entnext EN))
     )
   )) ;;end if BN progn
   (if TG (setq Pass 'T))

   (if TAGL
     (progn
       (setq TAGS (car TAGL)
             TAGT (cdr TAGL)
       )
       (foreach TAG TAGT
         (setq TAGS (strcat TAGS " " TAG))
       )
     )
   )

;   (while (and TAGS (null Pass))
;      (initget TAGS)
;      (prompt (strcat "\nKnown tag names for block: " TAGS))
;      (setq ESel (nentsel "\nSelect attribute or type attribute name: ")

;      (cond
;        ((= (type ESel) 'STR)
;           (setq ESel (strcase ESel))
;           (if (member ESel TAGL)
;             (setq Pass 'T
;                   TG    ESel
;             )
;             (prompt "\nInvalid attribute name.")
;           )
;        )
;        ((= (type ESel) 'LIST) ;;pick selection
;           (setq TG (cdr (assoc 2 (entget (car ESel)))))
;           (if TG
;             (setq Pass 'T)
;           )
;        )
;      );the conditional statement ends
;   ) ;;end of Attribute Name entry.


   (if (and BN (null TAGL))
      (setq BN (prompt "\nThe block selected has no attributes!")))
   (If (And BN TG)
      (Progn
         (prompt (strcat "\nBlock: " BN "   Attribute tag: " TG))
         (Setq
;            NA (GetString T "\nNew Text: ")
            SS1 (SsGet "X"
                 (List
                    (Cons 0 "INSERT")
                    (Cons 2 bn)
                    (Cons 66 1)
                 )
              )
            N (If SS1 (SsLength SS1) 0)
         )
;         (initget 0 "Yes No")
;         (setq TMP
;           (getkword
;             (strcat "\nNumber of inserts in drawing = "
;                     (itoa N)
;                     "   Process all of them? <Yes>/No: ")))
         (setq TMP "Yes")
         (if (and TMP (= TMP "No"))
            (setq SS1 (ssget (list (cons 0 "INSERT")
                                   (cons 2 BN)
                                   (cons 66 1)))
                  N (if SS1 (sslength SS1) 0)
            )
         )
         (if (> N 0) (Princ "\nPlease wait..."))
         (setq x 0)
         (repeat N
            (setq A (ssname SS1 x)
                  B (entget A)
                  la (cdr (assoc 8 B))      ;layer name from object
                  fl (tblsearch "LAYER" la) ;table entry for layer
                  fl (cdr (assoc 70 fl))    ;layer status flag
            )
            (if (/= fl 65) ;if layer not frozen
               (progn
                  (setq XX 1
                        oldcc cc)
                  (while XX
                     (setq
                        B (EntGet (EntNext (CDR (Assoc -1 B))))
                     )
                     (If (= (CDR (Assoc 0 B)) "SEQEND")
                        (Setq xx Nil)
                        (Progn
                           (If (= (CDR (Assoc 2 b)) tg)
                              (Progn
                                 (Setq B (subst (Cons 1 NA) (assoc 1 B) B)
                                       CC (1+ CC)
                                 )
                                 (EntMod B)
                              ) ;progn
                           ) ;if
                        ) ;progn
                     ) ;if
                  ) ;while
                  (If (/= cc oldcc) (EntUpd a))
               ) ;progn
            ) ;if
            (Setq X (1+ X))
         ) ;repeat
         (If (/= 1 cc)
            (Princ
               (Strcat
                  "\n"
                  (ItoA cc)
                  " attributes changed."
               )
            )
            (Princ
               (Strcat
                  "\n"
                  (ItoA cc)
                  " attribute changed."
               )
            )
         )
      ) ;progn
   )
   (bns_restore_old_error)
   (Princ)
) ;defun
