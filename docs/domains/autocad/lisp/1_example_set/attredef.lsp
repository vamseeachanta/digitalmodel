; Next available MSG number is    13 
; MODULE_ID ATTREDEF_LSP_
;;;
;;;    attredef.lsp
;;;
;;;    Copyright 1988, 1990, 1992, 1994, 1996 by Autodesk, Inc.
;;;
;;;    Permission to use, copy, modify, and distribute this software
;;;    for any purpose and without fee is hereby granted, provided
;;;    that the above copyright notice appears in all copies and
;;;    that both that copyright notice and the limited warranty and
;;;    restricted rights notice below appear in all supporting
;;;    documentation.
;;;
;;;    AUTODESK PROVIDES THIS PROGRAM "AS IS" AND WITH ALL FAULTS.
;;;    AUTODESK SPECIFICALLY DISCLAIMS ANY IMPLIED WARRANTY OF
;;;    MERCHANTABILITY OR FITNESS FOR A PARTICULAR USE.  AUTODESK, INC.
;;;    DOES NOT WARRANT THAT THE OPERATION OF THE PROGRAM WILL BE
;;;    UNINTERRUPTED OR ERROR FREE.
;;;
;;;    Use, duplication, or disclosure by the U.S. Government is subject to
;;;    restrictions set forth in FAR 52.227-19 (Commercial Computer
;;;    Software - Restricted Rights) and DFAR 252.227-7013(c)(1)(ii) 
;;;    (Rights in Technical Data and Computer Software), as applicable.
;;;
;;;.
;;;
;;; DESCRIPTION
;;;
;;;   This program allows you to redefine a Block and update the
;;;   Attributes associated with any previous insertions of that Block.
;;;   All new Attributes are added to the old Blocks and given their
;;;   default values. All old Attributes with equal tag values to the new
;;;   Attributes are redefined but retain their old value. And all old
;;;   Attributes not included in the new Block are deleted.
;;;
;;;   Note that if handles are enabled, new handles will be assigned to
;;;   each redefined block.
;;;
;;; --------------------------------------------------------------------------;

;;;
;;; Oldatts sets "old_al" (OLD_Attribute_List) to the list of old Attributes
;;; for each Block.  The list does not include constant Attributes.
;;;
(defun oldatts (/ e_name e_list cont)
  (setq oa_ctr 0 
        cont   T
        e_name b1
  )
  (while cont
    (if (setq e_name (entnext e_name))
      (progn
        (setq e_list (entget e_name))
        (if (and (= (cdr (assoc 0 e_list)) "ATTRIB")
                 ;; NOT a constant attribute -- (cdr (assoc 70 e_list)) != 2)
                 (/= (logand (cdr (assoc 70 e_list)) 2) 2))
          (progn
            (if old_al
              (setq old_al (cons e_list old_al))
              (setq old_al (list e_list))
            )
            (setq oa_ctr (1+ oa_ctr))           ; count the number of old atts
          )
          ;; else, exit
          (setq cont nil)
        )
      )
      (setq cont nil)
    )
  )
)
;;;
;;; Newatts sets "new_al" to the list of new Attributes in the new Block.
;;; The list does not include constant Attributes.
;;;
(defun newatts (ssetn ssl / i e_name e_list)
  (setq i 0 na_ctr 0)
  (while (< i ssl)
    (if (setq e_name (ssname ssetn i))
      (progn
        (setq e_list (entget e_name))
        (if (and (= (cdr (assoc 0 e_list)) "ATTDEF")
                 ;; NOT a constant attribute -- (cdr (assoc 70 e_list)) != 2)
                 (/= (logand (cdr (assoc 70 e_list)) 2) 2))
          (progn
            (if new_al
              (setq new_al (cons e_list new_al))
              (setq new_al (list e_list))
            )
            (setq na_ctr (1+ na_ctr))     ; count the number of new atts
          )
        )
      )
    )
    (setq i (1+ i))
  )
  na_ctr
)
;;;
;;; Compare the list of "old" to the list of "new" Attributes and make
;;; the two lists "same" and "preset". "Same" contains the old values of
;;; all the Attributes in "old" with equal tag values to some Attribute
;;; in "new" and the default values of all the other Attributes. "Preset"
;;; contains the preset Attributes in old with equal tag values to some
;;; Attribute in new.
;;;
(defun compare (/ i j)
  (setq i 0
        j 0
        pa_ctr 0
        same nil
        va_ctr 0
        preset nil)
  ;; "i" is a counter that increments until the number of new attributes
  ;; is reached.
  (while (< i na_ctr)
    (cond 
      ;; If there are old attributes AND the tag strings of the old and new 
      ;; attributes are the same...
      ((and old_al
            (= (cdr (assoc 2 (nth j old_al))) (cdr (assoc 2 (nth i new_al)))))
        ;; IS a preset attribute -- (cdr (assoc 70 e_list)) == 8)
        (if (= (logand (cdr (assoc 70 (nth i new_al))) 8) 8)
          ;; If the attribute is a preset attribute then add it to the list
          ;; of preset attributes and increment the counter "pa_ctr".
          ;; IS a preset attribute -- (cdr (assoc 70 e_list)) == 8)
          (progn
            (if preset
              (setq preset (cons (nth j old_al) preset))
              (setq preset (list (nth j old_al)))
            )
            (setq pa_ctr (1+ pa_ctr))     ; count preset atts
          )
          ;; Else, add it to the list of same attributes "same".
          (if same
            (setq same (cons (cdr (assoc 1 (nth j old_al))) same))
            (setq same (list (cdr (assoc 1 (nth j old_al)))))
          )
        )
        ;; If the attribute must be verified, increment counter "va_ctr".
        ;; NOT a preset attribute -- (cdr (assoc 70 e_list)) != 8)
        (if (and (/= (logand (cdr (assoc 70 (nth i new_al))) 8) 8)
                 ;; IS a verified attribute -- (cdr (assoc 70 e_list)) == 4)
                 (= (logand (cdr (assoc 70 (nth i new_al))) 4) 4))
          (setq va_ctr (+ 1 va_ctr))
        )
        (setq i (1+ i))
        (setq j 0)
      )
      ;; If the number of old attributes equals the old attribute counter "j"
      ((= j oa_ctr)
        ;; If this attribute is not a preset attribute, but is not in the 
        ;; old list, then add it to the list "same".
        ;; NOT a preset attribute -- (cdr (assoc 70 e_list)) != 8)
        (if (/= (logand (cdr (assoc 70 (nth i new_al))) 8) 8)
          (if same
            (setq same (cons (cdr (assoc 1 (nth i new_al))) same))
            (setq same (list (cdr (assoc 1 (nth i new_al)))))
          )
        )
        ;; NOT a preset attribute -- (cdr (assoc 70 e_list)) != 8)
        (if (and (/= (logand (cdr (assoc 70 (nth i new_al))) 8) 8)
                 ;; IS a verified attribute -- (cdr (assoc 70 e_list)) == 4)
                 (= (logand (cdr (assoc 70 (nth i new_al))) 4) 4))
          (setq va_ctr (+ 1 va_ctr))
        )
        (setq i (1+ i))
        (setq j 0)
      )
      ;; Increment the old attribute counter "j"...
      (t
        (setq j (1+ j))
      )
    )
  )
)
;;;
;;; Find the entity for each of the "preset" Attributes in the newly
;;; inserted Block.
;;;
(defun findpt ()
  (setq test T)
  (setq en (entnext e1))
  (setq e_list (entget en))
  (while test
    (if (and (= (cdr (assoc 0 e_list)) "ATTRIB") (= (cdr (assoc 2 e_list)) tag))
      (setq test nil)
      (progn
        (setq ex en)
        (setq en (entnext ex))
        (if e_list
          (setq e_list (entget en))
        )
      )
    )
  )
)
;;;
;;; Insert a new Block on top of each old Block and set its new Attributes
;;; to their values in the list "same". Then replace each of the "preset"
;;; Attributes with its old value.
;;;
(defun redef (/ xsf ysf zsf ls i e1 v)
  (command "_.UCS" "_E" b1)         ; define the block's UCS
  (setq xsf (cdr (assoc 41 (entget b1)))) ; find x scale factor
  (setq ysf (cdr (assoc 42 (entget b1)))) ; find y scale factor
  (setq zsf (cdr (assoc 43 (entget b1)))) ; find z scale factor
  (setq ls (length same))
  (setq i 0)
  (command "_.INSERT" bn "0.0,0.0,0.0" "_XYZ" xsf ysf zsf "0.0")
  (while (< i ls)                     ; set attributes to their values
    (command (nth i same))
    (setq i (1+ i))
  )
  (while (< 0 va_ctr)
    (command "")                      ; at prompts, verify attributes
    (setq va_ctr (1- va_ctr))
  )
  (setq i 0)
  (setq e1 (entlast))
  (while (< 0 pa_ctr)                    ; edit each of the "preset" attributes
    (setq tag (cdr (assoc 2 (nth i preset))))
    (setq v (cdr (assoc 1 (nth i preset))))
    (findpt)                          ; find the entity to modify
    (setq e_list (subst (cons 1 v) (assoc 1 e_list) e_list))
    (entmod e_list)                        ; modify the entity's value
    (setq i (1+ i))
    (setq pa_ctr (1- pa_ctr))
  )
  (command "_.UCS" "_P")                 ; restore the previous UCS
)
;;;
;;; System variable save
;;;
(defun modes (a)
  (setq mlst '())
  (repeat (length a)
    (setq mlst (append mlst (list (list (car a) (getvar (car a))))))
    (setq a (cdr a)))
)
;;;
;;; System variable restore
;;;
(defun moder ()
  (repeat (length mlst)
    (setvar (caar mlst) (cadar mlst))
    (setq mlst (cdr mlst))
  )
)
;;;
;;; Internal error handler
;;;
(defun attrerr (s)                    ; If an error (such as CTRL-C) occurs
                                      ; while this command is active...
  (if (/= s "Function cancelled")
    (princ (strcat "\nError: " s))
  )
  (moder)                             ; restore saved modes
  (setq *error* olderr)               ; restore old *error* handler
  (princ)
)
;;;
;;; Main program
;;;
(defun C:ATTREDEF (/ k n olderr bn sseto ssetn pt ssl new_al
                     old_al same preset b1 oa_ctr va_ctr na_ctr
                  ) 
  (setq k 0
      n 0
      test T
      olderr *error*
      *error* attrerr
  )

  (modes '("CMDECHO" "ATTDIA" "ATTREQ" "GRIDMODE" "UCSFOLLOW"))
  (setvar "cmdecho" 0)                ; turn cmdecho off
  (setvar "attdia" 0)                 ; turn attdia off
  (setvar "attreq" 1)                 ; turn attreq on
  (setvar "gridmode" 0)               ; turn gridmode off
  (setvar "ucsfollow" 0)              ; turn ucsfollow off

  (while 
    (progn
      (setq bn (xstrcase (getstring 
        "\nName of Block you wish to redefine: ")))
      (if (tblsearch "block" bn)
        (progn
          (setq sseto (ssget "_x" (list (cons 2 bn))))
          (setq test nil)
        )
        (progn
          (princ "\nBlock ")
          (princ bn)
          (princ " is not defined. Please try again.\n")
        )
       )
    )
  )
  (if sseto
    (progn
      (while 
        (progn
          (princ "\nSelect objects for new Block... ")
          (if (null (setq ssetn (ssget)))
            (princ "\nNo new Block selected. Please try again.")
            (setq test nil)
          )
        )
      )
      ;; find the list of new attributes
      (setq na_ctr (newatts ssetn (sslength ssetn)) )
      (if (> na_ctr 0)
        (progn
          (initget 1)
          (setq pt (getpoint "\nInsertion base point of new Block: "))
          (setq ssl (sslength sseto))
          ;; redefine the block
	  (command "_.UNDO" "_GROUP")
          (command "_.BLOCK" bn "_Y" pt ssetn "") 
          (while (< k ssl)
            (setq b1 (ssname sseto k))    ; For each old block...
            (setq old_al nil)
            (oldatts)                     ; find the list of old attributes,
            (compare)                     ; compare the old list with the new,
            (redef)                       ; and redefine its attributes.
            (entdel b1)                   ; delete the old block.
            (setq k (1+ k))
          )
          (command "_.REGENALL")
	  (command "_.UNDO" "_END")
        )
        (princ "\nNew block has no attributes. ")
      )
    )
    (princ (strcat "\nNo insertions of block " bn " found to redefine. "))
  )
  (moder)                             ; restore saved modes
  (setq *error* olderr)               ; restore old *error* handler
  (princ)
)

(defun c:at () (c:attredef))
(princ 
"\nC:ATtredef loaded.  Start command with AT or ATTREDEF.")
(princ)
