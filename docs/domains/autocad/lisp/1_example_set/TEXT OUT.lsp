; TEXTOUT.LSP - KETIV
;
; Copyright 1986, 1987, 1988, 1989 by KETIV Technologies, Inc.
; All rights reserved.
; KETIV Technologies, Inc.
; 6645 NE 78th Court #C-2
; Portland, OR 97218
;
; Layer Group: None
;
; Menu Location: Text I/O pull down
;
; Purpose: To export text strings from a drawing to an ascii text file.
;
; Prompts: Filename to write <Name.ext>
;          Pick the text in the order in which it will be written to the file:
;          Select objects:
;          XX Lines of text written to "file"
;
; Assumptions/Limitations: None
;
;======================================================================================================
; Initialize memory and important system variables
(defun c:textout (/ cnt e file1 fn mrk sl ss1)
  (setq clayer nil)
  (setq hdrlst (list
                 (cons "expert" (getvar "expert"))
               )
  )
                                (setvar "cmdecho" 0)
                                (setvar "expert"  1)
  ;======================================================================================================
  ; Initialize Important variables
  (setq mrk 0 FN nil SS1 nil E nil FILE1 nil)
  ;======================================================================================================
  ;Select filename to write to
  (while (or (= FN "") (= FN nil))
    (setq FN (getfiled "Filename to write" "" "txt" 1))
;;;    (setq FN (strcase (getstring"\nFilename to write <Name.ext> ") t))
  )
  ;======================================================================================================
  ;Check for file
;;;  (setq FILE1 (open FN "r"))
;;;  (if (/= FILE1 nil)
;;;    (progn
;;;      (prompt "\nFile already exist! ")
;;;      (close FILE1)
;;;    )
    (progn
      ;======================================================================================================
      ;Open file and select lines to write
      (setq FILE1 (open FN "w"))
      (if file1
        (progn
          (prompt "\nPick the text in the order in which it will be written to the file: ")
          (setq ss1 (ssget))
          (setq SL (sslength ss1))
          (setq CNT 0)
          ;======================================================================================================
          ;Write lines to file
          (repeat SL
            (if (= "TEXT" (cdr (assoc 0 (setq E (entget (ssname ss1 CNT))))))
              (progn
                (write-line (cdr (assoc 1 E)) FILE1)
                (setq mrk (+ mrk 1))
              )
            )
            (setq CNT (+ CNT 1))
          )
          (close FILE1)
        )
        (prompt "\nInvalid file name")
      )
    )
;;;  )
  (prompt "\n")
  (prin1 mrk) (prompt "  Lines of text written to ") (prompt FN)
  ;======================================================================================================
  ; Reset system variables and AutoLISP memory
  (foreach cnt hdrlst (setvar (car cnt) (cdr cnt)))
  (princ)
)
(princ)