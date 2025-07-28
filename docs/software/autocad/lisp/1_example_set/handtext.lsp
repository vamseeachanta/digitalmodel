; ----------------------------------------------------------------------
;            HANDEXP (Exports handles & strings to text file)
;       HANDIMP (Imports file created by HANDEXP and updates text)
;            Copyright (C) 1999 DotSoft, All Rights Reserved
;                    Website: http://www.dotsoft.com
; ----------------------------------------------------------------------
; DISCLAIMER:  DotSoft Disclaims any and all liability for any damages
; arising out of the use or operation, or inability to use the software.
; FURTHERMORE, User agrees to hold DotSoft harmless from such claims.
; DotSoft makes no warranty, either expressed or implied, as to the
; fitness of this product for a particular purpose.  All materials are
; to be considered ‘as-is’, and use of this software should be
; considered as AT YOUR OWN RISK.
; ----------------------------------------------------------------------

;
; --- Export Text Strings to File
;
(defun C:HANDEXP ()
  (setq sset (ssget '((0 . "TEXT"))))
  (if sset
    (progn
      (setq itm 0)
      (setq num (sslength sset))
      (setq fn (getfiled "Text Export File" "" "txt" 1))
      (if (/= fn nil)
        (progn
          (setq fh (open fn "w"))
          (while (< itm num)
            (setq hnd (ssname sset itm))
            (setq ent (entget hnd))
            (setq nam (cdr (assoc 5 ent)))
            (setq nam (strcat "'" nam))
            (setq val (cdr (assoc 1 ent)))
            (princ (strcat nam "\n") fh)
            (princ (strcat val "\n") fh)
            (setq itm (1+ itm))
          )
          (close fh)
        )
      )
    )
  )
  (setq sset nil)
  (princ)
)
;
; --- Import Text Strings from File
;
(defun C:HANDIMP ()
  (setq fn (getfiled "Text Import File" "" "txt" 0))
  (if (/= fn nil)
    (progn
      (setq done nil)
      (setq fh (open fn "r"))
      (while (/= done T)
        (setq lin (read-line fh))
        (if (= lin nil)
          (setq done T)
          (progn
            (if (= (substr lin 1 1) "'")
              (progn
                (setq chk (substr lin 2 (- (strlen lin) 1)))
                (setq hnd (handent chk))
              )
              (progn
                (setq str lin)
                (setq ent (entget hnd))
                (setq ent (subst (cons 1 str)(assoc 1 ent) ent))
                (entmod ent)
              )
            )
          )
        )
      )
      (close fh)
    )
  )
  (princ)
)
