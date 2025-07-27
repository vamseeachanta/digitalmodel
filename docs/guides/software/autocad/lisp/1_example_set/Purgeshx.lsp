;; Downloaded from CADTutor forum

(defun C:PSHX ()

  (vl-load-com)
  (vlax-for item
         (vla-get-textstyles
           (vla-get-ActiveDocument (vlax-get-acad-object))
         )
    (if
      (not
    (vl-filename-extension (setq fname (vla-get-fontfile item)))
      )
       (setq fname (strcat fname ".shx"))
    )
    (cond ((findfile fname) nil)
      ((findfile (strcat (getenv "WINDIR") "\\FONTS\\" fname))
       nil
      )
      (t
       (vla-put-fontfile item "ltypeshp.shx")
       (princ "\nChange ")
       (princ fname)
       (princ " on ltypeshp.shx")
      )
    )
  )
  (princ)
)
(princ "\nPurge unreferenced shape files, Lisp Command : PSHX")