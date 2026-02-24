(defun XrefRename (oldname newname newpath)
  (if (and (findfile (strcat newpath newname ".dwg"))
    (ssget "X" (list '(0 . "INSERT") (cons 2 oldname))))
    (progn
      ; rename the xref if it is found
      (command "rename" "b" oldname newname)
      ; change the path of the xref if it is found
     (command
       "-xref"
       "p"
       newname
       (strcat newpath newname)
     )
   )
  )
)