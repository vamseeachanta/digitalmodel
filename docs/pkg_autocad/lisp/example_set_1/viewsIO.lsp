;;; viewsIO.lsp
;;;
;;; Export and import views
;;;
;;; c:ExportViews
;;; c:ImportViews
;;; c:-ExportViews
;;; c:-ImportViews
;;;
;;; By Jimmy Bergmark
;;; Copyright (C) 1997-2008 JTB World, All Rights Reserved
;;; Website: www.jtbworld.com
;;; E-mail: info@jtbworld.com
;;;
;;; 2000-06-27
;;;
;;; Tested on AutoCAD 2000-2004
;;; Does not work in later versions
;;; To do: support for views with saved ucs and support for later versions

(defun c:ExportViews (/ fn)
  (if (setq fn
             (getfiled "Export views to"
                       (strcat (vl-filename-base (getvar "dwgname")) ".txt")
                       "txt"
                       1
             )
      )
    (ExportViews fn)
  )
  (princ)
)

(defun c:ImportViews (/ fn)
  (if (setq fn
             (getfiled "Import views to"
                       (strcat (vl-filename-base (getvar "dwgname")) ".txt")
                       "txt"
                       1
             )
      )
    (ImportViews fn)
  )
  (princ)
)

(defun c:-ExportViews (/ fn x)
  (setq fn (strcat (vl-filename-base (getvar "dwgname")) ".txt"))
  (if (setq fn
             (findfile
               (if (= ""
                      (setq nn (getstring
                                 T
                                 (strcat "Enter filename <"
                                         fn
                                         ">: "
                                 )
                               )
                      )
                   )
                 fn
                 nn
               )
             )
      )
    (progn
      (initget "Yes No")
      (setq x (getkword "\nFile exists.  Overwrite? [Yes/No] <No>: "))
      (if (= x "Yes") (ExportViews fn))
    )
    (princ "\nFile not found.")
  )
  (princ)
)

(defun c:-ImportViews (/ fn)
  (setq fn (strcat (vl-filename-base (getvar "dwgname")) ".txt"))
  (if (setq fn
             (findfile
               (if (= ""
                      (setq nn (getstring
                                 T
                                 (strcat "Enter filename <"
                                         fn
                                         ">: "
                                 )
                               )
                      )
                   )
                 fn
                 nn
               )
             )
      )
    (ImportViews fn)
    (princ "\nFile not found.")
  )
  (princ)
)

(defun ExportViews (fn / e tl f ed)
  (while (setq e (tblnext "VIEW" (null e)))
    (setq tl (cons (cdr (assoc 2 e)) tl))
  )
  (setq f (open fn "w"))
  (if f
    (progn
      (princ "Following views exported:\n")
      (foreach view tl
        (setq ed (entget (tblobjname "view" view)))
        (prin1 (cons (cons 0 "VIEW") (cdddr ed)) f)
        (princ "\n" f)
        (prin1 view)
        (terpri)
      )
      (close f)
    )
  )
)

(defun ImportViews (fn / tl f)
  (setq f (open fn "r"))
  (if f
    (progn
      (princ "Following views imported:\n")
      (while (setq tl (read-line f))
        (setq tl (read tl))
        (entmake tl)
        (print (cdr (assoc 2 tl)))
      )
      (close f)
    )
  )
)

(princ)



