;;;
;;;    LayoutsToDwgs.lsp
;;;    Created 2000-03-27

;;; By Jimmy Bergmark
;;; Copyright (C) 1997-2006 JTB World, All Rights Reserved
;;; Website: www.jtbworld.com
;;; E-mail: info@jtbworld.com
;;;
;;; 2003-12-12 Sets UCS to world in model space to avoid problem with wblock
;;;

;;;    For AutoCAD 2000, 2000i, 2002, 2004
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   Creates drawings of all layouts.
;;;   Only one layout at a time is saved, the rest are deleted.
;;;   This is handy when you want to save to pre A2k versions.
;;;   The new drawings are saved to the current drawings path
;;;   and overwrites existing drawings.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:LayoutsToDwgs (/ fn path msg msg2 fileprefix)
  (defun DelAllLayouts (Keeper / TabName)
    (vlax-for Layout
                     (vla-get-Layouts
                       (vla-get-activedocument (vlax-get-acad-object))
                     )
      (if
        (and
          (/= (setq TabName (strcase (vla-get-name layout))) "MODEL")
          (/= TabName (strcase Keeper))
        )
         (vla-delete layout)
      )
    )
  )

  (vl-load-com)
  (setq msg "")
  (setq msg2 "")
  (command "._undo" "_BE")
  (setq fileprefix (getstring "Enter filename prefix: "))
  (foreach lay (layoutlist)
    (if (/= lay "Model")
      (progn
        (command "_.undo" "_M")
        (DelAllLayouts lay)
        (setvar "tilemode" 1)
        (command "ucs" "w")
        (setvar "tilemode" 0)
        (setq path (getvar "DWGPREFIX"))
        (setq fn (strcat path fileprefix lay ".dwg"))
        (if (findfile fn)
          (progn
            (command ".-wblock" fn "_Y")
            (if (equal 1 (logand 1 (getvar "cmdactive")))
              (progn
                (setq msg (strcat msg "\n" fn))
                (command "*")
              )
              (setq msg2 (strcat msg2 "\n" fn))
            )
          )
          (progn
            (command ".-wblock" fn "*")
            (setq msg (strcat msg "\n" fn))
          )
        )
        (command "_.undo" "_B")
      )
    )
  )
  (if (/= msg "")
    (progn
      (prompt "\nFollowing drawings were created:")
      (prompt msg)
    )
  )
  (if (/= msg2 "")
    (progn
      (prompt "\nFollowing drawings were NOT created:")
      (prompt msg2)
    )
  )
  (command "._undo" "_E")
  (textscr)
  (princ)
)



