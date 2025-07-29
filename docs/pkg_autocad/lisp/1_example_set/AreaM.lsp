;;; AREAM.LSP
;;; Function: Calculates the total area of selected objects
;;; By Jimmy Bergmark
;;; Copyright (C) 1997-2006 JTB World, All Rights Reserved
;;; Website: www.jtbworld.com
;;; E-mail: info@jtbworld.com
;;; Tested on AutoCAD 2000

(defun c:aream (/ olderr oldcmdecho errexit undox restore ss1 nr en tot_area)
  (defun errexit (s)
    (restore)
  )

  (defun undox ()
    (command "._undo" "_E")
    (setvar "cmdecho" oldcmdecho)
    (setq *error* olderr)
    (princ)
  )

  (setq olderr  *error*
        restore undox
        *error* errexit
  )
  (setq oldcmdecho (getvar "cmdecho"))
  (setvar "cmdecho" 0)
  (command "._UNDO" "_BE")
  (if (setq ss1 (ssget '((-4 . "<OR")
                         (0 . "POLYLINE")
                         (0 . "LWPOLYLINE")
                         (0 . "CIRCLE")
                         (0 . "ELLIPSE")
                         (0 . "SPLINE")
                         (0 . "REGION")
                         (-4 . "OR>")
                        )
                )
      )
    (progn
      (setq nr 0)
      (setq tot_area 0.0)
      (setq en (ssname ss1 nr))
      (while en
        (command "._area" "_O" en)
        (setq tot_area (+ tot_area (getvar "area")))
        (setq nr (1+ nr))
        (setq en (ssname ss1 nr))
      )
      (princ "\nTotal Area = ")
      (princ tot_area)
    )
  )
  (restore)
)




