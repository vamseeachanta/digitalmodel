;;; By Jimmy Bergmark
;;; Copyright (C) 1997-2006 JTB World, All Rights Reserved
;;; Website: www.jtbworld.com
;;; E-mail: info@jtbworld.com
;;;
;;; Change the hatch base point on one or many hatches
;;;
(defun c:hatchbase (/ oldos oldsn oldcmdecho i ent)
  (setq oldos (getvar "osmode"))
  (setq oldsn (getvar "snapbase"))
  (setq oldcmdecho (getvar "cmdecho"))
  (setvar "osmode" 47)
  (princ "\nSelect point you wish Hatch(s) to start from...")
  (command "._snapbase" pause)
  (princ "\nSelect Hatch(s) to adjust snapbase")
  (if (not (setq ss (ssget)))
    (alert "\n No Entities selected..... Please try again.")
    (progn
      (setq i 0)
      (while (setq ent (ssname ss i))
	(command "._hatchedit" ent "" "" "" "")
	(setq i (1+ i))
      )
    )
  )
  (setvar "snapbase" oldsn)
  (setvar "osmode" oldos)
  (setvar "cmdecho" oldcmdecho)
  (princ)
)



