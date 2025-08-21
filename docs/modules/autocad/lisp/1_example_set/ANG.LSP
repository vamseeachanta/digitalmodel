;;; 
;;; ANG.LSP Version 1.0 Copyright (C) by DO MANH HIEN 44X5-HUCE.
;;;

(defun c:ang (/ ent pt1 pt2 ang)
  (setq cmd (getvar "cmdecho"))
  (setvar "cmdecho" 1)
  (prompt (strcat"\nSnap angle will be set to angle of line"))
  (setq ent (entsel "\nSelect a line:"))
  (if ent 
   (progn
    (setq ent (entget (car ent)))
    (setq pt1 (cdr (assoc 10 ent)))
    (setq pt2 (cdr (assoc 11 ent)))
    (setq ang (angle pt1 pt2))
    (setq ang (/ (* ang 180.0) pi))
    (setvar "cmdecho" 0)
    (command "setvar" "snapang" ang)
    (princ "\nNew snap angle: ")
    (princ ang)
   )
  )
  (setvar "cmdecho" cmd)
  (princ)
)
