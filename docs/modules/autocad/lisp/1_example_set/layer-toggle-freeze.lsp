;;; toggle the freeze state of layer
;;; and regens only the objects on the layer
;;; but if layer is xref dependant it regens
;;;
;;; By Jimmy Bergmark
;;; Copyright (C) 1997-2006 JTB World, All Rights Reserved
;;; Website: www.jtbworld.com
;;; E-mail: info@jtbworld.com
;;; 2000-03-05 - First release
;;;
;;; Tested on AutoCAD 2000
;;; Argument: layer {list of layers}
;;; Example: (layer-toggle-freeze '("Layer1" "Layer2"))

(defun layer-toggle-freeze (layer / en f ss ent i fg)
  (foreach la layer
    (setq en (entget (tblobjname "layer" la)))
    (setq f (cdr (assoc 70 en)))
    (setq f (boole 6 f 1))
    (if (and (wcmatch la "*|*") (eq 0 (logand 1 f))) (setq rg T))
    (setq en (subst (cons 70 f) (assoc 70 en) en))
    (entmod en)
    (setq ss (ssget "X" (list (cons 8 la))))
    (setq i 0)
    (if ss 
      (while (setq ent (ssname ss i))
        (entupd ent)
        (setq i (1+ i))
      )
    )
  )
  (setq sset nil)
  (if rg (command "._regenall"))
  (princ)
)