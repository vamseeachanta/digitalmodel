;;;---------------------------------------------------------------------------;
;;;
;;; layers-erase.lsp
;;;
;;; By Jimmy Bergmark
;;; Copyright (C) 1997-2006 JTB World, All Rights Reserved
;;; Website: www.jtbworld.com
;;; E-mail: info@jtbworld.com
;;;
;;; 2000-05-25 - First release
;;; Tested on AutoCAD 2000
;;;
;;;---------------------------------------------------------------------------;
;;;  DESCRIPTION
;;;
;;;  c:layers-erase - Erase all layers that are frozen or off
;;;---------------------------------------------------------------------------;
(vl-load-com)
(defun c:layers-erase (/ la)
  (defun layer-del (layer / e d c f)
    (if (setq e (tblobjname "layer" layer))
      (progn
        (setq
          d   (entget e)
          c   (cdr (assoc 62 d))
          f   (cdr (assoc 70 d))
          del nil
        )
        (if (minusp c)
                                        ; layer is off, force abs of color
          (progn (setq del T)
                 (setq d (subst (cons 62 (abs c)) (assoc 62 d) d))
          )
        )
        (if (eq 1 (logand 1 f))
                                        ; layer is frozen, mask off 1
          (progn (setq del T)
                 (setq f (boole 6 f 1))
          )
        )
        (if (eq 4 (logand 4 f))
                                        ; layer is locked, mask off 4
          (setq f (boole 6 f 4))
        )
                                        ; did we change the flag value?
        (if (not (eq f (cdr (assoc 70 d))))
          (setq d (subst (cons 70 f) (assoc 70 d) d))
        )
                                        ; did we change the dxf data at all?
        (if (not (equal d (entget e)))
          (entmod d)
        )
        (if del
          (progn
            (setq ss  (ssget "X" (list (cons 8 layer)))
                  doc (vla-get-activedocument (vlax-get-acad-object))
                  c   -1
            )
            (vla-put-activeLayer
              doc
              (vla-item (vla-get-layers doc) "0")
            )
            (if ss
              (repeat (sslength ss)
                (vla-erase
                  (vlax-ename->vla-object (ssname ss (setq c (1+ c))))
                )
              )
            )
;;; purge the layer
            (vl-catch-all-apply
              'vla-delete
              (list (vla-item (vla-get-layers doc) layer))
            )
;;; if not purged freeze it again
            (if (setq e (tblobjname "layer" layer))
              (command "._layer" "_f" layer "")
            )
          )
        )
      )
    )
  )
  (vlax-for la (vla-get-layers
                 (vla-get-activedocument (vlax-get-acad-object))
               )
    (layer-del (vla-get-name la))
  )
)




