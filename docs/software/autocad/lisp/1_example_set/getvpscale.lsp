;;; getvpscale.lsp
;;;
;;; Get Viewport Scale in active viewport or in selected
;;; Supports viewports with clipping boundary
;;; By Jimmy Bergmark
;;; Copyright (C) 1997-2006 JTB World, All Rights Reserved
;;; Website: www.jtbworld.com
;;; E-mail: info@jtbworld.com
;;; 2000-05-03 - First release
;;; 2000-05-09 - Detects perspective view
;;; Tested on AutoCAD 2000
(defun c:getvpscale (/ ss ent)
(defun printscale (/  data cvsize cvhgt)
  (setq cvscale (vla-get-customscale (vlax-ename->vla-object ent)))
  (princ "\nPS:MS == ")
  (cond
    ((> cvscale 1)
     (princ (rtos cvscale 2))
     (princ ":1")
    )
    (T
     (princ "1:")
     (princ (rtos (/ 1 cvscale) 2))
    )
  )
)
  (vl-load-com)
  (if (= (getvar "tilemode") 0)
    (if (= (getvar "cvport") 1)
      (if (/= (setq ss (ssget ":E:S" '((0 . "VIEWPORT")))) nil)
        (if (/= 1 (logand 1 (cdr (assoc 90 (entget (setq ent (ssname ss 0)))))))
          (printscale)
          (princ "\n Command not allowed in perspective view.")
        )
        (princ " No viewport found.")
      )
      (progn
        (setq ent (vlax-vla-object->ename
                    (vla-get-activepviewport
                      (vla-get-activedocument (vlax-get-acad-object)))))
        (if (/= 1 (logand 1 (cdr (assoc 90 (entget ent)))))
          (printscale)
          (princ "\n Command not allowed in perspective view.")
        )
      )
    )
    (princ "\n Command not allowed unless TILEMODE is set to 0.") 
  )
  (princ)
)

;;; return viewport scale if allowed else nil
(defun getvpscale1 (/ ss ent)
  (vl-load-com)
  (if (= (getvar "tilemode") 0)
    (if (= (getvar "cvport") 1)
      (if (/= (setq ss (ssget ":E:S" '((0 . "VIEWPORT")))) nil)
        (if (/= 1 (logand 1 (cdr (assoc 90 (entget (setq ent (ssname ss 0)))))))
          (vla-get-customscale (vlax-ename->vla-object ent))
        )
      )
      (progn
        (setq ent (vlax-vla-object->ename
                    (vla-get-activepviewport
                      (vla-get-activedocument (vlax-get-acad-object)))))
        (if (/= 1 (logand 1 (cdr (assoc 90 (entget ent)))))
          (vla-get-customscale (vlax-ename->vla-object ent))
        )
      )
    )
  )
)

;;; return viewport scale if allowed else nil
;;; no support for perspective view
(defun getvpscale2 (/ ss vpno vpsc)
  (if (= (getvar "tilemode") 0)
    (if (= (getvar "cvport") 1)
      (if (/= (setq ss (ssget ":E:S" '((0 . "VIEWPORT")))) nil)
        (progn
          (setq vpno (cdr (assoc 69 (entget (ssname ss 0)))))
          (command "_.mspace")
          (setvar "cvport" vpno)
          (setq vpsc (caddr (trans '(0 0 1) 2 3)))
          (command "_.pspace")
          vpsc
        )
      )
      (caddr (trans '(0 0 1) 2 3))
    )
  )
)

;;; return viewport scale
;;; no support for viewports with clipping boundary
;;; no support for perspective view
(defun getvpscale3(/ vpno vpsc)
  (setq vpno (cdr (assoc 69 (entget (car (entsel))))))
  (command "mspace")
  (setvar "cvport" vpno)
  (setq vpsc (caddr (trans '(0 0 1) 2 3)))
  (command "pspace")
  vpsc
)

;;; return scale in active viewport
;|
(caddr (trans '(0 0 1) 2 3))
|;




