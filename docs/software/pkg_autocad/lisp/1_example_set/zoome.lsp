;;; zoome.lsp
;;;
;;; Zoom extents in all viewports
;;;
;;; By Jimmy Bergmark
;;; Copyright (C) 1997-2006 JTB World, All Rights Reserved
;;; Website: www.jtbworld.com
;;; E-mail: info@jtbworld.com
;;; 2000-08-29
;;; Tested on AutoCAD 2000

(defun c:zoome (/ oldcmdecho vplist curcvport nr vpss ms en x)
  (setq oldcmdecho (getvar "cmdecho"))
  (setvar "cmdecho" 0)
  (setq vplist (mapcar 'car (vports)))
  (setq curcvport (getvar "cvport"))
  (if (= (getvar "tilemode") 0)
    (progn
      (if (= (setq ms (getvar "cvport")) 1)
        (command "._mspace")
      )
      (setq vpss (ssget "_x"
                        (list '(-4 . "<AND")
                              '(0 . "VIEWPORT")
                              (cons 410 (getvar "ctab"))
                              '(-4 . "<NOT")
                              '(69 . 1)
                              '(-4 . "NOT>")
                              '(-4 . "AND>")
                        )
                 )
      )
      (setq nr 0)
      (if vpss                          ; in case there are no viewports
        (repeat (sslength vpss)
          (setq en (entget (ssname vpss nr)))
          (if (and (= 0 (logand 1 (cdr (assoc 90 en))))
                                        ; not perspective
                   (< 0 (cdr (assoc 68 en))) ; on and active
                   (/= 16384 (logand 16384 (cdr (assoc 90 en))))
                                        ; not locked
              )
            (progn
              (setvar "cvport" (cdr (assoc 69 en)))
              (command "._zoom" "_e")
            )
          )
          (setq nr (+ 1 nr))
        )
      )
      (if (= ms 1) (command "._pspace"))
    )
    (foreach x vplist
      (setvar "cvport" x)
      (command "._zoom" "_e")
    )
  )
  (setvar "cvport" curcvport)
  (setvar "cmdecho" oldcmdecho)
  (princ)
)




