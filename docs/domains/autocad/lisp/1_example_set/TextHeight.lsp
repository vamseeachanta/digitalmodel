;;; th.LSP ver 1.0
;;; Set selected text or mtext to specified height
;;; Text is resized based on the justification point
;;; By Jimmy Bergmark
;;; Copyright (C) 2008 JTB World, All Rights Reserved
;;; Website: www.jtbworld.com
;;; E-mail: info@jtbworld.com
;;; 2008-03-14 - First release
;;; Tested on AutoCAD 2009
;;; should be working on older and newer versions too.

(defun c:th (/ sset i txtheight textsize)
  (vl-load-com)
  (setq i 0)
  (setq sset (ssget '((-4 . "<OR") (0 . "MTEXT") (0 . "TEXT") (-4 . "OR>"))))
  (if sset
   (progn
    (setq textsize (getvar "textsize"))
    (setq txtheight (getdist (strcat "\nSpecify text height <" (rtos textsize) ">: ")))
    (if (= txtheight nil) (setq txtheight textsize))
    (repeat (sslength sset)
      (vla-put-height (vlax-ename->vla-object (ssname sset i)) txtheight)
      (setq i (1+ i))
    )
    (setq sset nil)
   )
  )
  (princ)
)

(princ "\nRun with the TH command")
(princ)