;;; By Jimmy Bergmark
;;; Copyright (C) 1997-2006 JTB World, All Rights Reserved
;;; Website: www.jtbworld.com
;;; E-mail: info@jtbworld.com

;;; (ax:CreateVP (vla-get-activedocument (vlax-get-acad-object)) '(200 200 0) 150 100)

(defun c:CreateVP (ad center width height / ps ent)
  (setq ps (vla-get-paperspace ad))
  (vla-put-activespace ad acpaperspace)
  (vla-put-mspace ad :vlax-false)
  (setq ent
         (vla-addpviewport
           ps
           (vlax-safearray-fill
             (vlax-make-safearray
               vlax-vbdouble
               (cons 0 2)
             )
             center
           )
           width
           height
         )
  )
  (vla-put-viewporton ent :vlax-true)
  (vla-display ent :vlax-true)
  (vla-update ent)
)




