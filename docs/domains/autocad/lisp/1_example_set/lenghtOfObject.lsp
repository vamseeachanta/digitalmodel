;;; By Jimmy Bergmark
;;; Copyright (C) 1997-2006 JTB World, All Rights Reserved
;;; Website: www.jtbworld.com
;;; E-mail: info@jtbworld.com
;;; returns the lenght or the perimeter of selected object
;;; Made for AutoCAD 2000
(vl-load-com)
(defun c:lenghtOfObject (/ en curve len)
  (if (setq en (entsel))
    (progn
      (setq curve (vlax-ename->vla-object (car en)))
      (if (vl-catch-all-error-p
            (setq len (vl-catch-all-apply
                        'vlax-curve-getDistAtParam
                        (list curve
                              (vl-catch-all-apply
                                'vlax-curve-getEndParam
                                (list curve)
                              )
                        )
                      )
            )
          )
        nil
        len
      )
    )
  )
)



