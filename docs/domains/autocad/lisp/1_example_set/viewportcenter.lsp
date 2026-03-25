;;; By Jimmy Bergmark
;;; Copyright (C) 1997-2006 JTB World, All Rights Reserved
;;; Website: www.jtbworld.com
;;; E-mail: info@jtbworld.com
(vl-load-com)
;;; get the viewportcenter
(setq viewportcenter
  (vlax-safearray->list
   (vlax-variant-value
     (vla-get-center
       (vla-get-activepviewport
         (vla-get-activedocument
           (vlax-get-acad-object))))))
)



