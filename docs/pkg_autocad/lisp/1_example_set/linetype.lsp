;;; By Jimmy Bergmark
;;; Copyright (C) 1997-2006 JTB World, All Rights Reserved
;;; Website: www.jtbworld.com
;;; E-mail: info@jtbworld.com
(vl-load-com)
;;; (loadLinetype (vla-get-activedocument (vlax-get-acad-object)) "Divide" "acadiso.lin")
;;; returns: T if loaded else nil
(defun loadLinetype (doc LineTypeName FileName)
  (if (and
        (not (existLinetype doc LineTypeName))
        (vl-catch-all-error-p
          (vl-catch-all-apply
            'vla-load
            (list
              (vla-get-Linetypes doc)
              LineTypeName
              FileName
            )
          )
        )
      )
    nil
    T
  )
)

;;; (existLinetype (vla-get-activedocument (vlax-get-acad-object)) "Divide")
(defun existLinetype (doc LineTypeName / item loaded)
  (vlax-for item (vla-get-linetypes doc)
    (if (= (strcase (vla-get-name item)) (strcase LineTypeName))
      (setq loaded T)
    )
  )
)

;;; (purgeAllLinetypes (vla-get-activedocument (vlax-get-acad-object)))
(defun purgeAllLinetypes (doc / item)
  (vlax-for item (vla-get-linetypes doc)
    (vl-catch-all-apply 'vla-delete (list item))
  )
)



