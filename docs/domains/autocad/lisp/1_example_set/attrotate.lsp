(defun c:attrot (/ adoc selset)
  (vl-load-com)
  (vla-startundomark
    (setq adoc (vla-get-activedocument (vlax-get-acad-object)))
    ) ;_ end of vla-startundomark
  (if (setq selset (ssget "_:L" '((0 . "INSERT") (66 . 1))))
    (foreach blk (mapcar 'vlax-ename->vla-object
                         (vl-remove-if 'listp (mapcar 'cadr (ssnamex selset)))
                         ) ;_ end of mapcar
      (vl-catch-all-apply
        '(lambda ()
           (foreach att (vlax-safearray->list
                          (vlax-variant-value (vla-getattributes blk))
                          ) ;_ end of vlax-safearray->list
             (vl-catch-all-apply
               '(lambda ()
                  (vla-put-rotation att .15)
                  ) ;_ end of lambda
               ) ;_ end of vl-catch-all-apply
             ) ;_ end of foreach
           ) ;_ end of lambda
        ) ;_ end of vl-catch-all-apply
      ) ;_ end of foreach
    ) ;_ end of if
  (vla-endundomark adoc)
  (princ)
  ) ;_ end of defun
(princ "\nType ATTROT in command line to start lisp")