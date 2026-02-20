;ŞÏ ÊÖØÑ ÃÍíÇäÇğ áÌãÚ ÃÑŞÇã ãæÌæÏå Úáì ÇáÔÇÔå ÃãÇãß
;İÅäå ãä ÇáÃİÖá Ãä ÊŞæã ÈÚãáíÉ ÇáÌãÚ ÈÔßá ãÈÇÔÑ ÈÏæä ÇáÇÓÊÚÇäå ÈÇáÂáå ÇáÍÇÓÈå
;ãÇÚáíß Óæì  Çä ÊäŞÑ Úáì ÇáÑŞã ãÚ ÒÑ ÇáÇÏÎÇá İÊÑì ÇáÑŞã İí ÓØÑ ÇáÍÇáå ÊäŞÑ Úáì ÇáÑŞã ÇáËÇäí ãÚ
;ÒÑ ÇáÇÏÎÇá ÊÑì ÇáãÌãæÚ  æåßĞÇ ÍÊì ÊÊæŞİ Úä ÇáÇÏÎÇá
; add áÊÔÛíá ÇáÈÑäÇãÌ Íãáå Ëã ÇßÊÈ 
(defun c:add ()
(setq sum (list + ))
  (while
    (setq pt1 (ssget))
    (Setvar "osmode" 0)
    (setq oldelem (entget (ssname pt1 0)))      
  (setq txtstr (assoc 1 oldelem))
  (setq num (atof(cdr txtstr)))                                       
  (setq sum (append sum (list num)))
  (terpri)
  (princ num)
  (terpri)
  (princ "total=")
  (princ (rtos(eval sum)2 3))
   )
  )
