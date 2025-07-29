(defun c:sarea(/ aSum cSet cSet)
  (vl-load-com)
  (setq aSum 0)
  (if
    (setq cSet
	   (ssget '((0 . "*POLYLINE,SPLINE,CIRCLE,ELLIPSE"))))
    (progn
      (foreach c(vl-remove-if 'listp
                  (mapcar 'cadr(ssnamex cSet)))
	(if(vlax-curve-IsClosed c)
	  (setq aSum(+ aSum(vlax-curve-GetArea c)))
	  (ssdel c cSet)
	  ); end if
	); end foreach
      (princ(strcat "\nTotal area = " (rtos aSum)))
      (sssetfirst nil cSet)
      ); end progn
    ); end if
  (princ)
  ); end of c:sarea