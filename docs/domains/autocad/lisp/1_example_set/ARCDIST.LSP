;;; YZ Enterprise
;;;
;;; LISP routine for finding arc lenght 
;;; 
;;; by Allan Wise   12-Nov-1997
;;;
(princ "\nFind the arc length ")
(princ "\nType Arcdist to begin...... ")
(defun c:arcdist ( )
      (setq chord (getdist "\nPick two points on arc:  "))
      (setq et (nentsel "\nSelect arc:  "))
      (setq radius (cdr (assoc 40(entget (car et)))))
      (setq x1 (* 2 pi radius))
      (setq hc (* 0.5 chord))
      (setq x2 (- (* radius radius)(* hc hc)))
      (setq x3 (expt x2 0.5))
      (setq theta (/ hc x3))
      (setq ang (atan theta))
      (setq a1 ( * 2 ang))
      (setq a2 (rtd a1))
      (setq a3 (/ a2 360))
      (setq ans (* a3 x1))
      (prompt "\nArc length is ")
      (princ ans)
      (princ) 
) 

(defun rtd (a)
     (* 180 ( / a pi))
)
