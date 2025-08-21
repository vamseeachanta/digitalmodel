;;;
;;; =========================================================================
;;; DIMLINECHANGE.LSP
;;;
;;; By Jimmy Bergmark
;;; Copyright (C) 1997-2006 JTB World, All Rights Reserved
;;; Website: www.jtbworld.com
;;; E-mail: info@jtbworld.com
;;;
;;; 2000-03-02
;;;
;;; Description: Changes selected line on a dimension to current layer.
;;;              This layer can be with another linetype.
;;;              So, it can be used for dimensioning to centerlines.
;;;
(defun C:DIMLINECHANGE (/ nents nlst ent nen lasten pen ent en oe tbl)

  (defun *error* (MSG)
    (princ MSG)
    (princ)
  )

  (if (setq nents (nentsel))
    (progn
      (setq nen (car nents))
      (if (setq nlst (cadddr nents))
	(if (> (length nlst) 1)
	  (*error* "Nested block!\n")
	)
	(*error* "\nNo dimension selected!")
      )
      (setq pen (car nlst))
      (if (= (cdr (assoc 0 (entget pen))) "DIMENSION")
	(cond
	  ((= (cdr (assoc 0 (setq ent (entget nen)))) "LINE")
	   (if (setq
		 tbl (tblsearch "BLOCK" (cdr (assoc 2 (entget pen))))
	       )
	     (progn
	       (entmake tbl)
	       (setq en (cdr (assoc -2 tbl)))
	       (while (not lasten)
		 (setq ent (entget en))
		 (if (equal nen (cdr (assoc -1 ent)))
		   (progn
		     (setq ent (subst (cons 8 (getvar "clayer"))
				     (assoc 8 ent)
				     ent
			      )
		     )
		     (entmake ent)
		   )
		   (entmake ent)
		 )
		 (if (= (cdr (assoc 0 ent)) "ENDBLK")
		   (setq lasten T)
		   (setq lasten (not (setq en (entnext en))))
		 )
	       )
	       (if (entmake (list (cons 0 "ENDBLK")))
		 (entupd pen)
	       )
	     )
	   )
	  )
	  (T (prompt "\nSelect a dimensionline."))
	)
	(prompt "\nNo dimension selected!")
      )
    )
  )
  (princ)
)

(defun C:DLC () (C:DIMLINECHANGE))
(princ "\nLoaded command DIMLINECHANGE (or short DLC)")
(princ "\n(c) Jimmy Bergmark - 2000.")
(princ)



