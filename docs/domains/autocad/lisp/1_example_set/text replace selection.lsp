;-------------------------Changing Individual Charaters--------------------
;************************* Program Developed by - SABA ********************
;$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ Soft Ware House $$$$$$$$$$$$$$$$$$$$$$$$$$$
;----------------------------- ***** $$$$$$ ***** -------------------------
(defun c:chg ()
  (setq	a  (ssget)
	b  (sslength a)
	c  0
	c1 1
	ol (getstring t "\nOld String: ")
  )
  (if (/= ol "")
    (progn
      (setq ne (getstring t "\nNew String: ")
	    lo (strlen ol)
      )
      (while (< c b)
	(setq d (Entget (ssname a c)))
	(redraw (cdr (assoc -1 d)) 3)
	(if (= (cdr (assoc 0 d)) "TEXT")
	  (progn
	    (prompt (strcat "\r"
			    "Selected Strings "
			    (itoa b)
			    " in that "
			    (itoa c1)
			    "   String(s) Replaced"
		    )
	    )
	    (setq e (cdr (assoc 1 d))
		  f (strlen e)
		  g 1
		  h ""
	    )
	    (while (<= g f)
	      (if (= (substr e g lo) ol)
		(progn
		  (setq	h  (strcat h ne)
			g  (+ g lo)
			c1 (+ c1 1)
		  )
		)
		(progn
		  (Setq	h (strcat h (substr e g 1))
			g (+ 1 g)
		  )
		)
	      )
	    )
	    (setq d (subst (cons 1 h) (cons 1 e) d))
	    (entmod d)
	  )
	)
	(setq c (+ c 1))
      )
    )
  )
  (princ)
)