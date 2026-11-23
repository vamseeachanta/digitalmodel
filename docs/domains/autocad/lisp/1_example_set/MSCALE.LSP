
; ******************** MSCALE.LSP ***********************
;	Created by Chai Jnasrikesorn  (2/11/87)
;       Update the scale of symbols
; ========================================================
(defun c:mscale (/ p scl l s n e t1)
	(setvar "cmdecho" 0)
	(setq p (ssget))
	(setq scl (getreal "\nEnter the scale: "))
	(If p (progn
		(setq l 0 s 0 n (sslength p))
		(While (< l n)
	(If (= "INSERT" (cdr (assoc 0 (setq e (entget (ssname p l))))))
		(progn
		      (setq t1 (cdr (assoc 10 e))) ;insertion pt.
		      (setq e (ssname p l))
		      (command "scale" e "" t1 scl)
		      (setq s (1+ s))
	(setq l (1+ l)))
	(setq l (1+ l))
	)
	        )
		)
	)
	(prin1 s)
	(princ " symbols changed")
	(terpri)(princ)
)
