;purge13.lsp - batch compatible program to automatically purge
;in AutoCad R13 - NOT COMPATIBLE WITH EARILER VERSION
;OF AUTOCAD


;Released into the public domain by  SIERRA HERMITAGE
;Rod Young, programmer
;Sierra Hermitage
;140 Mesa Vista Drive
;Bishop, CA  93514
;Phone: 619-387-2479
;Email: hermit1@ix.netcom.com

;modified by Erik Murray 4/30/97

;reiterating purge routine

(defun purge_type (type)

	(defun apurge( / done)
	   (setq done T)
	   (command "purge" type )
	   (while (= (getvar "CMDNAMES") "PURGE")
	      (command "y" )
	      (setq done nil)
	   );end while
	   (if (not done) (apurge))
	   (princ)
	);end defun

	(apurge)
	(princ)
) ;end defun

;******* Purge all *******

(defun c:pga()
	(purge_type "a")
	(princ)
);end defun

;******* Purge blocks *******

(defun c:pgb()
	(purge_type "b")
	(princ)
);end defun

;******* Purge layers *******

(defun c:pgla()
	(purge_type "la")
	(princ)
);end defun
