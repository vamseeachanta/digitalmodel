; 940415 CBlliss
; If you find this code useful,
;      I am taking $10 donations.
; area3p - find Area given three points in space (using Heron's Formula)
; Charles Bliss
; 341 Monroe
; Palo Alto, CA 94306
; (408) 748-5474
;Modified by Kevin McHugh

'("Copyright Charles K. Bliss, April 92")


(defun area3p (p1 p2 p3 / a b c s)
	(setq
		a (distance p1 p2)
		b (distance p2 p3)
		c (distance p3 p1)
		s (* 0.5 (+ a b c))
	)
	(sqrt
		(*
			s
			(- s a)
			(- s b)
			(- s c)
		)
	)
)





(defun *error* (msg)
	(princ "error: ")
	(princ msg)
	(princ "\nMake sure the all the surfaces a meshed")
	(terpri)
)

(defun c:asarea( / area tcount vcount pmode bkn mn n el                                                
						vlst p1 p2 p3 p4 pt1 pt2 pt3 pt4)
	(setq
		area 0
		tcount 0
		vcount 0
		count 0
		tot_area 0
	)
	(princ "\nSelect the surfaces:  ")
	(setq sset (ssget))
	(setq bkname (ssname sset count))

    (while (/= bkname nil)
	(setq bkn (cdr (assoc 2 (entget bkname)))) ;Get Master Block bkn

	(setq no (1+ (atoi (substr bkn 3 3))))  ;kevins additions
	(setq bkn (strcat "*U" (itoa no)))  ;end of additions

	(setq mn (cdr (assoc -2 (tblsearch "BLOCK" bkn nil))))
	(setq n mn)                        ;init ename
	(setq rstr "\r									")
	
	(if (/= "POLYLINE" (cdr (assoc 0 (entget mn))))
		(abort "Not meshed...Run ADMESH first")
	)
	(while (setq n (entnext n))                             ;Loop for all Meshes in the block
		(if (= "POLYLINE" (cdr (assoc 0 (setq el (entget n))))) ;get past 'head'
			(setq n (entnext n))
		)
		(setq vlst nil)                                                 ;reset vertex name list on loop
		(prompt rstr)
		(while (and                                                             ;build list to ref later
				(= "VERTEX" (cdr (assoc 0 (setq el (entget n)))))
				(not (assoc 62 el))                             ;stop at first 'face'
			   )
			(setq
				vlst (cons n vlst)
				n (entnext n)
			)
			(prompt "\rSearching for Vertices...")
			(princ (setq vcount (1+ vcount)))
		)
		(setq vlst (cons "trash" (reverse vlst)))       ;pad '0th' out to '1st' for nth
		(prompt rstr)

		(while (/= "SEQEND" (cdr (assoc 0 (setq el (entget n)))))
			(setq
				p1 (cdr (assoc 71 el))
				p2 (cdr (assoc 72 el))
				p3 (cdr (assoc 73 el))
				p4 (cdr (assoc 74 el))
				pt1 (cdr (assoc 10 (entget (nth p1 vlst))))
				pt2 (cdr (assoc 10 (entget (nth p2 vlst))))
				pt3 (cdr (assoc 10 (entget (nth p3 vlst))))
			)
			(if (/= 0 p4)                                                   ;four sides?
				(setq                                                           ;calc 1 of 2 component triangles
					pt4 (cdr (assoc 10 (entget (nth p4 vlst))))
					area (+ (area3p pt1 pt3 pt4) area)
					tcount (1+ tcount)
				)
			)
			(setq                                                                   ;calc triangle (or 2nd half)
				area (+ (area3p pt1 pt2 pt3) area)      ;Calculate Area
				tcount (1+ tcount)
				n (entnext n)
			)
			(prompt "\rCalculating Face Areas...")
			(princ tcount)
		)
	)
	(princ "\nFace 'Triangle' Count:  ")
	(princ tcount)
	;(setq tot_area (+ area tot_area))
	(setq count (1+ count))
	(setq bkname (ssname sset count))
	;(princ "\nIndividual Surface area:  ")
	;(princ area)

    )
	(princ "\nApproximate Surface Area:  ")
	(princ area)
	(princ)
)
