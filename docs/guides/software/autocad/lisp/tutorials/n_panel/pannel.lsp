(vl-load-com)
(defun l-coor2l-pt (lst / )
	(if lst
		(cons
			(list
				(car lst)
				(cadr lst)
				(if (vlax-property-available-p nw_pl 'Elevation) (vlax-get nw_pl 'Elevation) 0.0)
			)
			(l-coor2l-pt (if flag (cdddr lst) (cddr lst)))
		)
	)
)
(defun c:pannel ( / AcDoc pt_low len old_snap old_orth ori wid Space ss nw_pl nw_coor count e1 e2 c1 c2 nb c l)
	(setq AcDoc (vla-get-ActiveDocument (vlax-get-acad-object)))
	(vla-StartUndoMark AcDoc)
	(initget 9)
	(setq pt_low (getpoint "\nLower left point: "))
	(initget 9)
	(setq len (getpoint pt_low "\nLength (horizontal dimension): "))
	(while (< (distance pt_low len) 50)
		(princ "\nDistance too short. Retry")
		(initget 9)
		(setq len (getpoint pt_low "\nLength (horizontal dimension): "))
	)
	(setq
		old_snap (getvar "SNAPANG")
		old_orth (getvar "ORTHOMODE")
	)
	(setvar "SNAPANG" (angle pt_low len))
	(setvar "ORTHOMODE" 1)
	(setq
		ori (angle (trans pt_low 1 0) (trans len 1 0))
		len (distance pt_low len)
	)
	(initget 7)
	(setq wid (getdist pt_low "\nWidth (vertical dimension): "))
	(while (< wid 50)
		(princ "\nDistance too short. Retry")
		(initget 7)
		(setq wid (getdist pt_low "\nWidth (vertical dimension): "))
	)
	(setvar "SNAPANG" old_snap)
	(setvar "ORTHOMODE" old_orth)
	(setq
		pt_low (trans pt_low 1 0)
		Space
		(if (eq (getvar "CVPORT") 1)
			(vla-get-PaperSpace AcDoc)
			(vla-get-ModelSpace AcDoc)
		)
		ss (ssadd)
	)
	(if (not (tblsearch "LAYER" "WHITE"))
		(vlax-put (vla-add (vla-get-layers AcDoc) "WHITE") 'color 7)
	)
	(if (not (tblsearch "LAYER" "GREEN"))
		(vlax-put (vla-add (vla-get-layers AcDoc) "GREEN") 'color 3)
	)
	(if (not (tblsearch "LAYER" "HOLES"))
		(vlax-put (vla-add (vla-get-layers AcDoc) "HOLES") 'color 4)
	)
	(setq
		nw_pl
		(vlax-invoke Space 'AddLightWeightPolyline
			(apply 'append
				(mapcar
					'(lambda (x) (list (car x) (cadr x)))
					(list
						pt_low
						(polar pt_low ori len)
						(polar (polar pt_low ori len) (+ ori (* pi 0.5)) wid)
						(polar pt_low (+ ori (* pi 0.5)) wid)
					)
				)
			)
		)
	)
	(vla-put-Closed nw_pl 1)
	(vla-put-Layer nw_pl "WHITE")
	(setq nw_pl_out (vla-Copy nw_pl))
	(vla-put-Layer nw_pl_out "GREEN")
	(setq
		nw_coor (l-coor2l-pt (vlax-get nw_pl_out 'Coordinates))
		count 0
	)
	(mapcar
		'(lambda (v x / newVertex)
			(setq newVertex (vlax-make-safearray vlax-vbDouble '(0 . 1)))
			(vlax-safearray-fill newVertex x)
			(vla-AddVertex nw_pl_out v newVertex)
		)
		'(1 2 4 5 7 8 10 11)
		(apply 'append
			(mapcar
				'(lambda (y / p)
					(cond
						((zerop count)
							(setq p (list (polar y (- ori (* pi 0.5)) 25.0)))
						)
						((eq count 1)
							(setq p
								(list
									(polar y (- ori (* pi 0.5)) 25.0)
									(polar y ori 25.0)
								)
							)
						)
						((eq count 2)
							(setq p
								(list
									(polar y ori 25.0)
									(polar y (+ ori (* pi 0.5)) 25.0)
								)
							)
						)
						((eq count 3)
							(setq p
								(list
									(polar y (+ ori (* pi 0.5)) 25.0)
									(polar y (+ ori pi) 25.0)
								)
							)
						)
						((eq count 4)
							(setq p (list (polar y (+ ori pi) 25.0)))
						)
					)
					(setq count (1+ count))
					(mapcar '(lambda (z) (list (car z) (cadr z))) p)
				)
				(append nw_coor (list (car nw_coor)))
			)
		)
	)
	(vla-Update nw_pl_out)
	(setq count 0)
	(foreach e
		(apply 'append
			(mapcar
				'(lambda (y / p)
					(cond
						((zerop count)
							(setq p (list (polar y (- ori (atan 0.5)) (sqrt (+ 625 156.25)))))
						)
						((eq count 1)
							(setq p
								(list
									(polar y (+ (- ori pi) (atan 0.5)) (sqrt (+ 625 156.25)))
									(polar y (- (+ ori (* pi 0.5)) (atan 0.5)) (sqrt (+ 625 156.25)))
								)
							)
						)
						((eq count 2)
							(setq p
								(list
									(polar y (+ (- ori (* pi 0.5)) (atan 0.5)) (sqrt (+ 625 156.25)))
									(polar y (- (+ ori pi) (atan 0.5)) (sqrt (+ 625 156.25)))
								)
							)
						)
						((eq count 3)
							(setq p
								(list
									(polar y (+ ori (atan 0.5)) (sqrt (+ 625 156.25)))
									(polar y (- ori (* pi 0.5) (atan 0.5)) (sqrt (+ 625 156.25)))
								)
							)
						)
						((eq count 4)
							(setq p (list (polar y (+ ori (* pi 0.5) (atan 0.5)) (sqrt (+ 625 156.25)))))
						)
					)
					(setq count (1+ count))
					p
				)
				(append nw_coor (list (car nw_coor)))
			)
		)
		(vla-put-Layer (vlax-invoke Space 'AddCircle e 2.385) "HOLES")
		(ssadd (entlast) ss)
	)
	(repeat 4
		(setq
			e1 (ssname ss 0)
			e2 (ssname ss 1)
			c1 (cdr (assoc 10 (entget e1)))
			c2 (cdr (assoc 10 (entget e2)))
			nb (atoi (rtos (/ (distance c1 c2) 400.0) 2 0))
			c c1
		)
		(cond
			((not (zerop nb))
				(setq l (/ (distance c1 c2) nb))
				(if (eq nb 1) (setq nb 2 l (* 0.5 (distance c1 c2))))
				(repeat (1- nb)
					(setq c (polar c (angle c1 c2) l))
					(vla-put-Layer (vlax-invoke Space 'AddCircle c 2.385) "HOLES")
				)
			)
		)
		(ssdel e1 ss)
		(ssdel e2 ss)
	)
	(vla-EndUndoMark AcDoc)
	(prin1)
)