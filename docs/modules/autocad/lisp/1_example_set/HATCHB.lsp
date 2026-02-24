;;; HATCHB.LSP ver 2.5
;;; Recreates hatch boundary by selecting a hatch
;;; Known problem with some elipses and splines
;;; By Jimmy Bergmark
;;; Copyright (C) 1997-2008 JTB World, All Rights Reserved
;;; Website: www.jtbworld.com
;;; E-mail: info@jtbworld.com
;;; 2000-02-12 - First release
;;; 2000-03-27 - Counterclockwise arc's and ellipse's fixed
;;;              Objects created joined to lwpolyline if possible
;;;              Error-handling, undo of command
;;;              Can handle PLINETYPE = 0,1,2
;;; 2000-03-30 - Integrating hatchb and hatchb14
;;;              Selection of many hatches
;;;              Splines supported if closed.
;;; 2001-04-02 - Fixed bug with entmake of line with no Z for r14
;;; 2001-07-31 - Removed an irritating semicolon to enable polylines to be created.
;;; 2001-10-04 - Changed mail and homepage so it's easy to find when new versions comes up.
;;; 2003-02-06 - Minor fix
;;; 2003-02-17 - Area returned if no islands is found since it's not consistant
;;; 2003-05-19 - Fix to take PEDITACCEPT variable used in AutoCAD 2004 into account
;;; 2004-11-05 - Minor bugs fixed
;;; 2006-03-18 - Nothing changed from 2.1 other that it's been confirmed to work with AutoCAD 2007
;;; 2006-05-13 - Create the boundary on the same layer as the hatch using the hbl command and
;;;              on current layer/color/linetype using the hb or hatchb command
;;; 2007-02-08 - Fixed a bug with the hbl command
;;; 2008-02-29 - Support for hatches in non WCS thanks to xiaocai
;;; Tested on AutoCAD r14, 2000, 2000i, 2002, 2004, 2005, 2006, 2007, 2008, 2009
;;; should be working on older versions too.

(defun c:hb () (hatchb nil)) ; this line can be commented out if there is an existing command called hb
(defun c:hbl () (hatchb T)) ; this line can be commented out if there is an existing command called hbl
(defun c:hatchb () (hatchb nil))
(defun hatchb (hl  /     es    blay  ed1   ed2   loops1      bptf  part
             et    noe   plist ic    bul   nr    ang1  ang2  obj *ModelSpace* *PaperSpace*
             space cw errexit undox olderr oldcmdecho ss1 lastent en1 en2 ss lwp
             list->variantArray 3dPoint->2dPoint A2k ent i ss2
             knot-list controlpoint-list kn cn pos xv bot area hst noarea
            )
 (setq A2k (>= (substr (getvar "ACADVER") 1 2) "15"))
 (if A2k
   (progn
     (defun list->variantArray (ptsList / arraySpace sArray)
       (setq arraySpace
	      (vlax-make-safearray
		vlax-vbdouble
		(cons 0 (- (length ptsList) 1))
	      )
       )
       (setq sArray (vlax-safearray-fill arraySpace ptsList))
       (vlax-make-variant sArray)
     )
     (defun areaOfObject (en / curve area)
       (if en
	 (if A2k
	   (progn
	     (setq curve (vlax-ename->vla-object en))
	     (if
	       (vl-catch-all-error-p
		 (setq
		   area
		    (vl-catch-all-apply 'vlax-curve-getArea (list curve))
		 )
	       )
		nil
		area
	     )
	   )
	   (progn
	     (command "._area" "_O" en)
	     (getvar "area")
	   )
	 )
       )
     )
   )
 )
 (if A2k
  (defun 3dPoint->2dPoint (3dpt)
    (list (float (car 3dpt)) (float (cadr 3dpt)))
  )
 )

  (defun errexit (s)
    (princ "\nError:  ")
    (princ s)
    (restore)
  )

  (defun undox ()
    (command "._ucs" "_p")
    (command "._undo" "_E")
    (setvar "cmdecho" oldcmdecho)
    (setq *error* olderr)
    (princ)
  )

  (setq olderr  *error*
        restore undox
        *error* errexit
  )
  (setq oldcmdecho (getvar "cmdecho"))
  (setvar "cmdecho" 0)
  (command "._UNDO" "_BE")
  (if A2k (progn
    (vl-load-com)
    (setq *ModelSpace* (vla-get-ModelSpace
                         (vla-get-ActiveDocument (vlax-get-acad-object))
                       )
          *PaperSpace* (vla-get-PaperSpace
                         (vla-get-ActiveDocument (vlax-get-acad-object))
                       )
    ))
  )


; Remove for testing purpose
; (setq A2k nil)
  
  (if (/= (setq ss2 (ssget '((0 . "HATCH")))) nil)
   (progn
    (setq i 0)
    (setq area 0)
    (setq bMoreLoops nil)
    (while (setq ent (ssname ss2 i))
      (setq noarea nil)
      (setq ed1 (entget ent))
      (setq layer (cdr (assoc 8 ed1)))
      ; (if (not (equal (assoc 210 ed1) '(210 0.0 0.0 1.0))) (princ "\nHatch not in WCS!"))  ;modified by xiaocai
      ; (setq xv (cdr (assoc 210 ed1)))                                                      ;modified by xiaocai
      (command "._ucs" "_w")
      (setq loops1 (cdr (assoc 91 ed1))) ; number of boundary paths (loops)
      (if (and A2k (= (strcase (cdr (assoc 410 ed1))) "MODEL"))
        (setq space *ModelSpace*)
        (setq space *PaperSpace*)
      )
      (repeat loops1
        (setq ed1 (member (assoc 92 ed1) ed1))
        (setq bptf (cdr (car ed1))) ; boundary path type flag
        (setq ic (cdr (assoc 73 ed1))) ; is closed
        (setq noe (cdr (assoc 93 ed1))) ; number of edges
	(setq bot (cdr (assoc 92 ed1))) ; boundary type
	(setq hst (cdr (assoc 75 ed1))) ; hatch style
        (setq ed1 (member (assoc 72 ed1) ed1))
        (setq bul (cdr (car ed1))) ; bulge
        (setq plist nil)
        (setq blist nil)
        (cond
          ((> (boole 1 bptf 2) 0) ; polyline
           (repeat noe
             (setq ed1 (member (assoc 10 (cdr ed1)) ed1))
             (setq plist (append plist (list    (trans (cdr (assoc 10 ed1)) ent 0)     ))) ;;add trans by xiaocai
             (setq blist (append blist
                                 (if (> bul 0)
                                   (list (cdr (assoc 42 ed1)))
                                   nil
                                 )
                         )
             )
           )
           (if A2k (progn
             (setq polypoints
                    (apply 'append
                           (mapcar '3dPoint->2dPoint plist)
                    )
             )
             (setq VLADataPts (list->variantArray polypoints))
             (setq obj (vla-addLightweightPolyline space VLADataPts))
             (setq nr 0)
             (repeat (length blist)
               (if (/= (nth nr blist) 0)
                 (vla-setBulge obj nr (nth nr blist))
               )
               (setq nr (1+ nr))
             )
             (if (= ic 1)
               (vla-put-closed obj T)
             )
	     (if hl (vla-put-layer obj layer))
            )
            (progn
	      (setq ne (append (list '(0 . "POLYLINE")) (list (cons 66 1))))
	      (if (= ic 1) (setq ne (append ne (list (cons 70 1)))))
	      (if hl (setq ne (append ne (list (cons 8 layer)))))
              (entmake ne)
              (setq nr 0)
              (repeat (length plist)
                (if (= bul 0)
                  (entmake (list (cons 0 "VERTEX")
                                 (cons 10 (trans (nth nr plist) ent 0) );;add trans by xiaocai
                           )
                  )
                  (entmake (list (cons 0 "VERTEX")
                                 (cons 10 (trans (nth nr plist) ent 0) );;add trans by xiaocai
                                 (cons 42 (nth nr blist))
                           )
                  )
                )
                (setq nr (1+ nr))
              )
              (entmake '((0 . "SEQEND")))
            )
           )
          )
          (t ; not polyline
           (setq lastent (entlast))
           (setq lwp T)
           (repeat noe
             (setq et (cdr (assoc 72 ed1)))
             (cond
               ((= et 1) ; line
                (setq ed1 (member (assoc 10 (cdr ed1)) ed1))
                (if A2k
		  (progn
                    (setq obj (vla-AddLine
                      space
                      (vlax-3d-point (trans (cdr (assoc 10 ed1)) ent 0)   ) ;;add trans by xiaocai
                      (vlax-3d-point (trans (cdr (assoc 11 ed1)) ent 0)   ) ;;add trans by xiaocai
                    ))
		    (if hl (vla-put-layer obj layer))
		  )
		  (progn
		    (setq ne (append (list (cons 0 "LINE"))
                        (list (list 10 (car (trans (cdr (assoc 10 ed1)) ent 0) ) (cadr (trans (cdr (assoc 10 ed1)) ent 0)) 0)) ;;add trans by xiaocai
                        (list (list 11 (car (trans (cdr (assoc 11 ed1)) ent 0) ) (cadr (trans (cdr (assoc 11 ed1)) ent 0)) 0)) ;;add trans by xiaocai
		        ;(cons 210 xv)
                      )
                    )
		    (if hl (setq ne (append ne (list (cons 8 layer)))))
                    (entmake ne)
		  )
                )
                (setq ed1 (cddr ed1))
               )
               ((= et 2) ; circular arc
                 (setq ed1 (member (assoc 10 (cdr ed1)) ed1))
                 (setq ang1 (cdr (assoc 50 ed1)))
                 (setq ang2 (cdr (assoc 51 ed1)))
                 (setq cw (cdr (assoc 73 ed1)))
                 (if (and (equal ang1 0 0.00001) (equal ang2 6.28319 0.00001))
                   (progn
                     (if A2k
		       (progn
                         (setq obj (vla-AddCircle
                           space
                           (vlax-3d-point (trans (cdr (assoc 10 ed1)) ent 0)  )
                           (cdr (assoc 40 ed1))
                         ))
		         (if hl (vla-put-layer obj layer))
		       )
		       (progn
			 (setq ne (append
				      (list (cons 0 "CIRCLE"))
				      (list (cons 8 layer))
                                      (list (cons 10 (trans (cdr (assoc 10 ed1)) ent 0)));;;add trans by xiaocai
                                      (list (assoc 40 ed1))
                                )
                         )
			 (if hl (setq ne (append ne (list (cons 8 layer)))))
                         (entmake ne)
		       )
                     )
                     (setq lwp nil)
                   )
                   (if A2k
		     (progn
                       (setq obj (vla-AddArc
                         space
                         (vlax-3d-point (trans (cdr (assoc 10 ed1)) ent 0) );;;add trans by xiaocai
                         (cdr (assoc 40 ed1))
                         (if (= cw 0)
                           (- 0 ang2)
                           ang1
                         )
                         (if (= cw 0)
                           (- 0 ang1)
                           ang2
                         )
		       ))
		       (if hl (vla-put-layer obj layer))
                     )
		     (progn
		       (setq ne (append (list (cons 0 "ARC"))
                                    (list (cons 10 (trans (cdr (assoc 10 ed1)) ent 0) ));;add trans by xiaocai
                                    (list (assoc 40 ed1))
                                    (list (cons 50
                                          (if (= cw 0)
                                            (- 0 ang2)
                                            ang1
                                          )
                                    ))
                                    (list (cons 51
                                          (if (= cw 0)
                                            (- 0 ang1)
                                            ang2
                                          )
                                    ))
                              )
		       )
		       (if hl (setq ne (append ne (list (cons 8 layer)))))
                       (entmake ne)
		     )
                   )
                 )
                 (setq ed1 (cddddr ed1))
               )
               ((= et 3) ; elliptic arc
                (setq ed1 (member (assoc 10 (cdr ed1)) ed1))
                (setq ang1 (cdr (assoc 50 ed1)))
                (setq ang2 (cdr (assoc 51 ed1)))
                (setq cw (cdr (assoc 73 ed1)))
                (if A2k (progn
                  (setq obj (vla-AddEllipse
                              space
                              (vlax-3d-point (trans (cdr (assoc 10 ed1)) ent 0)   )
                              (vlax-3d-point (trans (cdr (assoc 11 ed1)) ent 0) );;add trans by xiaocai
                              (cdr (assoc 40 ed1))
                            )
                  )
                  (vla-put-startangle obj (if (= cw 0) (- 0 ang2) ang1))
                  (vla-put-endangle obj (if (= cw 0) (- 0 ang1) ang2))
		  (if hl (vla-put-layer obj layer))
                 )
		 (progn
                   (princ "\nElliptic arc not supported!")
		   (setq noarea T)
		 )
                )
                (setq lwp nil)
               )
               ((= et 4) ; spline
                (setq ed1 (member (assoc 94 (cdr ed1)) ed1))
                (setq knot-list nil)
                (setq controlpoint-list nil)
		(setq kn (cdr (assoc 95 ed1)))
                (setq cn (cdr (assoc 96 ed1)))
                (setq pos (vl-position (assoc 40 ed1) ed1))
                (repeat kn
                  (setq knot-list (cons (cons 40 (cdr (nth pos ed1))) knot-list))
                  (setq pos (1+ pos))
                )
                (setq pos (vl-position (assoc 10 ed1) ed1))
                (repeat cn
                  (setq controlpoint-list (cons (cons 10 (trans (cdr (nth pos ed1)) ent 0)   ) controlpoint-list));;add trans by xiaocai
                  (setq pos (1+ pos))
                )
                (setq knot-list (reverse knot-list))
                (setq controlpoint-list (reverse controlpoint-list))
		(setq ne (append
		               (list '(0 . "SPLINE"))
                               (list (cons 100 "AcDbEntity"))
                               (list (cons 100 "AcDbSpline"))
                               (list (cons 70 (+ 1 8 (* 2 (cdr (assoc 74 ed1))) (* 4 (cdr (assoc 73 ed1))))))
                               (list (cons 71 (cdr (assoc 94 ed1))))
                               (list (cons 72 kn))
                               (list (cons 73 cn))
                               knot-list
                               controlpoint-list
                      )
		)
		(if hl (setq ne (append ne (cons 8 layer))))
                (entmake ne)
		(setq ed1 (member (assoc 10 ed1) ed1))
                (setq lwp nil)
               )
             ) ; end cond
           ) ; end repeat noe
           (if lwp (progn
             (setq en1 (entnext lastent))
             (setq ss (ssadd))
             (ssadd en1 ss)
             (while (setq en2 (entnext en1))
               (ssadd en2 ss)
               (setq en1 en2)
             )
	     (if (= (getvar "peditaccept") 1)
               (command "_.pedit" (entlast) "_J" ss "" "")
	       (command "_.pedit" (entlast) "_Y" "_J" ss "" "")
	     )
          ))

          ) ; end t
        ) ; end cond
;	Tries to get the area on islands but it's not clear how to know if an island is filled or not
;	and if it should be substracted or added to the total area.
;	(if (or (= bot 0) (= (boole 1 bot 1) 1)) (setq area (+ area (areaOfObject (entlast)))))
;	(if (and (/= hst 1) (/= bot 0) (= (boole 1 bot 1) 0)) (setq area (- area (areaOfObject (entlast)))))
;	(princ "\n") (princ bot) (princ "\n") (princ hst) (princ "\n")
;	(princ (areaOfObject (entlast)))
      ) ; end repeat loops1
      (if (and (= noarea nil) (= loops1 1)) (setq area (+ area (areaOfObject (entlast)))) (setq bMoreLoops T))
      (setq i (1+ i))
    )
   )
  )
  (if (and area (not bMoreLoops)) (progn
    (princ "\nTotal Area = ")
    (princ area)
  ))
  (restore)
  (princ)
)
