;;;---------------------------------------------------------------------------;
;;;     PL2D-3D.LSP
;;;
;;;     by Phillip L. Miller
;;;     Jan 15, 1993
;;;
;;;---------------------------------------------------------------------------;
;;;     DESCRIPTION:
;;;
;;;     Converts 2D-PolyLines to 3D-PolyLines. The original PLine is deleted and
;;;     the new entity is created via entmake. AutoCAD does not "like" to do
;;;     this operation and has to be tricked into it somewhat, so please excuse
;;;     the excess code.
;;;
;;;---------------------------------------------------------------------------;

(defun c:PL2D-3D (/ k p1 enm1 ent1 ent2 ent3)
    (setq qtx"\nSelect 2D PolyLine Replacement: " qtyp "POLYLINE"
          ent1 (QTSEL) k (+(cdr(assoc 70 ent1))8)
          p1(cdr(assoc 10 ent1)) ent2 ent1
    )
    (entmake ent2)
    (setq ent2 (subst(cons 70 k) (assoc 70 ent2)ent2))
    (entmod ent2)
    (setq ent2 (subst(cons 10 p1)(assoc 10 ent2)ent2))
    (entmod ent2)
    (setq enm1 (entnext qenm) ent1 (entget enm1))
    (while (/= k "SEQEND")
        (setq p1(cdr(assoc 10 ent1)) ;p1(trans p1 qenm 0))
              p1(list(car p1)(cadr p1))ent3 ent1)
        (entmake ent3)
        (setq ent3(subst(cons 70 32)(assoc 70 ent3)ent3))
        (entmod ent3)
        (setq ent3(subst(cons 10 p1)(assoc 10 ent3)ent3))
        (entmod ent3)
        (setq enm1 (entnext enm1) ent1 (entget enm1) k (cdr(assoc 0 ent1)))
    )
    (entmake ent1)
    (entdel (entlast))
    (command".MOVE" (entlast) "" "@" "@" ".REDRAW")
    (QSET)(prin1))

;;;---------------------------------------------------------------------------;
;;;     3PLTHK.LSP
;;;
;;;     by Phillip L. Miller
;;;     Aug 10, 1993
;;;
;;;---------------------------------------------------------------------------;
;;;     DESCRIPTION:
;;;
;;;     Changes the thickness assigned to 3D-PLines. Checks for a thickness
;;;     association and changes it if found or adds it if absent.
;;;     * Note: This routine is only of use for those exporting DXF files for
;;;     use with 3D-Studio. NO VISIBLE change will occur with the entity
;;;     while within AutoCAD.
;;;
;;;---------------------------------------------------------------------------;

(defun c:3PLTHK (/ k k1 k2)
    (prompt"\n3d-Pline Thickness Change")
    (setq qtyp "POLYLINE" qss (QTSSEL) k 0
          k2 (getreal"\nNew Thickness: "))
    (while (< k qssl)
        (setq qent (entget(ssname qss k))
              k1 (cdr(assoc 39 qent)) k(1+ k))
        (if k1
            (setq qent (subst(cons 39 k2)(assoc 39 qent)qent))
            (setq qent (append qent(list(cons 39 k2))))
        )
        (entmod qent)(setq k1 nil)
    )
    (QSET)(prin1))

;;;---------------------------------------------------------------------------;;;;;
;;;     <Query> Selection subroutines that I use routinely and are incorporated
;;;     in the above applications.
;;;     Note that these are required for there operation. Those wishing to can,
;;;     of course, incorportate the below code in lieu of the references above.
;;;---------------------------------------------------------------------------;;;;;

(defun QTSEL (/ q1)     ;qtyp is required | qent,qenm,qpt are passed back
    (if (not qtx)
        (setq qtx (strcat "\nSelect " qtyp " Entity: ")))
    (while (/= q1 qtyp)
        (setq qenm (entsel qtx) qpt(cadr qenm) qenm(car qenm))
        (if qenm
            (progn
                (redraw qenm 3)
                (setq qent (entget qenm)
                      q1(cdr(assoc 0 qent)))
                (if (/= q1 qtyp)
                    (princ(strcat " Entity Type is: " q1)))
                (redraw qenm 4))
            (princ" - None Found -")))
    (if qent qent) ;<qent echoed>
    )

(defun QTSSEL (/ q1 q2 q3 qss1 qenm) ;qtyp is passed variable | qss & qssl passed back
  (setq q1 0)
  (while (= q1 0)
    (setq qss (ssadd) qss1 (ssget) q2 0)
    (if qss1
        (while (< q2 (sslength qss1))
            (setq q3 (cdr(assoc 0(entget(setq qenm(ssname qss1 q2))))))
            (if (or(= q3(car qtyp)) (= q3(cadr qtyp)) )
                    (ssadd qenm qss) )
            (setq q2 (1+ q2)) ))
    (setq qssl(sslength qss))
    (if (< qssl 1)
        (princ(strcat "\nERROR""\nSelection MUST Contain AT LEAST 1: "
                      (car qtyp)" or "(cadr qtyp)))
        (setq q1 1) ))
  (if qss qss) ;<qss echoed>
  )

(defun QSET () ;sets global Query-Functions variables to nil
    (setq qtx nil qtyp nil qss nil qssl nil qenm nil qpt nil qent nil)
    (prin1))
