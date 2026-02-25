;; © Juan Villarreal 11.20.2011 ;;
;; massoc (Jaysen Long) ;;
;; Minor Modification by Jvillarreal ;;
;; Extracts info from list by key ;;
;; Found @ http://www.theswamp.org/index.php?topic=40149.0
(defun massoc (key alist / x nlist)
(foreach x alist
(if
(eq key (car x))
(setq nlist (cons x nlist))
)
)
(reverse nlist)
);defun
(defun c:MergeHatch ( / hentinfo ss i ent ent# seedpt# entinfo entinfo2 ent# seedpt# seedpts MergedHatchList)
(while (/= (cdr (assoc 0 hentinfo)) "HATCH")
(setq hentinfo (car (entsel "\nSelect Hatch Pattern to use:")))
(If hentinfo (setq hentinfo (entget hentinfo)) (princ "\nMissed. Try again.")))
(while (not ss) (princ "\nSelect hatch entities to merge:")(setq ss (ssget '((0 . "HATCH")))))
(setq MergedHatchList
(list (cons 0 "HATCH")
(cons 100 "AcDbEntity")
(assoc 8 hentinfo)
(cons 100 "AcDbHatch")
(assoc 10 hentinfo)
(assoc 210 hentinfo)
(assoc 2 hentinfo)
(assoc 70 hentinfo)
(assoc 71 hentinfo)
(cons 91 (sslength ss))
) i -1 seedpt# 0 ent# 0)
(repeat (sslength ss)
(setq n -1
entinfo (entget (ssname ss (setq i (1+ i))))
entinfo2 (member (assoc 92 entinfo) entinfo)
entinfo2 (reverse (cdr (member (assoc 75 entinfo2)(reverse entinfo2))))
ent# (+ ent# (cdr (assoc 91 entinfo)))
seedpt# (+ seedpt# (cdr (assoc 98 entinfo)))
seedpts (append seedpts (cdr (member (assoc 98 entinfo) entinfo)))
MergedHatchList (append MergedHatchList entinfo2)
)
(entdel (ssname ss i))
)
(setq MergedHatchList (subst (cons 91 ent#)(assoc 91 MergedHatchList) MergedHatchList)
MergedHatchList
(append MergedHatchList
(append
(reverse (cdr (member (assoc 98 hentinfo)(reverse (member (assoc 75 hentinfo) hentinfo)))))
(cons (cons 98 seedpt#) seedpts))))
(if (= (cdr (assoc 71 hentinfo)) 1)(setq MergedHatchList (append MergedHatchList '((-3 ("ACAD" (1010 0.0 0.0 0.0)))))))
(entmake MergedHatchList)
(setq ent (entlast))
(if (= (cdr (assoc 71 hentinfo)) 1)
(mapcar
'(lambda (x / entlist)
(setq entlist (entget (cdr x)))
(entmod (subst (cons 330 ent) (assoc 330 entlist) entlist))
)
(massoc 330 MergedHatchList)
)
)
)
(defun c:MH () (c:MergeHatch))