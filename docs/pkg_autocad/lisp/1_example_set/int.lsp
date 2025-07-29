;Projected intersection point of lines and polylines. (transparent too)
;
;	AUTHOR: HENRY C. FRANCIS
;		425 N. ASHE ST.
;		SOUTHERN PINES, NC 28387
;              
;		All rights reserved without prejudice.
;
;	Copyright:	2-13-96
;	Edited:		2-13-96
;
(defun c:int (/); line1 line2 pt1 pt2 pt3 pt4
  (setq nent1 (nentsel "\nPick first line for intersection")
        line1 (entget(car nent1))
        nent2 (nentsel "\nPick second line for intersection")
        line2 (entget(car nent2))
        pt1 (cdr(assoc 10 line1))
        pt2
          (cond
            ((eq "VERTEX"(cdr(assoc 0 line1)))
               (cdr(assoc 10(entget(entnext(cdr(assoc -1 line1)))))))
            ((eq "LINE"(cdr(assoc 0 line1)))
               (cdr(assoc 11 line1)))
          );cond
        pt3 (cdr(assoc 10 line2))
        pt4
          (cond
            ((eq "VERTEX"(cdr(assoc 0 line2)))
              (cdr(assoc 10(entget(entnext(cdr(assoc -1 line2)))))))
            ((eq "LINE"(cdr(assoc 0 line2)))
              (cdr(assoc 11 line2)))
          );cond
        pt1 (trans(list(car pt1)(cadr pt1))1 0)
        pt2 (trans(list(car pt2)(cadr pt2))1 0)
        pt3 (trans(list(car pt3)(cadr pt3))1 0)
        pt4 (trans(list(car pt4)(cadr pt4))1 0)
  );setq
  (setq intpt (inters pt1 pt2 pt3 pt4 nil))
);defun
