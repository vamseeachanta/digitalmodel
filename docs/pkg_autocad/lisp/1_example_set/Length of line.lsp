;Label line length (decimal feet).  (XREFs Blocks or current drawing, plines too.)
;
;	Author:
;		Henry C. Francis
;		425 N. Ashe St.
;		Southern Pines, NC 28387
;
;	http://www.pinehurst.net/~pfrancis
;	e-mail hfrancis@pinehurst.net
;	All rights reserved.
;
(defun c:ssln ( / );nentla entla vwtwst line1 clayr
(setq vwtwst (getvar"viewtwist"))
(setq dimsc (getvar"dimscale"))
(setq line1(nentselp "\nSelect line: "))
(while
  (and
    (not(=
      (cdr(assoc 0(entget(car line1))))
      "VERTEX"))
    (not(=
      (cdr(assoc 0(entget(car line1))))
      "LINE"))
  );and
(setq line1(nentsel "\nSelect first line: "))
);while
(setq ename1 (car line1)
      lpic1 (cadr line1)
      lent1 (entget ename1)
      nentla (cdr(assoc 8 lent1))
      entla nentla
      count 0
);setq
(if(<(strlen nentla)11)
  (setq clayr "C-SSWR6NOTE")
  (progn
    (while (and(< count (strlen nentla))(not(=(substr entla 1 1)"|")))
      (setq entla (substr entla 2))
    );while
    (if (= count (strlen nentla))
      (setq clayr nentla)
      (setq clayr (substr entla 2)
            clayr (strcat(substr clayr 1 6)"6"(substr clayr 8 4)))
;      (progn
;        (setq mjrg (substr entla 2 1)
;              llt (substr entla 3 1)
;              prod (substr entla 4 4)
;              colr "6"
;              modf (substr entla 9 4)
;        );setq
;        (c:mklayr)
;      );progn
    );if
  );progn
);if
(command ".layer" "m" clayr "")
;(setvar "clayer" clayr)
(if(=(cdr(assoc 0 lent1))"VERTEX")
  (setq lpnt1 (list(cadr(assoc 10 lent1))(caddr(assoc 10 lent1))0)
        ename1a (entnext ename1)
        lent1a (entget ename1a)
        lpnt1a (list(cadr(assoc 10 lent1a))(caddr(assoc 10 lent1a))0)
  );setq
  (setq lpnt1 (list(cadr(assoc 10 lent1))(caddr(assoc 10 lent1))0)
        lpnt1a (list(cadr(assoc 11 lent1))(caddr(assoc 11 lent1))0)
  );setq
);if
(setq lnang (angle lpnt1 lpnt1a)
      txang (angle lpnt1a lpnt1))
(if(>(sin(+ vwtwst lnang 1.29))0)
  (setq txang (angle lpnt1 lpnt1a)
        lnlen (distance lpnt1 lpnt1a)
        txpnt
          (polar
            (polar lpnt1 txang (distance lpnt1 lpic1))
            (+ txang (/ pi 2))
            (* 0.135 dimsc)
          );polar
  );setq
  (setq tmpnt lpnt1
        lpnt1 lpnt1a
        lpnt1a tmpnt
        txang (angle lpnt1 lpnt1a)
        lnlen (distance lpnt1 lpnt1a)
        txpnt
          (polar
            (polar lpnt1 txang (distance lpnt1 lpic1))
            (+ txang (/ pi 2))
            (* 0.135 dimsc)
          );polar
  );setq
);if
(setq adtent
  (list
    (cons 0 "TEXT")
    (cons 72 1)
    (cons 73 2)
    (cons 1 (strcat(rtos lnlen 2 2)"'"))
    (cons 8 clayr)
    (cons 50 txang)
    (cons 10 txpnt)
    (cons 11 txpnt)
    (cons 40 (* 0.125 dimsc))
  );list
);setq
(entmake adtent)
(princ)
);defun