;;;=====================================================
;;; Read Text object XY, string for Z, create autocad point object
;;; 07/09/1999 for land desktop newsgroup
;;; modified 03/14/2007 to use the Text's Alignment Point. Will not give
;;; expected results with FIT or ALIGNED text.
;;;=====================================================
(Defun C:Txt2pnt (/ SS SSL Cnt Ename Elst Str-A Pt-A Pt-B)
  (Setq SS  (SSget '((0 . "TEXT")))
 SSL (SSlength SS)
 Cnt 0
  )
  (repeat SSL
    (Setq Ename (SSname SS Cnt)
   Elst (Entget Ename)
    )
    (Setq Str-A (Cdr (assoc 1 Elst))
   Pt-A (if (= 0 (cdr (assoc 72 Elst)) (cdr (assoc 73 Elst)))
    (Cdr (Assoc 10 Elst))
    (cdr (assoc 11 Elst))
    )
    )
    (Setq Pt-B (list (car Pt-A) (Cadr Pt-a) (Atof Str-A)))
    (command "point" "_non" Pt-B)
    (Setq Cnt (+ 1 Cnt))
  )
    (command "ddptype")
  (princ)
)
(prompt
  "\n{0.9} Read Text object XYZ, create autocad point object "
)
(princ)
