(defun c:arclen (/ ob incl_ang AL);FOR DIMENSIONING ARC LENGTH
  (setq ob (entsel)
	incl_ang (ABS (- (cdr (assoc 50 (entget (car ob)))) (cdr (assoc 51 (entget (car ob))))))
	AL (RTOS (* INCL_ANG (cdr (assoc 40 (entget (car ob))))) 2 3))
  (command "dimANGULAR" ob "T" AL))
(defun c:dimtypefilter (/ ss cnt sfilt dtype nxt cnt1 s1)
  (princ "\nFilters objects for specific type of DIM")
  (setq ss (ssget '((0 . "Dimension")))
	cnt 0)
  (setq sfilt (ssadd))
  (repeat (sslength ss)
    (setq cnt1 0
	  data (entget (setq s1 (ssname ss cnt))))
    (while (setq nxt (nth cnt1 data))
      (setq cnt1 (1+ cnt1))
      (if (= (car nxt) 100)
	(progn
	  (setq dtype (cdr nxt))
	  (if (= dtype "AcDbOrdinateDimension") (ssadd s1 sfilt))
	  );progn
	);if
      );while
    (setq cnt (1+ cnt))
    );repeat
  (command "copy" sfilt "")
  (command)
  )
