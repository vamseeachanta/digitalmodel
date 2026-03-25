
;	The Clock

;	A refined analogue clock for AutoCAD

;	Designed and implemented in January 1986 by Kelvin R. Throop

;	Enter ^C to terminate

(expand 100)

(setq f '(3 791 893 4 789 913 4 783 933 4 773 951 4 760 966 4 745 979 4
	  727 989 4 708 994 4 688 996 4 667 994 4 648 989 4 630 979 4
	  615 966 4 602 951 4 592 933 4 586 913 4 584 893 4 586 873 4
	  592 854 4 602 836 4 615 820 4 630 808 4 648 798 4 667 792 4
	  688 790 4 708 792 4 727 798 4 745 808 4 760 820 4 773 836 4
	  783 854 4 789 873 4 791 893 4 771 893 3 760 935 4 777 945 3
	  729 966 4 739 983 3 688 977 4 688 996 3 636 983 4 646 966 3
	  615 935 4 598 945 3 604 893 4 584 893 3 598 842 4 615 851 3
	  646 821 4 636 804 3 688 809 4 688 790 3 739 804 4 729 821 3
	  760 851 4 777 842 3 791 756 4 791 241 4 584 241 4 550 206 4
	  550 137 4 825 137 4 825 206 4 791 241 3 825 206 4 550 206 3
	  584 241 4 584 756 3 550 756 4 550 1031 4 825 1031 4 825 756 4
	  550 756 3 516 996 4 688 1168 4 859 996 3 773 1048 4 602 1048
	  4 688 1134 4 773 1048))

(setq mname '("January" "February" "March" "April" "May" "June"
	      "July" "August" "September" "October" "November" "December"))

(setq dname '("Sunday" "Monday" "Tuesday" "Wednesday" "Thursday" "Friday"
	      "Saturday"))

(defun date (td / j y d m s)
	(setq j (fix td))
	(setq j (- j 1721119.0))
	(setq y (fix (/ (1- (* 4 j)) 146097.0)))
	(setq j (- (* j 4.0) 1.0 (* 146097.0 y)))
	(setq d (fix (/ j 4.0)))
	(setq j (fix (/ (+ (* 4.0 d) 3.0) 1461.0)))
	(setq d (- (+ (* 4.0 d) 3.0) (* 1461.0 j)))
	(setq d (fix (/ (+ d 4.0) 4.0)))
	(setq m (fix (/ (- (* 5.0 d) 3) 153.0)))
	(setq d (- (* 5.0 d) 3.0 (* 153.0 m)))
	(setq d (fix (/ (+ d 5.0) 5.0)))
	(setq y (+ (* 100.0 y) j))
	(if (< m 10.0)
	   (setq m (+ m 3))
	   (progn
	      (setq m (- m 9))
	      (setq y (1+ y))
	   )
	)
	(strcat (nth (fix (rem (1+ td) 7)) dname) ", "
	   (nth (1- m) mname) " " (rtos d 2 0) ", "
	   (rtos y 2 0))
)

(defun *ERROR* (s)
	(redraw)
	(grtext)
)

(defun c:clock (/ a gsf m n x y mhl hhl dtr a270 a9 op fp np tp pl
		  pend phh lx ly lh r i hlb pi2 pio2 time ti hh mm nh lh
		  lm nm)
	(grclear)
	(grtext -2 "")
	(setq i -1 hlb 0)
	(while (< i 20)
	   (grtext (setq i (1+ i)) "")
	)
	(setq n (/ (length f) 3)
	      gsf (/ 1200.0 (getvar "viewsize"))
	      i 0
	)
	(repeat n
	   (setq m (nth i f)
		 x (/ (nth (1+ i) f) gsf)
		 y (/ (nth (+ 2 i) f) gsf)
		 i (+ 3 i)
	   )
	   (if (= m 3)
	      (setq lx x ly y)
	      (grdraw (list lx ly) (list (setq lx x) (setq ly y)) 7)
	   )
	)

	(setq r 0.0)
	(setq fp (list (/ 688 gsf) (/ 722 gsf)))
	(setq op (list (/ 688 gsf) (/ 893 gsf)))
	(setq pl (/ (- 722 309) gsf))
	(setq mhl (/ 90 gsf))
	(setq hhl (/ 60 gsf))
	(setq dtr (/ pi 180.0))
	(setq a270 (* 270.0 dtr))
	(setq a9 (* 9 dtr))
	(setq pend nil)
	(while (<= r (* 2 pi))
	   (setq pend (cons (polar fp (+ a270 (* a9 (sin r))) pl) pend))
	   (setq r (+ r 0.25))
	)

	(grdraw fp (setq tp (car pend)) -1)
	(setq j (1+ (setq uc 10)))
	(setq lh op lm op)
	(setq pi2 (* 2 pi)
	      pio2 (/ pi 2)
	)
	(while t
	   (grtext (setq hlb (rem (1+ hlb) 20)) "" 1)
	   (if (> (setq j (1+ j)) uc)
	      (progn
		 (setq j 0)
		 (setq time (getvar "date"))
		 (grtext -1 (date time))
		 (setq ti (setq time (* 86400.0 (- time (fix time)))))
		 (setq hh (fix (/ time 3600.0)))
		 (setq time (- time (* hh 3600.0)))
		 (setq mm (fix (/ time 60.0)))
		 (grtext -2 (strcat (itoa (if (=
		   (setq phh (rem hh 12)) 0) 12 phh)) ":"
		   (if (< mm 10) "0" "")
		   (itoa mm) " " (if (< hh 13) "AM" "PM")))
		 (grdraw op (setq nh (polar op (- pio2 (* pi2 (/ ti 43200.0)))
			     hhl)) -1)
		 (grdraw op (setq nm (polar op (- pio2 (* pi2 (/ time 3600.0)))
			     mhl)) -1)
		 (grdraw op lh -1)
		 (grdraw op lm -1)
		 (setq lh nh lm nm)
	      )
	   )
	   (setq i 0)
	   (while (setq np (nth (setq i (1+ i)) pend))
	      (grdraw fp np -1)
	      (grdraw fp tp -1)
	      (setq tp np)
	   )
	)
)
