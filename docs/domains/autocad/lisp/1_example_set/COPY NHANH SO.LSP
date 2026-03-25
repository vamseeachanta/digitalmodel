;*********************************************************************
(defun ketthuc ()
	(setvar	"cmdecho"	luuecho)
	(setq *error*	luu
		luu		nil	
		luuecho	nil
	);setq
	(princ)
)		
;*********************************************************************
(defun modau ()
(setq 	luu *error
		luuecho	(getvar	"cmdecho")
		*error	(ketthuc)
)
)
;*********************************************************************
(defun xulytext (text / kytu ma sokt luusokt lui )
(setq 	kytu	(substr text (strlen text))
		ma	(ascii kytu)
		sokt	(read kytu) 
		lui	1
)
(if (numberp sokt)
		(progn
			(setq luusokt	(1+ sokt))
			(if (and 	(numberp sokt) 
					(> (strlen text) 1)
			    )	
			   (progn
				(setq 	kytu	(substr text (1- (strlen text)))
						sokt	(read kytu) 
										)
				(if 	(numberp sokt) 
					(setq luusokt (1+	sokt)
							lui 	2

						)
				)
			    );progn	
			)
			(if (= luusokt	100)	(setq 	luusokt	0))
			(setq 	kytu		(rtos luusokt 2 0)
					
					text	(strcat	(substr text 1 (- (strlen text) lui))  kytu)
			)
		);progn			 
		(if   (or 	(= kytu "z")
				(= kytu "Z")
			)
			(setq 	text		(strcat 	text	"0")
				textxl		"0"
			)
			(setq		ma	(1+	ma)
					text	(strcat	(substr text 1 (1- (strlen text)))  (chr ma))
			)
		);if
);if
)
;*********************************************************************
(defun doitext(tendoituong / chuoi doituong thoat tam dsach kieu text vitri10 vitri11 dem canle)
;Neu doi tuong la text thi tiep tuc
(setq 	doituong 	(entget  tendoituong)
	kieu		(cdr (assoc 	0	doituong))
	canle		(cdr (assoc 	72	doituong))
)	
(if (or (= kieu		"TEXT")
	(= kieu 	"MTEXT")	
    ) 	
	(progn
		(setq	textxl	(xulytext textxl)
			text	(cons 1 textxl)
			vitri10 	(cdr (assoc 10 doituong))
			vitri10 	(list (+ (car vitri10) (car vitrilech)) (+ (nth 1 vitri10) (nth 1 vitrilech)))
			vitri10		(cons 10 vitri10)
			vitri11 	(cdr (assoc 11 doituong))
			vitri11 	(list (+ (car vitri11) (car vitrilech)) (+ (nth 1 vitri11) (nth 1 vitrilech)))
			vitri11		(cons 11 vitri11)
			dem	0
			dsach	nil
		)
		(foreach tam 	doituong
			(cond
				((= (car tam)	1)	(setq dsach 	(append dsach (list text))))
				((= (car tam)	10)	(setq dsach 	(append dsach (list vitri10))))
				((= (car tam)	11)	(setq dsach 	(append dsach (list vitri11))))
				((setq dsach 	(append dsach (list tam))))
			)
		)
		(entmake dsach)
	);progn
);if
	);
;*********************************************************************
;sao doi tuong cu sang vi tri moi

(defun copy_dt (tendoituong )
(command "copy" tendoituong "" goc toi )
);defun

;*********************************************************************
(defun c:tt ( / cumdt dodai thoat dem ten doituong textxl dem goc toi)
; Khoi dau cua chuong trinh
(princ "\nCopy Inteligent...\n")
(setq 	luuecho	(getvar	"cmdecho")
	luu	*error*
	*error*	ketthuc
	cumdt 	(ssget)
	dodai 	(sslength cumdt)
	goc		(getpoint "\nSelect base point:")
	thoat		nil
	dem		0
	textxl		nil
);
(setvar "cmdecho" 0)
; Loc ra duoc ong text de xu ly
(while	(and 	(= thoat	nil)
		(< dem	dodai)
	)
	(setq 	ten	(ssname cumdt dem)
		dem	(1+ 	dem)
		doituong (entget ten)
		kieu	 (cdr (assoc 	0	doituong))			
	)
	
	(if (or (= kieu		"TEXT")
		(= kieu 	"MTEXT")	
    	    )
		(setq 	thoat	T
			textxl 	(cdr (assoc 1 doituong)) 	
		)
	)
);
(while T 
(setq	toi		(getpoint "\nSelect next point: " goc)
	vitrilech 	(list 	(- (car toi) (car goc)) (- (nth 1 toi) (nth 1 goc)))
	dem		0
)
(while	(< dem dodai)
	(setq 	ten	(ssname cumdt dem)
		dem	(1+ 	dem)
		doituong (entget ten)
		kieu	 (cdr (assoc 	0	doituong))			
	)

	(if (or (= kieu		"TEXT")
		(= kieu 	"MTEXT")	
    	    )
		(doitext	ten)
		(copy_dt	ten)

	);if
)
);while
(ketthuc)
);defun
(princ "Type \"DG\" to start")
;Note: bien toan cuc: textxl vitrilech
