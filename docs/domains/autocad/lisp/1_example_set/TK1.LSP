;;; Chuong trinh thong ke thep
;;;Goi hop hoi thoai
(defun tk_mod_box (/ dial_id)
  (setq dial_id (load_dialog "tk_mod.dcl"))
  (if (not (new_dialog "TKCK" dial_ID))
    (exit)
  )
  (cond
    ((= loai nil)
     (progn
       (mode_tile "type" 1)
       (mode_tile "edit_1" 1)
       (mode_tile "edit_2" 1)
       (mode_tile "edit_3" 1)
       (mode_tile "edit_4" 1)
       (mode_tile "edit_5" 1)
     )
    )
    ((= loai 0)
     (progn
       (slidelib "img" "tkvl.slb(so0)")
       (mode_tile "edit_4" 1)
       (mode_tile "edit_5" 1)
     )
    )
    ((= loai 1)
     (progn
       (slidelib "img" "tkvl.slb(so1)")
       (mode_tile "edit_4" 1)
       (mode_tile "edit_5" 1)
     )
    )
    ((= loai 2)
     (progn
       (slidelib "img" "tkvl.slb(so2)")
       (mode_tile "edit_4" 1)
       (mode_tile "edit_5" 1)
     )
    )
    ((= loai 3) (slidelib "img" "tkvl.slb(so3)"))
    ((= loai 4) (slidelib "img" "tkvl.slb(so4)"))
    ((= loai 5)
     (progn
       (slidelib "img" "tkvl.slb(so5)")
       (mode_tile "edit_5" 1)
     )
    )
    ((= loai 6)
     (progn
       (slidelib "img" "tkvl.slb(so6)")
       (mode_tile "edit_4" 1)
       (mode_tile "edit_5" 1)
     )
    )
    ((= loai 7)
     (progn
       (slidelib "img" "tkvl.slb(so7)")
       (mode_tile "edit_5" 1)
     )
    )
    ((= loai 8)
     (progn
       (slidelib "img" "tkvl.slb(so8)")
       (mode_tile "edit_5" 1)
     )
    )
  )
  (if (/= loai nil)
    (progn
      (set_tile "edit_1" SH_TXT)
      (set_tile "edit_2" DK_TXT)
      (set_tile "edit_3" L1)
      (set_tile "edit_4" L2)
      (set_tile "edit_5" L3)
      (set_tile "edit_6" SL_TXT)
      (set_tile "edit_7" SLA_TXT)
;;;      (action_tile
;;;	"edit_1"
;;;	"(set_tile \" edit_1 \" (get_attr \" edit_1 \" \" value \"))"
;;;      )
;;;      (action_tile
;;;	"edit_2"
;;;	"(set_tile \" edit_2 \" (get_attr \" edit_2 \" \" value \"))"
;;;      )
;;;      (action_tile
;;;	"edit_3"
;;;	"(set_tile \" edt_3 \" (get_attr \" edit_3 \" \" value \"))"
;;;      )
;;;      (action_tile
;;;	"edit_4"
;;;	"(set_tile \" edit_4 \" (get_attr \" edit_4 \" \" value \"))"
;;;      )
;;;      (action_tile
;;;	"edit_5"
;;;	"(set_tile \" edit_5 \" (get_attr \" edit_5 \" \" value \"))"
;;;      )
;;;      (action_tile
;;;	"edit_6"
;;;	"(set_tile \" edit_6 \" (get_attr \" edit_6 \" \" value \"))"
;;;      )
;;;      (action_tile
;;;	"edit_7"
;;;	"(set_tile \" edit_7 \" (get_attr \" edit_7 \" \" value \"))"
;;;      )
      (action_tile "accept" "(settext) (done_dialog)")
      (action_tile "cancel" "(done_dialog)")
    )
    (progn
;;;      (action_tile
;;;	"edit_6"
;;;	"(set_tile \" edit_6 \" (get_attr \" edit_6 \" \" value \"))"
;;;      )
;;;      (action_tile
;;;	"edit_7"
;;;	"(set_tile \" edit_7 \" (get_attr \" edit_7 \" \" value \"))"
;;;      )
      (action_tile "accept" "(smtext) (done_dialog)")
      (action_tile "cancel" "(done_dialog)")
    )
  )
  (Start_dialog)
  (unload_dialog dial_id)
)
;;; Gan gia tri trong truong hop chon nhieu loai
(defun smtext ()
  (setq Soluong (get_tile "edit_6"))
  (setq Tsoluong (get_tile "edit_7"))
)
;;;Gan gia tri
(defun settext ()
  (setq SH_TXT (get_tile "edit_1"))
  (setq DK_TXT (get_tile "edit_2"))
  (setq L1 (get_tile "edit_3"))
  (setq L2 (get_tile "edit_4"))
  (setq L3 (get_tile "edit_5"))
  (setq SL_TXT (get_tile "edit_6"))
  (setq SLA_TXT (get_tile "edit_7"))
)
;;;chuong trinh gan anh
(defun slidelib	(key_img img)
  (setq Wid (dimx_tile key_img))
  (setq Hei (dimy_tile key_img))
  (start_image key_img)
  (slide_image 0 0 wid hei img)
  (end_image)
)
;;; Chuong trinh gan loai thep
(defun C_loai_thep (/ Dial)
  (Setq Dial (load_Dialog "c_thep.dcl"))
  (if (not (New_dialog "Steel_type" dial))
    (Exit)
  )
  (slidelib "style0" "tkvl.slb(so0)")
  (slidelib "style1" "tkvl.slb(so1)")
  (slidelib "style2" "tkvl.slb(so2)")
  (slidelib "style3" "tkvl.slb(so3)")
  (slidelib "style4" "tkvl.slb(so4)")
  (slidelib "style5" "tkvl.slb(so5)")
  (slidelib "style6" "tkvl.slb(so6)")
  (slidelib "style7" "tkvl.slb(so7)")
  (slidelib "style8" "tkvl.slb(so8)")
  (start_dialog)
  (Unload_dialog dial)
  (princ loai)
)
;;; Chuong trinh con doi gia tri trong thuc the
(defun ch_oj (number new_oj o_j)
  (setq	o_j
	 (subst	(cons number new_oj)
		(assoc number o_j)
		o_j
	 )
  )
  (entmod o_j)
)
;;; Truy cap phan tu trong du lieu
(defun dxf (code elist)
  (cdr (assoc code elist))
)
;;; Tinh toan tong chieu dai thep
(defun cal_length (num_oj length_oj)
  (* num_oj (/ (atof (itoa length_oj)) 1000))
)
;;; Tinh toan khoi luong thep
(defun cal_weight (fi Total_length)
  (* pi 0.25 fi fi 0.000001 7850 Total_length)
)
;;; Chuong trinh tach so lieu
(defun entget_oj (block / BUFFER block1)
  (setq block1 (entnext block))
  (setq buffer block1)
  (setq block1 (entget block1))
  (if (OR (= "ATTRIB" (dxf 0 block1))(= "INSERT" (dxf 0 block1)))
    (progn
      (while (OR (= "ATTRIB" (dxf 0 block1))(= "INSERT" (dxf 0 block1)))
	(setq block1 buffer)
	(Setq buffer (entnext block1))
	(setq block1 (entget block1))
	(cond
	  ((= (dxf 2 block1) "SH")
	   (setq block1 (ch_oj 1 sh_txt block1))
	  )
	  ((= (dxf 2 block1) "DK")
	   (setq block1 (ch_oj 1 dk_txt block1))
	  )
	  ((= (dxf 2 block1) "L")
	   (setq block1 (ch_oj 1 cd_txt block1))
	  )
	  ((= (dxf 2 block1) "SL")
	   (setq block1 (ch_oj 1 sl_txt block1))
	  )
	  ((= (dxf 2 block1) "SCK")
	   (setq block1 (ch_oj 1 sla_txt block1))
	  )
	  ((= (dxf 2 block1) "LA")
	   (setq block1 (ch_oj 1 tcd_txt block1))
	  )
	  ((= (dxf 2 block1) "KL")
	   (setq block1 (ch_oj 1 kl_txt block1))
	  )
	  ((= (dxf 2 block1) "L1")
	   (setq block1 (ch_oj 1 l1 block1))
	  )
	  ((= (dxf 2 block1) "L2")
	   (setq block1 (ch_oj 1 l2 block1))
	  )
	  ((= (dxf 2 block1) "L3")
	   (setq block1 (ch_oj 1 l3 block1))
	  )
	  ((= (dxf 2 block1) "LNEO")
	   (setq block1 (ch_oj 1 neo block1))
	  )
	)
;;;COND
      )
;;;WHILE
    )
;;;PROGN
  )
;;;IF
)
;;; chuong trinh chinh
;;; Chuong trinh nhap so lieu moi
(defun c:tk	(/ buffer counter flag tkck)
  (setq clayer (getvar "clayer"))
  (SETVAR "CLAYER" "0")
  (setvar "osmode" 0)
  (setvar "luprec" 1)
  (setq ins_point (getpoint "\nDiem chen: "))
  (command "insert" "tkvl" ins_point 1 1 0)
  (command "explode" "last" "")
  (setq ins_point (polar ins_point 0 1600))
  (princ "\nNhap so lieu theo trinh tu sau :")
  (setq list_type '(0 1 2 3 4 5 6 7 8))
  (setq list_dk '(6 8 10 12 14 16 18 20 22 25 28 30 32 36))
  (setq list_dka1 '(6 8))
  (setq	SH_TXT "1"
	DK 0
	Sl 0
	SLA 0
	counter	0
  )
  (while (/= sh_txt "0")
    (setq buffer sh_txt)
    (setq sh_txt
	   (getstring
	     (strcat "\nSo hieu thep (0 de ket thuc) <"
		     sh_txt
		     ">: "
	     )
	   )
    )
    (if	(= sh_txt "")
      (setq sh_txt buffer)
    )
    (if	(/= sh_txt "0")
      (progn
;;; Nhap kieu thep
	(setq flag 1)
	(while (= flag 1)
	  (setq loai (getint "\nKieu thep <Chon>: "))
	  (if (= loai nil)
	    (c_loai_thep)
	  )
	  (if (= (member loai list_type) nil)
	    (princ "\nKieu thep khong dung.")
	    (setq flag 0)
	  )
	)
;;; Nhap duong kinh thep
	(if (or	(= loai 2)
		(= loai 5)
		(= loai 6)
		(= loai 8)
	    )
	  (progn
	    (setq flag 1)
	    (while (= flag 1)
	      (setq buffer dk)
	      (setq
		dk
		 (getint (strcat "\nDuong kinh (mm) <" (itoa dk) ">: ")
		 )
	      )
	      (if (= dk nil)
		(setq dk buffer)
	      )
	      (if (= (member dk list_dka1) nil)
		(princ "\nVao duong kinh khong dung.")
		(setq flag 0)

	      )
	    )
	  )
	  (progn
	    (setq flag 1)
	    (while (= flag 1)
	      (setq buffer dk)
	      (setq
		dk
		 (getint (strcat "\nDuong kinh (mm) <" (itoa dk) ">: ")
		 )
	      )
	      (if (= dk nil)
		(setq dk buffer)
	      )
	      (if (= (member dk list_dk) nil)
		(princ "\nVao duong kinh khong dung.")
		(setq flag 0)

	      )
	    )
	  )
	)
;;;;;; Nhap chieu dai thanh thep
	(cond
;;;;;;	Thep khong co kieu
	  ((= loai 0)
	   (progn
	     (setvar "luprec" 0)
	     (setq neo 0
		   L1 0
		   L2 0
		   L3 0
	     )
	     (princ "\nChieu dai (mm)")
	     (initget 7)
	     (setq L1 (getint "\n     L1 = "
		      )
	     )
	     (Setq CD L1)
	     (setvar "luprec" 1)
	   )
	  )
;;;;;;	Thep co kieu so 1
	  ((= loai 1)
	   (progn
	     (setvar "luprec" 0)
	     (setq neo 0
		   L1 0
		   L2 0
		   L3 0
	     )
	     (princ "\nChieu dai (mm)")
	     (initget 7)
	     (setq L1 (getint "\n     L1 = "
		      )
	     )
	     (Setq CD L1)
	     (setvar "luprec" 1)
	   )
	  )
;;;	Thep co kieu so 2
	  ((= loai 2)
	   (progn
	     (setvar "luprec" 0)
	     (setq neo 0
		   L1 0
		   L2 0
		   L3 0
	     )
	     (princ "\nChieu dai (mm)")
	     (initget 7)
	     (setq L1 (getint "\n     L1 = "
		      )
	     )
	     (setq neo (* 10 (atof (rtos (/ (* 7.5 dk) 10)))))
	     (Setq CD (+ L1 (* 2 neo)))
	     (setvar "luprec" 1)
	   )
	  )
;;;;;;	Thep co kieu so 3
	  ((= loai 3)
	   (progn
	     (setvar "luprec" 0)
	     (setq neo 0
		   L1 0
		   L2 0
		   L3 0
	     )
	     (princ "\nChieu dai (mm)")
	     (initget 7)
	     (setq L1 (getint "\n     L1 = "))
	     (initget 7)
	     (setq L2 (getint "\n     L2 = "))
	     (initget 7)
	     (setq L3 (getint "\n     L3 = "))
	     (Setq CD (+ L1 L2 L3))
	     (setvar "luprec" 1)
	   )
	  )
;;;;;;	Thep co kieu so 4
	  ((= loai 4)
	   (progn
	     (setvar "luprec" 0)
	     (setq neo 0
		   L1 0
		   L2 0
		   L3 0
	     )
	     (princ "\nChieu dai (mm)")
	     (initget 7)
	     (setq L1 (getint "\n     L1 = "))
	     (initget 7)
	     (setq L2 (getint "\n     L2 = "))
	     (initget 7)
	     (setq L3 (getint "\n     L3 = "))
	     (Setq CD (+ L1 L2 L3))
	     (setvar "luprec" 1)
	   )
	  )
;;;;;;	Thep co kieu so 5
	  ((= loai 5)
	   (progn
	     (setvar "luprec" 0)
	     (setq neo 0
		   L1 0
		   L2 0
		   L3 0
	     )
	     (princ "\nChieu dai (mm)")
	     (initget 7)
	     (setq L1 (getint "\n     L1 = "))
	     (initget 7)
	     (setq L2 (getint "\n     L2 = "))
	     (setq neo (* 10 (atof (rtos (/ (* 7.5 dk) 10)))))
	     (Setq CD (* (+ L1 L2 neo) 2))
	     (setvar "luprec" 1)
	   )
	  )
;;;;;;	Thep co kieu so 6
	  ((= loai 6)
	   (progn
	     (setvar "luprec" 0)
	     (setq neo 0
		   L1 0
		   L2 0
		   L3 0
	     )
	     (princ "\nDuong kinh dai (mm)")
	     (initget 7)
	     (setq L1 (getint "\n     D = "))
	     (setq neo (* 10 (atof (rtos (/ (* 7.5 dk) 10)))))
	     (Setq CD (+ (* 2 neo) (* pi L1)))
	     (setvar "luprec" 1)
	   )
	  )
;;;;;;	Thep co kieu so 7
	  ((= loai 7)
	   (progn
	     (setvar "luprec" 0)
	     (setq neo 0
		   L1 0
		   L2 0
		   L3 0
	     )
	     (princ "\nChieu dai (mm)")
	     (initget 7)
	     (setq L1 (getint "\n     L1 = "))
	     (initget 7)
	     (setq L2 (getint "\n     L2 = "))
	     (Setq CD (+ L1 L2))
	     (setvar "luprec" 1)
	   )
	  )
;;;;;;	Thep co kieu so 8
	  ((= loai 8)
	   (progn
	     (setvar "luprec" 0)
	     (setq neo 0
		   L1 0
		   L2 0
		   L3 0
	     )
	     (princ "\nChieu dai (mm)")
	     (initget 7)
	     (setq L1 (getint "\n     L1 = "))
	     (initget 7)
	     (setq L2 (getint "\n     L2 = "))
	     (setq neo (* 10 (atof (rtos (/ (* 7.5 dk) 10)))))
	     (Setq CD (+ (+ L1 L2) (* neo 2)))
	     (setvar "luprec" 1)
	   )
	  )
	)
;;; Nhap so luong thanh
	(setq buffer sl)
	(setq
	  sl (getint (strcat "\nSo luong (thanh) <" (itoa sl) ">: "))
	)
	(if (= sl nil)
	  (setq sl buffer)
	)
;;; Nhap so luong cau kien	
	(setq buffer sla)
	(setq
	  sla (getint
		(strcat	"\nSo luong cau kien (Cau kien) <"
			(itoa sla)
			">: "
		)
	      )
	)
	(if (= sla nil)
	  (setq sla buffer)
	)
	(setq cd (fix cd))
	(setq TCD (* sla (cal_length sl cd)))
	(setq kl (cal_weight dk TCD))
	(setq dk_txt (itoa dk))
	(setq cd_txt (itoa CD))
	(setq sl_txt (itoa sl))
	(setq sla_txt (itoa sla))
	(setq tcd_txt (rtos tcd))
	(setq kl_txt (rtos kl))
	(setq neo (itoa (fix neo)))
	(setq L1 (itoa L1))
	(setq L2 (itoa L2))
	(setq L3 (itoa L3))
;;; Hoan thien so lieu thong ke
	(cond
	  ((= loai 0)
	   (progn
	     (setq tkck nil)
	     (command "insert" "tk_0" ins_point	"" "" "" "" "" "" "" ""
		      "" "")
	     (setq tkck (entlast))
	     (entget_oj tkck)
	     (entupd tkck)
	   )
;;;progn
	  )
	  ((= loai 1)
	   (progn
	     (setq tkck nil)
	     (command "insert"	    "tk_1" ins_point	 ""	""
		      ""     ""	    ""	   ""	  ""	 ""	""
		      ""     ""
		     )
	     (setq tkck (entlast))
	     (entget_oj tkck)
	     (entupd tkck)
	   )
;;;progn
	  )
	  ((= loai 2)
	   (progn
	     (setq tkck nil)
	     (command "insert"	    "tk_2" ins_point	 ""	""
		      ""     ""	    ""	   ""	  ""	 ""	""
		      ""     ""	    ""	   ""
		     )
	     (setq tkck (entlast))
	     (entget_oj tkck)
	     (entupd tkck)
	   )
;;;progn
	  )
	  ((= loai 3)
	   (progn
	     (setq tkck nil)
	     (command "insert"	    "tk_3" ins_point	 ""	""
		      ""     ""	    ""	   ""	  ""	 ""	""
		      ""     ""	    ""	   ""
		     )
	     (setq tkck (entlast))
	     (entget_oj tkck)
	     (entupd tkck)
	   )
;;;progn
	  )
	  ((= loai 4)
	   (progn
	     (setq tkck nil)
	     (command "insert"	    "tk_4" ins_point	 ""	""
		      ""     ""	    ""	   ""	  ""	 ""	""
		      ""     ""	    ""	   ""
		     )
	     (setq tkck (entlast))
	     (entget_oj tkck)
	     (entupd tkck)
	   )
;;;progn
	  )
	  ((= loai 5)
	   (progn
	     (setq tkck nil)
	     (command "insert"	    "tk_5" ins_point	 ""	""
		      ""     ""	    ""	   ""	  ""	 ""	""
		      ""     ""	    ""	   ""
		     )
	     (setq tkck (entlast))
	     (entget_oj tkck)
	     (entupd tkck)
	   )
;;;progn
	  )
	  ((= loai 6)
	   (progn
	     (setq tkck nil)
	     (command "insert"	    "tk_6" ins_point	 ""	""
		      ""     ""	    ""	   ""	  ""	 ""	""
		      ""     ""	    ""
		     )
	     (setq tkck (entlast))
	     (entget_oj tkck)
	     (entupd tkck)
	   )
;;;progn
	  )
	  ((= loai 7)
	   (progn
	     (setq tkck nil)
	     (command "insert"	    "tk_7" ins_point	 ""	""
		      ""     ""	    ""	   ""	  ""	 ""	""
		      ""     ""	    ""
		     )
	     (setq tkck (entlast))
	     (entget_oj tkck)
	     (entupd tkck)
	   )
;;;progn
	  )
	  ((= loai 8)
	   (progn
	     (setq tkck nil)
	     (command "insert"	    "tk_8" ins_point	 ""	""
		      ""     ""	    ""	   ""	  ""	 ""	""
		      ""     ""	    ""	   ""	  ""
		     )
	     (setq tkck (entlast))
	     (entget_oj tkck)
	     (entupd tkck)
	   )
;;;progn
	  )
	)
;;;cond
	(setq sh_txt (itoa (1+ (atoi sh_txt))))
	(setq ins_point (polar ins_point (* 3 pi 0.5) 1000))
	(setq f_point (list (cons '10 ins_point)))
	(setq e_point (list (cons '11 (polar ins_point 0 18200))))
	(setq
	  line_data (append '((0 . "line") (8 . "0")) f_point e_point)
	)
	(entmake line_data)
      )
    )
    (setq counter (1+ counter))
  )
;;;  hoan thien duong bien
  (setq d_bien (entget (entlast)))
  (setq f_point (dxf 10 d_bien))
  (setq f_point (polar f_point pi 1600))
  (setq d_bien (ch_oj 10 f_point d_bien))
  (setq f_point (dxf 10 d_bien))
  (setq e_point (dxf 11 d_bien))
  (setq ins_point (polar f_point (/ pi 2) (+ (* 1000 counter) 400)))
  (setq	list_doc
	 (ssget	"f"
		(append
		  (list ins_point)
		  (list (polar ins_point 0 19800))
		)
		'((0 . "LINE"))
	 )
  )
  (setq counter 0)
  (while (< counter (sslength list_doc))
    (setq d_doc (entget (ssname list_doc counter)))
    (setq p1 (dxf 10 d_doc)
	  p2 (dxf 11 d_doc)
    )
    (setq p2 (inters f_point e_point p1 p2 nil))
    (ch_oj 11 p2 d_doc)
    (setq counter (1+ counter))
  )
  (setVAR "clayer" clayer)
  (setvar "osmode" 108)
)
;;; Chuong trinh gan so lieu da co vao hop hoi thoai
(defun get_oj (block / BUFFER block1)
  (setq block1 (entnext block))
  (setq buffer block1)
  (setq block1 (entget block1))
  (if (= "ATTRIB" (dxf 0 block1))
    (progn
      (while (= "ATTRIB" (dxf 0 block1))
	(setq block1 buffer)
	(Setq buffer (entnext block1))
	(setq block1 (entget block1))
	(cond
	  ((= (dxf 2 block1) "SH")
	   (setq sh_txt (dxf 1 block1))
	  )
	  ((= (dxf 2 block1) "DK")
	   (setq dk_txt (dxf 1 block1))
	  )
	  ((= (dxf 2 block1) "SL")
	   (setq sl_txt (dxf 1 block1))
	  )
	  ((= (dxf 2 block1) "SCK")
	   (setq sla_txt (dxf 1 block1))
	  )
	  ((= (dxf 2 block1) "L")
	   (setq Cd_txt (dxf 1 block1))
	  )
	  ((= (dxf 2 block1) "L1")
	   (setq L1 (dxf 1 block1))
	  )
	  ((= (dxf 2 block1) "L2")
	   (setq L2 (dxf 1 block1))
	  )
	  ((= (dxf 2 block1) "L3")
	   (setq l3 (dxf 1 block1))
	  )
	)
;;;COND
      )
;;;WHILE
    )
;;;PROGN
  )
;;;IF
)
;;; chuong trinh sua doi
(defun c:TK	(/ tkck)
  (setq list_data (ssget '((0 . "INSERT"))))
  (cond
;;; Neu chi sua chua mot doi tuong    
    ((= (sslength list_data) 1)
     (progn
;;; sua chua so lieu       
       (setq tkck (ssname list_data 0))
       (cond
;;; thep khong co kieu	 
	 ((= (dxf 2 (entget tkck)) "TK_0")
	  (progn
	    (get_oj tkck)
	    (setq L1   cd_txt
		  L2   ""
		  L3   ""
		  loai 0
	    )
	    (tk_mod_box)
	    (setvar "luprec" 0)
	    (setq dk (atoi dk_txt))
	    (setq sl (atoi sl_txt))
	    (setq sla (atoi sla_txt))
	    (setq L1 (atoi L1))
	    (setq CD L1)
	    (setvar "luprec" 1)
	    (setq TCD (* sla (cal_length sl cd)))
	    (setq kl (cal_weight dk TCD))
	    (setq dk_txt (itoa dk))
	    (setq cd_txt (itoa CD))
	    (setq sl_txt (itoa sl))
	    (setq sla_txt (itoa sla))
	    (setq tcd_txt (rtos tcd))
	    (setq kl_txt (rtos kl))
	    (setq L1 (itoa L1))
	    (entget_oj tkck)
	    (entupd tkck)
	  )
	 )
;;; Thep kieu 1 	 
	 ((= (dxf 2 (entget tkck)) "TK_1")
	  (progn
	    (get_oj tkck)
	    (setq L2 ""
		  L3 ""
		  loai 1
	    )
	    (tk_mod_box)
	    (setvar "luprec" 0)
	    (setq dk (atoi dk_txt))
	    (setq sl (atoi sl_txt))
	    (setq sla (atoi sla_txt))
	    (setq L1 (atoi L1))
	    (setq CD L1)
	    (setvar "luprec" 1)
	    (setq TCD (* sla (cal_length sl cd)))
	    (setq kl (cal_weight dk TCD))
	    (setq dk_txt (itoa dk))
	    (setq cd_txt (itoa CD))
	    (setq sl_txt (itoa sl))
	    (setq sla_txt (itoa sla))
	    (setq tcd_txt (rtos tcd))
	    (setq kl_txt (rtos kl))
	    (setq L1 (itoa L1))
	    (entget_oj tkck)
	    (entupd tkck)
	  )
	 )
;;; thep kieu 2	 
	 ((= (dxf 2 (entget tkck)) "TK_2")
	  (progn
	    (get_oj tkck)
	    (setq L2 ""
		  L3 ""
		  loai 2
	    )
	    (tk_mod_box)
	    (setvar "luprec" 0)
	    (setq dk (atoi dk_txt))
	    (setq sl (atoi sl_txt))
	    (setq sla (atoi sla_txt))
	    (setq L1 (atoi L1))
	    (setq neo (* 10 (atof (rtos (/ (* 7.5 dk) 10)))))
	    (Setq CD (+ L1 (* 2 neo)))
	    (setvar "luprec" 1)
	    (setq cd (fix cd))
	    (setq TCD (* sla (cal_length sl cd)))
	    (setq kl (cal_weight dk TCD))
	    (setq dk_txt (itoa dk))
	    (setq cd_txt (itoa CD))
	    (setq sl_txt (itoa sl))
	    (setq sla_txt (itoa sla))
	    (setq tcd_txt (rtos tcd))
	    (setq kl_txt (rtos kl))
	    (setq neo (itoa (fix neo)))
	    (setq L1 (itoa L1))
	    (entget_oj tkck)
	    (entupd tkck)
	  )
	 )
;;; thep kieu 3	 
	 ((= (dxf 2 (entget tkck)) "TK_3")
	  (progn
	    (get_oj tkck)
	    (setq loai 3)
	    (tk_mod_box)
	    (setvar "luprec" 0)
	    (setq dk (atoi dk_txt))
	    (setq sl (atoi sl_txt))
	    (setq sla (atoi sla_txt))
	    (setq L1 (atoi L1))
	    (setq L2 (atoi L2))
	    (setq L3 (atoi L3))
	    (setq CD (+ L1 l2 l3))
	    (setvar "luprec" 1)
	    (setq TCD (* sla (cal_length sl cd)))
	    (setq kl (cal_weight dk TCD))
	    (setq dk_txt (itoa dk))
	    (setq cd_txt (itoa CD))
	    (setq sl_txt (itoa sl))
	    (setq sla_txt (itoa sla))
	    (setq tcd_txt (rtos tcd))
	    (setq kl_txt (rtos kl))
	    (setq L1 (itoa L1))
	    (setq L2 (itoa L2))
	    (setq L3 (itoa L3))
	    (entget_oj tkck)
	    (entupd tkck)
	  )
	 )
;;; thep kieu 4	 
	 ((= (dxf 2 (entget tkck)) "TK_4")
	  (progn
	    (get_oj tkck)
	    (setq loai 4)
	    (tk_mod_box)
	    (setvar "luprec" 0)
	    (setq dk (atoi dk_txt))
	    (setq sl (atoi sl_txt))
	    (setq sla (atoi sla_txt))
	    (setq L1 (atoi L1))
	    (setq L2 (atoi L2))
	    (setq L3 (atoi L3))
	    (setq CD (+ L1 l2 l3))
	    (setvar "luprec" 1)
	    (setq TCD (* sla (cal_length sl cd)))
	    (setq kl (cal_weight dk TCD))
	    (setq dk_txt (itoa dk))
	    (setq cd_txt (itoa CD))
	    (setq sl_txt (itoa sl))
	    (setq sla_txt (itoa sla))
	    (setq tcd_txt (rtos tcd))
	    (setq kl_txt (rtos kl))
	    (setq L1 (itoa L1))
	    (setq L2 (itoa L2))
	    (setq L3 (itoa L3))
	    (entget_oj tkck)
	    (entupd tkck)
	  )
	 )
;;; thep kieu 5	 
	 ((= (dxf 2 (entget tkck)) "TK_5")
	  (progn
	    (get_oj tkck)
	    (setq L3 ""
		  loai 5
	    )
	    (tk_mod_box)
	    (setvar "luprec" 0)
	    (setq dk (atoi dk_txt))
	    (setq sl (atoi sl_txt))
	    (setq sla (atoi sla_txt))
	    (setq L1 (atoi L1))
	    (setq L2 (atoi L2))
	    (setq neo (* 10 (atof (rtos (/ (* 7.5 dk) 10)))))
	    (Setq CD (* (+ L1 L2 neo) 2))
	    (setvar "luprec" 1)
	    (setq cd (fix cd))
	    (setq TCD (* sla (cal_length sl cd)))
	    (setq kl (cal_weight dk TCD))
	    (setq dk_txt (itoa dk))
	    (setq cd_txt (itoa CD))
	    (setq sl_txt (itoa sl))
	    (setq sla_txt (itoa sla))
	    (setq tcd_txt (rtos tcd))
	    (setq kl_txt (rtos kl))
	    (setq neo (itoa (fix neo)))
	    (setq L1 (itoa L1))
	    (setq L2 (itoa L2))
	    (entget_oj tkck)
	    (entupd tkck)
	  )
	 )
;;; thep kieu 6	 
	 ((= (dxf 2 (entget tkck)) "TK_6")
	  (progn
	    (get_oj tkck)
	    (setq L2 ""
		  L3 ""
		  loai 6
	    )
	    (tk_mod_box)
	    (setvar "luprec" 0)
	    (setq dk (atoi dk_txt))
	    (setq sl (atoi sl_txt))
	    (setq sla (atoi sla_txt))
	    (setq L1 (atoi L1))
	    (setq neo (* 10 (atof (rtos (/ (* 7.5 dk) 10)))))
	    (Setq CD (+ (* 2 neo) (* pi L1)))
	    (setvar "luprec" 1)
	    (setq cd (fix cd))
	    (setq TCD (* sla (cal_length sl cd)))
	    (setq kl (cal_weight dk TCD))
	    (setq dk_txt (itoa dk))
	    (setq cd_txt (itoa CD))
	    (setq sl_txt (itoa sl))
	    (setq sla_txt (itoa sla))
	    (setq tcd_txt (rtos tcd))
	    (setq kl_txt (rtos kl))
	    (setq neo (itoa (fix neo)))
	    (setq L1 (itoa L1))
	    (entget_oj tkck)
	    (entupd tkck)
	  )
	 )
;;; thep kieu 7	  
	 ((= (dxf 2 (entget tkck)) "TK_7")
	  (progn
	    (get_oj tkck)
	    (setq L3 ""
		  loai 7
	    )
	    (tk_mod_box)
	    (setvar "luprec" 0)
	    (setq dk (atoi dk_txt))
	    (setq sl (atoi sl_txt))
	    (setq sla (atoi sla_txt))
	    (setq L1 (atoi L1))
	    (setq L2 (atoi L2))
	    (setq CD (+ L1 l2))
	    (setvar "luprec" 1)
	    (setq TCD (* sla (cal_length sl cd)))
	    (setq kl (cal_weight dk TCD))
	    (setq dk_txt (itoa dk))
	    (setq cd_txt (itoa CD))
	    (setq sl_txt (itoa sl))
	    (setq sla_txt (itoa sla))
	    (setq tcd_txt (rtos tcd))
	    (setq kl_txt (rtos kl))
	    (setq L1 (itoa L1))
	    (setq L2 (itoa L2))
	    (entget_oj tkck)
	    (entupd tkck)
	  )
	 )
;;; thep kieu 8	 
	 ((= (dxf 2 (entget tkck)) "TK_8")
	  (progn
	    (get_oj tkck)
	    (setq L3 ""
		  loai 8
	    )
	    (tk_mod_box)
	    (setvar "luprec" 0)
	    (setq dk (atoi dk_txt))
	    (setq sl (atoi sl_txt))
	    (setq sla (atoi sla_txt))
	    (setq L1 (atoi L1))
	    (setq L2 (atoi L2))
	    (setq neo (* 10 (atof (rtos (/ (* 7.5 dk) 10)))))
	    (Setq CD (+ (* 2 neo) (+ L1 L2)))
	    (setvar "luprec" 1)
	    (setq cd (fix cd))
	    (setq TCD (* sla (cal_length sl cd)))
	    (setq kl (cal_weight dk TCD))
	    (setq dk_txt (itoa dk))
	    (setq cd_txt (itoa CD))
	    (setq sl_txt (itoa sl))
	    (setq sla_txt (itoa sla))
	    (setq tcd_txt (rtos tcd))
	    (setq kl_txt (rtos kl))
	    (setq neo (itoa (fix neo)))
	    (setq L1 (itoa L1))
	    (setq L2 (itoa L2))
	    (entget_oj tkck)
	    (entupd tkck)
	  )
	 )
       )
     )
    )
    ((> (sslength list_data) 1)
     (progn
       (setq loai nil)
       (tk_mod_box)
       (setq soluong (atoi soluong))
       (setq tsoluong (atoi tsoluong))
       (setq counter 0)
       (while (< counter (sslength list_data))
	 (setq tkck (ssname list_data counter))
	 (get_oj tkck)
	 (setq cd (atoi cd_txt))
	 (setq dk (atoi dk_txt))
	 (if (/= 0 soluong)
	   (setq sl soluong)
	   (setq sl (atoi sl_txt))
	 )
	 (if (/= 0 tsoluong)
	   (setq sla tsoluong)
	   (setq sla (atoi sla_txt))
	 )
	 (setq TCD (* sla (cal_length sl cd)))
	 (setq kl (cal_weight dk TCD))
	 (setq sl_txt (itoa sl))
	 (setq sla_txt (itoa sla))
	 (setq tcd_txt (rtos tcd))
	 (setq kl_txt (rtos kl))
	 (entget_oj tkck)
	 (entupd tkck)
	 (setq counter (1+ counter))
       )
     )
    )
  )
)