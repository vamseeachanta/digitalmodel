;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; PROGRAM STATICSTIC ;;;;;;;;;;;;;;;;;;;;;;;;;
;;;==================== load-time error checking ========================
  (defun ai_abort (app msg)
     (defun *error* (s)
        (if old_error (setq *error* old_error))
        (princ)
     )
     (if msg
       (alert (strcat " Application error: "
                      app
                      " \n\n  "
                      msg
                      "  \n"
              )
       )
     )
     (exit)
  )

;;; Check to see if AI_UTILS is loaded, If not, try to find it,
;;; and then try to load it.
;;;
;;; If it can't be found or it can't be loaded, then abort the
;;; loading of this file immediately, preserving the (autoload)
;;; stub function.

  (cond
     (  (and ai_dcl (listp ai_dcl)))          ; it's already loaded.

     (  (not (findfile "ai_utils.lsp"))                     ; find it
        (ai_abort "tkth"
                  (strcat "Can't locate file AI_UTILS.LSP."
                          "\n Check support directory.")))

     (  (eq "failed" (load "ai_utils" "failed"))            ; load it
        (ai_abort "TKTH" "Can't load file AI_UTILS.LSP"))
  )

  (if (not (ai_acadapp))               ; defined in AI_UTILS.LSP
      (ai_abort "TKTH" nil)         ; a Nil <msg> supresses
  )                                    ; ai_abort's alert box dialog.

;;;=============================== end load-time operations ====================================
;;;
;;;
(defun set_ds()
    (cond
      ((= ds%% 3) ;thep I
        (setq ds_list (list"12 11.5""14 13.7""16 15.9""18 18.4""18a 19.9""20 21.0""20a 22.7""22 24.0"
		          "24 27.3""27 31.5""30 36.5""30a 39.2""33 42.2""36 48.6""40 56.1"
                          "45 65.2""50 76.8"
                     ) )
        (setq Q#0L "TH_4")(set_tile"THEP_I""1")
       )
      ((= ds%% 2) ; thep U
        (setq ds_list (list"5 4.84""6.5 5.9""8 7.05""10 8.59""12 10.4""14 12.3""14a 13.3""16 14.2"
		          "18 16.3""18a 17.4""20 18.4""20a 19.8""22 21.0""24 24.0""24a 25.8"
                          "27 27.7""30 31.8"
                     ) )
        (setq Q#0L "TH_3")(set_tile"THEP_U""1")
       )
      ((= ds%% 1) ; thep L simmetric
        (setq ds_list (list"45x4 2.73""45x5 3.37""50x5 3.77""63x4 3.9""63x5 4.81""63x6 5.72"
		          "70x5 5.38""70x6 6.39""75x5 5.80""75x6 6.89""80x6 7.36""80x7 8.51"
                          "90x6 8.33""90x7 9.64""100x7 10.8""100x8 12.2""100x9 15.1"
                     ) )
        (setq Q#0L "TH_2")(set_tile"THEP_L""1")
       )
     ((= ds%% 5) ; thep 2L simmetric
        (setq ds_list (list"45x4 5.46""45x5 6.74""50x5 9.6""63x4 7.8""63x5 9.62""63x6 11.5"
		          "70x5 11.8""70x6 12.8""75x5 11.6""75x6 13.8""80x6 14.8""80x7 17.1"
                          "90x6 16.7""90x7 19.3""100x7 21.6""100x8 24.4""100x9 30.2"
                     ) )
        (setq Q#0L "TH_1")(set_tile"THEP_2L""1")
       )
      ((= ds%% 4) ; thep L unsimmetric
        (setq ds_list (list"56x36x4 2.81""56x36x5 3.46""63x40x5 3.91""63x40x6 4.63"
		          "70x45x5 4.39""75x50x5 4.79""75x50x6 5.69""80x50x5 4.99"
		          "80x50x6 5.92""90x56x6 6.70""90x56x8 11.2""100x63x6 7.53""100x63x7 11.1"
                     ) )
        (setq Q#0L "TH_5")(set_tile"THEP_Lunsim""1")
       )
      ((= ds%% 6) ; thep O 
        (setq ds_list (list"45x3,5 3.58""45x4,0 4.04""50x3,5 4.01""50x4 4.45"
		          "54x3,5 4.36""54x4 4.93""54x5 6.04""57x4 5.23""57x5 6.41""60x4 5.52""60x5 6.78"
		          "63,5x3,5 5.18" "63,5x4 5.87" "63,5x5 7.21" "63,5x6 8.51" "68x3,5 5.57"
			"68x4 6.317""68x5 7.77""68x6 9.17""70x4 6.51""70x5 8.01""70x6 9.47"
			"73x4 6.81""73x5 8.38""73x6 9.91""76x4 7.1""76x5 8.75""76x6 10.36""76x7 11.91"
		       "76x8 13.42""83x4 7.79""83x5 9.62""83x6 11.9" "83x8 14.8" "89x4 10.7" "89x5 10.4" "89x6 12.3"
"89x8 16.0""95x4 8.96""95x5 11.1""95x6 13.2""95x8 17.2""102x4 9.67""102x5 12.0""102x6 14.2""102x8 18.6"
"102x10 22.7""108x4 10.3""108x5 12.7""108x6 15.1""102x8 19.7""102x10 24.2""114x4 10.9""114x5 13.5"
"114X6 15.98""114X8 20.91""114X10 25.65""121X4 11.54""121X5 14.30""121X6 17.02""121X8 22.29""121X10 27.37"
"127X4 12.1""127X5 15.1""127X6 17.9""127x8 23.5""127x10 28.9""127x12 34.1""127x14 39.0""133x4 12.4"
"133x5 15.8""133x6 18.8""133x7 21.8""133x8 24.7""133x10 30.3""133x12 35.8""133x14 41.0"
"140x5 16.7""140x6 19.8""140x7 22.9""140x8 26.1""140x10 32.1""140x12 37.9""140x14 43.5""140x16 48.9"
"146x5 17.4""146x6 20.7""146x8 27.2""146x10 33.5""146x12 39.7""146x14 45.6""146x16 51.3"
"152x5 18.1""152x6 21.6""152x8 28.4""152x10 35.0""152x12 41.4""152x14 47.6""152x16 53.7"
"159x5 19.0""159x6 22.6""159x8 29.8""159x10 36.8""159x12 43.5""159x14 50.1""159x16 56.4""168x6 24.0"
"168x8 31.6""168x10 39.0""168x12 46.2""168x14 53.2""168x16 60.0""180x8 25.75""180x8 33.9""180x10 41.9"
"180x12 49.7""180x16 64.7""180x18 79.7""194x6 27.8""194x8 36.7""194x10 45.4""194x12 53.9""194x14 62.2"
			   "194x16 70.2""194x18 78.1""203x8 38.5""203x10 47.6""203x12 56.5""203x14 65.9"
			   "203x16 73.8""203x18 82.1""219x8 41.6""219x10 51.5""219x12 61.3""219x14 70.8"
			   "219x16 80.1""219x20 98.2""245x7 41.1""245x8 46.8""245x9 52.4""245x10 57.6"		   			   
	) )
        (setq Q#0L "TH_6")(set_tile"THEP_O""1")
       )
     )	   
    (start_list "DANH_SACH")
    (mapcar 'add_list ds_list)
    (end_list)
    (if (null ten%th)(set_tile "DANH_SACH" "0")(set_tile "DANH_SACH" ten%th))
  )
;;;-------------------------check error---------------------------
  (defun rs_error()
     (set_tile "error""")
   )
;;;-------------------------check input data---------------------------
  (defun check_scl%()
   (setq scl% (ai_num (get_tile "SCL")" ti le vao sai. Vao lai"6))
   (if (null scl%) (progn ( set_tile"error""Vao lai tile")Nil)T)
  )
  (defun check_SH()
   (setq q&sh (get_tile "SH"))
   (if (null q&sh) (progn ( set_tile"error""Vao lai so hieu")Nil)T)
  )
  (defun check_ten%th()
  (setq ten%th (get_tile "DANH_SACH"))
  (if (null ten%th) (progn ( set_tile"error""Nhap ten thep hinh")Nil)T)
  )
  (defun check_l&th()
   (setq l&th (ai_num (get_tile "L%TH")" chieu dai vao sai. Vao lai"6))
   (if (null l&th) (progn ( set_tile"error""Vao lai so hieu")Nil)T)
  )
  (defun check_L1()
   (setq ql1% (ai_num (get_tile "L1")" chieu dai vao sai. Vao lai"6))
   (if (null ql1%) (progn ( set_tile"error""Vao lai so hieu")Nil)T)
  )
  (defun check_L2()
   (setq ql2% (ai_num (get_tile "L2")" chieu dai vao sai. Vao lai"6))
   (if (null ql2%) (progn ( set_tile"error""Vao lai so hieu")Nil)T)
  )
  (defun check_T()
   (setq q&t% (ai_num (get_tile "T")" chieu day ban vao sai. Vao lai"6))
   (if (null q&t%) (progn ( set_tile"error""Vao lai so hieu")Nil)T)
  )
  (defun check_sl1()
   (setq sl1% (ai_num (get_tile "SL1")" chieu dai vao sai. Vao lai"6))
   (if (null sl1%) (progn ( set_tile"error""Vao lai so hieu")Nil)T)
  )
  (defun check_slck()
   (setq slck% (ai_num (get_tile "SLCK")" chieu dai vao sai. Vao lai"6))
   (if (null slck%) (progn ( set_tile"error""Vao lai so hieu")Nil)T)
  )

;;;-----------------lay du lieu------------------------------------
  (defun getdata ()
   (cond 
     ((= "1" va%1)
        (cond
           ((not(check_scl%))(mode_tile "SCL" 2))
           ((not(check_sh))(mode_tile "SH" 2))
           ((not(check_sl1))(mode_tile "SL1" 2))
           ((not(check_slck))(mode_tile "SLCK" 2))
           ((not(check_l&th))(mode_tile "L%TH" 2))
           ((not(check_ten%th))(mode_tile "DANH_SACH" 2))
           (T(done_dialog 1))
         )
      )
     ((= "1" va%2)
        (cond
           ((not(check_scl%))(mode_tile "SCL" 2))
           ((not(check_sh))(mode_tile "SH" 2))
           ((not(check_sl1))(mode_tile "SL1" 2))
           ((not(check_slck))(mode_tile "SLCK" 2))
           ((not(check_l1))(mode_tile "L1" 2))
           ((not(check_l2))(mode_tile "L2" 2))
           ((not(check_T))(mode_tile "T" 2))
           (T(done_dialog 1))
         )
      )
    )
   )
;;;
;;;------setloc function------------------------------------
;;; 
   (defun mt_sltb()
              (set_tile "SLTB" "0")
              (mode_tile "L1" 1)(mode_tile "L2" 1)(mode_tile "T" 1)
              (mode_tile "L%TH" 0)(mode_tile "THEP_L" 0)
              (mode_tile "THEP_2L" 0)(mode_tile "THEP_I" 0)
              (mode_tile "THEP_U" 0)(mode_tile "THEP_Lunsim" 0)
              (mode_tile "THEP_O" 0)
	      (setq va%1 "1"	va%2 "0")
    )
    (defun mt_slth()
              (set_tile "SLTH" "0")
              (mode_tile "L%TH" 1)(mode_tile "THEP_L" 1)
              (mode_tile "THEP_2L" 1)(mode_tile "THEP_I" 1)
              (mode_tile "THEP_U" 1)(mode_tile "THEP_Lunsim" 1)
              (mode_tile "L1" 0)(mode_tile "L2" 0)(mode_tile "T" 0)
              (mode_tile "THEP_O" 1)
	      (setq va%1 "0"	va%2 "1")
    )
;;;
;;;-------------------------------
;;;
  (defun doi_wd1()
      (WHILE Q$0L
         (SETQ QO0L(ENTGET Q$0L))
         (IF(="ATTRIB"(CDR(ASSOC 0 QO0L)))
               (PROGN 
                   (SETQ Q|0L(CDR(ASSOC 2 QO0L)))
		   (COND
		     ((="SH"Q|0L)(SETQ QO0L(SUBST(CONS 1 q&sh)(ASSOC 1 QO0L)QO0L)))
		     ((="TENTH"Q|0L)(SETQ QO0L(SUBST(CONS 1 tt%h)(ASSOC 1 QO0L)QO0L)))
		     ((="L1"Q|0L)(SETQ QO0L(SUBST(CONS 1(RTOS l&th 2 0))(ASSOC 1 QO0L)QO0L)))
		     ((="DAI"Q|0L)(SETQ QO0L(SUBST(CONS 1(RTOS l&th 2 0))(ASSOC 1 QO0L)QO0L)))
		     ((="SL1"Q|0L)(SETQ QO0L(SUBST(CONS 1(itoa (fix sl1%)))(ASSOC 1 QO0L)QO0L)))
		     ((="SLA"Q|0L)(SETQ QO0L(SUBST(CONS 1(ITOA (fix slt%)))(ASSOC 1 QO0L)QO0L)))
		     ((="DT"Q|0L)(SETQ QO0L(SUBST(CONS 1(RTOS q%td 2 2))(ASSOC 1 QO0L)QO0L)))
		     ((="TL"Q|0L)(SETQ QO0L(SUBST(CONS 1(RTOS q%tl 2 2))(ASSOC 1 QO0L)QO0L)))
		    )
                    (ENTMOD QO0L)
		)
          )
          (SETQ Q$0L(ENTNEXT Q$0L))
       )
       (ENTMOD QJLL)
  )
;;;
;;;-------------------------------
;;;
  (defun doi_wd2()
      (WHILE Q$0L
         (SETQ QO0L(ENTGET Q$0L))
         (IF(="ATTRIB"(CDR(ASSOC 0 QO0L)))
               (PROGN 
                   (SETQ Q|0L(CDR(ASSOC 2 QO0L)))
		   (COND
		     ((="SH"Q|0L)(SETQ QO0L(SUBST(CONS 1 q&sh)(ASSOC 1 QO0L)QO0L)))
		     ((="L1"Q|0L)(SETQ QO0L(SUBST(CONS 1(RTOS ql1% 2 0))(ASSOC 1 QO0L)QO0L)))
		     ((="L2"Q|0L)(SETQ QO0L(SUBST(CONS 1(RTOS ql2% 2 0))(ASSOC 1 QO0L)QO0L)))
		     ((="T"Q|0L)(SETQ QO0L(SUBST(CONS 1(RTOS q&t% 2 0))(ASSOC 1 QO0L)QO0L)))
		     ((="DAI"Q|0L)(SETQ QO0L(SUBST(CONS 1(RTOS l&th 2 6))(ASSOC 1 QO0L)QO0L)))
		     ((="SL1"Q|0L)(SETQ QO0L(SUBST(CONS 1(itoa (fix sl1%)))(ASSOC 1 QO0L)QO0L)))
		     ((="SLA"Q|0L)(SETQ QO0L(SUBST(CONS 1(ITOA (fix slt%)))(ASSOC 1 QO0L)QO0L)))
		     ((="DT"Q|0L)(SETQ QO0L(SUBST(CONS 1(RTOS q%td 2 2))(ASSOC 1 QO0L)QO0L)))
		     ((="TL"Q|0L)(SETQ QO0L(SUBST(CONS 1(RTOS q%tl 2 2))(ASSOC 1 QO0L)QO0L)))
		    )
                    (ENTMOD QO0L)
		)
          )
          (SETQ Q$0L(ENTNEXT Q$0L))
       )
       (ENTMOD QJLL)
   )
;;;
;;;-------------------------------
;;;
   (defun cal_wd1()
   (setq ds_tth(nth (atoi ten%th) ds_list))
   (setq lg_st(strlen ds_tth))
   (setq tlt% (atof(substr ds_tth (- lg_st 4))))
   (setq tt%h (substr ds_tth  1 (- lg_st 4)))
   (setq slt% (* sl1% slck%))
   (setq q%td (* (* l&th sl1% ) slck% 0.001))
   (setq q%tl (* tlt% q%td))
   )
  
;;;
;;;-------------------------------
;;;
   (defun cal_wd2()
   (setq slt% (* sl1% slck%))
   (setq l&th (* (* ql1% 0.001) (* ql2% 0.001)))
   (setq q%td (* (* l&th sl1% ) slck% ))
   (setq q%tl (* (* q&t% 0.001) 7800 q%td ))
   )
  
;;;
;;;-------------------------------
;;;

  (defun w_down1(/ lg_st tlt% tt%h q00l qo0l qjll q$0l)
   (cal_wd1)
      (if Q##
        (setq Q## (LIST(NTH 0 Q##)(-(NTH 1 Q##) (* scl% 900))))  
        (setq Q## (getpoint "Insert point :"))
        )
   (COMMAND"INSERT"Q#0L Q## scl% scl%"""""""""""""""")
   (SETQ Q00L(ENTLAST))(SETQ QJLL(ENTGET Q00L))(SETQ Q$0L(ENTNEXT Q00L))
   (doi_wd1)
  )
;;;
;;;-------------------------------
;;;
  (defun w_down2(/ q00l qo0l qjll q$0l)
   (cal_wd2)
      (if Q##
        (setq Q## (LIST(NTH 0 Q##)(-(NTH 1 Q##) (* scl% 900))))  
        (setq Q## (getpoint "Insert point :"))
        )
   (setq Q#0L "tb_1")
      (COMMAND"INSERT"Q#0L Q## scl% scl%"""""""""""""""""""")
      (SETQ Q00L(ENTLAST))(SETQ QJLL(ENTGET Q00L))(SETQ Q$0L(ENTNEXT Q00L))
   (doi_wd2)
  )

;;;
;;;-------------------------------
;;;
   (defun set_data1()
    (if (null scl%)(set_tile"SCL" "1")(set_tile"SCL" (rtos scl%)))
    (if(= va%1 "1")(mt_sltb)(mt_slth))
    (if (null q%1)(set_tile"SH" "1")(set_tile"SH" q%1))
    (if (null q%2)(set_tile"L1" "100")(set_tile"L1"(itoa(fix q%2))))
    (if (null q%3)(set_tile"L2" "100")(set_tile"L2"(itoa(fix q%3))))
    (if (null q%4)(set_tile"T" "10")(set_tile"T"(itoa (fix q%4))))
    (if (null q%5)(set_tile"SL1" "1")(set_tile"SL1"(itoa(fix q%5))))
    (if (null q%6)(set_tile"SLCK" "1")(set_tile"SLCK"(itoa(fix q%6))))
    (if (null q%7)(set_tile"L%TH" "1500")(set_tile"L%TH"(itoa (fix q%7))))
   )
;;;;
;;;=============================================================================
  (defun set_data2()
    (if (null ds%%)(setq ds%% 1))
    (set_ds)
  )
;;;
;;;
  (defun conti_sl(/ qq&%)
    (setq qq&% (atof q&sh))
    (if (numberp qq&%) (setq qq&% (+ 1 qq&%)))  
    (setq q%1 (itoa(fix qq&%)))
    (setq q%2 ql1%  q%3 ql2%  q%4 q&t%   
          q%5 sl1%  q%6 slck%  q%7 l&th
      )
  )

;;;
;;;---------het phan lay du lieu------------------------------------
;;;
  (defun show_dialog()
    (setq kt%% ( load_dialog "tkth.dcl"))
            (if (not (new_dialog "nhap_du_lieu" kt%%))(exit))
            (set_data1)
            (set_data2)
            (action_tile "SLTH" "(mt_sltb)")
            (action_tile "SCL" "(rs_error)(check_scl%)")
            (action_tile "SLTB" "(mt_slth)")
            (action_tile "SH" "(rs_error)(check_sh)")
	    (action_tile "L%TH" "(rs_error)(check_l&th)")
	    (action_tile "L1" "(rs_error)(check_L1)")
	    (action_tile "L2" "(rs_error)(check_L2)")
	    (action_tile "T" "(rs_error)(check_T)")
	    (action_tile "SL1" "(rs_error)(check_sl1)")
	    (action_tile "SLCK" "(rs_error)(check_slck)")
	    (action_tile "THEP_L" "(rs_error)(setq ds%% 1)(set_ds)")
	    (action_tile "THEP_2L" "(rs_error)(setq ds%% 5)(set_ds)")
	    (action_tile "THEP_U" "(rs_error)(setq ds%% 2)(set_ds)")
	    (action_tile "THEP_I" "(rs_error)(setq ds%% 3)(set_ds)")
	    (action_tile "THEP_Lunsim" "(rs_error)(setq ds%% 4)(set_ds)")
	    (action_tile "THEP_O" "(rs_error)(setq ds%% 6)(set_ds)")
	    (action_tile "CONTINUE" "(rs_error)(getdata)(conti_sl)(setq f_done 1)") 
	    (action_tile "accept" "(rs_error)(getdata)(setq f_done 0)") 
	    (action_tile "cancel" "(done_dialog)(setq f_done 0)")         
    (if (= (start_dialog) 1)(if(= "1" va%1)(w_down1)(w_down2)))
    (unload_dialog kt%%)
  )
;;;
;;;---------------------------main program ttkth-------------------------------------
;;;
(defun c:tkth(/ kt%% tlt% va%1 va%2 ds_list ds%% q&sh ql1% ql2% q&t% sl1% slck% slt% ten%th 
               l&th tt%h q%tl q%td Q#0L f_done  q%1 q%2  q%3  q%4  q%5  q%6  q%7 Q## scl%
                )
(setq old_cmd (getvar "cmdecho")    ; save current setting of cmdecho
        old_error  *error*            ; save current error function
        *error* ai_error              ; new error function
)
(setvar "cmdecho" 0) 
(setq q&sh 0)
(setq f_done 1)
 (while (/= 0 f_done)
   (cond
     (  (not (ai_trans)))                        ; transparent OK
     (  (not (ai_acadapp)))                      ; ACADAPP.EXP xloaded?
     (  (not (setq dcl_id (ai_dcl "TKTH"))))  ; is .DCL file loaded?
     (T  (ai_undo_push)(show_dialog)(ai_undo_pop))
    )
 )
(setq *error* old_error) 
(setvar "cmdecho" old_cmd)
(princ)
)
;;;
;;;================================== end tkth program ===================================
;;;
;;;
;;;Phan truong trinh sua chua bang thong ke thep hinh
;;;
;;;==================================  program modify ===================================
;;; phan nay gom cac muc con sau day
;;;
  (defun lay_so_lieu()
	(defun mo_sltb()  ; lay so lieu tu blcok va dua vao bang sua chua thep ban
	(mt_slth) 
	    (COND
	       ((= Q&$L 1)
	         (SETQ Q% 1)(SETQ bien_2(ENTNEXT bien_1))
	            (WHILE(AND bien_2(<= Q% 9))
	              (SETQ bien_3(ENTGET bien_2))
	              (IF(="ATTRIB"(CDR(ASSOC 0 bien_3)))
	                (PROGN
                            (SETQ bien_4 (CDR(ASSOC 2 bien_3)))
                         (COND
  	                    ((="SH" bien_4)(SETQ q&sh (CDR(ASSOC 1 bien_3))))
 	                    ((="L1" bien_4)(SETQ ql1% (CDR(ASSOC 1 bien_3))))
   	                    ((="L2" bien_4)(SETQ ql2% (CDR(ASSOC 1 bien_3))))
   	                    ((="T" bien_4)(SETQ q&t% (CDR(ASSOC 1 bien_3))))
        	            ((="DAI" bien_4)(SETQ l%th (CDR(ASSOC 1 bien_3))))
    	                    ((="SL1" bien_4)(SETQ sl1% (CDR(ASSOC 1 bien_3))))
      	                    ((="SLA" bien_4)(SETQ slt% (CDR(ASSOC 1 bien_3))))                    
                          )
                        (SETQ Q%(+ Q% 1))
                       )
                      )
                    (SETQ bien_2 (ENTNEXT bien_2))
                  )
   	       	(SETQ slck% (RTOS(/(ATOF slt%)(ATOF sl1%))2 0))
	        (SET_TILE"SH"q&sh)(SET_TILE"L1"ql1%)
         	(SET_TILE"L2"ql2%)(SET_TILE"T"q&t%)
         	(SET_TILE"SLCK"slck%)(SET_TILE"SL1"sl1%)
               )  
               ((> Q&$L 1)(MODE_TILE"SH"1)(MODE_TILE"L1"1)(MODE_TILE"L2"1)(MODE_TILE"T"1))
               )
         )

	(defun mo_slth() ; lay so lieu tu blcok va dua vao bang sua chua thep hinh
	(mt_sltb) 
	    (COND
	       ((= Q&$L 1)
	         (SETQ Q% 1)(SETQ bien_2(ENTNEXT bien_1))
	         (WHILE(AND bien_2(<= Q% 7))
	            (SETQ bien_3(ENTGET bien_2))
	            (IF(="ATTRIB"(CDR(ASSOC 0 bien_3)))
	                (PROGN
                   (SETQ bien_4 (CDR(ASSOC 2 bien_3)))
                   (COND
                       ((="SH" bien_4)(SETQ q&sh (CDR(ASSOC 1 bien_3))))
                       ((="DAI" bien_4)(SETQ l%th (CDR(ASSOC 1 bien_3))))
                       ((="SL1" bien_4)(SETQ sl1% (CDR(ASSOC 1 bien_3))))
                       ((="SLA" bien_4)(SETQ slt% (CDR(ASSOC 1 bien_3))))                    
                       ((="TENTH" bien_4)(SETQ tt%h (CDR(ASSOC 1 bien_3))))                    
                   )
                 (SETQ Q%(+ Q% 1))
                )
              )
                (SETQ bien_2 (ENTNEXT bien_2))
              )
          	(SETQ slck% (RTOS(/(ATOF slt%)(ATOF sl1%))2 0))
	        (SET_TILE"SH"q&sh)(SET_TILE"L%TH"l%th)
         	(SET_TILE"SLCK"slck%)(SET_TILE"SL1"sl1%)
  	      (cond
	    	((= Q@#L "TH_1")(setq ds%% 5)(set_ds)(set_tile "THEP_2L" "1"))
	    	((= Q@#L "TH_2")(setq ds%% 1)(set_ds)(set_tile "THEP_L" "1"))
	    	((= Q@#L "TH_3")(setq ds%% 2)(set_ds)(set_tile "THEP_U" "1"))
	    	((= Q@#L "TH_4")(setq ds%% 3)(set_ds)(set_tile "THEP_I" "1"))
	    	((= Q@#L "TH_5")(setq ds%% 4)(set_ds)(set_tile "THEP_Lunsim" "1"))
	    	((= Q@#L "TH_6")(setq ds%% 6)(set_ds)(set_tile "THEP_O" "1"))
              )
	    (start_list "DANH_SACH")
	    (mapcar 'add_list ds_list)
	    (end_list)
	        )  
 	     ((> Q&$L 1)(MODE_TILE"SH"1))
 	    )
 	  )

;;;;phan chinh lay_so_lieu
    (cond 
     ((= Q@#L"TH_1")(mo_slth)(setq va%1 "1"))
     ((= Q@#L"TH_2")(mo_slth)(setq va%1 "1"))
     ((= Q@#L"TH_3")(mo_slth)(setq va%1 "1"))
     ((= Q@#L"TH_4")(mo_slth)(setq va%1 "1"))
     ((= Q@#L"TH_5")(mo_slth)(setq va%1 "1"))
     ((= Q@#L"TH_6")(mo_slth)(setq va%1 "1"))
     ((= Q@#L"TB_1")(mo_sltb)(setq va%2 "1"))
     )

  )
;;;
;;;================== modify dialog======================================
   (defun show_dialog_modify()
           (setq kt%% ( load_dialog "tkth.dcl"))
           (if (not (new_dialog "nhap_du_lieu" kt%%))(exit))
            (mode_tile"CONTINUE" 1)
	   (lay_so_lieu)
            (action_tile "SLTB" "(mt_slth)")
            (action_tile "SLTH" "(mt_sltb)")
            (action_tile "SCL" "(rs_error)(check_scl%)")
            (action_tile "SH" "(rs_error)(check_sh)")
	    (action_tile "L%TH" "(rs_error)(check_l&th)")
	    (action_tile "L1" "(rs_error)(check_L1)")
	    (action_tile "L2" "(rs_error)(check_L2)")
	    (action_tile "T" "(rs_error)(check_T)")
	    (action_tile "SL1" "(rs_error)(check_sl1)")
	    (action_tile "SLCK" "(rs_error)(check_slck)")
	    (action_tile "THEP_L" "(rs_error)(setq ds%% 1)(set_ds)")
	    (action_tile "THEP_2L" "(rs_error)(setq ds%% 5)(set_ds)")
	    (action_tile "THEP_U" "(rs_error)(setq ds%% 2)(set_ds)")
	    (action_tile "THEP_I" "(rs_error)(setq ds%% 3)(set_ds)")
	    (action_tile "THEP_Lunsim" "(rs_error)(setq ds%% 4)(set_ds)")
	    (action_tile "THEP_O" "(rs_error)(setq ds%% 6)(set_ds)")
	    (action_tile "accept" "(rs_error)(getdata)") 
	    (action_tile "cancel" "(done_dialog)")         
    (if (= (start_dialog) 1)(if(= "1" va%1)(modify_th)(modify_tb)))
    (unload_dialog kt%%)
  )
;;;
;;;================== nhap du lieu modify======================================
;;;
(defun nhap_du_lieu()
   (SETQ QL$L(SSGET(quote ((0 . "INSERT")(-4 . "<OR")(2 . "TB_1")(2 . "TH_1")
		(2 . "TH_2")(2 . "TH_3")(2 . "TH_4")(2 . "TH_5")(2 . "TH_6")(-4 . "OR>")))))
   (IF QL$L(SETQ Q&$L(SSLENGTH QL$L))(SETQ Q&$L 0))
       (SETQ Q@#L NIL)
       (SETQ bien_1(SSNAME QL$L 0))(SETQ QJLL(ENTGET bien_1))
       (setq diem_chen (CDR(ASSOC 10(ENTGET bien_1))))
       (SETQ Q$0L(ENTNEXT bien_1))
       (SETQ Q@#L(STRCASE(CDR(ASSOC 2(ENTGET bien_1)))))
       (setq scl% (CDR(ASSOC 41(ENTGET bien_1))))
  )
;;;
;;;
;;;
  (defun modify_th() 
   (cal_wd1)
   (command "erase" "P" "")
   (COMMAND"INSERT"Q#0L diem_chen scl% scl%"""""""""""""""")
   (SETQ Q00L(ENTLAST))(SETQ QJLL(ENTGET Q00L))(SETQ Q$0L(ENTNEXT Q00L))
   (doi_wd1)
   )
  (defun modify_tb() 
   (cal_wd2)
   (command "erase" "P" "")
   (if(= va%2 "1")(setq Q#0L "tb_1"))
   (COMMAND"INSERT"Q#0L diem_chen scl% scl%"""""""""""""""""""")
   (SETQ Q00L(ENTLAST))(SETQ QJLL(ENTGET Q00L))(SETQ Q$0L(ENTNEXT Q00L))
   (doi_wd2)
   )
;;;
;;;================== main program for modifying data ======================================
;;;
(defun c:tkthm(/ kt%% tlt% va%1 va%2 ds_list ds%% q&sh ql1% ql2% q&t% sl1% slck% slt% ten%th 
              tt%h q%tl q%td QL$L Q&$L l%th Q#0L Q$0L bien_1 diem_chen scl%)
(SETQ Q@@(GETVAR"cmdecho")QQJ *ERROR* *ERROR* AI_ERROR)
  (PROGN
  (setvar "cmdecho" 0)
  (nhap_du_lieu)
    (IF(/= Q&$L 0)
      (COND
       ((NOT(AI_NOTRANS)))((NOT(AI_ACADAPP)))((NOT(SETQ QLJ(AI_DCL"tkth"))))
       (T(AI_UNDO_PUSH)(show_dialog_modify)(AI_UNDO_POP))
      )
    )
  )
(SETQ *ERROR* QQJ)(SETVAR"cmdecho"Q@@)
(PRINC)
)
