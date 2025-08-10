;;;   B_SEC.LSP Version 1.0
;;;   Copyright (C) BY DO MANH HIEN 44X5.
;;; ===========================================================================
;;; ===================== load-time error checking ============================

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
        (ai_abort "b_sec"
                  (strcat "Can't locate file AI_UTILS.LSP."
                          "\n Check support directory.")))

     (  (eq "failed" (load "ai_utils" "failed"))            ; load it
        (ai_abort "b_sec" "Can't load file AI_UTILS.LSP"))
  )

  (if (not (ai_acadapp))               ; defined in AI_UTILS.LSP
      (ai_abort "b_sec" nil)         ; a Nil <msg> supresses
  )                                    ; ai_abort's alert box dialog.

;;; ==================== end load-time operations ===========================

;;; =========================================================================
(defun c:bb ( / app dcl_id old_cmd old_osmode old_blip
                   drawmode
                   reset check
	 	   b_sec_sd  b_sec_ld b_sec_lt b_sec_lg
                   b_sec_draw b_sec_set

                   b_sec_start  b_sec_end  b_sec_main
                   )
;;; ==================== hien ban quyen tac gia =============================
 (defun b_sec_about ()
 ;; --------------------------------------------
 (defun about_main ()
  (if (not (new_dialog "about" dcl_id))
    (exit)
  )
  (start_image "msg")
  (slide_image
    19 0
    (dimx_tile "msg") (dimy_tile "msg")
;    "b_about"
    "b_sec(b_about)"
  )
  (end_image)

  (action_tile "a_accept" "(done_dialog)")
  (start_dialog)
 )
 ;; --------------------------------------------
;    (setq old_error  *error*            ; save current error function
;          *error* ai_error)
;    (cond
;     (  (not (ai_notrans)))                     ; transparent not OK
;     (  (not (ai_acadapp)))                     ; ACADAPP.EXP xloaded?
;     (  (not (setq a_dcl_id (ai_dcl "about"))))      ; is .DCL file loaded?
;     (T (ai_undo_push)

        (about_main)
;        (ai_undo_pop)
;     )
;    )
;    (setq *error* old_error)
    (princ)
 ;; --------------------------------------------
)
;; --------- Ve tiet dien --------------------
(defun b_sec_draw (/ inspoint p0 p1 p2 p3 p4 p5 p6 p7 p8 p9 p10
                  )
 ; --------- Dat lop hien hanh ---------------
 (defun set_layer ( ten )
  (command "LAYER" "M" ten "")
 )
 ; --------- Ve net bao ----------------------
 (defun d_bao (/ dh db dhs
              )
  ; ++++++++ Net cat +++++++++++++++++++++++++
  (defun cat ( diem1 diem2 /
               ps1 ps2 sb_c sh_c se_c ang
               pf1 pf2 pf3 pf4 pf5 pf6
             )
   (setq ps1 diem1)
   (setq ps2 diem2)
   (setq sb_c 2.5 sh_c 3.5 se_c 1.0)
   (setq ang (angle ps1 ps2))
   (setq pf1 (polar ps1 ang (- 0.0 se_c)))
   (setq pf2 (polar ps1 ang (/ (- (distance ps1 ps2) sb_c) 2.0) ))
   (setq pf3 (polar pf2 (+ ang (/ pi 2)) (/ sh_c 2.0)))
   (setq pf5 (polar pf2 ang sb_c))
   (setq pf4 (polar pf5 (- ang (/ pi 2)) (/ sh_c 2.0)))
   (setq pf6 (polar ps2 ang se_c))
   (setvar "PLINEWID" 0)
   (command "PLINE" pf1 pf2 pf3 pf4 pf5 pf6 "")
  )
  ; ++++++++++++++++++++++++++++++++++++++++++
  (setq dh  (/ h tl ))
  (setq db  (/ b tl ))
  (setq dhs  (/ hs tl ))
  (setq p0 inspoint
	p1 (polar p0 pi (/ db 2))
	p2 (polar p0 0 (/ db 2))
	p3 (polar p1 (- (/ pi 2)) dh)
	p4 (polar p2 (- (/ pi 2)) dh)
	p5 (polar p1 (- (/ pi 2)) dhs)
	p6 (polar p2 (- (/ pi 2)) dhs)
	p7 (polar p1 pi canh)
	p8 (polar p2 0  canh)
	p9 (polar p5 pi canh)
	p10 (polar p6 0 canh)
  )
  (set_layer "BAO")
  (command "LINE" p7 p8 "")
  (command "LINE" p9 p5 p3 p4 p6 p10 "")
  (cat p7 p9)
  (cat p8 p10)
 )
 ; --------- Ve cot thep dai bao --------------
 (defun d_dbao (/ c1 c2 c3 c4 c5 c6 c7 c8
                 k_ho dau ket diemchen chu so v1 i
              )
  ; ++++++++ Ve dai ++++++++++++++++++++++++++
  (setvar "PLINEWID" net)
  (command "FILLET" "R" (* 0.5 donut) )

  (setq c1 (list (+ (car p1) bv) (- (cadr p1) bv))
        c2 (list (- (car p2) bv) (- (cadr p2) bv))
        c3 (list (+ (car p3) bv) (+ (cadr p3) bv))
        c4 (list (- (car p4) bv) (+ (cadr p4) bv))
  )
  (if (= sdai 2)
   (progn
    (setq k_ho (/ (- (distance c1 c2) (+ net donut)) 3) )
    (setq c5 (list (+ (car c1) k_ho) (cadr c1) ))
    (setq c6 (list (- (car c2) k_ho) (cadr c2) ))
    (setq c7 (list (+ (car c3) k_ho) (cadr c3) ))
    (setq c8 (list (- (car c4) k_ho) (cadr c4) ))
    (set_layer "THEP")
    (command "PLINE" c1 c6 c8 c3 "C")
    (command "FILLET" "P" "L" )
    (command "PLINE" c5 c2 c4 c7 "C")
    (command "FILLET" "P" "L" )
   )
   (progn
    (set_layer "THEP")
    (command "PLINE" c1 c2 c4 c3 "C")
    (command "FILLET" "P" "L" )
   )
  )
  ; ++++++++ Ghi chu +++++++++++++++++++++++++
  ; ........ Xu li chuoi .....................
  (if (and (/= nil dbao) (/= dbao ""))
   (progn
    (setq i 1)
    (setq chu "")
    (setq so "")
    (while  (and (<= i (strlen dbao))
                  (/= (substr dbao i 1) "*")
                  (/= (substr dbao i 1) "/")
                  (/= (substr dbao i 1) "-")
                  (/= (substr dbao i 1) "+")
             )
             (setq v1 (substr dbao i 1))
             (setq chu (strcat chu v1))
             (setq i (+ i 1))
    )
    (setq i (+ i 1))
    (while  (and (<= i (strlen dbao))
                  (/= (substr dbao i 1) "*")
                  (/= (substr dbao i 1) "/")
                  (/= (substr dbao i 1) "-")
                  (/= (substr dbao i 1) "+")
             )
             (setq v1 (substr dbao i 1))
             (setq so (strcat so v1))
             (setq i (+ i 1))
    )
   )
  )
  ; ........  Ghi chu ........................
  (if (or (and (/= nil chu) (/= chu ""))
          (and (/= nil so) (/= so ""))
      )
   (progn
    (setq dau (list (- (car p2) bv)
                    (- (cadr p2) (+ bv donut k_ho_lop chi)) )
    )
    (setq ket (polar dau 0 (- tua (* 2 giong)) ) )
    (set_layer "CHI")
    (command "LINE" dau ket "")
   )
  )
  (if (and (/= nil chu) (/= chu ""))
   (progn
    (setq diemchen (list (- (car ket) (* nang 2)) (+ (cadr ket) nang) ))
    (setq chu (strcat "%%C" chu))
    (set_layer "TEXT")
    (command "TEXT" "R" diemchen cao goc chu )
   )
  )
  (if (and (/= nil so) (/= so ""))
   (progn
    (set_layer "CHI")
    (command "CIRCLE" (polar ket 0 bkinh) bkinh)
    (set_layer "XANH")
    (command "TEXT"  "M" (polar ket 0 bkinh) cao goc so)
   )
  )
  ; ++++++++++++++++++++++++++++++++++++++++++
 )
 ; --------- Ve cot thep dai giua ------------
 (defun d_dgiua (/ vitri tam
                   c1 c2 c3 c4 c5 c6
                   dau ket diemchen chu so v1 i
              )
  ; ++++++++ Xu li chuoi +++++++++++++++++++++
  (if (and (/= nil dgiua) (/= dgiua ""))
   (progn
    (setq i 1)
    (setq chu "")
    (setq so "")
    (while  (and (<= i (strlen dgiua))
                  (/= (substr dgiua i 1) "*")
                  (/= (substr dgiua i 1) "/")
                  (/= (substr dgiua i 1) "-")
                  (/= (substr dgiua i 1) "+")
             )
             (setq v1 (substr dgiua i 1))
             (setq chu (strcat chu v1))
             (setq i (+ i 1))
    )
    (setq i (+ i 1))
    (while  (and (<= i (strlen dgiua))
                  (/= (substr dgiua i 1) "*")
                  (/= (substr dgiua i 1) "/")
                  (/= (substr dgiua i 1) "-")
                  (/= (substr dgiua i 1) "+")
             )
             (setq v1 (substr dgiua i 1))
             (setq so (strcat so v1))
             (setq i (+ i 1))
    )
   )
  )
  ; ++++++++ Ve thep dai +++++++++++++++++++++
  (setq vitri (/ (- (distance p1 p3) (+ bv bv net donut)) (+ lgiua 1)) )
  (setq k_ho     (- (distance p1 p2) (+ bv bv net donut))              )
  (setq i 1)
  (repeat lgiua
   ;----------------
   (setq tam (list (+ (car  p3) bv (/ net 2) (/ donut 2)            )
                   (+ (cadr p3) bv (/ net 2) (/ donut 2) (* vitri i))
             )
    )
   (setq c2 (list (- (car tam)  (+ (/ net 2) (/ donut 2)) )
                  (- (cadr tam) (+ (/ net 2) (/ donut 2)) )
            )
   )
   ; Ve thep
   (setq c3 (polar c2 (/ pi 2) (+ net donut)))
   (setq c1 (polar c2 0 (* 2 donut)))
   (setq c5 (polar c2 0 (- (/ b tl) (* 2 bv)) ))
   (setq c4 (polar c5 (/ pi 2) (+ net donut)))
   (setq c6 (polar c5 pi (* 2 donut)))
   (setvar "PLINEWID" net)
   (command "FILLET" "R" (* 0.5 donut))
   (set_layer "THEP")
   (command "PLINE" c1 c2 c3 c4 c5 c6 "")
   (command "FILLET" "P" "L" )

   ; Ghi chu
   (if (or (and (/= nil chu) (/= chu ""))
           (and (/= nil so) (/= so ""))
       )
    (progn
     (setq dau (polar c3 0 (/ (- (distance p1 p2) (+ bv bv)) 2) ))
     (setq ket (polar dau (- (/ pi 2)) (* (/ chi 4) 3)) )
     (set_layer "CHI")
     (command "LINE" dau ket "")
     (setq dau ket)
     (setq ket (polar ket 0
                (+ (/ (- (distance p1 p2) (+ bv bv)) 2)
                   (- tua (* 2 giong))
                )
               )
     )
     (command "LINE" dau ket "")
    )
   )
   (if (and (/= nil chu) (/= chu ""))
    (progn
     (setq diemchen (list (- (car ket) (* nang 2)) (+ (cadr ket) nang) ))
     (setq chu (strcat "%%C" chu))
     (set_layer "TEXT")
     (command "TEXT" "R" diemchen cao goc chu )
    )
   )
   (if (and (/= nil so) (/= so ""))
    (progn
     (set_layer "CHI")
     (command "CIRCLE" (polar ket 0 bkinh) bkinh)
     (set_layer "XANH")
     (command "TEXT"  "M" (polar ket 0 bkinh) cao goc so)
    )
   )
   ;----------------
   (setq i (+ 1 i))
  )
  ; ++++++++++++++++++++++++++++++++++++++++++
 )
 ; --------- Ve cot thep doc -----------------
 (defun d_doc ( loai / sthanh sthanh1 sthanh2 lthanh1 lthanh2 kh1 kh2
                 k_ho
               )
  ; +++++++++++ Ve thep ++++++++++++++++++++++
  (defun ve ( lop / tam)
   (setq k_ho (/ (- (distance p1 p2) (+ bv bv net donut)) (- sthanh 1)) )
   (if (= loai "duoi")
    (setq tam (list (+ (car  p3) bv (/ net 2) (/ donut 2))
                    (+ (cadr p3) bv (/ net 2) (/ donut 2))))
    (setq tam (list (+ (car  p1) bv (/ net 2) (/ donut 2))
                    (- (cadr p1) bv (/ net 2) (/ donut 2))))
   )
   (if (= lop 2)
    (if (= loai "duoi")
     (setq tam (polar tam    (/ pi 2)  k_ho_lop))
     (setq tam (polar tam (- (/ pi 2)) k_ho_lop))
    )
   )
   (if (= loai "duoi")
    (if (= lop 2) (setq quay (/ pi 2)) (setq quay (- (/ pi 2)))  )
    (if (= lop 2)  (setq quay (- (/ pi 2))) (setq quay (/ pi 2)) )
   )

   (repeat sthanh
    (set_layer "THEP")
    (command "DONUT" 0 donut tam "")
    (setq tam (polar tam 0 k_ho))
   )
  )
  ; +++++++++++ Ghi chu ++++++++++++++++++++++
  (defun ghichu ( lop tp / soluong k_cach tam quay keo huong ket
                  diemchen chu so
                  )
   (setq k_ho (/ (- (distance p1 p2) (+ bv bv net donut)) (- sthanh 1)) )
   (if (= loai "duoi")
    (setq tam (list (+ (car  p3) bv (/ net 2) (/ donut 2))
                    (+ (cadr p3) bv (/ net 2) (/ donut 2))))
    (setq tam (list (+ (car  p1) bv (/ net 2) (/ donut 2))
                    (- (cadr p1) bv (/ net 2) (/ donut 2))))
   )
   (if (= lop 2)
    (if (= loai "duoi")
     (setq tam (polar tam    (/ pi 2)  k_ho_lop))
     (setq tam (polar tam (- (/ pi 2)) k_ho_lop))
    )
   )
   (if (= loai "duoi")
    (if (= lop 2) (setq quay (/ pi 2)) (setq quay (- (/ pi 2)))  )
    (if (= lop 2)  (setq quay (- (/ pi 2))) (setq quay (/ pi 2)) )
   )

   (setq keo chi)
   (if (and (= loai "duoi") (= lop 2)) (setq keo (/ keo 2)))

   (if (= tp 2)
    (progn
     (setq soluong sthanh2)
     (setq k_cach k_ho)
     (setq tam (polar tam 0  (* sthanh2 k_ho)))
     (setq keo (+ keo (/ keo 3)))
     (setq huong pi)
    )
    (progn
     (setq soluong sthanh1)
     (setq k_cach (/ (- (distance p1 p2) (+ bv bv net donut)) (- sthanh1 1)) )
     (setq huong 0)
    )
   )
   ; Ve duong giong
   (set_layer "CHI")
   (repeat soluong
    (command "LINE" tam (polar tam quay keo) "")
    (setq ket (polar tam quay keo))
    (setq tam (polar tam huong k_cach))
   )
   (if ket
    (progn
     (command "LINE" ket
                    (polar ket (- huong pi) (- tua (* (- tp 1) giong )) )
     "")
     (setq ket (polar ket (- huong pi) (- tua (* (- tp 1) giong))))
    )
   )

   ; Ghi chu
   (if ket
    (if (= tp 1)
      (progn
       (if (/= lthanh1 "")
        (progn
         (setq diemchen (list (+ (car ket) (* nang 2)) (+ (cadr ket) nang) ))
         (setq chu (strcat (itoa sthanh1) " /G" lthanh1))
         (set_layer "TEXT")
         (command "TEXT" diemchen cao goc chu )
        )
       )
       (if (/= kh1 "")
        (progn
         (setq so kh1)
         (set_layer "CHI")
         (command "CIRCLE" (polar ket pi bkinh) bkinh)
         (set_layer "XANH")
         (command "TEXT"  "M" (polar ket pi bkinh) cao goc kh1)
        )
       )
      )
      (progn
       (if (/= lthanh2 "")
        (progn
         (setq diemchen (list (- (car ket) (* nang 2)) (+ (cadr ket) nang) ))
         (setq chu (strcat (itoa sthanh2) " /G" lthanh2))
         (set_layer "TEXT")
         (command "TEXT" "R" diemchen cao goc chu )
        )
       )
       (if (/= kh2 "")
        (progn
         (setq so kh2)
         (set_layer "CHI")
         (command "CIRCLE" (polar ket 0 bkinh) bkinh)
         (set_layer "XANH")
         (command "TEXT"  "M" (polar ket 0 bkinh) cao goc kh2)
        )
       )
      )
    )
   )
  )
  ; +++++++++++ Xu li chuoi ++++++++++++++++++
  (defun xlc (lop / chuoi i v1 v2)
   (if (= loai "duoi")
    (if (= lop 1) (setq chuoi shduoi1) (setq chuoi shduoi2))
    (if (= lop 1) (setq chuoi shtren1) (setq chuoi shtren2))
   )
   (setq sthanh 0)
   (setq sthanh1 0)
   (setq sthanh2 0)
   (setq lthanh1 "")
   (setq lthanh2 "")
   (setq kh1 "")
   (setq kh2 "")

   (setq i 1)
   (setq v1 "")
   (while  (and (<= i (strlen chuoi))
                (/= (substr chuoi i 1) "*")
;                (/= (substr chuoi i 1) " ")
                (/= (substr chuoi i 1) "/")
                (/= (substr chuoi i 1) "-")
                (/= (substr chuoi i 1) "+")
           )
           (setq v2 (substr chuoi i 1))
           (setq v1 (strcat v1 v2))
           (setq i (+ i 1))
   )
   (if (setq sthanh1 (atoi v1)) (setq sthanh (+ sthanh sthanh1) ))
   (if (/= (substr chuoi i 1) "+")
    (progn
     (setq i (+ i 1))
     (setq v1 "")
     (while  (and (<= i (strlen chuoi))
                  (/= (substr chuoi i 1) "*")
;                  (/= (substr chuoi i 1) " ")
                  (/= (substr chuoi i 1) "/")
                  (/= (substr chuoi i 1) "-")
                  (/= (substr chuoi i 1) "+")
             )
             (setq v2 (substr chuoi i 1))
             (setq v1 (strcat v1 v2))
             (setq lthanh1 v1)
             (setq i (+ i 1))
     )
    )
   )
   (if (/= (substr chuoi i 1) "+")
    (progn
     (setq i (+ i 1))
     (setq v1 "")
     (while  (and (<= i (strlen chuoi))
                  (/= (substr chuoi i 1) "*")
;                  (/= (substr chuoi i 1) " ")
                  (/= (substr chuoi i 1) "/")
                  (/= (substr chuoi i 1) "-")
                  (/= (substr chuoi i 1) "+")
             )
             (setq v2 (substr chuoi i 1))
             (setq v1 (strcat v1 v2))
             (setq kh1 v1)
             (setq i (+ i 1))
     )
    )
   )
   (if (/= (substr chuoi i 1) "+")
    (progn
     (setq i (+ i 1))
     (setq v1 "")
     (while  (and (<= i (strlen chuoi))
                  (/= (substr chuoi i 1) "+")
             )
             (setq i (+ i 1))
     )
    )
   )
   (if (= (substr chuoi i 1) "+")
    (progn
     (setq i (+ i 1))
     (setq v1 "")
     (while  (and (<= i (strlen chuoi))
                  (/= (substr chuoi i 1) "*")
;                  (/= (substr chuoi i 1) " ")
                  (/= (substr chuoi i 1) "/")
                  (/= (substr chuoi i 1) "-")
                  (/= (substr chuoi i 1) "+")
             )
             (setq v2 (substr chuoi i 1))
             (setq v1 (strcat v1 v2))
             (setq i (+ i 1))
     )
     (if (setq sthanh2 (atoi v1)) (setq sthanh (+ sthanh sthanh2) ))
    )
   )
   (if (/= (substr chuoi i 1) "+")
    (progn
     (setq i (+ i 1))
     (setq v1 "")
     (while  (and (<= i (strlen chuoi))
                  (/= (substr chuoi i 1) "*")
;                  (/= (substr chuoi i 1) " ")
                  (/= (substr chuoi i 1) "/")
                  (/= (substr chuoi i 1) "-")
                  (/= (substr chuoi i 1) "+")
             )
             (setq v2 (substr chuoi i 1))
             (setq v1 (strcat v1 v2))
             (setq lthanh2 v1)
             (setq i (+ i 1))
     )
    )
   )
   (if (/= (substr chuoi i 1) "+")
    (progn
     (setq i (+ i 1))
     (setq v1 "")
     (while  (and (<= i (strlen chuoi))
                  (/= (substr chuoi i 1) "*")
;                  (/= (substr chuoi i 1) " ")
                  (/= (substr chuoi i 1) "/")
                  (/= (substr chuoi i 1) "-")
                  (/= (substr chuoi i 1) "+")
             )
             (setq v2 (substr chuoi i 1))
             (setq v1 (strcat v1 v2))
             (setq kh2 v1)
             (setq i (+ i 1))
     )
    )
   )
  )
  ; ++++++++++++++++++++++++++++++++++++++++++
  (if (= loai "duoi")
   (progn
    (if (/= nil shduoi1)
     (progn
      (xlc 1)
      (if (and (/= nil sthanh) (> sthanh 1)) (ve 1))
      (if (and (/= nil sthanh) (<= sthanh 4))
       (progn
        (if (and (> sthanh1 1) (or (/= lthanh1 "") (/= kh1 "")) )
            (ghichu 1 1)
        )
        (if (and (> sthanh2 0) (or (/= lthanh2 "") (/= kh2 "")) )
            (ghichu 1 2)
        )
       )
      )
     )
    )
    (if (/= nil shduoi2)
     (progn
      (xlc 2)
      (if (and (/= nil sthanh) (> sthanh 1)) (ve 2))
      (if (and (/= nil sthanh) (<= sthanh 4))
       (progn
        (if (and (> sthanh1 1) (or (/= lthanh1 "") (/= kh1 "")) )
            (ghichu 2 1)
        )
        (if (and (> sthanh2 0) (or (/= lthanh2 "") (/= kh2 "")) )
            (ghichu 2 2)
        )
       )
      )
     )
    )
   )
   ; Thep lop tren
   (progn
    (if (/= nil shtren1)
     (progn
      (xlc 1)
      (if (and (/= nil sthanh) (> sthanh 1)) (ve 1))
      (if (and (/= nil sthanh) (<= sthanh 4))
       (progn
        (if (and (> sthanh1 1) (or (/= lthanh1 "") (/= kh1 "")) )
            (ghichu 1 1)
        )
        (if (and (> sthanh2 0) (or (/= lthanh2 "") (/= kh2 "")) )
            (ghichu 1 2)
        )
       )
      )
     )
    )
    (if (/= nil shtren2)
     (progn
      (xlc 2)
      (if (and (/= nil sthanh) (> sthanh 1)) (ve 2))
      (if (and (/= nil sthanh) (<= sthanh 4))
       (progn
        (if (and (> sthanh1 1) (or (/= lthanh1 "") (/= kh1 "")) )
            (ghichu 2 1)
        )
        (if (and (> sthanh2 0) (or (/= lthanh2 "") (/= kh2 "")) )
            (ghichu 2 2)
        )
       )
      )
     )
    )
   )
  )
  ; ++++++++++++++++++++++++++++++++++++++++++
 )
 ; --------- Ve cot thep gia -----------------
 (defun d_giua ( / sthanh lthanh kh
                 vitri k_ho
               )
  ; +++++++++++ Ve thep ++++++++++++++++++++++
  (defun ve ( / i tam quay keo huong ket
                diemchen chu so
            )
   (setq vitri (/ (- (distance p1 p3) (+ bv bv net donut)) (+ lgiua 1)) )
   (setq k_ho (/ (- (distance p1 p2) (+ bv bv net donut)) (- sthanh 1)) )
   (setq i 1)
   (repeat lgiua
    ;----------------
    (setq tam (list (+ (car  p3) bv (/ net 2) (/ donut 2))
                    (+ (cadr p3) bv (/ net 2) (/ donut 2) (* vitri i))
              ))
    (setq quay (- (/ pi 2)))
    (setq keo (/ chi 2))
    ;----------------
    (repeat sthanh
     (set_layer "THEP")
     (command "DONUT" 0 donut tam "")
     (if (or (/= lthanh "") (/= kh ""))
      (progn
       (setq ket (polar tam quay keo))
       (set_layer "CHI")
       (command "LINE" tam ket "")
      )
     )
     (setq tam (polar tam 0 k_ho))
    )
    ;----------------
    (if ket
     (progn
      (set_layer "CHI")
      (command "LINE" ket (polar ket pi tua) "")
      (setq ket (polar ket pi tua))

      ; Ghi chu
      (if (/= lthanh "")
       (progn
        (setq diemchen (list (+ (car ket) (* nang 2)) (+ (cadr ket) nang) ))
        (setq chu (strcat (itoa sthanh) " /G" lthanh))
        (set_layer "TEXT")
        (command "TEXT" diemchen cao goc chu )
       )
      )
      ; Danh so
      (if (/= kh "")
       (progn
        (setq so kh)
        (set_layer "CHI")
        (command "CIRCLE" (polar ket pi bkinh) bkinh)
        (set_layer "XANH")
        (command "TEXT"  "M" (polar ket pi bkinh) cao goc kh)
       )
      )
     )
    )
    ;----------------
    (setq i (+ i 1))
   )
  )
  ; +++++++++++ Xu li chuoi ++++++++++++++++++
  (defun xlc (/ chuoi i v1 v2)
   (setq chuoi shgiua)

   (setq sthanh 0)
   (setq lthanh "")
   (setq kh "")

   (setq i 1)
   (setq v1 "")
   (while  (and (<= i (strlen chuoi))
                (/= (substr chuoi i 1) "*")
;                (/= (substr chuoi i 1) " ")
                (/= (substr chuoi i 1) "/")
                (/= (substr chuoi i 1) "-")
                (/= (substr chuoi i 1) "+")
           )
           (setq v2 (substr chuoi i 1))
           (setq v1 (strcat v1 v2))
           (setq i (+ i 1))
   )
   (if (setq v1 (atoi v1)) (setq sthanh v1 ))
   (if (<= i (strlen chuoi))
    (progn
     (setq i (+ i 1))
     (setq v1 "")
     (while  (and (<= i (strlen chuoi))
                  (/= (substr chuoi i 1) "*")
;                  (/= (substr chuoi i 1) " ")
                  (/= (substr chuoi i 1) "/")
                  (/= (substr chuoi i 1) "-")
                  (/= (substr chuoi i 1) "+")
             )
             (setq v2 (substr chuoi i 1))
             (setq v1 (strcat v1 v2))
             (setq lthanh v1)
             (setq i (+ i 1))
     )
    )
   )
   (if (<= i (strlen chuoi))
    (progn
     (setq i (+ i 1))
     (setq v1 "")
     (while  (and (<= i (strlen chuoi))
                  (/= (substr chuoi i 1) "*")
;                  (/= (substr chuoi i 1) " ")
                  (/= (substr chuoi i 1) "/")
                  (/= (substr chuoi i 1) "-")
                  (/= (substr chuoi i 1) "+")
             )
             (setq v2 (substr chuoi i 1))
             (setq v1 (strcat v1 v2))
             (setq kh v1)
             (setq i (+ i 1))
     )
    )
   )
  )
  ; ++++++++++++++++++++++++++++++++++++++++++
  (if (/= nil shgiua)
   (progn
    (xlc)
    (if (and (/= sthanh 0) (> sthanh 1)) (ve))
   )
  )
  ; ++++++++++++++++++++++++++++++++++++++++++
 )
 ; --------- Ghi kich thuoc ------------------
 (defun d_kt   ( / dd dc vt chu
               )
  (set_layer "DIM")
  ; ++++++++ Kich thuoc ngang ++++++++++++++++
  (setq dd (polar p3 (- (/ pi 2)) (+ lui giong)) )
  (setq dc (polar p4 (- (/ pi 2)) (+ lui giong)) )
  (setq vt (polar dd (- (/ pi 2)) giong) )
  (setq chu (rtos b 2 0))
  (COMMAND "DIM1" "HOR" dd dc vt chu )

  (setq dd (polar p3 (- (/ pi 2)) lui) )
  (setq dc (polar dd 0 bv) )
  (setq vt (polar dd (- (/ pi 2)) giong) )
  (setq chu (rtos a 2 0))
  (COMMAND "DIM1" "HOR" dc dd vt chu )

  (setq dc (polar p4 (- (/ pi 2)) lui) )
  (setq dd (polar dc pi bv) )
  (setq vt (polar dd (- (/ pi 2)) giong) )
  (setq chu (rtos a 2 0))
  (COMMAND "DIM1" "HOR" dd dc vt chu )

  ; ++++++++ Kich thuoc dung +++++++++++++++++
  (setq dd (polar p5 pi (+ canh (/ lui 2))) )
  (setq dc (polar p1 pi (+ canh (/ lui 2))) )
  (setq vt (polar dd pi giong) )
  (setq chu (rtos hs 2 0))
  (COMMAND "DIM1" "VER" dd dc vt chu )


  (setq dd (polar p2 0 (+ canh lui giong)) )
  (setq dc (polar p4 0 (+ canh lui giong)) )
  (setq vt (polar dd 0 giong) )
  (setq chu (rtos h 2 0))
  (COMMAND "DIM1" "VER" dd dc vt chu )

  (setq dd (polar p4 0 (+ canh lui )) )
  (setq dc (polar dd (/ pi 2) bv) )
  (setq vt (polar dd 0 giong) )
  (setq chu (rtos a 2 0))
  (COMMAND "DIM1" "VER" dc dd vt chu )

  (setq dc (polar p2 0 (+ canh lui )) )
  (setq dd (polar dc (- (/ pi 2)) bv) )
  (setq vt (polar dd 0 giong) )
  (setq chu (rtos a1 2 0))
  (COMMAND "DIM1" "VER" dd dc vt chu )

 )
 ; --------- Ghi so hieu ---------------------
 (defun d_sh   (/ diemchen)
  (if (/= sh "")
   (progn
    (setvar "TEXTSTYLE" "ROMAND")
    (setq diemchen (polar inspoint
                          (-(/ pi 2))
                          (+ (/ h tl) lui (* 2 giong) (* 2 cao))
                   ))
    (set_layer "XANH")
    (command "TEXT" "M" diemchen (* 2 cao) goc sh)
    (set_layer "THEP")
    (command "PLINE" (list (- (car  diemchen) (/ (* (strlen sh) (* 1.6 cao)) 2))
                           (- (cadr diemchen) (+ (/ cao 2) (* 2 nang)) ))
                     (list (+ (car  diemchen) (/ (* (strlen sh) (* 1.6 cao)) 2))
                           (- (cadr diemchen) (+ (/ cao 2) (* 2 nang)) ))
    "")
   )
  )
 )
 ; -------------------------------------------
 (command "_.UNDO" "_GROUP")
 (setq inspoint (getpoint "\nDiem chen : "))
 (if inspoint
  (progn
   (setvar "TEXTSTYLE" "ROMANS")
   (d_bao)
   (d_doc "duoi")
   (d_doc "tren")
   (d_dbao)
   (d_giua)
   (if (and (/= nil shgiua) (/= shgiua "")) (d_dgiua))
   (d_kt)
   (d_sh)
   (setvar "TEXTSTYLE" "ROMANS")
  )
 )
 (command "_.UNDO" "_END")
)
;; --------- Dat cac thong so ve -------------
(defun b_sec_set ( / old_canh     old_bv        old_lui       old_giong
                     old_net      old_donut     old_k_ho_lop
                     old_chi      old_tua       old_cao       old_goc
                     old_nang     old_bkinh
                 )
 ; -----------------
 (defun set_check ()
  (if (or (<= canh 0)
          (<= bv 0)
          (<  lui 0)
          (<= giong 0)
          (<  net 0)
          (<= donut 0)
          (<= k_ho_lop donut)
          (<= chi 0)
          (<= tua canh)
          (<= cao 0)
         ; (< goc 0)
          (<= nang 0)
          (<  bkinh cao)
      )
      (set_tile "error" "So lieu sai. Vao lai")
      (set_tile "error" "")
  )
 )
 ; -----------------------
 (defun save_old_setting ()
  (setq old_canh     canh   )
  (setq old_bv       bv     )
  (setq old_lui      lui    )
  (setq old_giong    giong  )
  (setq old_net      net    )
  (setq old_donut    donut  )
  (setq old_k_ho_lop k_ho_lop)
  (setq old_chi      chi    )
  (setq old_tua      tua    )
  (setq old_cao      cao    )
;  (setq old_goc      goc    )
  (setq old_nang     nang   )
  (setq old_bkinh    bkinh  )
 )
 ; -----------------------
 (defun set_old_setting ()
  (setq canh     old_canh   )
  (setq bv       old_bv     )
  (setq lui      old_lui    )
  (setq giong    old_giong  )
  (setq net      old_net    )
  (setq donut    old_donut  )
  (setq k_ho_lop old_k_ho_lop)
  (setq chi      old_chi    )
  (setq tua      old_tua    )
  (setq cao      old_cao    )
;  (setq goc      old_goc    )
  (setq nang     old_nang   )
  (setq bkinh    old_bkinh  )
 )
 ; -----------------------
 (defun b_sec_set_start ()
  (reset)
  (set_tile "canh"     (rtos canh  2 0))
  (set_tile "bv"       (rtos bv    2 2))
  (set_tile "lui"      (rtos lui   2 0))
  (set_tile "giong"    (rtos giong 2 0))
  (set_tile "net"      (rtos net   2 2))
  (set_tile "donut"    (rtos donut 2 2))
  (set_tile "k_ho_lop" (rtos k_ho_lop 2 2))
  (set_tile "chi"      (rtos chi   2 0))
  (set_tile "tua"      (rtos tua   2 0))
  (set_tile "cao"      (rtos cao   2 1))
;  (set_tile "goc"      (rtos goc   2 0))
  (set_tile "nang"     (rtos nang  2 1))
  (set_tile "bkinh"    (rtos bkinh 2 1))
 )
 ; -----------------------
 (defun set_image (/ i)
  (setq i 0)
  (repeat 3
   (setq i (+ 1 i))
   (start_image (strcat "hv_" (itoa i)))
   (slide_image
     0 0
     (dimx_tile (strcat "hv_" (itoa i))) (dimy_tile (strcat "hv_" (itoa i)))
     (strcat "b_sec(b_sec_" (itoa i) ")")
;      (strcat "b_sec_" (itoa i))
   )
   (end_image)
  )
 )
 ; -----------------------
 (defun b_sec_set_end ()
  (reset)
  (setq canh  (ai_num (get_tile "canh") "So lieu sai. Vao lai" 0))
  (setq bv  (ai_num (get_tile "bv") "So lieu sai. Vao lai" 0))
  (setq lui  (ai_num (get_tile "lui") "So lieu sai. Vao lai" 0))
  (setq giong  (ai_num (get_tile "giong") "So lieu sai. Vao lai" 0))
  (setq net  (ai_num (get_tile "net") "So lieu sai. Vao lai" 0))
  (setq donut  (ai_num (get_tile "donut") "So lieu sai. Vao lai" 0))
  (setq k_ho_lop  (ai_num (get_tile "k_ho_lop") "So lieu sai. Vao lai" 0))
  (setq chi  (ai_num (get_tile "chi") "So lieu sai. Vao lai" 0))
  (setq tua  (ai_num (get_tile "tua") "So lieu sai. Vao lai" 0))
  (setq cao  (ai_num (get_tile "cao") "So lieu sai. Vao lai" 0))
;  (setq goc  (ai_num (get_tile "goc") "So lieu sai. Vao lai" 0))
  (setq nang  (ai_num (get_tile "nang") "So lieu sai. Vao lai" 0))
  (setq bkinh  (ai_num (get_tile "bkinh") "So lieu sai. Vao lai" 0))

  (if (= (get_tile "error") "")
   (progn
    (set_check)
    (if (= (get_tile "error") "") (done_dialog))
   ))
 )
 ; -----------------------
 (defun b_sec_set_main ()
  (if (not (new_dialog "option" dcl_id))
    (exit)
  )
  (save_old_setting)
  (b_sec_set_start)
  (set_image)
  (action_tile "nhan" "(b_sec_set_end)")
  (action_tile "mdinh" "(set_default)(b_sec_set_start)")
  (action_tile "thoat" "(set_old_setting)(done_dialog)")
  (start_dialog)
 )
 ; -----------------------
 ; ----- Set function ----
 (b_sec_set_main)
 ; ----- End function ----
)
;; --------- Dat che do ban dau --------------
(defun reset ()
 (setq drawmode 0)
 (set_tile "error" "")
)

(defun set_default ()
 (setq canh 7)
 (setq bv 1.5)
 (setq lui 10)
 (setq giong 6)

 (setq net 0.35)
 (setq donut 1.2)

 (setq k_ho_lop 3)
 (setq chi 8)
 (setq tua 29)

 (setq cao 2.5)
 (setq goc 0)
 (setq nang 1)
 (setq bkinh 3)
)
;; --------- Kiem tra so lieu ----------------
(defun check ()
 (if (or (<= tl 0)
         (<= h 0)
         (<= b 0)
         (<= a 0)
         (<= a1 0)
         (<= hs 0)
     )
     (set_tile "error" "So lieu sai. Vao lai")
     (set_tile "error" "")
 )
)
;; --------- Doi so luong cot dai ------------
(defun b_sec_sd ()
  (if (= sdai 1) (setq sdai 2) (setq sdai 1))
  (set_tile "sdai" (itoa sdai))
)
;; --------- Chuyen thep lop duoi ------------
(defun b_sec_ld ()
  (if (= lduoi 1)
   (progn
    (setq lduoi 2)
    (setq shduoi1 (get_tile "shduoi"))
    (set_tile "shduoi" shduoi2)
   )
   (progn
    (setq lduoi 1)
    (setq shduoi2 (get_tile "shduoi"))
    (set_tile "shduoi" shduoi1)
   )
  )
  (set_tile "lduoi" (itoa lduoi))
)
;; --------- Chuyen thep lop tren ------------
(defun b_sec_lt ()
  (if (= ltren 1)
   (progn
    (setq ltren 2)
    (setq shtren1 (get_tile "shtren"))
    (set_tile "shtren" shtren2 )
   )
   (progn
    (setq ltren 1)
    (setq shtren2 (get_tile "shtren"))
    (set_tile "shtren" shtren1 )
   )
  )
  (set_tile "ltren" (itoa ltren))
)
;; --------- Doi so lop thep giua ------------
(defun b_sec_lg ()
  (if (= lgiua 1) (setq lgiua 2) (setq lgiua 1))
  (set_tile "lgiua" (itoa lgiua))
)
;; --------- Khoi tao hop hoi thoai ----------
(defun b_sec_start ()
  ; ------------------------
  (start_image "hv")
  (slide_image
    0 0
    (dimx_tile "hv") (dimy_tile "hv")
    "b_sec(b_sec_0)"
;    "b_sec_0"
  )
  (end_image)
  ; ------------------------
  (if (null sh) (setq sh "1-1"))
  (if (null tl) (setq tl 15))
  (if (null h)  (setq h 500))
  (if (null b)  (setq b 220))
  (if (null a)  (setq a 25))
  (if (null a1) (setq a1 25))
  (if (null hs) (setq hs 100))

  (set_tile "sh" sh)
  (set_tile "tl" (rtos tl 2 0))
  (set_tile "h" (rtos h 2 2))
  (set_tile "b" (rtos b 2 2))
  (set_tile "a" (rtos a 2 2))
  (set_tile "a1" (rtos a1 2 2))
  (set_tile "hs" (rtos hs 2 2))
  ; ------------------------
  (setq lduoi 1)
  (set_tile "lduoi" (itoa lduoi))
  (if (null shduoi1) (setq shduoi1 "3*22*1"))
  (if (null shduoi2) (setq shduoi2 ""))
  (if shduoi1 (set_tile "shduoi" shduoi1))
  (setq ltren 1)
  (set_tile "ltren" (itoa ltren))
  (if (null shtren1) (setq shtren1 "3*22*2"))
  (if (null shtren2) (setq shtren2 ""))
  (if shtren1 (set_tile "shtren" shtren1))
  (if (null lgiua) (setq lgiua 1))
  (set_tile "lgiua" (itoa lgiua))
  (if shgiua (set_tile "shgiua" shgiua))
  ; ------------------------
  (if (null sdai) (setq sdai 1))
  (set_tile "sdai" (itoa sdai))
  (if (null dbao) (setq dbao "6 a150*3"))
  (if dbao (set_tile "dbao" dbao))
  (if dgiua (set_tile "dgiua" dgiua))
  ; ------------------------
  (reset)
  (if (null b_sec_setmode)
   (progn
    (set_default)
    (setq b_sec_setmode 1)
   ))
)
;; ---------- Dong hop hoi thoai - Ve --------
(defun b_sec_end ()

  (reset)

  (initget 4)
  (setq sh  (get_tile "sh"))
  (setq tl  (ai_num (get_tile "tl") "So lieu sai. Vao lai" 0))

  (setq h  (ai_num (get_tile "h") "So lieu sai. Vao lai" 0))
  (setq b  (ai_num (get_tile "b") "So lieu sai. Vao lai" 0))
  (setq a  (ai_num (get_tile "a") "So lieu sai. Vao lai" 0))
  (setq a1  (ai_num (get_tile "a1") "So lieu sai. Vao lai" 0))
  (setq hs  (ai_num (get_tile "hs") "So lieu sai. Vao lai" 0))

  (if (= lduoi 1)
      (setq shduoi1 (get_tile "shduoi"))
      (setq shduoi2 (get_tile "shduoi"))
  )
  (if (= ltren 1)
      (setq shtren1 (get_tile "shtren"))
      (setq shtren2 (get_tile "shtren"))
  )
  (setq shgiua (get_tile "shgiua"))

  (setq dbao  (get_tile "dbao"))
  (setq dgiua  (get_tile "dgiua"))

  (if (= (get_tile "error") "")
   (progn
    (check)
    (if (= (get_tile "error") "")
     (progn
      (setq drawmode 1)
      (done_dialog)
     ))
   ))
)
;; ---------- Ham thi hanh tren HHT ----------
(defun b_sec_main ()
  (if (not (new_dialog "b_sec" dcl_id))
    (exit)
  )
  (b_sec_start)
  (action_tile "duoi" "(b_sec_ld)")
  (action_tile "tren" "(b_sec_lt)")
  (action_tile "giua" "(b_sec_lg)")
  (action_tile "sodai" "(b_sec_sd)")
  (action_tile "accept" "(b_sec_end)")
  (action_tile "option" "(b_sec_set)")
  (action_tile "cancel" "(done_dialog)")
  (action_tile "b_about" "(b_sec_about)")
  (start_dialog)
)

;;; ======================== Main function ====================================
  ;; Set up error function.
  (setq old_cmd (getvar "CMDECHO")    ; save current setting of cmdecho
        old_error  *error*            ; save current error function
        *error* ai_error              ; new error function
        old_osmode (getvar "OSMODE")  ; save current setting of osmode
        old_blip (getvar "BLIPMODE")  ; save current setting of blipmode
  )

  (setvar "CMDECHO" 0)
  (setvar "OSMODE" 0)
  (setvar "BLIPMODE" 0)

  (cond
     (  (not (ai_notrans)))                     ; transparent not OK
     (  (not (ai_acadapp)))                     ; ACADAPP.EXP xloaded?
     (  (not (setq dcl_id (ai_dcl "b_sec"))))   ; is .DCL file loaded?
     (T (ai_undo_push)
; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
        (b_sec_main)
        (if (= drawmode 1) (b_sec_draw) )
; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
        (ai_undo_pop)
     )
  )

  (setvar "OSMODE" old_osmode)
  (setvar "BLIPMODE" old_blip)
  (setq *error* old_error)
  (setvar "cmdecho" old_cmd)

  (princ)
)
;;; ================= End main function ==================
;;; ================= Setting Draw =======================
    (layer_style)
    (text_style)
    (dim_style)
;;; ======================================================
(princ "  B_SEC loaded.  ")
(princ)
