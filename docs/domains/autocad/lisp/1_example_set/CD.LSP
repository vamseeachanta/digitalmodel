;;;   CD.LSP Version 1.2
;;;   Copyright (C) 2000 by Tran Manh Dung.
;;; ===================== load-time error checking ============================
;;;
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
        (ai_abort "COD"
                  (strcat "Can't locate file AI_UTILS.LSP."
                          "\n Check support directory.")))

     (  (eq "failed" (load "ai_utils" "failed"))            ; load it
        (ai_abort "COD" "Can't load file AI_UTILS.LSP"))
  )

  (if (not (ai_acadapp))               ; defined in AI_UTILS.LSP
      (ai_abort "COD" nil)              ; a Nil <msg> supresses
  )                                    ; ai_abort's alert box dialog.

;;; ==================== end load-time operations ===========================

  ;=================  DRAW ALL  ================================
    (defun DRAW (stt_cot / M_LIST id_mc dist
                 pick_pt insertpt basept ccao scale 
                 WNAME GHISEC CN COLOP2H COLOP2 DCON MAXLI M_LISEC
                 DSEC DCSEC DHEIGHT DDIM DAXIS DSTE TEN_THEP GACH SO_THEPTR
                 SO_THEPCN1 SO_THEPCN CTD CTN DIMTEXT DIMSEC
                 CHI_TRUC hcott1 hcotp1)

    ;.............   Tao ham xac dinh lop 2 co mat  ..............
    (defun CN ()
      (if (= (strcase (nth 1 (assoc 'hdang (nth stttang solieu1cot)))) "CN") T)
    )
    (defun CO_THEPH (lop)
      (if (= lop 'ng)
        (if (/= (atof (nth 1 (assoc 'slnng  (nth stttang solieu1cot)))) 0) T)
        (if (/= (atof (nth 1 (assoc 'slntr  (nth stttang solieu1cot)))) 0) T)
      )
    )
    (defun CO_LOP2  ()
      (if (/= (atof (nth 1 (assoc 'sldtr  (nth stttang solieu1cot)))) 0) T)
    )
    ;............  Tao ham viet ten cot  ..............
    (defun WNAME (pt / i str_i str nstr litencot)
       (setq litencot (nth 1 (assoc 'truc solieu1cot)))
       (setq nstr (length litencot))
       (setq str_i "")
       (setq i 0)
       (repeat (- nstr 1)
         (if (< i (- nstr 2) )
           (setq str_i (strcat str_i (nth i litencot) " ") )
           (setq str_i (strcat str_i (nth i litencot)) )
         )
         (setq i (+ i 1))
       )
       (setq str (strcase (strcat "%%UCO/M/JT " str_i)))
       (set_lay "XANH")
       (set_ts "romant")
       (command "text" "bc" pt 6 0 str)
    );end WNAME

      ;......   Tao ham xac dinh thep doc tren mc chu nhat  ........
      (defun SO_THEPCN1 (HorV lop / slhor slver)
       (if (= HorV 'h)
        (if (= lop 'ng)
         (setq slhor (atoi (nth 1 (assoc 'slnng (nth stttang solieu1cot)))))
         (setq slhor (atoi (nth 1 (assoc 'slntr (nth stttang solieu1cot)))))
        )
        (if (= lop 'ng)
         (setq slver (atoi (nth 1 (assoc 'sldng (nth stttang solieu1cot)))))
         (setq slver (atoi (nth 1 (assoc 'sldtr (nth stttang solieu1cot)))))
        )
       )
      );end SO_THEPCN1

      (defun SO_THEPCN (/ sothep slhor)
         (setq slhor (SO_THEPCN1 'h 'ng))
         (if (>= slhor 2)
           (setq sothep
             (* 2
               (+ (SO_THEPCN1 'v 'ng)
                  (SO_THEPCN1 'v 'tr)
                  (SO_THEPCN1 'h 'tr)
                  (- slhor 2)
               )
             )
           );else
           (setq sothep
             (* 2
               (+ (SO_THEPCN1 'v 'ng)
                  (SO_THEPCN1 'v 'tr)
               )
             )
           )
         )
         (setq sothep (itoa sothep))
      );end SO_THEPCN

      ;......   Tao ham xac dinh thep doc tren mc tron  ........
      (defun SO_THEPTR (lop / sothep)
        (if (= lop 'ng)
          (setq sothep
            (+ (atoi (nth 1 (assoc 'sldng (nth stttang solieu1cot))))
               (atoi (nth 1 (assoc 'slnng (nth stttang solieu1cot))))
            )
          );else
          (setq sothep
            (+ (atoi (nth 1 (assoc 'sldtr (nth stttang solieu1cot))))
               (atoi (nth 1 (assoc 'slntr (nth stttang solieu1cot))))
            )
          )
        )
      );end SO_THEPTR

      ;...........  Tao ham tra ve so hieu thep ...............
      (defun TEN_THEP (loaithep / id_th)
        (if (= loaithep 'doc)
          (setq id_th (nth 1 (assoc 'tdoc (nth stttang solieu1cot))))
          (setq id_th (nth 1 (assoc 'tdai (nth stttang solieu1cot))))
        )
      );end TEN_THEP

    ;------------   Ve so hieu mat cat   -----------------
    (defun GHISEC (id_mc / pt1 pt2 id_sec)
       (setq pt1 (polar (polar noipt (/ pi 2) (/ ccao 2.0)) pi (* scale 450))
             pt2 (polar (polar noipt (/ pi 2) (/ ccao 2.0)) 0  (* scale 450))
       )
       (set_lay "XANH")
       (set_wid 0.35)
       (command "pline" (polar pt1 pi 3) (polar pt1 0 3) "")
       (command "pline" (polar pt2 pi 3) (polar pt2 0 3) "")
       (setq id_sec (rtos (nth id_mc listt_mc) 2 0))
       (set_ts "romand")
       (command "text" "bc" pt1 4 0 id_sec)
       (command "text" "bc" pt2 4 0 id_sec)
    );end GHISEC

    ;--------------   Draw concrete  -----------------
    (defun DCON ()
       (setq
          hcott  (* scale (atof (nth 1 (assoc 'hct (nth stttang solieu1cot)))))
          hcotp  (* scale (atof (nth 1 (assoc 'hcp (nth stttang solieu1cot)))))
       )
       (setq
          hdamt  (* scale (atof (nth 1 (assoc 'hdt (nth stttang solieu1cot)))))
          hdamp  (* scale (atof (nth 1 (assoc 'hdp (nth stttang solieu1cot)))))
          ccao   (* scale (atof (nth 1 (assoc 'cc  (nth stttang solieu1cot)))))
       )
       (cond
         ((and (= stttang 1) (> sotang 1))
           (setq
              hcottn (* scale (atof (nth 1 (assoc 'hct (nth (+ stttang 1) solieu1cot)))))
              hcotpn (* scale (atof (nth 1 (assoc 'hcp (nth (+ stttang 1) solieu1cot)))))
           )
         )
         ((and (= stttang sotang) (> sotang 1))
           (setq
              hcottb (* scale (atof (nth 1 (assoc 'hct (nth (- stttang 1) solieu1cot)))))
              hcotpb (* scale (atof (nth 1 (assoc 'hcp (nth (- stttang 1) solieu1cot)))))
           )
           (setq 
              hdamtb  (* scale (atof (nth 1 (assoc 'hdt (nth (- stttang 1) solieu1cot)))))
              hdampb  (* scale (atof (nth 1 (assoc 'hdp (nth (- stttang 1) solieu1cot)))))
           )
           (setq hcottn hcott hcotpn hcotp)
         )
         ((> sotang 1)
           (setq
              hcottb (* scale (atof (nth 1 (assoc 'hct (nth (- stttang 1) solieu1cot)))))
              hcotpb (* scale (atof (nth 1 (assoc 'hcp (nth (- stttang 1) solieu1cot)))))
           )
           (setq
              hdamtb  (* scale (atof (nth 1 (assoc 'hdt (nth (- stttang 1) solieu1cot)))))
              hdampb  (* scale (atof (nth 1 (assoc 'hdp (nth (- stttang 1) solieu1cot)))))
           )
           (setq
              hcottn (* scale (atof (nth 1 (assoc 'hct (nth (+ stttang 1) solieu1cot)))))
              hcotpn (* scale (atof (nth 1 (assoc 'hcp (nth (+ stttang 1) solieu1cot)))))
           )
         )
         ((= sotang 1) (setq hcottn hcott hcotpn hcotp))
       );end cond
       (setq
           ddam    (* scale 450)
           ctdpt  (polar noipt pi hcott)
           cpdpt  (polar noipt 0  hcotp)
           cttpt  (polar ctdpt (/ pi 2) (- ccao hdamt))      ;cot trai tren 
           cptpt  (polar cpdpt (/ pi 2) (- ccao hdamp))      ;cot phai tren 
           dtdpt  (polar cttpt pi (- ddam hcott))              ;dam trai duoi 
           dpdpt  (polar cptpt 0  (- ddam hcotp))              ;dam phai duoi 
           dttpt  (polar dtdpt (/ pi 2) hdamt)                 ;dam trai tren 
           dptpt  (polar dpdpt (/ pi 2) hdamp)                 ;dam phai tren 
           dttpt1 (polar dttpt 0  (- ddam hcottn))
           dptpt1 (polar dptpt pi (- ddam hcotpn))
       )
       (set_lay "bao")
       (cond
         ((= hdamt 0) (command "line" ctdpt (polar ctdpt (/ pi 2) ccao) dttpt1 ""))
         (T (progn (command "line" ctdpt cttpt dtdpt "")
                   (command "line" dttpt dttpt1 ""))
         )
       )
       (cond
         ((= hdamp 0) (command "line" cpdpt (polar cpdpt (/ pi 2) ccao) dptpt1 ""))
         (T (progn (command "line" cpdpt cptpt dpdpt "")
                   (command "line" dptpt dptpt1 ""))
         )
       )
       (if (= stttang sotang) (command "line" dttpt1 dptpt1 ""))
    );end DCON

    ;----------------   Draw steel  -----------------------------
    (defun DSTE ( / lopbv1 lopbv2 dmut kcdai1 kcdai11 kcdai2 kcdai3 
                    tr1 ph1 dai1 dai2 dai3 dai4 
                    delta dist dchong dchongb
                    ang hc DSTE1 entl dchan)
 
    ;..............  Tao ham ve thep doc   ................
    (defun DSTE1 ( ben lop / pt0 pt1 pt2 pt3 pt4 pt5 pt11
                   delta dist hc hcb hcn hd hdb ang heso lbv)
      (if (= ben 't)
          (setq hc hcott hcb hcottb hcn hcottn hd hdamt hdb hdamtb ang (/ pi 3) ang1 pi)
          (setq hc hcotp hcb hcotpb hcn hcotpn hd hdamp hdb hdampb ang (* 2 (/ pi 3)) ang1 0)
      )
      (cond
         ((= lop 'lop1) (setq lbv lopbv1))
         ((= lop 'lop2) (setq lbv lopbv2))
         (T             (setq lbv lop))
      )
      (setq
          pt1    (polar noipt ang1 (- hc lbv))
          pt11   (polar pt1 (/ pi 2) dchong)
          pt0    (polar pt1 (/ pi -2) dchong)
          pt2    (polar pt1 (/ pi 2) (- ccao hd))
          pt3    (if (< stttang sotang)
                   (polar (polar noipt (/ pi 2) ccao) ang1 (- hcn lbv))
                   (polar (polar noipt (/ pi 2) ccao) ang1 (- hc  lbv))
                 )
          pt4    (polar pt3  (/ pi 2) dchong)
          pt5    (polar pt1 (/ pi 2) (- ccao lopbv1))
      )

      (set_lay "thep")
      (set_wid 0.35)
      (cond
        ((= sotang 1)
         (command "pline" pt1 pt5 (polar pt5 (* -1 ang) dmut) "")
         (command "pline" pt11 (polar pt11 (* -1 ang) dmut) "")
        )
        ((= stttang 1)
          (command "pline" pt11 (polar pt11 (* -1 ang) dmut) "")
          (cond ((or (= hd 0) (<= (/ (- hc hcn) hd) (/ 1.0 6)))
            (command "pline" pt1 pt2 pt3 pt4 (polar pt4 (* -1 ang) dmut) ""))
            (T (command "pline" pt1 pt5 (polar pt5 (* -1 ang) dmut) ""))
          )
        )
        ((= stttang sotang)
          (cond ((or (= hdb 0) (<= (/ (- hcb hc) hdb) (/ 1.0 6)))
            (command "pline" (polar pt1 ang dmut) pt1 pt5 (polar pt5 (* -1 ang) dmut) ""))
            (T (command "pline" (polar pt0 ang dmut) pt0 pt5 (polar pt5 (* -1 ang) dmut) ""))
          )
        )
        (T
          (cond
             ((or (= hdb 0) (<= (/ (- hcb hc) hdb) (/ 1.0 6)))
               (cond
                 ((or (= hd 0) (<= (/ (- hc hcn) hd) (/ 1.0 6)))
                    (command "pline" (polar pt1 ang dmut) pt1 pt2 pt3 pt4 (polar pt4 (* -1 ang) dmut) ""))
                 (T (command "pline" (polar pt1 ang dmut) pt1 pt5 (polar pt5 (* -1 ang) dmut) ""))
               )
             )
             (T
                (cond
                  ((or (= hd 0) (<= (/ (- hc hcn) hd) (/ 1.0 6)))
                     (command "pline" (polar pt0 ang dmut) pt0 pt2 pt3 pt4 (polar pt4 (* -1 ang) dmut) ""))
                  (T (command "pline" (polar pt0 ang dmut) pt0 pt5 (polar pt5 (* -1 ang) dmut) ""))
                )
             )
          )
        )
      )
    );end DSTE1

      ;................    Ve gach  ..................
      (defun GACH (chieu pt / halflen)
         (set_lay "chi")
         (set_wid 0.25)
         (setq halflen 1)
         (if (= chieu 'p)
             (command "pline" 
               (polar pt (* 3 (/ pi -4)) halflen)
               (polar pt (/ pi 4) halflen) ""
             )
             (command "pline"
               (polar pt (/ pi -4) halflen)
               (polar pt (* 3 (/ pi 4)) halflen) ""
             )
         )
      );end GACH

      ;-----------------  start drawing DSTE ------------------
      (setq
          dchongb    (if (/= stttang 1) (* scale 30 (atof (nth 1 (assoc 'fidng (nth (- stttang 1) solieu1cot))))))
          dchong     (* scale 30 (atof (nth 1 (assoc 'fidng (nth stttang solieu1cot)))))
          lopbv1     (* scale 45)
          lopbv2     (* scale 112.5)
          dmut       (* scale 60)
          kcdai1     (* ccao 0.0)
          kcdai11    (if (/= stttang 1) dchongb dchong)
          kcdai2     (* ccao 0.35)
          kcdai22    (+ kcdai2  (* scale 200))
          kcdai3     (+ kcdai22 (* scale 200))
          tr1        (polar noipt pi (- hcott lopbv1 ))
          ph1        (polar noipt 0  (- hcotp lopbv1 ))	
          dai1       (polar tr1 (/ pi 2) kcdai1)
          dai11      (polar tr1 (/ pi 2) kcdai11)
          dai2       (polar tr1 (/ pi 2) kcdai2)
          dai22      (polar tr1 (/ pi 2) kcdai22)
          dai3       (polar tr1 (/ pi 2) kcdai3)
      );end setq

      ;-------------  Ve thep doc  -------------
      (set_lay "thep")
      (set_wid 0.35)
      (DSTE1 'P 'lop1)
      (DSTE1 't 'lop1)
      (if (CN)
        (progn
          (if (CO_LOP2) (progn (DSTE1 'T 'lop2) (DSTE1 'P 'lop2)))
          (if (CO_THEPH 'ng)
            (progn
              (setq kc2ct (/ (distance tr1 ph1) (- (SO_THEPCN1 'h 'ng) 1)))
              (setq n_ct1ben (- (/ (SO_THEPCN1 'h 'ng) 2) 1))
              (if (/= (rem (SO_THEPCN1 'h 'ng) 2) 0)
                  (setq le T)
                  (setq le nil)
              )
              (setq ben 't ang pi hc hcott)
              (repeat 2
                (setq lbv_i (+ lopbv1 kc2ct))
                (repeat n_ct1ben
                  (DSTE1 ben lbv_i)
                  (GACH 'p (polar (polar noipt (/ pi 2) (* ccao 0.60)) ang (- hc lbv_i)))
                  (setq lbv_i (+ lbv_i kc2ct))
                )
                (setq ben 'p ang 0 hc hcotp)
                (if le (setq n_ct1ben (+ n_ct1ben 1)))
              )
            )
          )
        );else
        (progn
          (setq  n_thepmc (+ (/ (SO_THEPTR 'ng) 2) 1) )
          (setq kc2ct (/ (distance tr1 ph1) (- n_thepmc 1)))
          (setq n_ct1ben (- (/ n_thepmc 2) 1))
          (if (/= (rem n_thepmc 2) 0)
              (setq le T)
              (setq le nil)
          )
          (setq ben 't ang pi hc hcott)
          (repeat 2
            (setq lbv_i (+ lopbv1 kc2ct))
            (repeat n_ct1ben 
              (DSTE1 ben lbv_i)
              (GACH 'p (polar (polar noipt (/ pi 2) (* ccao 0.60)) ang (- hc lbv_i)))
              (setq lbv_i (+ lbv_i kc2ct))
            )
            (setq ben 'p ang 0 hc hcotp)
            (if le (setq n_ct1ben (+ n_ct1ben 1)))
          )
        )
      );end if

      ;-------------  Ve thep ngang (thep dai) -------------
      (set_lay "thep")
      (set_wid 0.35)
      (command "pline" dai1
        (polar dai1 0 (- (+ hcott hcotp) (* 2 lopbv1))) "")
      (command "pline" dai11
        (polar dai11 0 (- (+ hcott hcotp) (* 2 lopbv1))) "")
      (command "pline" dai2
        (polar dai2 0 (- (+ hcott hcotp) (* 2 lopbv1))) "")
      (command "pline" dai22
        (polar dai22 0 (- (+ hcott hcotp) (* 2 lopbv1))) "")
      (command "pline" dai3
        (polar dai3 0 (- (+ hcott hcotp) (* 2 lopbv1))) "")
      (if (= stttang sotang)
        (progn
          (setq dai4 (polar (polar noipt (/ pi 2) ccao) pi (- hcott  lopbv1)))
          (command "pline" (polar dai4 (/ pi -2) lopbv1)
            (polar (polar dai4 (/ pi -2) lopbv1) 0 (- (+ hcott hcotp) (* 2 lopbv1))) "")
          (if (= hdamt hdamp 0)
            (progn
              (setq entl (ssadd (entlast)))
              (command "array" entl "" "R" 6 1 (* scale 75 -1))
            )
          );if
        )
      );if

      ;----------------------   Ghi chu thep   -----------------------
      ;................    ghi thep doc   ..................
      (defun CTD (solop trucpt dist / pt1 pt2 id_th
                  hr hl entl strpt tronpt lbv1 sothep sl&fi)
         (setq
           lbv1 lopbv1
           hr   (* scale (atof (nth 1 (assoc 'hcp (nth stttang solieu1cot)))))
           hl   (* scale (atof (nth 1 (assoc 'hct (nth stttang solieu1cot)))))
           pt1  (polar trucpt pi (- hl lbv1))
           pt2  (polar trucpt 0  (- hr lbv1))
         )

         (if (and (= solop '2) (CN))
           (progn
             (setq lbv2 lopbv2)
             (GACH 'P (polar trucpt pi (- hl lbv2)))
             (GACH 'P (polar trucpt 0  (- hr lbv2)))
           )
         )
         (GACH 'p pt1)
         (GACH 'p pt2)
         (setq tronpt (polar trucpt 0 dist))
         (set_lay "chi")
         (command "line" pt1 tronpt "")
         (setq strpt (polar tronpt pi 2))
         (set_lay "text")
         (set_ts "romans")
         (setq sl&fi
           (strcat 
             (if (CN)
                 (SO_THEPCN)
                 (itoa (+ (SO_THEPTR 'ng) (SO_THEPTR 'tr)))
             )
             "/g"
             (nth 1 (assoc 'fidng (nth stttang solieu1cot)))
           )
         )
         (command "text" "br" strpt 2.5 0 sl&fi)
         (set_lay "chi")
         (command "circle" (polar tronpt 0 3) 3)
         (set_lay "XANH")
         (setq entl (entget (entlast)))
         (command "text" "m" (cdr (assoc '10 entl)) 2.5 0 (TEN_THEP 'doc))
      );end CTD

      ;....................    ghi thep ngang   ................
      (defun CTN (pt1 pt2 pt3 giuapt dist strt strd
                  / entl strtpt strdpt tronpt id_th)
         (GACH 't pt1)
         (GACH 't pt2)
         (GACH 't pt3)
         (setq tronpt (polar giuapt 0 dist))
         (set_lay "chi")
         (command "line" pt1 pt3 "")
         (command "line" giuapt tronpt "")
         (setq strtpt (polar tronpt pi 2))
         (setq strdpt (polar strtpt (/ pi -2) 2))
         (set_lay "text")
         (set_ts "romans")
         (command "text" "br" strtpt 2.5 0 strt)
         (command "text" "tr" strdpt 2.5 0 strd)
         (set_lay "chi")
         (command "circle" (polar tronpt 0 3) 3)
         (set_lay "XANH")
         (setq entl (entget (entlast)))
         (command "text" "m" (cdr (assoc '10 entl)) 2.5 0 (TEN_THEP 'dai))
      );end CTN

              ;--------- start commenting steel ---------
      ;Ghi doan chan cot co adai thay doi ................
      (if (= stttang 1) (setq hcott1 hcott))
      (setq dchan
        (if (/= stttang 1)
            (polar (polar basept pi (+ 25.25 hcott1)) (/ pi 2) (+ ctr (/ dchongb 2)))
            (polar (polar basept pi (+ 25.25 hcott1)) (/ pi 2) (/ dchong 2))
        )
      )
      (set_lay "text")
      (set_ts "romans")
      (command "text" "m" dchan 2.5 90
        (strcat
           "%%c"
           (nth 1 (assoc 'fidai (nth stttang solieu1cot)))
           "a"
           (nth 1 (assoc 'achan (nth stttang solieu1cot)))
        )
      )

      ;Ghi doc..................
      (if (= stttang 1) (setq hcotp1 hcotp))
      (if (CO_LOP2)
        (CTD '2 (polar noipt (/ pi 2) (* ccao 0.60)) (+ 15.5 hcotp1))
        (CTD '1 (polar noipt (/ pi 2) (* ccao 0.60)) (+ 15.5 hcotp1))
      )

      ;Ghi dai.................
      (if (>= hcott hcotp)
          (setq delta (/ (- hcott lopbv2) 2.0)
                dist  (- (+ 17 hcotp delta) lopbv1)
          )
          (setq delta (/ (- lopbv2 hcotp) 2.0)
                dist  (- (+ 17 hcotp) lopbv1 (abs delta))
          )
      )
      (CTN (polar (polar noipt (/ pi 2) kcdai2) pi delta)
           (polar (polar noipt (/ pi 2) kcdai22) pi delta)
           (polar (polar noipt (/ pi 2) kcdai3) pi delta)
           (polar (polar noipt (/ pi 2) (* ccao 0.425)) pi delta)
            dist
           (strcat "%%c" (nth 1 (assoc 'fidai (nth stttang solieu1cot))))
           (strcat "a"   (nth 1 (assoc 'agiua (nth stttang solieu1cot))))
      );end ghi dai
    );end DSTE 

      ;............   Tao ham ve kich thuoc mat cat  .............
      (defun DIMSEC (HorV LorR pt1 pt2 fac / df)
        (setq df (getvar "dimlfac"))
        (setvar "dimlfac" (/ 1.0 fac))
        (if (= HorV 'h)
          (command "dim" "horizontal" pt1 pt2 (polar pt1 (/ pi -2) 7) "" "exit")
          (if (= LorR 'l)
            (command "dim" "vertical" pt1 pt2 (polar pt1 pi 7) "" "exit")
            (command "dim" "vertical" pt1 pt2 (polar pt1 0  7) "" "exit")
          )
        )
        (setvar "dimlfac" df)
      );end DIMSEC
      
      ;...............   Tao ham ve kich thuoc cot   ...............
      (defun DIMTEXT (HorV ben pt1 pt2 dist)
        (cond
          ((= HorV 'v)
            (cond ((= ben 't) (command "dim" "vertical" pt1 pt2 (polar pt1 pi 7)
                      (rtos dist 2 0) "exit"))
                  ((= ben 'p) (command "dim" "vertical" pt1 pt2 (polar pt1 0  7)
                      (rtos dist 2 0) "exit"))
            )
          )
          ((= HorV 'h)
            (cond ((= ben 'd) (command "dim" "horizontal" pt1 pt2 (polar pt1 (/ pi -2) 7)
                      (rtos dist 2 0) "exit"))
                  ((= ben 't) (command "dim" "horizontal" pt1 pt2 (polar pt1 (/ pi  2) 7)
                      (rtos dist 2 0) "exit"))
            )
          )
        )
      );end DIMTEXT

      ;...............   Tao ham chi truc cot   ...............
      (defun CHI_TRUC (horv pt str / tronpt entl cenpt)
          (if (= horv 'v)
             (setq tronpt (polar pt (/ pi -2) 2)
                   cenpt  (polar tronpt (/ pi -2) 4)
             )
             (setq tronpt (polar pt 0 2)
                   cenpt  (polar tronpt 0 4)
             )
          )
          (set_lay "dim")
          (command "line" pt tronpt "")
          (command "circle" cenpt 4)
          (set_lay "XANH")
          (set_ts "romand")
          (setq str (strcase str))
          (setq entl (entget (entlast)))
          (command "text" "m" (cdr (assoc '10 entl)) 4 0 str)
      );end CHI_TRUC

    ;------------------   Ve kich thuoc   ------------------------
    (defun DDIM ( / midpt lefpt rigpt pt1 pt2 pt3 pt4
                    litencot lenlist dnoi )
      (if (= stttang 1)
        (progn
          (setq midpt (polar basept (/ pi -2) 4)
                lefpt (polar midpt pi hcott)
                rigpt (polar midpt 0  hcotp)
          )
          (set_lay "dim")
          (DIMTEXT 'h 'd midpt lefpt (/ hcott scale))
          (DIMTEXT 'h 'd midpt rigpt (/ hcotp scale))
          (setq litencot (nth 1 (assoc 'truc solieu1cot)))
          (setq lenlist (length litencot))
          (CHI_TRUC
             'v
             (polar midpt (/ pi -2) 9)
             (nth (- lenlist 1) litencot)
          )
        )
      )
      (setq
        dnoi (if (/= stttang 1)
                 (* scale 30 (atof (nth 1 (assoc 'fidng (nth (- stttang 1) solieu1cot)))))
                 (* scale 30 (atof (nth 1 (assoc 'fidng (nth 1             solieu1cot)))))
             )
        pt1  (polar noipt pi (+ 22.25 hcott1))
        pt2  (polar pt1 (/ pi  2) dnoi)
        pt4  (polar pt1 (/ pi  2) ccao)
      )
      (set_lay "dim")
      (DIMTEXT 'v 't pt1 pt2 (/ dnoi scale))
      (DIMTEXT 'v 't (polar pt1 pi 7) (polar pt4 pi 7) (/ ccao scale))
    );end DDIM

    ;------------------   Ve duong cat    -----------------------
    (defun DSEC (startpt endpt / pt dist ang pt1 pt2 
                pt3 pt4 pt5 pt6)
      (setq dist (distance startpt endpt))
      (setq ang (angle startpt endpt))
      (setq pt1 (polar startpt (+ ang pi) (* scale 70)))
      (setq pt2 (polar startpt ang (- (/ dist 2) (* scale 60))))
      (setq pt3 (polar startpt ang (+ (/ dist 2) (* scale 60))))
      (setq pt4 (polar startpt ang (+ dist (* scale 70))))
      (setq pt5 (polar pt2 (+ (* 3 (/ pi 2)) ang) (* scale 70)))
      (setq pt6 (polar pt3 (+ (/ pi 2) ang) (* scale 70)))
      (set_lay "chi")
      (set_wid 0)
      (command "pline" pt1 pt2 pt5 pt6 pt3 pt4 "")
    );end DSEC

    ;------------------    Ve truc    ---------------------
    (defun DAXIS (startpt endpt)
      (set_lay "truc")
      (command "line" startpt endpt "")
    );end DAXIS
    ;------------------    Ve cao trinh    ------------------------
    (defun DHEIGHT ( / DH)

      ;...............  Tao ham ve cao trinh  ................
      (defun DH (ben numorstr basept alphanum / len ctr pt1 pt2 pt3 pt4
                 pt5 pt6 ch pt5t pt5p sptp eptt lastli ktloc i)
         (cond ((= ben 't) (setq ch "ctt"))
               ((= ben 'p) (setq ch "ctp"))
         )
         (cond ((= numorstr 'num) (setq ctr (rtos alphanum 2 0)))
               ((= numorstr 'str)  (setq ctr alphanum))
         )
         (setq pt1  (polar basept pi 3)
               pt2  (polar basept 0  3)
               pt3  (polar pt1 (/ pi 2) 3)
               pt4  (polar pt2 (/ pi 2) 3)
               pt6  (polar basept (/ pi 2) 6)
               pt5t (polar pt6 pi 3)
               pt5p (polar pt6 0 3)
               pt8  (polar basept (/ pi 2) 12)
               sptp (polar pt6 0 2)            ;172: chieu rong trung binh
               eptt (polar pt6 pi 2)
         )
         (setq i 1)
         (setq len (strlen ctr))
         (while (<= i (strlen ctr))
            (setq ktloc (substr ctr i 2))
            (if (or (= ktloc "/f") (= ktloc "/r") (= ktloc "/s")
                    (= ktloc "/x") (= ktloc "/j") (= ktloc "%%")
                    (= ktloc "/m") (= ktloc "/w")
                    (= ktloc "/F") (= ktloc "/R") (= ktloc "/S")
                    (= ktloc "/X") (= ktloc "/J")
                    (= ktloc "/M") (= ktloc "/W"))
                (setq len (- len 2))
            )
            (if (or (= ktloc "/g") (= ktloc "/]") (= ktloc "/[")
                    (= ktloc "/d") (= ktloc "/G") (= ktloc "/D"))
                (setq len (- len 1))
            )
            (setq i (1+ i))
         )
         (setq len (* (+ len 2) 2))
         (if (= ch "ctp")
             (progn
                (set_lay "text")
                (set_ts "romans")
                (command "text" "bl" sptp 2.5 0 ctr)
                (set_lay "chi")
                (command "line" pt5t (polar pt6 0 len) "")
;                (command "line" pt1 (polar pt1 (/ pi -4) 1.5) "")
;                (command "array" (entlast) "" "r" 1 5 1.5)
             )
             (progn
                (set_lay "text")
                (set_ts "romans")
                (command "text" "br" eptt 2.5 0 ctr) 
                (set_lay "chi")
                (command "line" pt5p (polar pt6 pi len) "")
;                (command "line" pt1 (polar pt1 (* 3 (/ pi -4)) 1.5) "")
;                (command "array" (entlast) "" "r" 1 5 1.5)
             )
         )
         (command "line" pt1 pt2 "")
         (command "line" pt3 pt4 "")
         (command "line" basept pt3 "")
         (command "line" basept pt4 "")
         (command "line" basept pt8 "")
         (command "solid" basept pt3 (polar pt3 0 3) "" "")
      );end DH

                  ;--------- start drawing height -----------
      (if (= stttang 1)
          (progn
            (DH 't 'str (polar basept pi (+ 45 hcott1)) "%%p0000")
            (DH 't 'num (polar (polar basept pi (+ 45 hcott1)) (/ pi 2) ccao) (/ (+ ccao ctr) scale))
          )
          (DH 't 'num (polar (polar basept pi (+ 45 hcott1)) (/ pi 2) (+ ccao ctr)) (/ (+ ccao ctr) scale))
      )
    );end DHEIGHT

    ;-----------------  draw column section  ----------------
    (defun DCSEC (cen id_mc / hd 
                  kc2lop ddonut rdonut
                  CHITRUC CN_SEC TR_SEC CHITHEP a)
      (setq
            a       20
            ddai    (* scale 10.5)
            rdonut  (* scale 18)
            ddonut  (* rdonut 2)
            lbvve1  (* scale 45)
            lbvve2  (* scale 112.5)
            kc2lop  (- lbvve2 lbvve1)
      )
    ;------------------    Ve mat cat ngang   ------------------
       ;..............  tao ham chi thep  .......................
       (defun CHITHEP (ben pt dist str tenthep / entl strpt tronpt a b c)
         (if (= ben 'tr)
             (setq a pi b 0  c "bl")
             (setq a 0  b pi c "br")
         )
         (setq tronpt (polar pt a dist))
         (set_lay "chi")
         (command "line" pt tronpt "")
         (setq strpt (polar tronpt b 2))
         (set_lay "text")
         (set_ts "romans")
         (command "text" c strpt 2.5 0 str)
         (set_lay "chi")
         (command "circle" (polar tronpt a 3) 3)
         (set_lay "XANH")
         (setq entl (entget (entlast)))
         (command "text" "m" (cdr (assoc '10 entl)) 2.5 0 tenthep)
       );end CHITHEP

       ;Chi truc cot---------------
       (defun CHITRUC (/ litencot lenlist)
         (setq litencot (nth 1 (assoc 'truc solieu1cot)))
         (setq lenlist (length litencot))
         (CHI_TRUC 'v (polar hdimg (/ pi -2) 14) (nth (- lenlist 1) litencot))
         (if (= (length litencot) 3)
            (CHI_TRUC 'h (polar vdimg 0 14) (nth 2 litencot))
            (CHI_TRUC 'h (polar vdimg 0 14) "A")
         )
       );end CHITRUC

      ;..................  Tao ham ve mat cat  chu nhat  ..................
      (defun CN_SEC (cen id_mc / 
              bcott bcotd b_cott b_cotd b_cot h_cott h_cottp h_cot
              sldng sldtr slnng slntr
              vaxis haxis td tt pd pt
              hdimbv1 hdimbv2 hdimbv3 hdimbv4 
              vdimbv1 vdimbv2 vdimbv3 vdimbv4 
              hdimttr hdimg hdimptr hdimtng hdimpng
              vdimdtr vdimg hdimttr vdimdng vdimtng
              daitd daitt daipt daipd doctd doctt docpd docpt
              dai1 dai2 dai3 dai4 dai5 dai6 dai7 dai8
              vd vt hp ht pt1 pt2
              point chum chidaibao litencot
              THEP_VER THEP_HOR
             )

       (setq
           b_cott  (* scsec (atof (nth 1 (assoc 'bct (nth stttang solieu1cot)))))
           b_cotd  (* scsec (atof (nth 1 (assoc 'bcd (nth stttang solieu1cot)))))
           h_cott  (* scsec (atof (nth 1 (assoc 'hct (nth stttang solieu1cot)))))
           h_cotp  (* scsec (atof (nth 1 (assoc 'hcp (nth stttang solieu1cot)))))

           sldng   (atoi (nth 1 (assoc 'sldng (nth stttang solieu1cot))))
           sldtr   (atoi (nth 1 (assoc 'sldtr (nth stttang solieu1cot))))
           slnng   (atoi (nth 1 (assoc 'slnng (nth stttang solieu1cot))))
           slntr   (atoi (nth 1 (assoc 'slntr (nth stttang solieu1cot))))

           h_cot    (+ h_cott h_cotp)
           b_cot    (+ b_cott b_cotd)

           vaxis   (+ b_cotd b_cott 12)
           haxis   (+ h_cott h_cotp 12)
           td      (polar (polar cen (/ pi -2) b_cotd) pi h_cott)
           tt      (polar td (/ pi 2) b_cot)
           pt      (polar tt 0 h_cot)
           pd      (polar td 0 h_cot)

           hdimbv1 (polar td (/ pi -2) 4)
           hdimbv2 (polar hdimbv1 0 lbvve1)
           hdimbv4 (polar pd (/ pi -2) 4)
           hdimbv3 (polar hdimbv4 pi lbvve1)

           hdimttr (polar td (/ pi -2) 11)
           hdimg   (polar hdimttr 0 h_cott)
           hdimptr (polar hdimg 0 h_cotp)
           hdimtng (polar td (/ pi -2) 18)
           hdimpng (polar hdimtng 0 h_cot)

           vdimbv1 (polar pd 0 4)
           vdimbv2 (polar vdimbv1 (/ pi 2) lbvve1)
           vdimbv4 (polar pt 0 4)
           vdimbv3 (polar vdimbv4 (/ pi -2) lbvve1)

           vdimdtr (polar pd 0 11)
           vdimg   (polar vdimdtr (/ pi 2) b_cotd)
           vdimttr (polar vdimg (/ pi 2) b_cott)
           vdimdng (polar pd 0 18)
           vdimtng (polar vdimdng (/ pi 2) b_cot)

           daitd   (polar td (/ pi 4)         (* 1.4142 lbvve1))
           daitt   (polar tt (/ pi -4)        (* 1.4142 lbvve1))
           daipd   (polar pd (* 3 (/ pi 4))   (* 1.4142 lbvve1))
           daipt   (polar pt (* 5 (/ pi 4))   (* 1.4142 lbvve1))

           dai1    (polar daitt (/ pi -2) rdonut)
           dai2    (polar daitd (/ pi  2) rdonut)
           dai3    (polar daitd  0        rdonut)
           dai4    (polar daipd  pi       rdonut)
           dai5    (polar daipd (/ pi  2) rdonut)
           dai6    (polar daipt (/ pi -2) rdonut)
           dai7    (polar daipt  pi       rdonut)
           dai8    (polar daitt  0        rdonut)

           doctd   (polar daitd (/ pi 4)      (* 1.4142 (+ (/ ddai 2) rdonut)))
           doctt   (polar daitt (/ pi -4)     (* 1.4142 (+ (/ ddai 2) rdonut)))
           docpd   (polar daipd (* 3 (/ pi 4))(* 1.4142 (+ (/ ddai 2) rdonut)))
           docpt   (polar daipt (* 5 (/ pi 4))(* 1.4142 (+ (/ ddai 2) rdonut)))
       )

       ;Ve truc--------------
       (setq vd (polar hdimg (/ pi 2) 5))
       (setq vt (polar vd (/ pi 2) vaxis))
       (setq hp (polar vdimg pi 5))
       (setq ht (polar hp pi haxis))
       (DAXIS vd vt)
       (DAXIS hp ht)

       ;------------------   Ve betong   -------------------
       (set_lay "bao")
       (command "line" td tt pt pd "close")

       ;------------------   Ve thep  -------------------
       (set_lay "thep")
       (set_wid 0.35)
       (setq pt1 (polar daitt (/ pi -2) (* scale 38.1)))
       (setq pt2 (polar daitt 0 (* scale 38.1)))
       (command "pline"
          dai1 dai2 "A" 
          dai3 "L" dai4 "A"
          dai5 "L" dai6 "A"
          dai7 "L" dai8 "A" dai1 "")
       (command "pline" pt1 (polar pt1 (/ pi -4) (* scale 60)) "")
       (command "pline" pt2 (polar pt2 (/ pi -4) (* scale 60)) "")

       ;................  Tao ham ve thep phuong B cot  ................
       (defun THEP_VER (lop / jpt sl fi dis dit point dist)
         (if (= lop 'ng)
           (progn
             (setq pt1 doctt pt2 docpt)
             (setq sl (atoi (nth 1 (assoc 'sldng (nth stttang solieu1cot)))))
           )
           (progn
             (setq pt1 (polar doctt 0 kc2lop) pt2 (polar docpt pi kc2lop))
             (setq sl (atoi (nth 1 (assoc 'sldtr (nth stttang solieu1cot)))))
           )
         )
         (setq fi (atoi (nth 1 (assoc 'fidng (nth stttang solieu1cot)))))
         (setq dist (/ (distance doctd doctt) (- sl 1)))
         (set_lay "thep")
         (setq point pt1)
         (repeat 2
           (repeat sl
             (command "donut" 0 ddonut point "")
             (setq point (polar point (/ pi -2) dist))
           )
           (setq point pt2)
         )
       );end THEP_VER
       ;................  Tao ham ve thep phuong H cot  ................
       (defun THEP_HOR (lop / sl fi point dist)
         (if (= lop 'ng)
           (progn
             (setq pt1 doctd pt2 doctt)
             (setq sl (atoi (nth 1 (assoc 'slnng (nth stttang solieu1cot)))))
           )
           (progn
             (setq pt1 (polar doctd (/ pi 2) kc2lop) pt2 (polar doctt (/ pi -2) kc2lop))
             (setq sl (atoi (nth 1 (assoc 'slntr (nth stttang solieu1cot)))))
           )
         )
         (setq fi (atoi (nth 1 (assoc 'finng (nth stttang solieu1cot)))))
         (setq dist (/ (distance doctd docpd) (- sl 1)))
         (set_lay "thep")
         (setq point pt1)
         (repeat 2
           (repeat sl
             (command "donut" 0 ddonut point "")
             (setq point (polar point 0 dist))
           )
           (setq point pt2)
         )
       );end THEP_HOR

       (THEP_VER 'ng)
       (THEP_HOR 'ng)
       (if (CO_LOP2) (THEP_VER 'tr))
       (if (CO_THEPH 'tr) (THEP_HOR 'tr))

       (setq jpt (polar (polar cen 0 (/ (- h_cotp h_cott) 2)) (/ pi 2) (+ b_cott 13)))
       (set_lay "chi")
       (command "line" doctt jpt docpt "")
       (CHITHEP 'tr jpt (+ 15.5 lbvve1)
           (strcat 
             (SO_THEPCN)
             "/g"
             (nth 1 (assoc 'fidng (nth stttang solieu1cot)))
           )
           (TEN_THEP 'doc)
       )

       ;Chi thep dai----------------
       (setq chidaibao  (polar daitd (/ pi 2) (/ (distance daitd daitt) 4)))
       (CHITHEP 'tr chidaibao (+ 15.5 lbvve1)
         (strcat "%%c" (nth 1 (assoc 'fidai (nth stttang solieu1cot)))
                 "a"   (nth 1 (assoc 'agiua (nth stttang solieu1cot)))
         )
         (TEN_THEP 'dai)
       )
    
       ;Ve kich thuoc-----------------
       (set_lay "dim")
       (DIMTEXT 'h 'd hdimbv2 hdimbv1 a)
       (DIMTEXT 'h 'd hdimbv3 hdimbv4 a)
       (DIMTEXT 'h 'd hdimg   hdimttr (/ h_cott scsec) )
       (DIMTEXT 'h 'd hdimg   hdimptr (/ h_cotp scsec) )
       (DIMTEXT 'h 'd hdimtng hdimpng (/ h_cot  scsec) )

       (DIMTEXT 'v 'p vdimbv2 vdimbv1 a)
       (DIMTEXT 'v 'p vdimbv3 vdimbv4 a)
       (DIMTEXT 'v 'P vdimg   vdimdtr (/ b_cotd scsec) )
       (DIMTEXT 'v 'P vdimg   vdimttr (/ b_cott scsec) )
       (DIMTEXT 'v 'P vdimdng vdimtng (/ b_cot  scsec) )

       ;----------  Ve truc cot mat cat  ------------
       (CHITRUC)

       ;----------  Ten mat cat  ------------
       (setq point (polar (polar cen 0 (- (/ h_cot 2) h_cott)) (/ pi 2) (+ b_cott 23)))
       (command "text" "bc" point 5 0
          (strcat "%%u" (rtos id_mc 2 0) "-" (rtos id_mc 2 0))
       )
      );end CN_SEC

      ;..................  Tao ham ve mat cat tron ..................
      (defun TR_SEC (cen id_mc / 
                      bcott bcotd hcott hcotp dcot
                      b_cott b_cotd h_cott h_cottp b_cot h_cot
                      sldng sldtr slnng slntr slng sltr
                      kdcot sl dcot rcot rdoc
                      axislen gocdoc
                      qual quar quab quat quadb quadt
                      hdimbv1 hdimbv2 hdimbv3 hdimbv4 
                      vdimbv1 vdimbv2 vdimbv3 vdimbv4 
                      hdimttr hdimg hdimptr hdimtng hdimpng
                      vdimdtr vdimg hdimttr vdimdng vdimtng
                      vd vt ht hp point
                    )
        (setq
           b_cott  (* scsec (atof (nth 1 (assoc 'bct (nth stttang solieu1cot)))))
           b_cotd  (* scsec (atof (nth 1 (assoc 'bcd (nth stttang solieu1cot)))))
           h_cott  (* scsec (atof (nth 1 (assoc 'hct (nth stttang solieu1cot)))))
           h_cotp  (* scsec (atof (nth 1 (assoc 'hcp (nth stttang solieu1cot)))))

           sldng   (atoi (nth 1 (assoc 'sldng (nth stttang solieu1cot))))
           sldtr   (atoi (nth 1 (assoc 'sldtr (nth stttang solieu1cot))))
           slnng   (atoi (nth 1 (assoc 'slnng (nth stttang solieu1cot))))
           slntr   (atoi (nth 1 (assoc 'slntr (nth stttang solieu1cot))))
           slng    (+ sldng slnng)
           sltr    (+ sldtr slntr)

           b_cot   (+ b_cott b_cotd)
           h_cot   (+ h_cott h_cotp)

           dcot    (+ h_cott h_cotp)
           vaxis   (+ b_cotd b_cott 12)
           haxis   (+ h_cott h_cotp 12)


          rcot    (/ dcot 2.0)
          rdai    (- rcot lbvve1 (/ ddai 2.0))
          rdoc    (- rdai rdonut (/ ddai 2.0))
          axislen (+ dcot 12)
          gocng   (/ (* 2 pi) slng)

          qual    (polar cen pi rcot)
          quar    (polar cen 0  rcot)
          quab    (polar cen (/ pi -2) rcot)
          quat    (polar cen (/ pi  2) rcot)
          quadb   (polar cen (/ pi -2) rdai)
          quadt   (polar cen (/ pi  2) rdai)

          hdimbv1 (polar qual (/ pi -2) (+ rcot 4))
          hdimbv2 (polar hdimbv1 0 lbvve1)
          hdimbv4 (polar quar (/ pi -2) (+ rcot 4))
          hdimbv3 (polar hdimbv4 pi lbvve1)

          hdimttr (polar hdimbv1 (/ pi -2) 7)
          hdimg   (polar hdimttr 0 rcot)
          hdimptr (polar hdimg 0 rcot)
          hdimtng (polar hdimttr (/ pi -2) 7)
          hdimpng (polar hdimtng 0 dcot)
 
          vdimbv1 (polar quab 0 (+ rcot 4))
          vdimbv2 (polar vdimbv1 (/ pi 2) lbvve1)
          vdimbv4 (polar quat 0 (+ rcot 4))
          vdimbv3 (polar vdimbv4 (/ pi -2) lbvve1)

          vdimdtr (polar vdimbv1 0 7)
          vdimg   (polar vdimdtr (/ pi 2) rcot)
          vdimttr (polar vdimg (/ pi 2) rcot)
          vdimdng (polar vdimdtr 0 7)
          vdimtng (polar vdimdng (/ pi 2) dcot)

        )

        ;Ve truc--------------
        (setq vd (polar hdimg (/ pi 2) 5))
        (setq vt (polar vd (/ pi 2) axislen))
        (setq hp (polar vdimg pi 5))
        (setq ht (polar hp pi axislen))
        (DAXIS vd vt)
        (DAXIS hp ht)

        ;----------  Ve betong   ------------
        (set_lay "bao")
        (command "circle" cen rcot)

        ;-------  Ve cot thep (doc+dai) lop ngoai (va trong neu co) --------
        (set_lay "thep")
        (set_wid 0.35)
        (command "pline" quadb "arc" "direction" 0 quadt quadb "")
        (command "donut" 0 ddonut (polar cen (/ pi 2) rdoc) "")
        (command "array" (entlast) "" "p" cen slng 360 "yes")
        (cond ((CO_LOP2)
          (setq
            quadb (polar quadb (/ pi  2) kc2lop)
            quadt (polar quadt (/ pi -2) kc2lop)
          )
          (command "pline" quadb "arc" "direction" 0 quadt quadb "")
          (command "donut" 0 ddonut (polar cen (/ pi -2) (- rdoc kc2lop)) "")
          (command "array" (entlast) "" "p" cen sltr 360 "yes")
        ))

        ;-------------  Chi thep doc lop trong neu co  ---------------
        (cond ((CO_LOP2)
          (setq goctr (/ (* 2 pi) sltr))
          (setq chum  (polar qual (/ pi -2) rcot))
          (setq point (polar cen (/ pi -2) (- rdoc kc2lop)))
          (setq i 2)
          (set_lay "chi")
          (repeat 3
             (command "line" chum point "")
             (setq point (polar cen (- (/ pi -2) (* i goctr)) (- rdoc kc2lop)))
             (setq i (- i 1))
          )
          (CHITHEP 'tr chum (+ 15.5 lbvve1)
            (strcat 
              (itoa (SO_THEPTR 'tr))
              "/g"
              (nth 1 (assoc 'fidtr (nth stttang solieu1cot)))
            )
            (TEN_THEP 'doc)
          )
        ))

        ;------------  Chi thep doc lop ngoai --------------
        (setq chum (polar qual (/ pi 2) (+ rcot 10)))
        (setq point (polar cen (/ pi 2) rdoc))
        (setq i 1)
        (set_lay "chi")
        (repeat 3
           (command "line" chum point "")
           (setq point (polar cen (+ (/ pi 2) (* i gocng)) rdoc))
           (setq i (+ i 1))
        )
        (CHITHEP 'tr chum (+ 15.5 lbvve1)
           (strcat
              (itoa (SO_THEPTR 'ng))
              "/g"
              (nth 1 (assoc 'fidng (nth stttang solieu1cot)))
           )
           (TEN_THEP 'doc)
        )

        ;--------------  Chi thep dai bao lop ngoai  ---------------
        (setq betang (/ (* 75 pi) 180))
        (setq betatr (/ (* 60 pi) 180))
        (setq pt1 (polar cen betang rdai))
        (setq chum (polar cen betang (+ rdai 12.6)))
        (setq pt2 (polar cen betatr (- rdai kc2lop)))
        (set_lay "chi")
        (command "line" pt1 chum "")
        (if (CO_LOP2) (command "line" pt2 chum ""))
        (CHITHEP 'ph chum (+ 15.5 lbvve1)
          (strcat 
             "%%c" (nth 1 (assoc 'fidai  (nth stttang solieu1cot)))
             "a"   (nth 1 (assoc 'agiua  (nth stttang solieu1cot)))
          )
          (TEN_THEP 'dai)
        )

        ;Ve kich thuoc--------
        (set_lay "dim")
        (DIMTEXT 'h 'd hdimbv2 hdimbv1 a)
        (DIMTEXT 'h 'd hdimbv3 hdimbv4 a)
        (DIMTEXT 'h 'd hdimg   hdimttr (/ h_cott scsec) )
        (DIMTEXT 'h 'd hdimg   hdimptr (/ h_cotp scsec) )
        (DIMTEXT 'h 'd hdimtng hdimpng (/ h_cot  scsec) )
  
        (DIMTEXT 'v 'p vdimbv2 vdimbv1 a)
        (DIMTEXT 'v 'p vdimbv3 vdimbv4 a)
        (DIMTEXT 'v 'p vdimg   vdimdtr (/ b_cotd scsec) )
        (DIMTEXT 'v 'p vdimg   vdimttr (/ b_cott scsec) )
        (DIMTEXT 'v 'p vdimdng vdimtng (/ b_cot  scsec) )

        ;----------  Ve truc cot mat cat  ------------
        (CHITRUC)

        ;Ten mat cat---------------
        (setq point (polar cen (/ pi 2) (+ rcot 23)))
        (command "text" "bc" point 5 0
           (strcat "%%u" (rtos id_mc 2 0) "-" (rtos id_mc 2 0))
        )
      );end TR_SEC

      ;--------------   start drawing section  -----------------
      (cond ((CN) (CN_SEC cen id_mc))
            (T    (TR_SEC cen id_mc))
      )
    );end DCSEC

    ;.............  Tao ham tinh max cac phan tu trong 1 list  .........
    (defun MAXLI (li / i)
     (setq maxnum 0)
     (setq i 0)
     (if (/= li nil) 
       (progn
         (repeat (length li)
           (if (> (nth i li) maxnum)
             (setq maxnum (nth i li))
           )
           (setq i (+ i 1))
         )
         (setq maxnum maxnum)
       )
       (setq maxnum 0)
     )
    );end MAXLI

    ;................   Tao list id_mc  ................
    (defun M_LISEC (id_start / diff stt_t stt sotang
                       stttang)

     ;.............  Tao ham xac dinh stt trong list  ..............
     (setq listt_mc nil)
     (setq sotang (- (length solieu1cot) 1))
     (setq stttang 1)
     (repeat sotang
         (setq diff T)
         (setq stt_t 1)
         (while
             (and
               (<  stt_t stttang)
               diff
             )
             (if (and (= (strcase (nth 1 (assoc 'hdang (nth stt_t   solieu1cot))))
                         (strcase (nth 1 (assoc 'hdang (nth stttang solieu1cot))))
                      )
                      (= (nth 1 (assoc 'hct   (nth stt_t   solieu1cot)))
                         (nth 1 (assoc 'hct   (nth stttang solieu1cot)))
                      )
                      (= (nth 1 (assoc 'hcp   (nth stt_t   solieu1cot)))
                         (nth 1 (assoc 'hcp   (nth stttang solieu1cot)))
                      )
                      (= (nth 1 (assoc 'bcd   (nth stt_t   solieu1cot)))
                         (nth 1 (assoc 'bcd   (nth stttang solieu1cot)))
                      )
                      (= (nth 1 (assoc 'bct   (nth stt_t   solieu1cot)))
                         (nth 1 (assoc 'bct   (nth stttang solieu1cot)))
                      )
                      (= (nth 1 (assoc 'sldng (nth stt_t   solieu1cot)))
                         (nth 1 (assoc 'sldng (nth stttang solieu1cot)))
                      )
                      (= (nth 1 (assoc 'sldtr (nth stt_t   solieu1cot)))
                         (nth 1 (assoc 'sldtr (nth stttang solieu1cot)))
                      )
                      (= (nth 1 (assoc 'slnng (nth stt_t   solieu1cot)))
                         (nth 1 (assoc 'slnng (nth stttang solieu1cot)))
                      )
                      (= (nth 1 (assoc 'slntr (nth stt_t   solieu1cot)))
                         (nth 1 (assoc 'slntr (nth stttang solieu1cot)))
                      )
                      (= (nth 1 (assoc 'fidng (nth stt_t   solieu1cot)))
                         (nth 1 (assoc 'fidng (nth stttang solieu1cot)))
                      )
                  )
                  (setq diff nil)
             );end if
             (setq stt_t (+ stt_t 1))
         );end while
         (if diff
             (setq listt_mc (append listt_mc (list (+ (MAXLI listt_mc) 1))))
             (setq listt_mc (append listt_mc (list (nth (- stt_t 2) listt_mc))))
         )
         (setq stttang (+ stttang 1))
     );end repeat
     (setq listt_mc
       (mapcar '(lambda (x)
                   (+ x (- id_start 1))
                )
                listt_mc 
       )
     )
     (setq listt_mc (append (list 0) listt_mc))

    );end M_LISEC


     ;=====================   START DRAWING ALL  ============================
      (setq solieu1cot (nth stt_cot so_lieu))
      (M_LISEC id_start)
      (initget 1)
      (if (setq pick_pt (getpoint "\nInsertion point: "))
          (setq insertpt pick_pt)
      )
      (setq basept insertpt)
      (setq li_count (list 0))
      (setq id_sec 1)
      (setq scale (/ 1.0 30))
      (setq scsec (/ 1.0 dscale))
      (setq dist (/ 6 scale))
      (setq sotang (- (length solieu1cot) 1) )
      (setq noipt basept)
      (setq ctr 0)
      (setq stttang 1)
      (repeat sotang 
           (DCON)
           (if (= stttang 1) (DSEC ctdpt cpdpt))
           (DSTE)
           (if (/= hdamt 0) (DSEC dtdpt dttpt))
           (if (/= hdamp 0) (DSEC dpdpt dptpt))
           (DHEIGHT)
           (DDIM)
           (GHISEC id_sec)
           (if (> (nth id_sec listt_mc) (MAXLI li_count))
             (progn
              (setq cen (polar (polar noipt (/ pi 2) (/ ccao 2)) 0 (+ hcott1 50 (* (/ hcotp1 scale) scsec)) ))
               (setq id_mc (nth id_sec listt_mc))
               (DCSEC cen id_mc)
             )
           )
           (setq
                li_count (append li_count (list (nth id_sec listt_mc)))
                noipt    (polar noipt (/ pi 2) ccao)
                ctr      (+ ctr ccao)
                stttang  (+ stttang 1)
                id_sec   (+ id_sec 1)
           ) 
      );end repeat
      (setq id_start (+ 1 (MAXLI li_count)))
      (WNAME (polar noipt (/ pi 2) 30))
      (DAXIS basept (polar basept (/ pi 2) ctr))
      (setq basept (polar basept 0 dist))
    );end DRAW

;;;----------------------------------------------------------------------------
;;; The main function.
;;;----------------------------------------------------------------------------
(defun C:COD (/
                    col_list1    picklist       pick_index  CHECK_VALID
                    dcl_id                      dscale      ADD2LIST
                    er                          id_start    GET&SET_STATUS
                                 rs_err         what_next   SET_TS 
                    COL_MAIN     fname_from_dlb             RESET_STATUS
                    UPDBOX                      len         SET_WID
                    clay         clin           cwid        SET_LAY  
                                                            MAKE_LIST
            )

   (defun SET_LAY (ten)
      (setvar "clayer" ten)
      (setvar "celtype" "bylayer")
   )
   (defun SET_WID (wid)
      (setvar "plinewid" wid)
   )
   (defun SET_TS (ten)
      (setvar "textstyle" ten)
   )
   (defun GET&SET_STATUS ()
      (setq clay (getvar "clayer"))
      (setq clin (getvar "celtype"))
      (setq cwid (getvar "plinewid"))
      (setq old_osmode (getvar "osmode"))
      (setq old_blip (getvar "blipmode"))

      (setvar "blipmode" 0)
      (setvar "osmode" 0)
      (setvar "cmdecho" 0)
      (command "undo" "begin")
   )
   (defun RESET_STATUS ()
      (setvar "clayer" clay)
      (setvar "celtype" clin)
      (setvar "plinewid" cwid)
      (setvar "osmode" old_osmode)
      (setvar "blipmode" old_blip)
      (setvar "cmdecho" old_cmd)
      (command "undo" "end")
   )
 ;;
 ;; The function reads "excute.ini" and returns a file name along with its
 ;; path. This file contains data to draw column.
 ;; 
 (defun COL_PATH (/ file path_name fname)
   (setq file (open "c:\\sapoutw\\excute.ini" "r"))
   (setq path_name (read-line file))
   (close file)
   (setq fname (strcat path_name ".soc"))
 )
  ;;
  ;; Reset the error tile.
  ;;
  (defun rs_err()
    (set_tile "error" "")
  )
  ;;
  ;; Take all columns in "solieu" into a list, using the File Dialog box and 
  ;; add columns in the file to the internal lists used for drawing.
  ;; Also update the edit_box.
  ;;
  (defun ADD2LIST (fname / READFILE i len sttcot socot linamecol colname
                           licolname1 finame
                  )

;;
;; By openning a ".soc" file whose path is shown in the "excute.ini" file,
;; the function reads this file (.soc file) and returns a list of data 
;; (called solieu) which afterwards will be referenced to get data to draw.
;;
(defun READFILE ( / STOL LTOD solieu last_colload_dir
                 listline projectname)
  ;;
  ;; The function converts a string to a list with all blanks ignored.
  ;; The created list has only itoms.
  ;;
  (defun STOL (str / len cont esc ok st first ch cha i flag
             le str j li new old found)
    (setq le (strlen str))
    (setq len le)
    (setq cont T)
    (setq i 1)
    (while (and (<= i le) cont)
        (setq ch (substr str i 1))
        (if (or (= ch " ") (= ch ","))
            (setq len (1- len))
            (setq cont nil)
        )
        (setq i (1+ i))
    )
    (if (/= len 0) (setq str (substr str (1+ (- le len)))))
    (if (/= len 0)
      (progn
        (setq li (list (list 1 nil)))
        (setq j 2)
        (while (<= j len)
            (setq li (append li (list (list j nil))))
            (setq j (1+ j))
        )
        (setq linelist nil)
        (setq ok T)
        (setq i 1)
        (while ok
            (setq found T)
            (setq st "")
            (while found
                (setq cha (substr str i 1))
                (if (and (/= cha " ") (/= cha ","))
                    (progn
                        (setq st (strcat st cha))
                        (setq old (assoc i li) new (list i T))
                        (setq li (subst new old li))
                        (setq first T)
                    )
                    (progn
                        (if (= (nth 1 (assoc (1- i) li)) T)
                            (setq first T)
                            (setq first nil)
                        )
                        (setq found nil)
                    )
                )
                (setq i (1+ i))
                (if (> i len) (setq ok nil found nil))
            )
            (if first (setq linelist (append linelist (list st))))
        )
      )
      (setq linelist nil)
    )
    (setq linelist linelist)
  );end STOL
  ;;
  ;; Takes a list which returned by STOL and returns structured list
  ;; - solieu - which afterwards will be referenced to get data to draw.
  ;;
  (defun LTOD (li / tang stttang sttcot sotang socot lisotang
               M_LISTST XLTH TIM_VT XLSTR)

   ;............   tao list so tang moi cot  ..............
   (defun M_LISTST (li / i sotang)
      (setq lisotang nil)
      (setq li_stt nil)
      (setq i 2)
      (while (< i (length li))
         (setq sotang (atoi (nth 0 (nth i li))))
         (setq lisotang (append lisotang (list sotang)))
         (setq li_stt (append li_stt (list i)))
         (setq i (+ i sotang 2))
      )
      (setq lisotang (append '(0) lisotang))
      (setq li_stt (append '(0) li_stt))
   );end M_LISTST

   ;..............   xu ly so lieu thep doc   ................
   (defun XLTH (str data / vitri+ slng fing sltr fitr)
     (defun TIM_VT (ch str / i cont sust vitri)
      (setq i 1)
      (setq cont T)
      (while cont
        (setq sust (substr str i))
        (if (= (ascii (strcase sust)) (ascii (strcase ch)))
            (setq vitri i cont nil)
        )
        (setq i (1+ i))
      )
      (setq vitri vitri)
     );end TIM_VT
     ;.....................................
     (defun XLSTR (str doan / vitrip sl fi)
       (setq vitrip (TIM_VT "P" str))
       (if (= doan 'sl)
        (setq sl (substr str 1 (1- vitrip)))
        (setq fi (substr str (1+ vitrip)))
       )
     );end XLSTR
     ;......................................
     (setq vitri+ (TIM_VT "+" str))
     (cond
        ((= data 'slng) (setq str_ (substr str 1 (- vitri+ 1)))
                        (setq slng (XLSTR str_ 'sl)))
        ((= data 'fing) (setq str_ (substr str 1 (- vitri+ 1)))
                        (setq fing (XLSTR str_ 'fi)))
        ((= data 'sltr) (setq str_ (substr str (+ vitri+ 1)))
                        (setq sltr (XLSTR str_ 'sl)))
        ((= data 'fitr) (setq str_ (substr str (+ vitri+ 1)))
                        (setq fitr (XLSTR str_ 'fi)))
     )
   );end XLTH

           ;------------  start ltod  -------------
   (M_LISTST li)
   (setq solieu nil)
   (setq socot (- (length lisotang) 1))
   (setq sttcot 1)
   (repeat socot
     (setq sotang (nth sttcot lisotang))
     (setq solieu_1cot nil)
     (setq stttang 1)
     (repeat sotang
       (setq hdang (list (list 'hdang (nth 1 (nth (+ (nth sttcot li_stt) stttang) li)))))
       (setq h_dang
          (list
            (list 'hct    (nth 2 (nth (+ (nth sttcot li_stt) stttang) li))) 
            (list 'hcp    (nth 3 (nth (+ (nth sttcot li_stt) stttang) li))) 
            (list 'bcd    (nth 4 (nth (+ (nth sttcot li_stt) stttang) li))) 
            (list 'bct    (nth 5 (nth (+ (nth sttcot li_stt) stttang) li))) 
            (list 'cc     (nth 6 (nth (+ (nth sttcot li_stt) stttang) li))) 

            (list 'sldng  (XLTH (nth 7 (nth (+ (nth sttcot li_stt) stttang) li))  'slng))
            (list 'fidng  (XLTH (nth 7 (nth (+ (nth sttcot li_stt) stttang) li))  'fing))
            (list 'sldtr  (XLTH (nth 7 (nth (+ (nth sttcot li_stt) stttang) li))  'sltr))
            (list 'fidtr  (XLTH (nth 7 (nth (+ (nth sttcot li_stt) stttang) li))  'fitr))
            (list 'slnng  (XLTH (nth 8 (nth (+ (nth sttcot li_stt) stttang) li))  'slng))
            (list 'finng  (XLTH (nth 8 (nth (+ (nth sttcot li_stt) stttang) li))  'fing))
            (list 'slntr  (XLTH (nth 8 (nth (+ (nth sttcot li_stt) stttang) li))  'sltr))
            (list 'fintr  (XLTH (nth 8 (nth (+ (nth sttcot li_stt) stttang) li))  'fitr))
            (list 'tdoc   (nth 9 (nth (+ (nth sttcot li_stt) stttang) li)))

            (list 'fidai  (substr (nth 10 (nth (+ (nth sttcot li_stt) stttang) li))  2))
            (list 'tdai   (nth 11 (nth (+ (nth sttcot li_stt) stttang) li)))

            (list 'achan  (substr (nth 12 (nth (+ (nth sttcot li_stt) stttang) li))  2))
            (list 'agiua  (substr (nth 13 (nth (+ (nth sttcot li_stt) stttang) li))  2))
            (list 'hdt    (nth 14 (nth (+ (nth sttcot li_stt) stttang) li))) 
            (list 'hdp    (nth 15 (nth (+ (nth sttcot li_stt) stttang) li)))
          )
       )
       (setq solieu_1tang (append hdang h_dang))
       (setq solieu_1cot (append solieu_1cot (list solieu_1tang)))
       (setq stttang (1+ stttang))
     )
     (setq solieu_1cot (append (list (list 'truc (nth (- (nth sttcot li_stt) 1) li))) solieu_1cot))
     (setq solieu (append solieu (list solieu_1cot)))
     (setq sttcot (+ sttcot 1))
   )
   (setq solieu (append (list (nth 0 li)) solieu))
  );end LTOD

           ;------------   start READFILE function   -------------
    (if finame
      (progn
        (setq fides (open finame "r"))
        (setq projectname (read-line fides))
        (setq listline nil)
        (setq strli T)
        (while strli
          (setq strli (read-line fides))
          (if (/= strli nil)
            (progn
               (setq list1 (STOL strli))
               (if (/= list1 nil)
                   (setq listline (append listline (list list1)))
               )
            )
          )
        )
        (close fides)
        (setq listline (append (list projectname) listline))
        (LTOD listline)
      )
    );end if
);end READFILE

    ;------------------   start ADD2LIST  function  ---------------
    (setq finame fname)
    (setq so_lieu (READFILE))
    (setq licolname1 nil)
    (setq sttcot 1)
    (setq socot (- (length so_lieu) 1))
    (while (<= sttcot socot)
       (setq colname "")
       (setq linamecol (nth 1 (assoc 'truc (nth sttcot so_lieu))))
       (setq len (length linamecol))
       (setq i 0)
       (repeat len
          (if (< i (- len 1))
            (setq colname (strcat colname (nth i linamecol) ","))
            (setq colname (strcat colname (nth i linamecol)))
          )
          (setq i (+ i 1))
       )
       (setq licolname1 (append licolname1 (list colname)))
       (setq sttcot (+ sttcot 1))
    )
    (UPDBOX)
    ;; Show the file name in the edit_box.
    (set_tile "path_name" finame)
  );end ADD2LIST
  ;;
  ;; Build and display a list in the list_box
  ;;
  (defun UPDBOX ()
    (start_list "list_column")
    (mapcar 'add_list licolname1)
    (end_list)
  )
  ;; 
  ;; Display a dialog box which show informations about the sorfwear provider.
  ;;
  (defun ABOUT ( / dcl_id)
    (setq dcl_id (load_dialog "cd.dcl"))
    (if (not (new_dialog "about_dlb" dcl_id)) (exit))
    (action_tile "a_accept" "(done_dialog 0)")
    (start_dialog)
    (unload_dialog dcl_id)
  )
  ;;
  ;; This function checks the validity of the initial number of section.
  ;; 
  (defun cod_check_pint (intp_number / pint_num)
    (setq pint_num (atoi intp_number))
    (cond
      ((or (< pint_num 0) (/= (type pint_num) 'int) )
        (set_tile "error" (strcat "Invalid initial number of section."))
        (mode_tile "id_start" 2)
;        (mode_tile "id_start" 3)
      )
      (T
        (rs_error)
        (set_tile "id_start" (itoa pint_num))
        (setq id_start pint_num)
      )
    )
  );end cod_check_pint
  ;;
  ;; This function checks the validity of the scale factor.
  ;;
  (defun cod_check_real (real_num / real_number)
    (cond
      ((or (not (setq real_number (distof real_num))) (< real_number 0))
         (set_tile "error" (strcat "Invalid scale factor."))
         (mode_tile "dscale" 2)
;         (mode_tile "dscale" 3)
      )
      (T
         (rs_error)
         (set_tile "dscale" (ai_rtos real_number))
         (setq dscale real_number)
      )
    )
  );end cod_check_real
  ;;
  ;; Clear the error message tile.
  ;;
  (defun rs_error () 
    (set_tile "error" "")
  )
  ;;
  ;; Check validity, if good then terminate the dialog, else alert a message'
  ;;
  (defun CHECK_VALID ()
    (cond 
       (not (cod_check_pint (get_tile "id_start")))
       (not (cod_check_real (get_tile "dscale")))
       (T)
    )
  )
  ;;
  ;; Figure defaults, for initial dialogue.
  ;;
  (defun cod_defaults ()
    (set_tile "dscale" (rtos dscale))
    (set_tile "id_start" (itoa id_start))
    (mode_tile "list_column" 2)
    (set_tile "list_column" pick_index)
  );end cod_defaults
  ;;
  ;; The function check user input validity, terminate dialog to draw column
  ;;
  (defun LIST_ACTION ( / index)
    (setq index $value)
    (cond
      ((/= index "")
        (setq pick_index index)
        (mode_tile "draw" 0)
        (if (= $reason 4) 
          (progn
            (mode_tile "draw" 0)
            (if (CHECK_VALID) (done_dialog 1))
          )
        )
      )
    )
  );end LIST_ACTION
  ;;
  ;; Open a data file (*.soc) from dialog box. Return a name of the 
  ;; opened file. If user cancels openning a file then returns nil.
  ;;
  (defun openfile ( / fname)
    ;; make sure the directory variable exists
    (if (= last_colload_dir nil)
        (setq last_colload_dir "")
    )
    (setq fname (getfiled "Select Soc File" last_colload_dir filetype (+ 2 8)))
    (if fname 
      (progn
        (setq last_appload_dir (car (fnsplitl fname)))
        (setq fname_from_dlb fname)
      )
      nil
    )
  );end openfile
  ;;
  ;; Let's start cod main funciton
  ;;
  (defun COL_MAIN ( / listcot filetype what_next escape
                      f_name)
    (setq listcot nil)
    (setq filetype "soc")
    (setq what_next 3)
    ;; Start the dialogue.
      (setq dcl_id (load_dialog "cd.dcl"))
      (if (not (new_dialog "cod_dlb" dcl_id)) (exit))
      ;; Set up defaults, for initial load.
      (cod_defaults)
      ;; Load the file which created by Sapoutw.
      (if (not fname_from_dlb)
          (ADD2LIST (COL_PATH))
          (ADD2LIST fname_from_dlb)
      )
      ;; Define what happens when each control is picked.  Mode_tile is
      ;; used to set focus to the next relevant action, cuts down mouse
      ;; handling in the dialogue.
      (action_tile "file"        "(rs_error)(if (setq f_name (openfile)) (ADD2LIST f_name))")
      (action_tile "id_start"    "(rs_error)(cod_check_pint $value)")
      (action_tile "dscale"      "(rs_error)(cod_check_real $value)")
      (action_tile "list_column" "(rs_error)(LIST_ACTION)")
      (action_tile "draw"        "(if (CHECK_VALID)(done_dialog 1))")
      (action_tile "close"       "(done_dialog 0)")
      (action_tile "about"       "(ABOUT)")
;      (action_tile "help"        "(help \"\" \"column_drawing_dialog\")")

      (setq what_next (start_dialog))   ; Throw up the dialogue.
      (cond
        ((= 0 what_next) (setq escape t))
        ((= 1 what_next) (DRAW (+ 1 (atoi pick_index))))
      )
    (unload_dialog dcl_id)
    (if (not escape) (COL_MAIN))
  );end COL_MAIN

  ;; Set up error function.
  (setq old_error  *error*            ; save current error function
        *error* ai_error              ; new error function
  )
  (setq
     fname_from_dlb  nil
     dscale          15
     id_start        1
     pick_index      "0"
  )
  (GET&SET_STATUS)
  (cond
     (  (not (ai_transd)))                       ; transparent OK
     (  (not (setq dcl_id (ai_dcl "cd"))))   ; is .DCL file loaded?
     (t (COL_MAIN))                              ; proceed!
  )
  (setq *error* old_error) 
  (RESET_STATUS)
  (princ)
);end COD
