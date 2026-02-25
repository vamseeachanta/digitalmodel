;;  -----------------------------------------------------------
;; *What you get:
;;  -----------------------------------------------------------
;; *files: TrueGear.lsp TrueGear.dcl
;;         (TrueGear.fas TrueGear.txt)-later
;;         (GearHelp.txt kln_tour.dcl kln_gear.slb)-later
;; *AutoCAD commands: TrueGear
;; *Purpose: To create true gear and rack teeth as polyline curves
;; *Main algorithms:
;;   -Golden section iteration to find angle for involute value;
;;   -Intermeshing of gear and rack teeth poins as cycle procedure;
;;
;;  -----------------------------------------------------------
;; *TrueGear LISP software
;;  Copyright (C) 2002 by Eugeny Kalney
;;
;;  e-mail: kalneyed@hotmail.com
;;
;; internationalization fixes by AAC Solutions - www.aac-solutions.cz
;;
;;  -----------------------------------------------------------
;; *Why Use TrueGear?
;;
;;  TrueGear is useful in at least two situations:
;;  1) You want to draw true tooth profile for standard gear;
;;  2) You want to draw your special tooth profile for gear;
;;  Just add your lines to file TrueGear.lsp or
;;  contact to me. Good luck!
;;
;;  -----------------------------------------------------------
;; *Put into practice:
;;  -Involute (evolvent) gears in drawings;
;;  -To look at gear and rack intermeshing;
;;  -Base for new LISP programs...
;;
;;  -----------------------------------------------------------
;; *How to Use TrueGear
;;
;;  This version of TrueGear works with AutoCAD R14 through 2002.
;;
;;  To run TrueGear, load it using AutoCAD's APPLOAD command,
;;  or type:
;; >Command:(load "TrueGear")
;;  ========------------------
;;  at the AutoCAD command prompt. Once you've loaded
;;  TrueGear.LSP, type:
;; >Command:TrueGear
;;  ========---------
;;  to run it. Then you can draw standard (metric) gear profiles.
;;
;;  TrueGear ask you to input gear values.
;;
;;  You must specify center point to define center of gear to be created.
;;
;;  -----------------------------------------------------------
;; *Disclaimer:
;;  Permission to use, copy, modify, and distribute this software
;;  for any purpose and without fee is hereby granted, provided
;;  that the above copyright notice appears in all copies and
;;  that both that copyright notice and the limited warranty and
;;  restricted rights notice below appear in all supporting
;;  documentation.
;;
;;  THIS PROGRAM IS PROVIDED "AS IS" AND WITH ALL FAULTS.  THE AUTHOR
;;  SPECIFICALLY DISCLAIMS ANY IMPLIED WARRANTY OF MERCHANTABILITY OR
;;  FITNESS FOR A PARTICULAR USE.  THE AUTHOR ALSO DOES NOT WARRANT THAT
;;  THE OPERATION OF THE PROGRAM WILL BE UNINTERRUPTED OR ERROR FREE.
;;
;;  -----------------------------------------------------------
;;  -----------------------------------------------------------
;;  TrueGear Version 1.0 Beta
;;  To create true gear tooth profile as polyline.
;;
;;  -----------------------------------------------------------
;; *31/01/2002
;;  Now available DCL interface.
;;  -----------------------------------------------------------
;; *28/01/2002
;;  Now available gear and rack profiles.
;;
;;  -----------------------------------------------------------
;;  -----------------------------------------------------------


;; ----------------------------------------------
;; Radius function of a profile involute value
;;   Takes: involute function value, base circle radius
;; Returns: current radius
;; =======
(defun ro_inv (inv Rb / ang)
  (ro_ang (ang_inv inv) Rb)
);defun

;; ----------------------------------------------
;; Involute function of a radius value
;;   Takes: current radius, base circle radius
;; Returns: involute function value
;; =======
(defun inv_ro (ro Rb / ang)
  (inv_ang (ang_ro ro Rb))
);defun

;; ----------------------------------------------
;; Involve angle function of a radius value
;;   Takes: current radius, base circle radius
;; Returns: involve angle value
;; =======
(defun fi_ro (ro Rb / )
  (* Rb (sqrt (- (/ (* Rb Rb) ro ro) 1.)))
);defun

;; ----------------------------------------------
;; Radius function of a involve angle
;;   Takes: involve angle, base circle radius
;; Returns: current radius value
;; =======
(defun ro_fi (fi Rb / )
  (* Rb (sqrt (+ 1. (* fi fi))))
);defun

;; ----------------------------------------------
;; Profile involute angle function of a radius value
;;   Takes:  Radius value, base circle radius
;; Returns: current angle
;; =======
(defun ang_ro (ro Rb / )
  (acos (/ Rb ro))
);defun

;; ----------------------------------------------
;; Radius function of a profile involute angle value
;;   Takes:  angle value, base circle radius
;; Returns: current radius
;; =======
(defun ro_ang (ang Rb / )
  (/ Rb (cos ang))
);defun

;; ----------------------------------------------
;; Involute function of a profile involute angle value
;;   Takes:  angle value
;; Returns: involute function value
;; =======
(defun inv_ang (ang / ta inv)
  (setq
    ta (/ (sin ang) (cos ang))
    inv (- ta ang)
  );setq
  inv;ret
);defun

;; ----------------------------------------------
;; Angle function of a profile involute value
;;   Takes: involute function value
;; Returns: angle in radian, exactly matching to this involute value
;; ======= Copyright (C) 2002 by Eugeny Kalney
(defun ang_inv ( inv
  /
    flag k618 dfi prec_zd prec_fi ret_fi
    fi1 fi2 fi_1 fi_2 fi_
    zd1 zd2 zd_1 zd_2 zd_
  )

;; local function
(defun zd_inv (fi / ta zd)
  (setq
    ret_fi fi
    ta (/ (sin fi) (cos fi))
    zd (- ta fi inv)
  );setq
  zd;ret
);defun

  (setq
    k618 (/ (- (sqrt 5.) 1.) 2.)
    prec_zd 1.e-7
    prec_fi 1.e-9
    flag T
    dfi (d2r 2.)
    fi1 20.
  );setq
  (cond
((and (>= inv 0.00000000E+00) (< inv 9.14494543E-04)) (setq fi1 4.))
((and (>= inv 9.14494543E-04) (< inv 7.49270544E-03)) (setq fi1 12.))
((and (>= inv 7.49270544E-03) (< inv 2.63496648E-02)) (setq fi1 20.))
((and (>= inv 2.63496648E-02) (< inv 6.63639913E-02)) (setq fi1 28.))
((and (>= inv 6.63639913E-02) (< inv 1.40967930E-01)) (setq fi1 36.))
((and (>= inv 1.40967930E-01) (< inv 2.72854474E-01)) (setq fi1 44.))
((and (>= inv 2.72854474E-01) (< inv 5.05176587E-01)) (setq fi1 52.))
  );cond
  (setq
    fi1 (d2r fi1)
    zd1 (zd_inv fi1)
    fi2 (+ fi1 dfi)
    zd2 (zd_inv fi2)
  );setq
  (if (= zd1 0.)
    (setq flag nil zd2 zd1 fi2 fi1)
  );if
  (if (= zd2 0.)
    (setq flag nil)
  );if
  (if flag (progn;
    (if (> zd1 0.)
      (progn
        (if (> zd2 zd1)
          (setq dfi (- 0. dfi));setq
        );if
        (while (> zd2 0.)
          (setq
            fi1 fi2
            fi2 (+ fi2 dfi)
            zd1 zd2
            zd2 (zd_inv fi2)
          );setq
        );while
      );progn
    );if
    (if (< zd1 0.)
      (progn
        (if (< zd2 zd1)
          (setq dfi (- 0. dfi));setq
        );if
        (while (< zd2 0.)
          (setq
            fi1 fi2
            fi2 (+ fi2 dfi)
            zd1 zd2
            zd2 (zd_inv fi2)
          );setq
        );while
      );progn
    );if
  ));if
  (if (= zd1 0.)
    (setq flag nil zd2 zd1 fi2 fi1));if
  (if (= zd2 0.)
    (setq flag nil));if

(while flag
  (setq
    fi_ (+ fi1 (* (- fi2 fi1) k618))
    zd_ (zd_inv fi_)
  );setq
  (if (>= (* zd_ zd1) 0.)
    (setq
      fi1 fi_
      zd1 zd_
    );setq
    (setq
      fi2 fi1
      zd2 zd1
      fi1 fi_
      zd1 zd_
    );setq
  );if
  (if (> (* zd1 zd2) 0.)
    (princ "*iteration error*")
  );if
  (if (< (abs zd_) prec_zd)(progn
    (setq
      flag nil
    );setq
  ));if
  (if (< (abs (- fi2 fi1)) prec_fi)(progn
    (setq
      flag nil
      fi_1 (+ fi1 (* (- fi2 fi1) k618))
      zd_1 (zd_inv fi_1)
      fi_2 (+ fi2 (* (- fi1 fi2) k618))
      zd_2 (zd_inv fi_2)
      fi_ fi_2
      zd_ zd_2
    );setq
    (if (< (abs zd_1) (abs zd_2)) (setq fi_ fi_1));if
    (if (< (abs zd1) (abs zd_1)) (setq fi_ fi1));if
    (if (< (abs zd2) (abs zd1)) (setq fi_ fi2));if
    (if (/= fi_ fi_2)
      (setq zd_ (zd_inv fi_));setq
    );if
  ));if
);while
  ret_fi;return
);defun




;;
;;  To generate COND values for angle_inv.
;;

;;  (defun c:inv_tab ( / )
;;  (defun zd_inv (fi / t zd)
;;    (setq
;;      fi (d2r fi)
;;      t (/ (sin fi) (cos fi))
;;      zd (- t fi)
;;    );setq
;;  );defun
;;    (setq
;;      fi 0.
;;    );setq
;;
;;    (while (<= fi 48.)
;;
;;  (princ (strcat
;;    "\n"
;;    "((and (>= inv "  (rtos (zd_inv fi) 1 8)
;;    ") (< inv "  (rtos (zd_inv (+ fi 8.)) 1 8)
;;    ")) (setq fi1 "  (rtos (+ fi 4))  ".))"
;;  ));princ
;;
;;      (setq fi (+ fi 8.));setq
;;    )
;;  );defun


(setq
  #invf-pos (list -1 -1); ***dialog location
  #invf-ang 0.
  #invf-Rb  1.
);setq

(defun c:invf ()
  (c_invf nil)
);defun

(defun c_invf (dcl
  / eng dialog-state dcl_id pr16
    Rb  ro ro_max
    ang     inv     fi
    ang_d   inv_d   fi_d
    ang_min inv_min fi_min
    ang_max inv_max fi_max
  )
  (setq
    ang_min 0.
    inv_min 0.
    fi_min 0.
    ang_max (d2r 89.)
    inv_max (inv_ang ang_max)
    fi_max (+ ang_max inv_max)
  );setq
  (if russ_mode
    (setq eng 0);setq
    (setq eng 1);setq
  );if
(defun check ()
  (if (< ang ang_min) (setq ang ang_min));if
  (if (< inv inv_min) (setq inv inv_min));if
  (if (< fi  fi_min)  (setq fi fi_min));if
  (if (> ang ang_max) (setq ang ang_max));if
  (if (> inv inv_max) (setq inv inv_max));if
  (if (> fi  fi_max)  (setq fi fi_max));if
  (if (<= Rb 0.)  (setq Rb 1.));if
  (if (< ro  Rb)  (setq ro Rb));if
  (if (> ro  (ro_ang ang_max Rb))  (setq ro (ro_ang ang_max Rb)));if
);defun
(defun set_tiles ()
  (setq
    ang_d (r2d ang)
    inv_d (r2d inv)
    fi_d (r2d fi)
  );setq
  (set_tile "Rb"   (rtos Rb 2 pr16))
  (set_tile "ro"   (rtos ro  2 pr16))
  (set_tile "ang"  (rtos ang 2 pr16))
  (set_tile "inv"  (rtos inv 2 pr16))
  (set_tile "fi"   (rtos fi  2 pr16))
  (set_tile "ang_d"  (rtos ang_d 2 pr16))
  (set_tile "inv_d"  (rtos inv_d 2 pr16))
  (set_tile "fi_d"   (rtos fi_d  2 pr16))
  (set_tile "eng" (itoa eng))
  (im_ImgBoot "invf_im" "kln_gear(invf)" 0)
);defun

(defun action_tiles ()
  (action_tile "accept" "(setq #invf-pos (done_dialog 1))")
  (action_tile "cancel" "(setq #invf-pos (done_dialog 0))")
  (action_tile "Rb"  "(progn (setq Rb  (atof $value)) (do-ang))")
  (action_tile "ro"  "(progn (setq ro  (atof $value)) (do-ro))")
  (action_tile "ang" "(progn (setq ang (atof $value)) (do-ang))")
  (action_tile "inv" "(progn (setq inv (atof $value)) (do-inv))")
  (action_tile "fi"  "(progn (setq fi  (atof $value)) (do-fi))")
  (action_tile "ang_d" "(progn (setq ang (d2r (atof $value))) (do-ang))")
  (action_tile "inv_d" "(progn (setq inv (d2r (atof $value))) (do-inv))")
  (action_tile "fi_d"  "(progn (setq fi  (d2r (atof $value))) (do-fi))")
  (action_tile "help" "(invf_help)")
  (action_tile "eng" "(setq #invf-pos (done_dialog 4))")
);defun
(defun invf_help ()
  (if (findfile "invfHelp.txt")
    (c_tour "invfHelp.txt")
    (progn
      (if (findfile (russ "GearHelp.txt" "GearHlp1.txt"))
        (c_tour (russ "GearHelp.txt" "GearHlp1.txt"))
        (progn;else
          (if (and russ_mode (findfile "GearHelp.txt"))
            (c_tour "GearHelp.txt")
          );if
        );progn
      );if
    );progn
  );if
);defun
(defun do-ang ()
  (check)
  (setq
    inv (inv_ang ang)
    ro (ro_ang ang Rb)
    fi (+ ang inv)
  );setq
  (set_tiles)
);defun
(defun do-inv ()
  (check)
  (setq
    ang (ang_inv inv)
    ro (ro_ang ang Rb)
    fi (+ ang inv)
  );setq
  (set_tiles)
);defun
(defun do-ro ()
  (check)
  (setq
    ang (ang_ro ro Rb)
    inv (inv_ang ang)
    fi (+ ang inv)
  );setq
  (set_tiles)
);defun
(defun do-fi ()
  (check)
  (setq
    ro (ro_fi fi Rb)
    ang (ang_ro ro Rb)
    inv (inv_ang ang)
  );setq
  (set_tiles)
);defun

  (if dcl
    (setq dcl_id dcl)
    (setq dcl_id (load_dialog "TrueGear.dcl"))
  );if
(if (>= dcl_id 0)(progn
  (setq
    ang #invf-ang
    Rb #invf-Rb
    inv (inv_ang ang)
    ro (ro_ang ang Rb)
    fi (+ ang inv)
    pr16 7
  );setq
  (setq dialog-state 100);setq
  (while (>= dialog-state 3)
    (if (not (new_dialog (russ
        "invf"
        "invf_rus"
      );russ
        dcl_id "" #invf-pos));not
      (progn (unload_dialog dcl_id) (exit))
    );if
    (set_tiles)
    (action_tiles)

    (setq dialog-state (start_dialog))
    (if (= dialog-state 4)
      (progn
        (setq
          russ_mode (not russ_mode)
        );setq
        (if russ_mode
          (setq eng 0);setq
          (setq eng 1);setq
        );if
      );progn
    );if
  );while

  (if (not dcl)
    (unload_dialog dcl_id)
  );if
  (setq
    #invf-ang ang
    #invf-Rb Rb
  );setq
));if
  (princ)
);defun
; -------------------------------
; Degrees to radians
(defun d2r (ang)
  (* pi (/ ang 180.));ret
);defun
; -------------------------------
; Radians to degrees
(defun r2d (ang)
  (* 180. (/ ang pi));ret
);defun
; -------------------------------
;; arcsine
(defun asin (x / i)
  (if (= x -1.)
    (/ pi -2.);return
    (if (= x 1.)
      (/ pi 2.);return
      (atan (/ x (sqrt (- 1. (* x x)))));return
    )
  );if
);defun
; -------------------------------
;; arccosine
(defun acos (x)
  (- (/ pi 2.) (asin x));return
);defun
; -------------------------------
;; sine takes degrees
(defun sin_d (ang)
  (sin (d2r ang));return
);defun
; -------------------------------
;; cosine takes degrees
(defun cos_d (ang)
  (cos (d2r ang));return
);defun
; -------------------------------
;; tangent takes degrees
(defun tan_d (ang)
  (setq ang (d2r ang));setq
  (/ (sin ang) (cos ang));return
);defun
; -------------------------------
;; tangent takes radians
(defun tan_r (ang)
  (/ (sin ang) (cos ang));return
);defun


(setq russ_mode nil)
;;(setq russ_mode T)
; -------------------------------
; second language supporting
;; ======= Copyright (C) 2002 by Eugeny Kalney
(defun russ (eng_str rus_str)
  (if russ_mode
    rus_str;return russian text
    eng_str;return english text
  );if
)
; -------------------------------

;;(defun input_x (msg default / val req)
;;  (if (not default) (setq default 0.));if
;;  (setq req (strcat "\n" msg " <" (rtos default 2) ">: "))
;;  (setq val (getstring req))
;;  (if (= val "")
;;    (setq val default)
;;    (setq val (atof val))
;;  )
;;  (float val);return
;;)

; -------------------------------
; makes lwpolyline from poin list ( (10 coord_x coord_y coord_z) ... )
;; ======= Copyright (C) 2002 by Eugeny Kalney
(defun make_pline (lst / elist)
  (setq elist (list
      '(0 . "LWPOLYLINE")
      '(100 . "AcDbEntity")
      '(67 . 0)
      ;;'(410 . "Model")
      (cons 8 (getvar "clayer"))
      '(100 . "AcDbPolyline")
      (cons 90 (length lst))
      '(70 . 0)
      '(43 . 0.0)
      '(38 . 0.0)
      '(39 . 0.0)
    );list
    elist (append elist lst (list (list 210 0. 0. 1.)))
  );setq
  (entmake elist)
  (princ)
);defun make_pline

;;
;; From bonus AutoCAD14 samples
;;
;; wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww\\\\
;; ERROR handle                                                     \\\\
;; wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww\\\\

(setq file_kln_erro T)
; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;
;  *** vars
;
;  kln_varlist
;  kln_alive
;  old_kln_error
;
;  *** func
;
;  init_kln_error
;  restore_kln_error
;
;  kln_error
;  k_restore_undo
;
;  k_set_sysvars
;  k_restore_sysvars
;
(setq kln_varlist nil)
(setq kln_alive 0)
(setq old_kln_error nil)
(setq kln_error1 nil)

;  EXAMPLE
;
;  init_kln_error (lst) - handler function *error*
;    (car lst) - list of system variables and it's new values
;    (cadr lst) - T/nil - use undo then terminated
;    (caddr lst) - evaluate string then terminated
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  (init_kln_error (list
;    (list
;      "cmdecho" 0
;      "osmode" (getvar "osmode")
;    )
;    T
;    '(progn
;      (alert "\nFunction terminated!")
;    )
;  ));init_kln_error(list (list sysvar1 newvalue1) T '(progn ...))
;
;  ...
;  ...
;
;  (restore_kln_error)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  Get saved value of system variable
;  (setvar "osmode" (cadr (assoc "OSMODE" (car kln_varlist))))
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  ;; temporarily restore original highlight setting.
;  (k_set_sysvars (assoc "HIGHLIGHT" (car kln_varlist)))
;  ...
;  (k_restore_sysvars)
;
; *******************************************************
; init_kln_error
; *******************************************************
(defun init_kln_error ( lst / ss)
  (defun undo_init ( / undo_ctl)
   (k_set_sysvars (list "cmdecho" 0));0
   (setq undo_ctl (getvar "undoctl"))
   (if (equal 0 (getvar "UNDOCTL"))
       (command "_.undo" "_all")
   )
   (if (or (not (equal 1 (logand 1 (getvar "UNDOCTL"))))
           (equal 2 (logand 2 (getvar "UNDOCTL")))
       );or
       (command "_.undo" "_control" "_all")
   )
   (if (equal 4 (logand 4 (getvar "undoctl")))
       (command "_.undo" "_Auto" "_off")
   )
   (while (equal 8 (logand 8 (getvar "undoctl")))
        (command "_.undo" "_end")
   );while
   (while (not (equal 8 (logand 8 (getvar "undoctl"))))
    (command "_.undo" "_begin")
   );while
   (k_restore_sysvars)
   undo_ctl
  );defun undo_init
 (setq ss (ssgetfirst))
 (if (not kln_alive)
     (setq kln_alive 0)
 );if
 (setq kln_alive (1+ kln_alive))

 (if (and (> kln_alive 1)
          (or (not (equal 'LIST (type *error*)))
              (not (equal "kln_error" (cadr *error*)))
          );or
     );and
     (progn
      (princ "\nNested Error trapping is being used incorrectly.")
      (princ "\nResetting the nested index to 1.")
      (setq     *error* kln_error
            kln_alive 0
      );setq
      (restore_kln_error)
      (setq kln_alive 1)
     );progn then
 );if
 (if (<= kln_alive 0)
     (progn
      (setq kln_alive 0)
      (restore_kln_error)
      (setq kln_alive 1)
     );progn then
 );if
 (if (= kln_alive 1)
     (progn
      (if (and *error*
               (or (not (equal 'LIST (type *error*)))
                   (not (equal "kln_error" (cadr *error*)))
               );or
          );and
          (setq old_kln_error *error*)
      );if
      (if (cadr lst)
          (setq kln_undoctl (undo_init))
          (setq kln_undoctl nil)
      );if
    );progn
 );if
 (k_set_sysvars (car lst))
 (setq kln_error1 nil)
 (if (caddr lst)
    (setq kln_error1 (caddr lst)));if
 (if (and (= kln_alive 1) (equal 'LIST (type *error*)));end
     (progn
      (setq *error* kln_error);setq
      (if (caddr lst)
          (setq *error* (append (reverse (cdr (reverse *error*)))
                                (list (caddr lst)
                                      (last *error*)
                                );list
                        );append
          );setq
      );if
     );progn
     (progn
      (if (and (>= kln_alive 1)
               (or (not (equal 'LIST (type *error*)))
                   (not (equal "kln_error" (cadr *error*)))
               );or
          );and
          (setq *error* kln_error);setq
      );if
     );progn
 );if
 (if (and ss
          (equal 1 (logand 1 (getvar "pickfirst")))
     );and
     (sssetfirst (car ss) (cadr ss))
 );if
);defun init_kln_error

; ;;
; (if (= kln_alive 1)
;   (command "_ucs" "_s" "kln_error_ucs_stored")
; );if
; (princ)
; ;;
; ;;
; (if (= kln_alive 1)
;   (command "_ucs" "_r" "kln_error_ucs_stored")
; );if
; ;;


; *******************************************************
; kln_error
; *******************************************************
(defun kln_error ( msg / )

"kln_error"

(setq kln_alive -1)
(print msg)

(while (not (equal (getvar "cmdnames") "")) (command nil))

(if kln_undoctl
    (progn
     (setvar "cmdecho" 0);0

     (while (not (wcmatch (getvar "cmdnames") "*UNDO*"))
            (command "_.undo")
     );while
     (command "_end")
     (command "_.undo" "1")
     (while (not (equal (getvar "cmdnames") ""))
      (command nil)
     );while

    );progn
);if

(if kln_error1
  (eval kln_error1));if
(setq kln_error1 nil)

(k_restore_sysvars)
(k_restore_undo)

;Restore original error handler
(if old_kln_error
    (setq *error* old_kln_error)
);if

(setq kln_alive 0)

(princ)
);defun kln_error

; *******************************************************
; restore_kln_error
; *******************************************************
;restore_kln_error
;This function should be the last thing called in a lisp
;defined command. It does a (princ) at the end for a quiet
;finish.
(defun restore_kln_error ( / )
(setq kln_alive (- kln_alive 1))
(if (>= kln_alive 0)
    (k_restore_sysvars)
    (setq kln_varlist nil)
);if
(if (<= kln_alive 0)
    (progn
     (k_restore_undo)
     (if old_kln_error
         (setq *error* old_kln_error)
     );if
    );progn then
);if
(princ)
);defun old_kln_error

; *******************************************************
; k_restore_undo
; *******************************************************
(defun k_restore_undo ()

(if kln_undoctl
    (progn
      (k_set_sysvars (list "cmdecho" 0))

      (while (equal 8 (logand 8 (getvar "undoctl")))
         (command "_.undo" "_end")
      );while

      (if (not (equal kln_undoctl (getvar "undoctl")))
          (progn
           (cond
            ((equal 0 kln_undoctl)
             (command "_.undo" "_control" "_none")
            )
            ((equal 2 (logand 2 kln_undoctl))
             (command "_.undo" "_control" "_one")
            )
           );;cond
           (if (equal 4 (logand 4 kln_undoctl))
               (command "_.undo" "_auto" "_on")
           );if

         );progn
      );if
      (if (not (equal 2 (logand 2 (getvar "undoctl"))))
          (k_restore_sysvars)
      );if
    );progn then restore undo to it's original setting
);if
(setq kln_undoctl nil)

);defun k_restore_undo


; *******************************************************
; k_set_sysvars
; *******************************************************
(defun k_set_sysvars (lst / lst2 lst3 a b n)

(setq lst3 (car kln_varlist));setq

(setq n 0)
(repeat (/ (length lst) 2)
 (setq a (strcase (nth n lst))
       b (nth (+ n 1) lst)
 );setq
 (setq lst2 (append lst2
                    (list (list a (getvar a)))
            );append
 );setq
 (if (and kln_varlist
          (not (assoc a lst3))
     );and
     (setq lst3 (append lst3
                        (list (list a (getvar a)))
                );append
     );setq
 );if

 (setvar a b)

(setq n (+ n 2));setq
);repeat
(if kln_varlist
    (setq kln_varlist (append (list lst3)
                                (cdr kln_varlist)
                                (list lst2)
                      );append
    );setq
    (setq kln_varlist (list lst2))
);if
);defun k_set_sysvars

; *******************************************************
; k_restore_sysvars
; *******************************************************
(defun k_restore_sysvars ( / lst n a b)

 (if (<= kln_alive 0)
     (setq           lst (car kln_varlist)
           kln_varlist (list lst)
     );setq
     (setq lst (last kln_varlist))
 );if

 (setq n 0);setq
 (repeat (length lst)
 (setq a (nth n lst)
       b (cadr a)
       a (car a)
 )
 (setvar a b)
 (setq n (+ n 1));setq
 );repeat
 (setq kln_varlist (reverse (cdr (reverse kln_varlist))))

);defun k_restore_sysvars
;; wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww////
;; ERROR handle                                                     ////
;; wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww////




;; wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww\\\\
;; Animation of gears intermeshing                                  \\\\
;; wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww\\\\
(setq
  rot-body1  nil
  rot-body2  nil
  rot-body3  nil
  rot-body4  nil
  rot-pt1    nil
  rot-pt2    nil
  rot-pt3    nil
  rot-pt4    nil
  rot-z1     nil
  rot-z2     nil
  rot-z3     nil
  rot-Rw1    nil
  rot-Aw     nil
  rot-i21    nil
  rot-i31    nil
  rot-sangle nil
  rot-A      nil
  rot-N      nil
  rot-text   nil ;text objects
)

; -------------------------------
; Initilizing of animate data
; ======= Copyright (C) 2002 by Eugeny Kalney
(defun c:rotini ( / echo)
  (setq echo (getvar "CMDECHO"))
  (setvar "CMDECHO" 0);0
  (c_rotini)
  (setvar "CMDECHO" echo)
  (princ)
);defun

(defun c_rotini ( / text_pt)
  (princ "\nRotors")
  (princ "\nSelect objects 1 (Rotor 1): ")
  (setq
    rot-body1 (ssget)
    rot-pt1 (getpoint "\nEnter center point of rotor 1: ")
    rot-pt1 (trans rot-pt1 1 0)
  );setq
  (princ "\nSelect objects 2 (Rotor 2): ")
  (setq rot-body2 (ssget))
  (if rot-body2
    (setq
      rot-pt2 (getpoint "\nEnter center point of rotor 2: ")
      rot-pt2 (trans rot-pt2 1 0)
      rot-Aw (distance rot-pt1 rot-pt2)
    );setq
    (setq
      rot-body2 (ssadd)
      rot-pt2 (getpoint "\nEnter radius point of rotor 1: ")
      rot-pt2 (trans rot-pt2 1 0)
      rot-Aw (distance rot-pt1 rot-pt2)
      rot-pt2 (polar rot-pt1 0. rot-Aw)
      rot-Aw (distance rot-pt1 rot-pt2)
    );setq
  );if
;;  (princ "\nSelect objects 3 (Gate rotor, or nil): ")
;;  (setq rot-body3 (ssget))
;;  (if rot-body3 (progn
;;    (setq rot-pt3 (getpoint "\nEnter center point of gate rotor 3: "))
;;    (setq rot-pt3 (trans rot-pt3 1 0))
;;  ));if
  (princ "\nSelect objects 4 (Rack, or nil):")
  (setq rot-body4 (ssget))
  (setq
    rot-z1 (inputx "\nInput z1 (Number lobes of rotor 1): " rot-z1)
  );setq
  (if rot-body2
    (setq
      rot-z2 (inputx "\nInput z2 (Number lobes of rotor 2): " rot-z2)
    );setq
    (setq rot-z2 rot-z1);setq
  );if
  (setq
    rot-i21 (- 0. (/ rot-z1 rot-z2))
  );setq
  (if rot-body3
    (setq
      rot-z3 (inputx "\nInput z3 (Number lobes of gate rotor 3): " rot-z3)
      rot-i31 (- 0. (/ rot-z1 rot-z3))
    );setq
  );if
  (if rot-body4
    (setq
      rot-Rw1 (/ (* rot-Aw rot-z1) (+ rot-z1 rot-z2))
      rot-Rw1 (inputx
        "\nInput Rw1 (Radius of rotor 1 root circle): " rot-Rw1)
      rot-pt4 (polar rot-pt1 0. rot-Rw1)
    );setq
  );if
  (setq
    rot-sangle 0.0
    rot-A 1.0
    rot-N 90
  );setq
  (princ "\nText object (nil-make new):")
  (setq rot-text (ssget))
  (if (= rot-text nil) (progn
    (setq text_pt (getpoint "\nLocate text object: "))
    (command_text_j "_BL" text_pt 7. 0.0 "0")
    (setq rot-text (ssadd (entlast)))
  ));if
  (princ)
);defun

; -------------------------------
; change angle text object
(defun rot-msg ( / en ed )
  (setq en (ssname rot-text 0))
  (setq ed (entget en))
  (setq ed
    (subst  (cons 1 (rtos rot-sangle 2 2))
      (assoc 1 ed)
      ed
    )
  )
  (entmod ed)
)
; -------------------------------
; Animating of rotors rotating
; ======= Copyright (C) 2002 by Eugeny Kalney
(defun c:rotgo ( / ans osmo)
  (kln_undo_off)
(init_kln_error
  (list
    (list
         "cmdecho" 0
          "osmode" (getvar "osmode")
          "HIGHLIGHT" 0
    )
    nil     ;flag. True means use undo for error clean up.
    '(kln_undo_on)
  );list
);init_kln_error
  ;;(princ "\nRotors")
  (setq rot-A (inputx
    (russ
      "\nStep angle A (step of rotating)"
      "\nШаг по углу A (шаг вращения)"
    );rus
    rot-A)
  );setq
  (setq brk 0)
  (while (= brk 0)
;;    (initget "Step Angle Minus Copy Regen eXit")
;;    (setq ans (getkword "\nRotate ? <Step>/Angle/Minus/Copy/Regen/eXit: "))
    (initget (russ
      "Step Angle Minus Copy Regen eXit"
      (strcat
      "Step Angle Minus Copy Regen eXit"
      " Шаг Угол Минус Копировать Освежить Выход"
      );strcat
    ));initget
    (setq ans (getkword (russ
      "\nRotate ? <Step>/Angle/Minus/Copy/Regen/eXit: "
      "\nВращение ? <S-Шаг>/A-Угол/M-Минус/C-Копировать/R-Освежить/X-Выход: "
      ));getkword
    );setq
    (cond
((= ans "Угол") (setq ans "Angle"))
((= ans "Минус") (setq ans "Minus"))
((= ans "Копировать") (setq ans "Copy"))
((= ans "Освежить") (setq ans "Regen"))
((= ans "Выход") (setq ans "eXit"))
    );cond
    (if (= ans "Angle")(progn
      (princ (strcat "\nStep angle A = " (rtos rot-A) ";"))
      (setq rot-A (inputx "\nStep angle A (step)" rot-A))
    ));if
    (if (= ans "Minus")(progn
      (setq rot-A (- 0. rot-A))
      (princ (strcat "\nStep angle A = " (rtos rot-A) ";"))
    ));if
    (if (= ans "Regen")
      (command "_.regen")
    );if
    (if (= ans "Copy") (progn
      (if rot-body1
        (command "_.copy" rot-body1 ""
          '(0. 0. 0.) '(0. 0. 0.))
      );if
      (if rot-body2
        (command "_.copy" rot-body2 ""
          '(0. 0. 0.) '(0. 0. 0.))
      );if
      (if rot-body3
        (command "_.copy" rot-body3 ""
          '(0. 0. 0.) '(0. 0. 0.))
      );if
      (if rot-body4
        (command "_.copy" rot-body4 ""
          '(0. 0. 0.) '(0. 0. 0.))
      );if
      (princ "\nObjects was duplicated.")
    ));if
    (if (= ans "eXit")
      (setq brk 1))
    (if (= brk 0) (progn
      (setq osmo (getvar "osmode"))
      (setvar "OSMODE" 0)
      (if rot-body1
        (command "_.rotate" rot-body1 ""
          (trans rot-pt1 0 1) rot-A)
      );if
      (if rot-body2
        (command "_.rotate" rot-body2 ""
          (trans rot-pt2 0 1) (* rot-i21 rot-A))
      );if
      (if rot-body3
        (command "_.rotate" rot-body3 ""
          (trans rot-pt3 0 1) (* rot-i31 rot-A))
      );if
      (if rot-body4
        (command "_.move" rot-body4 "" '(0.0 0.0 0.0)
          (list 0.0 (* rot-Rw1 (* 0.01745329252 rot-A)) 0.0))
      );if
      (setq rot-sangle (+ rot-sangle rot-A))
      (if rot-text
        (rot-msg)
      );if
      (setvar "OSMODE" osmo)
    ));if
  );while
(restore_kln_error);
  (kln_undo_on)
  (princ)
)

; -------------------------------
; Animating of rotor revolving
; ======= Copyright (C) 2002 by Eugeny Kalney
(defun c:rotrev ( / ans pt2 pt4 pt_4 brk)
  (kln_undo_off)
(init_kln_error
  (list
    (list
         "cmdecho" 0
          "osmode" 0
          "highlight" 0
    )
    nil     ;flag. True means use undo for error clean up.
    '(kln_undo_on)
  );list
);init_kln_error
  (setq rot-A (inputx
    (russ
      "\nStep angle A (step of rotating)"
      "\nШаг по углу A (шаг вращения)"
    );rus
    rot-A)
  );setq
  (setq brk 0)
  (while (= brk 0)
    (initget (russ
      "Step Angle Minus Copy Regen eXit"
      (strcat
      "Step Angle Minus Copy Regen eXit"
      " Шаг Угол Минус Копировать Освежить Выход"
      );strcat
    ));initget
    (setq ans (getkword (russ
      "\nRotate ? <Step>/Angle/Minus/Copy/Regen/eXit: "
      "\nВращение ? <S-Шаг>/A-Угол/M-Минус/C-Копировать/R-Освежить/X-Выход: "
      ));getkword
    );setq
    (cond
((= ans "Угол") (setq ans "Angle"))
((= ans "Минус") (setq ans "Minus"))
((= ans "Копировать") (setq ans "Copy"))
((= ans "Освежить") (setq ans "Regen"))
((= ans "Выход") (setq ans "eXit"))
    );cond
    (if (= ans "Angle")(progn
      (princ (strcat "\nStep angle A = " (rtos rot-A) ";"))
      (setq rot-A (inputx "\nStep angle A (step)" rot-A))
    ));if
    (if (= ans "Minus")(progn
      (setq rot-A (- 0. rot-A))
      (princ (strcat "\nStep angle A = " (rtos rot-A) ";"))
    ));if
    (if (= ans "Regen")
      (command "_.regen")
    );if
    (if (= ans "Copy") (progn
      (if rot-body2
        (command "_.copy" rot-body2 ""
          '(0. 0. 0.) '(0. 0. 0.))
      );if
      (if rot-body4
        (command "_.copy" rot-body4 ""
          '(0. 0. 0.) '(0. 0. 0.))
      );if
      (princ "\nObject was duplicated.")
    ));if
    (if (= ans "eXit")
      (setq brk 1)
    );if
    (if (= brk 0) (progn
(if rot-body2 (progn
  (setq pt2 (polar rot-pt1
    (+ (angle rot-pt1 rot-pt2) (d2r rot-A)) rot-Aw);polar
  );setq
  (command "_move" rot-body2 "" rot-pt2 pt2)
  (command "_rotate" rot-body2 ""
    (trans pt2 0 1)
    (- rot-A (* rot-i21 rot-A))
  )
  (setq rot-pt2 pt2);setq
));if
(if rot-body4 (progn
  (setq
    pt4 (polar rot-pt1
      (+ (angle rot-pt1 rot-pt4) (d2r rot-A)) rot-Rw1);polar
    pt_4 (polar rot-pt4
      (+ (angle rot-pt1 rot-pt4) (* 0.5 pi))
      (* (d2r rot-A) rot-Rw1));polar
  );setq
  (command "_.move" rot-body4 "" pt_4 pt4)
  (command "_.rotate" rot-body4 "" (trans pt4 0 1) rot-A)
  (setq rot-pt4 pt4);setq
));if
      (setq rot-sangle (+ rot-sangle rot-A))
      (if rot-text
        (rot-msg)
      );if
    ));if-brk
  );while
(restore_kln_error);
  (kln_undo_on)
  (princ)
)
;; wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww////
;; Animation of gears intermeshing                                  ////
;; wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww////

; -------------------------------
; restore previouus UNDO control mode
; ======= Copyright (C) 2002 by Eugeny Kalney
(defun kln_undo_on ( / und_kln )
  (if (not undo_kln)
    (setq undo_kln (getvar "undoctl")));if
  (setq und_kln (getvar "undoctl"))
  (if (and (/= 1 (logand und_kln 1)) (= 1 (logand undo_kln 1)))
    (command "_.undo" "_all"));if
  (if (and (/= 4 (logand und_kln 4)) (= 4 (logand undo_kln 4)))
    (command "_.undo" "_auto" "_off"));if
  (if (and (/= 8 (logand und_kln 8)) (= 8 (logand undo_kln 8)))
    (command "_.undo" "_begin"));if
);defun

; -------------------------------
; set to OFF the UNDO control mode
; ======= Copyright (C) 2002 by Eugeny Kalney
(defun kln_undo_off ()
  (setq undo_kln (getvar "undoctl"))
  (if (= 8 (logand undo_kln 8))
    (command "_.undo" "_end"));if
  (if (= 4 (logand undo_kln 4))
    (command "_.undo" "_auto" "_off"));if
  (if (= 1 (logand undo_kln 1))
    (command "_.undo" "_control" "_none"));if
)

; -------------------------------
; Compare strings using upper case
; =======
(defun eq_str (str1 str2)
  (= (strcase str1) (strcase str2))
)

; -------------------------------
; Height of current text style
; ======= Copyright (C) 2002 by Eugeny Kalney
(defun current_text_h ( / tstyle a th)
  (setq
    tstyle (getvar "TEXTSTYLE")
    a nil
    th nil
  );setq
  (while (and (= th nil) (setq a (tblnext "STYLE" (not a))))
    (if (eq_str tstyle (cdr (assoc 2 a)))
      (setq th (cdr (assoc 40 a)))
    );if
  );while
  th;return
);defun

; -------------------------------
; Create text object use justify BMT+LCR="BL","MC",...
; ======= Copyright (C) 2002 by Eugeny Kalney
(defun command_text_j (just pt h ang txt / elist)
  (if (= (current_text_h) 0.0)
    (command "_.text" "_j" just pt h ang txt)
    (progn
      (command "_.text" "_j" just pt ang txt)
      (setq elist (entget (entlast)))
      (entmod (subst (cons 40 h) (assoc 40 elist) elist))
    );progn
  );if
);defun

; -------------------------------
; Get text justify BMT+LCR="BL","MC",...
; ======= Copyright (C) 2002 by Eugeny Kalney
(defun just_text (pt1 pt2 / just)
  (setq just "_BL")
  (cond
  ;;B
  ((and (< (car pt1) (car pt2)) (< (cadr pt1) (cadr pt2))
    (setq just "_BL")))
  ((and (> (car pt1) (car pt2)) (< (cadr pt1) (cadr pt2))
    (setq just "BR")))
  ((and (= (car pt1) (car pt2)) (< (cadr pt1) (cadr pt2))
    (setq just "BC")))
  ;;T
  ((and (< (car pt1) (car pt2)) (> (cadr pt1) (cadr pt2))
    (setq just "TL")))
  ((and (> (car pt1) (car pt2)) (> (cadr pt1) (cadr pt2))
    (setq just "TR")))
  ((and (= (car pt1) (car pt2)) (> (cadr pt1) (cadr pt2))
    (setq just "TC")))
  ;;M
  ((and (< (car pt1) (car pt2)) (= (cadr pt1) (cadr pt2))
    (setq just "ML")))
  ((and (> (car pt1) (car pt2)) (= (cadr pt1) (cadr pt2))
    (setq just "MR")))
  ((and (= (car pt1) (car pt2)) (= (cadr pt1) (cadr pt2))
    (setq just "MC")))
  );cond
  just;return
)





(setq inputx_pt nil)
; -------------------------------
; input numeric value (using distance and point inputx_pt)
; ======= Copyright (C) 2002 by Eugeny Kalney
(defun inputx ( txt x / in ret chk)
  (setq chk nil)
  (setq ret 0.0)
  (while (= chk nil)
    (if (and (/= x nil)
       (or (= 'INT (type x)) (= 'REAL (type x))))
      (progn
        (setq ret x)
        (princ (strcat txt " <" (rtos (float ret)) ">: "))
      )
      (princ txt)
    )
    (initget 128)
    (if (/= inputx_pt nil)
      (setq in (getdist inputx_pt))
      (setq in (getdist))
    )
    (if (/= in nil)
      (progn
        (setq ret in)
        (setq chk T)
      )
      (if (and (/= x nil)
        (or (= 'INT (type x)) (= 'REAL (type x))))
        (setq chk T)
        (princ "\nTry again: ")
      )
    )
  );while
  (setq inputx_pt nil)
  ret ;return
)

;;***********************************************
;; Get substring from string (*-any symbols)
;; (get_param "*=param1;*=param2;*=param3;", 2)
;; returns string "param2"
;;***********************************************
(defun get_param (str n / flag noend i j ch ret)
  (setq
    noend T
    i 1
  );setq
(repeat n
  (setq
    flag T
  );setq
  (while (and flag noend)
    (setq ch (substr str i 1));setq
    (if (= ch "")
      (setq noend nil)
      (progn
        (if (= ch "=") (setq flag nil));if
        (setq i (1+ i));setq
      );progn
    );if
  );while
  ;;
  (setq
    flag T
    j 0
  );setq
  (while (and flag noend)
    (setq ch (substr str (+ i j) 1));setq
    (if (= ch "")
      (setq noend nil)
      (progn
        (if (= ch ";")
          (setq flag nil)
          (setq j (1+ j));setq
        );if
      );progn
    );if
  );while
  (if noend
    (setq
      ret (substr str i j)
      i (+ i j)
    );setq
    (setq
      ret nil
    );setq
  );if
);repeat

  ret;return

);defun

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun im_ImgBoot (Tile SlideName f_color / x y)
  (setq
    x (- (dimx_tile Tile) 1)
    y (- (dimy_tile Tile) 1)
  );setq
  (start_image Tile)
  (if (>= f_color 0)
    (fill_image 0 0 x y f_color)
  )
  (slide_image 0 0 x y SlideName)
  (if (>= f_color 0)(progn
    (vector_image 0 0 x 0 8)
    (vector_image x 0 x y 8)
    (vector_image x y 0 y 8)
    (vector_image 0 y 0 0 8)
  ));if
  (end_image)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun tour-help_file ( fname topic
  / text lin ff paragraph
    subline termline mode len errline len_err
    im col im1 col1 im2 col2 ret
  )
  (setq
    errline "### Topic find error ###"
    len_err (strlen errline)
    subline (strcat
      "### " topic " ###"
    );strcat
    len (strlen subline)
    mode T
    paragraph (append)
  );setq
  (if (setq ff (findfile fname)) (progn
    (setq ff (open ff "r"))
    (while mode
      (setq lin (read-line ff))
      (if (or
            (not lin)
            (= subline (substr lin 1 len))
            (= errline (substr lin 1 len_err))
          );or
          (setq mode nil);setq-then
      );if
      (if (not lin) (princ "**not**"));if
    );while
    (setq
      text
      (list
        (substr (read-line ff) 2)
        (substr (read-line ff) 2)
        (substr (read-line ff) 2)
      );list
    );setq
    (setq
      lin (substr (read-line ff) 2)
      im (get_param lin 1)
      col (atoi (get_param lin 2))
      text (append text (list (substr (read-line ff) 2)))
      lin (substr (read-line ff) 2)
      im1 (get_param lin 1)
      col1 (atoi (get_param lin 2))
      text (append text (list (substr (read-line ff) 2)))
      lin (substr (read-line ff) 2)
      im2 (get_param lin 1)
      col2 (atoi (get_param lin 2))
    );setq
    (setq
      termline "###"
      len (strlen termline)
      mode T
    );setq
    (while mode
      (setq lin (read-line ff))
      (if (or (not lin)
            (= termline (substr lin 1 len)));or
        (setq mode nil);setq-then
        (setq paragraph (append paragraph (list lin)));setq-else
      );if
    );while
    (setq ret
      (tour-help_topic_dcl
        text
        im col
        im1 col1
        im2 col2
        paragraph
      );c_tourhelp
    );setq
    (close ff)
  ));if
  (if (= (length paragraph) 0) (quit));if
  ret;return
)

(setq tour-help_pos1 '(406 132));'(-1 -1))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun tour-help_topic_dcl (
    text;top text list, 3 elements
    im col;top image
    im1 col1;left image
    im2 col2;right image
    paragraph;paragraph text list
  / di_dcl_id lin ret
  )

(defun prev_do ( / n)
  (setq ret (list T nil));setq
  (setq tour-help_pos1 (done_dialog 2))
)
(defun next_do ( / n)
  (setq ret (list nil T));setq
  (setq tour-help_pos1 (done_dialog 2))
)
  (setq ret nil);setq

  (setq di_dcl_id (load_dialog "kln_tour.dcl"))
  (if (< di_dcl_id 0) (exit))

  ; display dialog
  (if (not (new_dialog "tour_help_topic" di_dcl_id
    "" tour-help_pos1)) (exit));if
  (im_ImgBoot "tourlogo" im col)
  (set_tile "text1" (nth 0 text))
  (set_tile "text2" (nth 1 text))
  (set_tile "text3" (nth 2 text))
  (set_tile "tourhelp_text1" (nth 3 text))
  (set_tile "tourhelp_text2" (nth 4 text))

  (start_list "listbox")
  (foreach lin paragraph
    (add_list (strcat "  " lin))
  );while
  (end_list)

  (im_ImgBoot "tourhelp_im1" im1 col1)
  (im_ImgBoot "tourhelp_im2" im2 col2)
  (action_tile "accept" "(setq tour-help_pos1 (done_dialog 1))")
  (action_tile "prev" "(prev_do)")
  (action_tile "next" "(next_do)")

  (start_dialog)                ;returns after OK or CANCEL selected
  (unload_dialog di_dcl_id)     ;free DCL from memory
  ret;return
)

(setq tour-help_pos '(70 162));'(-1 -1))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c_tour ( fname
  / text lin ff paragraph
    subline termline mode len im col
    di_dcl_id
  )

(defun topic_do ( / n prev next what)
  (setq
    n (get_tile "listbox")
    prev T
    next T
  )
  (if (= n nil)
    (setq n 0);setq
    (setq n (atoi n));setq
  );if
  (while (or prev next)
    (if (>= n (length paragraph))
      (setq n (1- (length paragraph)));setq
    );if
    (if (< n 0)
      (setq n 0);setq
    );if
    (setq
      what (tour-help_file fname (nth n paragraph))
      prev (car what)
      next (cadr what)
    );setq
    (if prev
      (setq n (1- n));setq
    );if
    (if next
      (setq n (1+ n));setq
    );if
  );while
);defun

  (setq di_dcl_id (load_dialog "kln_tour.dcl"))
  (if (< di_dcl_id 0) (exit))

  ; display dialog
  (if (not (new_dialog "tour_help" di_dcl_id
    "" tour-help_pos)) (exit));if

  (setq
    subline "### _TOPICS_ ###"
    len (strlen subline)
    mode T
  );setq
  (if (setq ff (findfile fname)) (progn
    (setq ff (open ff "r"))
    (while mode
      (setq lin (read-line ff))
      (if (or (not lin)
          (= subline (substr lin 1 len)));or
        (setq mode nil);setq-then
      );if
    );while
    (set_tile "text1" (substr (read-line ff) 2))
    (set_tile "text2" (substr (read-line ff) 2))
    (set_tile "text3" (substr (read-line ff) 2))
    (setq
      lin (substr (read-line ff) 2)
      im (get_param lin 1)
      col (atoi (get_param lin 2))
    );setq
    (im_ImgBoot "tourlogo" im col)
    (setq
      paragraph (append)
      termline "###"
      len (strlen termline)
      mode T
    );setq
    (while mode
      (setq lin (read-line ff))
      (if (or (not lin)
            (= termline (substr lin 1 len)));or
        (setq mode nil);setq-then
        (setq paragraph (append paragraph (list lin)));setq-else
      );if
    );while
    (close ff)
  ));if

  (start_list "listbox")
  (foreach lin paragraph
    (add_list lin)
  );while
  (end_list)
  (action_tile "topic" "(topic_do)")
  (action_tile "accept" "(setq tour-help_pos (done_dialog 1))")

  (start_dialog)                ;returns after OK or CANCEL selected
  (unload_dialog di_dcl_id)     ;free DCL from memory
  (princ)

);defun

(defun c:tour ( )
    (c_tour "tour.txt")
);defun

;;(princ)

;;(princ)

(setq
  #tooth_varList nil;; ***List of vars to save and restore values
  #tooth-var_pos (list -1 -1); ***dialog location
  #tooth-w_backgr 0
);setq

;; ----------------------------------------------
;; TOOTH command to draw gear tooth profile
;; ======= Copyright (C) 2002 by Eugeny Kalney
(defun c:tooth (
  / cen fi20_ m_ fi20 m z x beta Da rack h_a h_b h_c h_ro
    Pd echo vars dcl_id dialog-state
    draw_mode draw_n Pd_flag pr16 cancel_flag
    fine pw1 pw2 pw3 pw4
    eng
  )
(init_kln_error (list (list
  "cmdecho" 0;0
  "osmode" 0
  "clayer" "0"
  "cecolor" "7"
  ) T ) ;flag. True means use undo for error clean up.
);init_kln_error

;; local func
(defun check_h_ro (h_b h_c h_ro fi20 / h_f ro_max)
  (setq
    h_f (- h_b h_c)
    ro_max (/ h_c (- 1. (sin (d2r fi20))))
  );setq
  (if (> h_ro ro_max)
    (setq h_ro ro_max);setq
  );if
  (setq
    x_max (- (/ pi 4.) (* h_b (tan_d fi20)))
    ro_max (* x_max (tan_d (/ (+ fi20 90.) 2.)))
  );setq
  (if (> h_ro ro_max)
    (setq h_ro ro_max);setq
  );if
  (if (< h_ro 0.01)
    (setq h_ro 0.01);setq
  );if
  h_ro;ret
);defun

  (setq vars #tooth_varList);setq
  (if vars
    (progn
      (setq; ***read saved values
        fi20  (nth 0 vars)
        m     (nth 1 vars)
        Pd    (nth 2 vars)
        z     (nth 3 vars)
        x     (nth 4 vars)
        beta  (nth 5 vars)
        x75   (nth 6 vars)
        h_a   (nth 7 vars)
        h_b   (nth 8 vars)
        h_c   (nth 9 vars)
        cen   (nth 10 vars)
        Da    (nth 11 vars)
        Pd_flag (nth 12 vars)
        h_ro  (nth 13 vars)
        fine (nth 14 vars)
      );setq-then
      (if Pd_flag
        (setq m (/ 1. Pd));setq
        (setq Pd (/ 1. m));setq
      );if
    );progn
    (setq; ***set default values
      fi20 20.
      m    4.
      Pd   (/ 1. m)
      z    17
      x    0.0
      beta 0.
      x75  0.75
      h_a  1.
      h_b  1.25
      h_c  0.25
      h_ro (/ (* 1. h_c) (- 1. (sin (d2r fi20))))
      cen  (list 0. 0. 0.)
      Da (+ (/ (* m z) (cos (d2r beta))) (* 2. m (+ x h_a)))
      Pd_flag nil
      fine 0
    );setq-else
  );if
  (setq
    draw_mode nil
    pr16 6
    cancel_flag nil
  );setq
  (if russ_mode
    (setq eng 0);setq
    (setq eng 1);setq
  );if
  ;;
  ; ***input values from dialog window
  (setq dcl_id (load_dialog "TrueGear.dcl"))
  (if (>= dcl_id 0)
(progn;-then
  ;;::::::::::::::::::::::::::::::;
  ;=========================================================================
  ;;
(defun data-set_tiles ()
  (set_tile "fi20"  (rtos  fi20  2 pr16))
  (set_tile "m"     (rtos  m     2 pr16))
  (set_tile "Pd"    (rtos  Pd    2 pr16))
  (set_tile "z"     (itoa  z))
  (set_tile "x"     (rtos  x     2 pr16))
  (set_tile "beta"  (rtos  beta  2 pr16))
  (set_tile "x75"   (rtos  x75   2 pr16))
  (set_tile "h_a"   (rtos  h_a   2 pr16))
  (if Pd_flag
    (mode_tile "fine" 0)
    (mode_tile "fine" 1)
  );if
  (if (and (= 1 fine) Pd_flag)
    (progn
      (set_tile "h_b" "1.2+.002*Pd")
      (set_tile "h_c" "0.2+.002*Pd")
    );progn
    (progn
      (set_tile "h_b" (rtos h_b 2 pr16))
      (set_tile "h_c" (rtos h_c 2 pr16))
    );progn
  );if
  (set_tile "h_ro"  (rtos  h_ro  2 pr16))
  (set_tile "x1_pt" (rtos  (car cen)    2 pr16))
  (set_tile "y1_pt" (rtos  (cadr cen)   2 pr16))
  (set_tile "z1_pt" (rtos  (caddr cen)  2 pr16))
  (set_tile "Da"    (rtos  Da    2 pr16))
  ;;
;;  (im_ImgBoot "tooth-01" "TrueGear(tooth-01)" 0)
  (start_list "listbox")
  (add_list (russ "Fast,  single tooth" "Быстрый, один зуб"))
  (add_list (russ "Array,  all teeth"   "Массив, все зубья"))
  (add_list (russ "Rotate,  meshing"    "Вращение, зацепление"))
  (add_list (russ "Envelope,  rack cut" "Огибание, кромка рейки"))
  (end_list)
  (set_tile "w_backgr" (itoa #tooth-w_backgr))
  (set_tile "fine" (itoa fine))
  (set_tile "eng" (itoa eng))
);defun
(defun data-accept ()
  (setq
    fi20  (atof (get_tile "fi20"))
    m     (atof (get_tile "m"))
    Pd    (atof (get_tile "Pd"))
    z     (atoi (get_tile "z"))
    fine (atoi (get_tile "fine"))
    x     (atof (get_tile "x"))
    beta  (atof (get_tile "beta"))
    x75   (atof (get_tile "x75"))
    h_a   (atof (get_tile "h_a"))
    h_b   (atof (get_tile "h_b"))
    h_c   (atof (get_tile "h_c"))
    h_ro  (atof (get_tile "h_ro"))
    cen (list
      (atof (get_tile "x1_pt"))
      (atof (get_tile "y1_pt"))
      (atof (get_tile "z1_pt"))
    );list
    Da  (atof (get_tile "Da"))
    draw_n (atoi (get_tile "listbox"))
    #tooth-w_backgr (atoi (get_tile "w_backgr"))
    #tooth-var_pos (done_dialog 1)
  );setq
  (if (and draw_n (>= draw_n 0) (<= draw_n 3))
    (setq
      draw_mode (nth draw_n (list
        "Fast"
        "Array"
        "Rotate"
        "Envelope"
      ));nth
    );setq
  );if
);defun
(defun data-action_tiles ()
  (action_tile "accept" "(data-accept)")
  (action_tile "help" "(gear_help)")
  (action_tile "cancel" "(setq #tooth-var_pos (done_dialog 0))")
  (action_tile "m"   "(data-m)")
  (action_tile "z"   "(data-Da)")
  (action_tile "x"   "(data-x)")
  (action_tile "h_a" "(data-h_a)")
  (action_tile "h_b" "(data-h_b)")
  (action_tile "h_c" "(data-h_c)")
  (action_tile "h_ro" "(data-h_ro)")
  (action_tile "fi20" "(data-fi20)")
  (action_tile "Pd"  "(data-Pd)")
  (action_tile "beta"  "(data-Da)")
  (action_tile "fine"  "(data-fine)")
  (action_tile "pick_1"
   "(setq Da (atof (get_tile \"Da\")) #tooth-var_pos (done_dialog 3))")
  (action_tile "x75" "(setq x75 (distof $value))")
  (action_tile "w_backgr" "(setq #tooth-w_backgr (atoi $value))")
  (action_tile "invf"  "(data-invf)")
  (action_tile "eng" "(setq #tooth-var_pos (done_dialog 4))")
);defun

(defun data-x ( / a m1 R Rb s_p ang_p ang_b)
  (setq
    m1 (/ m (cos_d fi20))
    R (* 0.5 m1 z); ***pitch radius
    Rb (* R (cos (d2r fi20))); ***base circle radius
    s_p (+ (* 0.5 pi m1) (* x m 2. (tan_d fi20))); ***pitch arc len
    ang_p (/ s_p R 2.); ***pitch profile angle
    ang_b (+ ang_p (inv_ro R Rb)); ***base (start) angle
  );setq
  (data-Da)
);defun

;; === *** === *** ===
(defun data-h_a (/ a)
  (setq
    h_a (atof (get_tile "h_a"))
    a (/ (* pi 0.25) (tan_d fi20))
    a (* a 0.95)
  );setq
  (if (> h_a a) (progn
    (setq h_a a);setq
    (set_tile "h_a" (rtos h_a 2 pr16))
  ));if
  (data-Da)
);defun
;; === *** === *** ===
(defun data-h_b (/ b)
  (setq
    h_b (atof (get_tile "h_b"))
    b (/ (* pi 0.25) (tan_d fi20))
    b (* b 0.95)
  );setq
  (if (> h_b b) (progn
    (setq h_b b);setq
    (set_tile "h_b" (rtos h_b 2 pr16))
  ));if
  (data-h_ro)
);defun
;; === *** === *** ===
(defun data-h_c ()
  (data-h_ro)
);defun
;; === *** === *** ===
(defun data-fi20 ()
  (setq
    fi20 (atof (get_tile "fi20"))
  );setq
  (if (> fi20 80.) (progn
    (setq fi20 80.);setq
    (set_tile "fi20" (rtos fi20 2 pr16))
  ));if
  (if (< fi20 1.) (progn
    (setq fi20 1.);setq
    (set_tile "fi20" (rtos fi20 2 pr16))
  ));if
  (data-h_ro)
);defun
(defun data-h_ro ()
  (setq
    h_b (atof (get_tile "h_b"))
    h_c (atof (get_tile "h_c"))
    h_ro (atof (get_tile "h_ro"))
    fi20 (atof (get_tile "fi20"))
    h_ro (check_h_ro h_b h_c h_ro fi20)
  );setq
  (set_tile "h_ro"  (rtos  h_ro  2 pr16))
  (data-fine)
);defun
;; === *** === *** ===
(defun gear_help ()
  (if (findfile (russ "GearHelp.txt" "GearHlp1.txt"))
    (c_tour (russ "GearHelp.txt" "GearHlp1.txt"))
    (progn;else
      (if (and russ_mode (findfile "GearHelp.txt"))
        (c_tour "GearHelp.txt")
      );if
    );progn
  );if
);defun
;; === *** === *** ===
(defun data-fine ()
  (setq
    fine (atoi (get_tile "fine"))
  );setq
  (if (and (= 1 fine) Pd_flag)
    (progn
      (setq
        h_b (+ 1.2 (* 0.002 Pd))
        h_c (- h_b 1.0)
        h_ro (check_h_ro h_b h_c h_ro fi20)
      );setq
      (set_tile "h_b" "1.2+.002*Pd")
      (set_tile "h_c" "0.2+.002*Pd")
      (set_tile "h_ro"  (rtos  h_ro  2 pr16))
    );progn
    (progn
      (setq fine 0);setq
      (set_tile "fine" (itoa fine))
      (set_tile "h_b" (rtos h_b 2 pr16))
      (set_tile "h_c" (rtos h_c 2 pr16))
    );progn
  );if
);defun
;; === *** === *** ===
(defun data-Da ()
  (setq
    m     (atof (get_tile "m"))
    z     (atoi (get_tile "z"))
    x     (atof (get_tile "x"))
    h_a   (atof (get_tile "h_a"))
    beta  (atof (get_tile "beta"))
    Da (+ (/ (* m z) (cos (d2r beta))) (* 2. m (+ x h_a)))
  );setq
  (set_tile "Da" (rtos Da 2 pr16))
);defun
;; === *** === *** ===
(defun data-m ()
  (data-Da)
  (setq
    Pd (/ 1. m)
    Pd_flag nil
    pr16 6
  );setq
  (set_tile "Pd" (rtos  Pd 2 pr16))
  (mode_tile "fine" 1)
  (data-fine)
);defun
;; === *** === *** ===
(defun data-Pd ()
  (setq
    Pd (atof (get_tile "Pd"))
    m (/ 1. Pd)
    Da (+ (/ (* m z) (cos (d2r beta))) (* 2. m (+ x h_a)))
    Pd_flag T
    pr16 4
  );setq
  (set_tile "m" (rtos m 2 pr16))
  (set_tile "Da" (rtos Da 2 pr16))
  (mode_tile "fine" 0)
);defun
;; === *** === *** ===
(defun data-cen ( / la)
  (command "_.circle" cen (/ Da 2.))
  (setq la (entlast));setq
  (princ (russ
    "\nSpecify center point of gear:"
    "\nЗадайте точку центра шестерни:"
  ));princ
  (command "_.move" la "" cen pause)
  (setq
    cen (cdr (assoc 10 (entget la)))
  );setq
  (command "_.delay" "1")
  (command "_.erase" la "")
  (if (not cen)
    (setq cen (list 0. 0. 0.));setq
  );if
  (princ "\n")
);defun
;; === *** === *** ===
(defun data-invf ( / old_russ)
  (setq old_russ russ_mode);setq
  (c_invf dcl_id)
  (if (/= russ_mode old_russ)
    (setq
      russ_mode (not russ_mode)
      #tooth-var_pos (done_dialog 4)
    );setq
  );if
);defun
;; === *** === *** === \\\\
(setq dialog-state 100);setq
(while (>= dialog-state 3)
  (if (not (new_dialog (russ
      "tooth_varlist"
      "tooth_varlist_rus"
    );russ
    dcl_id "" #tooth-var_pos));not
    (progn (unload_dialog dcl_id) (exit))
  );if
  (data-set_tiles)
  (data-action_tiles)

  (setq dialog-state (start_dialog))
  (if (= dialog-state 3);  ***cen point
    (progn
      (data-cen)
    );progn
  );if
  (if (= dialog-state 4);  ***eng language
    (progn
      (setq
        russ_mode (not russ_mode)
      );setq
      (if russ_mode
        (setq eng 0);setq
        (setq eng 1);setq
      );if
    );progn
  );if
);while
;; === *** === *** === ////
  (unload_dialog dcl_id)
  (if (= dialog-state 0);  ***cancel
    (setq cancel_flag T);setq
  );if
);progn-then
(progn;-else
  ; ***input values from command area
  (setq
    fi20 (inputx (russ
      "\nInput fi20 (pressure angle)"
      "\nInput fi20 (угол профиля рейки)"
    ) fi20)
    m (inputx (russ
      "\nInput m (module [mm], 0-pitch)"
      "\nInput m (модуль [мм], 0-pitch)"
    ) m)
  );setq
  (if (= m 0.)
    (setq
      Pd (inputx (russ
        "\nInput Pd (Diametral pitch)"
        "\nInput Pd (Диаметральный питч)"
      ) Pd)
      m (/ 1. Pd);(/ 25.4 Pd)
    );setq
  );if
  (setq
    z (fix (inputx (russ
      "\nInput N (number of teeth)"
      "\nInput z (число зубьев)"
    ) z) );fix
    x (inputx (russ
      "\nInput x (enlargement correction)"
      "\nInput x (коэффициент смещения)"
    ) x)
    cen (getpoint (strcat
      (russ
        "\nSpecify center point of gear:<"
        "\nЗадайте точку центра шестерни:<"
      );russ
      (rtos (car cen) 2 3) ","
      (rtos (cadr cen) 2 3) ","
      (rtos (caddr cen) 2 3) "> "
      ));getpoint
    Da (+ (/ (* m z) (cos (d2r beta))) (* 2. m (+ x h_a)))
    Da (inputx (russ
      "\nInput Da (Outside diameter)"
      "\nInput Da (Диаметр окружности выступов)"
    ) Da)
    beta (inputx (russ
      "\nInput beta (helix angle)"
      "\nInput beta (Угол наклона)"
    ) beta)
    x75 (inputx (russ
      "\nInput dens (Points density factor on profile, from 0.5 to 2.0) "
      "\nInput x75 (Коэффициент плотности точек профиля, от 0.5 до 2.0) "
    ) x75)
    h_ro (check_h_ro h_b h_c h_ro fi20)
  );setq
);progn-else
);if-dcl_id
(if cancel_flag
(progn
  (restore_kln_error);
  (princ (russ
    "\nFunction cancelled by user."
    "\nВыполнение прекращено пользователем."
  ));princ
);progn
(progn
  (if (not cen)
    (setq cen (list 0. 0. 0.));setq
  );if
  (setq
    fi20_ fi20
    m_ m
  );setq
  (if (/= 0. beta)
    (setq
      fi20 (r2d (atan (/ (tan_d fi20_) (cos (d2r beta)))))
      m (/ m_ (cos (d2r beta)))
    );setq
  );if
  (if (and (= 1 fine) Pd_flag)
    (setq
      h_b (+ 1.2 (* 0.002 Pd))
      h_c (- h_b 1.0)
      h_ro (check_h_ro h_b h_c h_ro fi20_)
    );setq
  );if
  (setq
    rack (list h_a h_b h_c h_ro)
  );setq
  ; ***Selection of tooth draw mode
(if (not draw_mode) (progn
  (initget (russ
    "Fast Array Rotate Envelope"
    (strcat
    "Fast Array Rotate Envelope"
    " Быстрый Массив Вращение Огибание"
    );strcat
  ));initget
  (setq draw_mode (getkword (russ
      "\nCreate gear tooth mode - <Fast>/Array/Rotate/Envelope: "
      "\nРежим создания шестерни - <F-Быстрый>/A-Массив/R-Вращение/E-Огибание: "
    ));getkword
  );setq
  (if (not draw_mode)
    (setq draw_mode "Fast");setq
  );if
  (cond
    ((= draw_mode "Быстрый") (setq draw_mode "Fast"))
    ((= draw_mode "Массив") (setq draw_mode "Array"))
    ((= draw_mode "Вращение") (setq draw_mode "Rotate"))
    ((= draw_mode "Огибающая") (setq draw_mode "Envelope"))
  );cond
));if


;;Debuging

;;(princ "\n cen      =") (princ cen       )
;;(princ "\n z        =") (princ z         )
;;(princ "\n m        =") (princ m         )
;;(princ "\n fi20     =") (princ fi20      )
;;(princ "\n m_       =") (princ m_        )
;;(princ "\n fi20_    =") (princ fi20_     )
;;(princ "\n x        =") (princ x         )
;;(princ "\n (/ Da 2.)=") (princ (/ Da 2.) )
;;(princ "\n rack     =") (princ rack      )
;;(princ "\n beta     =") (princ beta      )
;;(princ "\n draw_mode=") (princ draw_mode )
;;(princ "\n x75      =") (princ x75       )



  (draw_tooth ; ***Call function to draw tooth
    cen;  ---center point
    z;  ***number of teeth
    m;  ***module normal
    fi20;  ***rack angle normal
    m_;  ***module standard
    fi20_;  ***rack angle standard
    x;  ***correction
    (/ Da 2.);  ***outside radius
    rack; ***rack list h_a h_b h_c h_ro
    beta; ***helical angle
    draw_mode; ***draw mode
    x75; ***density
  );draw_tooth
(restore_kln_error);
  (setq
    pw1 (polar pitch (* 0.75 pi) (* 12. m))
    pw2 (polar pitch (* -0.25 pi) (* 4. m))
    pw3 (polar cen (* 0.75 pi) (* z m))
    pw4 (polar cen (* -0.25 pi) (* z m))
  );setq
  (setq echo (getvar "cmdecho"));setq
  (setvar "cmdecho" 0)
(cond
((= draw_mode "Fast") (progn
  (princ (russ
  "\nTrue involute tooth profile is created."
  "\nИстинный эвольвентный профиль зуба построен."
  ));princ
  (command "_.zoom" "_w" pw1 pw2 ^C)
  (command "_.regen")
));---
((= draw_mode "Array") (progn
  (princ (russ
  "\nTrue involute teeth array is created."
  "\nМассив истинных эвольвентных профилей зубьев построен."
  ));princ
  (command "_.zoom" "_w" pw3 pw4 ^C)
  (command "_.regen")
));---
((= draw_mode "Rotate") (progn
  (princ (russ
  "\nAnimation of the gear meshing..."
  "\nВизуализация зацепления..."
  ));princ
  (command "_.zoom" "_w" pw1 pw2 ^C)
  (command "_.regen")
  (c:rotgo)
));---
((= draw_mode "Envelope")  (progn
  (princ (russ
  "\nEnvelope of the rack curves set..."
  "\nОгибающая семейства профилей рейки..."
  ));princ
  (command "_.zoom" "_w" pw1 pw2 ^C)
  (command "_.regen")
  (c:rotrev)
));---
);cond
  (setvar "cmdecho" echo)
(setq; ***Saving the current values
  #tooth_varList
  (list
    fi20_ ;0
    m_    ;1
    Pd    ;2
    z     ;3
    x     ;4
    beta  ;5
    x75   ;6
    h_a   ;7
    h_b   ;8
    h_c   ;9
    cen   ;10
    Da    ;11
    Pd_flag ;12
    h_ro  ;13
    fine  ;14
  );list
);setq

));if-cancel_flag
  (princ)
);defun

;; ----------------------------------------------
;; draw standard gear tooth
;; ======= Copyright (C) 2002 by Eugeny Kalney
(defun draw_tooth
  (
    cen z  m fi20  m_ fi20_  x R_a rack beta draw_mode x75
  /
    h_a h_b h_c h_ro h_f noFastArray pi05 pi_05
    R Rb Ra Rf Rc s_p ang_p ro pt pts pts1
    r_1 ro_1 r_2 i n n0 la text_pt
    ;;
    p0 c_ro x_max ro_max p1 p2 p3 p4 p_1 p_2 p_3 p_4
    q1 q2 q3 q4 q_1 q_2 q_3
    inv_pt0 inv_pt1 inv_pt2 arc_pt1 arc_pt2 arc_ang
    la_pl1 la_pl2 la_arc la_arc1 la_arc2 a
    rp1 rp2 rp_1 rp_2 rp_a1 rp_a2 rp_f1 rp_f2 cos1beta
    el_a el_a1 el_a2
  )
;; local func ***Sets color of last entity
(defun la_col (col / c)
  (setq
    la (entlast)
    c col
  )
  (if (= 1 #tooth-w_backgr)
    (cond
      ((= col "3")(setq c "5"));green to blue
    );cond
    (cond
      ((= col "1")(setq c "1"));red to red(yellow)
    );cond
  );if
  (command "_.chprop" la "" "_color" c "")
);defun

;; local func
;  ***Returns rotated point at 180 degrees relative to base point
(defun ptmi_180 (pt base)
  (polar base (+ (angle base pt) pi) (distance base pt))
);defun

;; local func
;  ***Returns mirrored point using Y axis to base point
(defun ptmi_y (pt base)
  (list
    (car pt)
    (- (* 2. (cadr base)) (cadr pt))
    (caddr pt)
  );ret
);defun

;; local func
(defun pt_beta (pt)
  (list
    (car pt)
    (+ (cadr cen) (* cos1beta (- (cadr pt) (cadr cen))))
    (caddr pt)
  );ret
);defun


  ;  ***Entry point
  (setq
    pi05 (* pi 0.5)
    pi_05 (* pi -0.5)
    noFastArray (and (/= draw_mode "Fast") (/= draw_mode "Array"))
    cos1beta (/ 1. (cos (d2r beta)))
    h_a (nth 0 rack)
    h_b (nth 1 rack)
    h_c (nth 2 rack)
    h_f (- h_b h_c)
    h_ro (nth 3 rack)
    R (* 0.5 m z); ***pitch radius
    pitch (polar cen 0. R); ***pitch point
    Rb (* R (cos (d2r fi20))); ***base circle radius
    Ra (+ R (* m_ (+ x h_a))); ***outside circle radius
    Rf (+ R (* m_ (- x h_f))); ***inside circle radius using rack involute
    Rc (+ R (* m_ (- x h_f h_c))); ***inside circle radius
  );setq
  (if (> Ra R_a); ***check outside presrt and standard value
    (setq Ra R_a);setq
  );if
  (setq
    r_1 Rf
    r_2 Ra
    s_p (+ (* 0.5 pi m) (* x m_ 2. (tan_r (d2r fi20)))); ***pitch arc len
    ang_p (/ s_p R 2.); ***pitch profile angle
    ang_b (+ ang_p (inv_ro R Rb)); ***base (start) angle
  );setq
; ***init animation data
(setq
  rot-body1  (ssadd);  **gear (rotor1)
  rot-body2  nil;  **gear (rotor2)
  rot-body3  nil;  **gear (gate rotor)
  rot-body4  (ssadd); **rack
  rot-pt1    cen;  **center point of gear (rotor1)
  rot-pt2    (polar cen 0. (* 2. R)); **center point of rotor2
  rot-pt3    nil;  **center point of gate rotor
  rot-pt4    (polar cen 0. R);  **rolling point of rack
  rot-z1     z;  **number of teeth (gear, rotor1)
  rot-z2     z;  **number of teeth, rotor2
  rot-z3     nil;  **number of teeth, gate rotor
  rot-Rw1    R;  **Radius of pitch circle, rotor1
  rot-Aw     (* 2. R);  **center distance
  rot-i21    -1.;  **velocity ratio, rotor1 - rotor2
  rot-i31    nil;  **velocity ratio, rotor1 - gate rotor
  rot-sangle 0.0;  **start (current) angle value
  rot-A      0.5;  **step angle value
  rot-N      90;  **step number in cycle
  rot-text   nil ;text objects
)
; ***draw geometry
;;  (command "_.circle" cen R)   (la_col "7")
;;  (command "_.circle" cen Ra)  (la_col "3")
;;  (command "_.circle" cen Rc)  (la_col "3")
  (command "_.arc" "_c" cen
    (polar cen (* -0.25 pi) R) (polar cen (* 0.5 pi) R));command
  (la_col "7")
  (command "_.arc" "_c" cen
    (polar cen (* -0.25 pi) Ra) (polar cen (* 0.5 pi) Ra));command
  (la_col "3")
  (command "_.arc" "_c" cen
    (polar cen (* -0.25 pi) Rc) (polar cen (* 0.5 pi) Rc));command
  (la_col "3")
  (command "_.line" (polar cen pi m_) (polar cen 0. m_) "");command
  (la_col "7")
  (command "_.line" (polar cen pi05 m_) (polar cen pi_05 m_) "");command
  (la_col "7")

(if noFastArray (progn; ***more geometry
  ;;
  (setq text_pt (polar pitch pi (* 5. m)))
  (setq text_pt (polar text_pt (* 0.5 pi) (* 1.5 m)))
  (command_text_j "_BL" text_pt (* 1. m) 0.0 "0")
  (setq rot-text (ssadd (entlast)))
  ;;
  (command "_.line"  (polar cen 0. (* m_ 2.))
    (polar cen 0. R) "") (la_col "7")
;;  (command "_.circle" cen Rf)  (la_col "3")
;;  (command "_.circle" cen Rb)  (la_col "1")
  (command "_.arc" "_c" cen
    (polar cen (* -0.25 pi) Rf) (polar cen (* 0.5 pi) Rf))
  (la_col "3")
  ;;
  (command "_.arc" "_c" cen
    (polar cen (* -0.25 pi) Rb) (polar cen (* 0.5 pi) Rb))
  (la_col "1")
  ;;
  (command "_.line" (polar cen (/ pi 1. z) (* m_ 2.))
    (polar cen (/ pi 1. z) Rc) "") (la_col "3")
  (setq rot-body1 (ssadd (entlast) rot-body1));setq
  ;;
  (command "_.line" (polar cen (/ pi -1. z) (* m_ 2.))
    (polar cen (/ pi -1. z) Rc) "") (la_col "3")
  (setq rot-body1 (ssadd (entlast) rot-body1));setq
  ;;
  (command "_.line" (polar cen (* 1. ang_p) (* m_ 2.))
    (polar cen (* 1. ang_p) R) "") (la_col "3")
  (setq rot-body1 (ssadd (entlast) rot-body1));setq
  ;;
  (command "_.line" (polar cen (* -1. ang_p) (* m_ 2.))
    (polar cen (* -1. ang_p) R) "") (la_col "3")
  (setq rot-body1 (ssadd (entlast) rot-body1));setq
  ;;
  (if (= draw_mode "Rotate") (progn
    (setq
      p1 (polar cen (- 0. (d2r fi20)) Rb)
      p2 (polar p1 (- (* 0.5 pi) (d2r fi20))
            (/ (- Ra (* Rb (cos (d2r fi20)))) (sin (d2r fi20))));polar
    );setq
    (command "_.line" p1 p2 "");command
    (setq la (entlast));setq
    (la_col "1")
    (command "_.mirror" la "" cen pitch "_n")
  ));if
));if-noFastArray

;; ***Points to draw rack profile
  (setq
    p0 (polar pitch 0. (* m_ x))
    p0 (polar p0 (* 0.5 pi) (* 0.25 pi m_))
    c_ro (* m_ h_ro)
    p1 (polar p0 (- pi (d2r fi20_))
      (/
        (* m_ (- (+ h_f h_c) (* h_ro (- 1. (sin (d2r fi20_))))))
        (cos (d2r fi20_))
      ));polar
    p2 (polar p1 (- (* 0.5 pi) (d2r fi20_)) c_ro)
    p3 (polar p2 pi c_ro)
    p4 (polar p3 (* 0.5 pi)
        (- (* 0.25 m_ pi) (- (cadr p3) (cadr p0)))
      );polar
    p_1 (polar p0 (- 0. (d2r fi20_))
      (/
        (* m_ h_a)
        (cos (d2r fi20_))
      ));polar
    x_max (- (/ pi 4.) (* h_a (tan_d fi20_)))
    ro_max (* m_ (/ x_max (cos_d fi20_)))
  );setq
  (if (and (= h_a h_f)) (progn
    (setq
      ro_max (* m_ h_ro)
      p_1 (polar p0 (- 0. (d2r fi20_))
        (/
          (* m_ (- (+ h_a h_c) (* h_ro (- 1. (sin (d2r fi20_))))))
          (cos (d2r fi20_))
        ));polar
    );setq
  ));if
  (setq
    p_2 (polar p_1 (- (* -0.5 pi) (d2r fi20_)) ro_max)
    p_3 (polar p_2 0. ro_max)
    p_4 (polar p_3 (* -0.5 pi)
        (- (* 0.25 m_ pi) (- (cadr p0) (cadr p_3)))
      );polar
    q4 (ptmi_y p4 p_4)
    q3 (ptmi_y p3 p_4)
    q2 (ptmi_y p2 p_4)
    q1 (ptmi_y p1 p_4)
    q0 (ptmi_y p0 p_4)
    ;;
    q_1 (ptmi_y p_1 p_4)
    q_2 (ptmi_y p_2 p_4)
    q_3 (ptmi_y p_3 p_4)
    ;; ***dedendum and addendum arcs
    arc_pt1 (polar cen (/ pi z) Rc)
    arc_ang (/ (- (* 0.25 m pi)
      (* cos1beta (- (cadr p3) (cadr p0))) ) R)
    arc_pt2 (polar cen (- (/ pi z) arc_ang) Rc)
    ;;
    inv_pt2 (polar cen (- ang_b (inv_ro r_2 Rb)) r_2)
  );setq
  (setq
    la_arc1 nil
    la_arc2 nil
  );setq
  (if (> (- (angle cen arc_pt1) (angle cen arc_pt2)) 0.001)
    (progn
      (command "_.arc" arc_pt2 "_c" cen arc_pt1)
      (setq la_arc1 (entlast))
      (command "_.arc" (ptmi_y arc_pt1 cen) "_c" cen (ptmi_y arc_pt2 cen))
      (setq la_arc2 (entlast))
    );progn
    (progn
      (command "_.line" arc_pt2 arc_pt1 "")
      (setq la_arc1 (entlast))
      (command "_.line" (ptmi_y arc_pt1 cen) (ptmi_y arc_pt2 cen) "")
      (setq la_arc2 (entlast))
    );progn
  );if
  (setq a (angle cen inv_pt2))
  (if (and (> a 0.001) (< a pi))
    (command "_.arc" (ptmi_y inv_pt2 cen) "_c" cen inv_pt2)
    (command "_.line" (ptmi_y inv_pt2 cen) inv_pt2 "")
  );if
  (setq la_arc (entlast))
(if noFastArray (progn; ***more geometry (draw rack profile)
  (if (= beta 0.)
    (progn
      (command "_.pline"
        p4 p3
        "_a" "_ce" p2 "_a" (- 90. fi20) "_l"
        p1 p0 p_1
        "_a" "_ce" p_2 "_a" (- fi20 90.) "_l"
        p_3 p_4 q_3
        "_a" "_ce" q_2 "_a" (- fi20 90.) "_l"
        q_1 q0 q1
        "_a" "_ce" q2 "_a" (- 90. fi20) "_l"
        q3 q4
        ""
      );command
      (la_col "7")
      (setq
        la (entlast)
        rot-body4 (ssadd la rot-body4)
      )
    );progn
    (progn
(setq
  pts (append)
  i 0
  n (fix (* x75 25))
  el_a1 pi
  el_a2 (- (* 1.5 pi) (d2r fi20_))
  pts (cons (cons 10 (pt_beta p4)) pts)
  pts (cons (cons 10 (pt_beta p3)) pts)
);setq
(while (<= i n)
  (setq
    el_a (+ el_a1 (/ (* i (- el_a2 el_a1)) n))
    pt (polar p2 el_a c_ro)
    pts (cons (cons 10 (pt_beta pt)) pts)
  );setq
  (setq i (1+ i));setq
);while
(setq
  pts (cons (cons 10 (pt_beta p1)) pts)
  pts (cons (cons 10 (pt_beta p0)) pts)
  pts (cons (cons 10 (pt_beta p_1)) pts)
);setq
(setq
  i 0
  n (fix (* x75 25))
  el_a1 (- (* 0.5 pi) (d2r fi20_))
  el_a2 0.
);setq
(while (<= i n)
  (setq;el_a el_a1 el_a2
    el_a (+ el_a1 (/ (* i (- el_a2 el_a1)) n))
    pt (polar p_2 el_a ro_max)
    pts (cons (cons 10 (pt_beta pt)) pts)
  );setq
  (setq i (1+ i));setq
);while
(setq
  pts (cons (cons 10 (pt_beta p_3)) pts)
  pts (cons (cons 10 (pt_beta p_4)) pts)
  pts1 (append)
);setq
(foreach pt pts
  (setq
    pts1 (cons (cons 10 (ptmi_y (cdr pt) cen)) pts1)
  );setq
)
; ***create rack pofile as polyline
(make_pline (append pts1 pts))
(la_col "7")
(setq
  la (entlast)
  rot-body4 (ssadd la rot-body4); ***animation data (tooth)
);setq
    );progn
  );if
  (repeat 3
    (command "_.copy" la "" cen cen);command
    (setq la (entlast))
    (command "_.move" la "" cen (polar cen (* 0.5 pi) (* m pi)));command
    (setq la (entlast))
    rot-body4 (ssadd la rot-body4)
  );repeat
));if-noFastArray
  (setq
    ;; ***call function to get points of fillet curve
    pts (mesh_pts cen R p3 p2 p1 p0
      ang_b Rb beta (* m 0.001) x75
    )
    ro_1 (distance cen (cdr (car pts))); ***last point of fillet curve
    pts (cons (cons 10 arc_pt2) (reverse pts))
  );setq
  (setq
    pts (reverse pts)
    pts1 (append)
    r_1 ro_1; ***radius for first point of involute curve
  );setq
  (if (< r_1 Rb); ***chech base radius
    (setq r_1 Rb);setq
  );if
  (setq; ***first point of involute to be created
    inv_pt1 (polar cen (- ang_b (inv_ro r_1 Rb)) r_1)
    i 1
    n (fix (* x75 85))
  );setq
  (while (<= i n)
    (setq
      ro (+ r_1 (/ (* i (- r_2 r_1)) n))
      pt (polar cen (- ang_b (inv_ro ro Rb)) ro)
      pts (cons (cons 10 pt) pts)
    );setq
    (setq i (1+ i));setq
  );while
  (make_pline pts); ***create top tooth pofile as polyline
  (la_col "7")
  (setq la_pl1 (entlast))
  (foreach pt pts
    (setq
      pts1 (cons (cons 10 (ptmi_y (cdr pt) cen)) pts1)
    );setq
  )
  (make_pline pts1); ***create bottom tooth pofile as polyline
  (la_col "7")
  (setq la_pl2 (entlast))
  ;; ***to join portions of tooth profile
  (command "_.pedit" la_pl1 "_j"
    la_arc1 la_arc la_pl2 la_arc2 "" "_x"
  );command

  (setq
    la (entlast)
    la_pl1 la
    rot-body1 (ssadd la rot-body1); ***animation data (tooth)
  );setq
(if noFastArray (progn; ***more geometry (draw rack profile)
  (repeat 3
    (command "_.copy" la "" cen cen);command
    (setq la (entlast))
    (command "_.rotate" la "" cen (r2d (/ (* 2. pi) z)));command
    (setq la (entlast))
    rot-body1 (ssadd la rot-body1); ***animation data (tooth)
  );repeat
  (setq
    rp1 (polar pitch (* -0.5 pi) (* 0.5 m pi));polar
    rp2 (polar rp1 (* 0.5 pi) (* m pi 2.))
    rp_1 (polar rp1 0. (* m_ x))
    rp_2 (polar rp2 0. (* m_ x))
    rp_a1 (polar rp1 0. (* m_ (+ x h_a)))
    rp_a2 (polar rp2 0. (* m_ (+ x h_a)))
    rp_f1 (polar rp1 0. (* m_ (- x h_f)))
    rp_f2 (polar rp2 0. (* m_ (- x h_f)))
  );setq
  (command "_.line" rp1 rp2 ""); ***rack pitch line
  (setq
    rot-body4 (ssadd (entlast) rot-body4); ***animation data (rack)
  );setq
  (la_col "7"); ***rack pitch line
  (command "_.line" rp_a1 rp_a2 ""); ***rack addendum line
  (la_col "3")
  (command "_.line" rp_f1 rp_f2 ""); ***rack dedendum line
  (la_col "3")
  (if (/= x 0.) (progn
    (command "_.line" rp_1 rp_2 ""); ***rack midline
    (la_col "3")
    (setq
      la (entlast)
      rot-body4 (ssadd la rot-body4); ***animation data (rack)
    );setq
  ));if
  ;; ***base involute portion
  (setq
    inv_pt0 (polar cen ang_b Rb); ***base involute point
    i 0
    n0 (/ (* n (- r_1 Rb)) (- r_2 Rb))
    n0 (+ 2 (fix n0))
    pts0 (append)
  );setq
  (while (<= i n0)
    (setq
      ro (+ Rb (/ (* i (- r_1 Rb)) n0))
      pt (polar cen (- ang_b (inv_ro ro Rb)) ro)
      pts0 (cons (cons 10 pt) pts0)
    );setq
    (setq i (1+ i));setq
  );while
  (make_pline pts0); ***create base involute portion as polyline
  (la_col "1")
  (setq
    la (entlast)
    rot-body1 (ssadd la rot-body1); ***animation data (base involute)
  );setq
  (command "_.mirror" la "" cen pitch "_n")
  (setq
    la (entlast)
    rot-body1 (ssadd la rot-body1)
  );setq

));if-noFastArray

(if (= draw_mode "Array") (progn; ***draw all teeth
  (command "_.array" la_pl1 "" "_p" cen (fix z) 360. "_y")
));if-Array

);defun


;; ----------------------------------------------
;; make points list of gear tooth fillet curve
;; ======= Copyright (C) 2002 by Eugeny Kalney
(defun mesh_pts (cen R p1 p2 p3 p4 ang_b Rb beta inv_prec x75
  /
    i n pts ro a a1 a2 pt
    lst_x lst_y j jn jn_ n50 jn20
    x y y_ fi fi_ r1 r2 pt_ r_ cos1beta
    count1 count2
  )
  ;; ***init data values
  (setq
    count1 0
    count2 0
    n50 (fix (* x75 500))
    jn20 (fix (* x75 64))
    cos1beta (/ 1. (cos (d2r beta)))
    cen_x (car cen)
    cen_y (cadr cen)
    i 0
    n n50
    lst_x (append); ***rack x coords
    lst_y (append); ***rack x coords
    pts (append)
    ro (distance p2 p1)
    a1 (angle p2 p1)
    a2 (angle p2 p3)
  );setq
  (while (<= i n); ***rack x,y coords (arc portion)
    (setq
      a (+ a1 (/ (* i (- a2 a1)) n))
      pt (polar p2 a ro)
      lst_x (cons (- (car pt) cen_x) lst_x)
      lst_y (cons (* (- (cadr pt) cen_y) cos1beta) lst_y)
      i (1+ i)
    );setq
  );while
(if (not (equal p3 p4)) (progn; ***next portion of rack profile
  (setq
    a1 (* ro (- a2 a1))
    a2 (distance p3 p4)
    jn (+ 1 (/ (* n a2) a1))
    n50 (+ n50 jn)
    j 1
    a (angle p3 p4)
  );setq
  (while (<= j jn); ***rack x,y coords (straight line portion)
    (setq
      ro (/ (* j a2) jn)
      pt (polar p3 a ro)
      lst_x (cons (- (car pt) cen_x) lst_x)
      lst_y (cons (* (- (cadr pt) cen_y) cos1beta) lst_y)
      j (1+ j)
    );setq
  );while
));if
  (setq
    lst_x (reverse lst_x)
    lst_y (reverse lst_y)
    j 1
    jn jn20
    jn_ (fix (* 0.4 jn))
    r1 (- (car p1) cen_x)
    r2 (- (car p4) cen_x)
  );setq
  (princ (russ
  "\nWait... \n... "
  "\nЖдите... \n... "
  ));princ
  (while (< j jn)
    (if (<= j jn_); ***adjust step distance for fillet curve points
      (setq
        r_ (/ (float j) jn)
        r_ (expt r_ 1.5)
        r_ (+ r1 (* r_ (- r2 r1)))
        ro r_; ***radius
      );setq
      (setq
        ro (+ r_ (* (/ (float (- j jn_)) (- jn jn_)) (- r2 r_)))
      );setq
    );if
    (setq
      fi (* 2.25 pi); ***start angle value
      i 0
      n n50
    );setq
    (while (<= i n); find true value of angle
      (setq
        x (nth i lst_x)
      );setq
      (if (>= ro x)
      (progn
        (setq
          y (nth i lst_y)
          y_ (sqrt (abs (- (* ro ro) (* x x))))
          a (atan y_ x)
          y_ (- 0. y_)
          a (- 0. a)
          fi_ (+ (/ (- y y_) R) a)
          ;;fi_ a - contact line angle
        );setq
        (if (>= fi fi_); ***check intermeshing
           (setq fi fi_);setq
        );if
      );if-then
        (setq i n);setq-else (terminate finding)
      );if
      (setq
        i (1+ i)
        count1 (1+  count1)
      );setq
      (if (= 1000 count1)
        (setq
          count1 0
          count2 (1+  count2)
        );setq
      );if
    );while
    ;; add true fillet curve point
    (setq
      pt (polar cen fi ro)
      pts (cons (cons 10 pt) pts)
      j (1+ j)
    );setq
    (if (>= ro Rb) (progn; ***check involute and fillet curves distance
      (setq
        pt_ (polar cen (- ang_b (inv_ro ro Rb)) ro)
      );setq
      (if (< (distance pt pt_) inv_prec)
        (setq j jn);setq (break cycle j)
      );if
    ));if
  );while
  (princ (russ
  "\nOk! Statistic: "
  "\nOk! Статистика: "
  ))
  (if (> count2 0) (progn
    (princ (strcat (itoa count2) ","))
    (if (< count1 100) (princ "0"));if
    (if (< count1 10) (princ "0"));if
  ));if
  (princ (strcat (itoa count1) (russ
  " cycles was calculated to obtain fillet curve points."
  " циклов было посчитано для отыскания точек огибающей."
  )));princ

  pts;ret
);defun

(princ (russ
(strcat
"\n TrueGear LISP coftware Copyright (C) 2002 Eugeny Kalney"
"\n Use command INVF to calculate values of involute function. "
"\n Use command ROTINI to select objects for animation then ROTGO or ROTREV "
"\n to look at animation of created gear and generating rack meshing;"
"\n Type first TOOTH to create gear tooth profile."
);strcat
(strcat
"\n TrueGear LISP coftware Copyright (C) 2002 Eugeny Kalney"
"\n Используйте команду INVF для расчета значений функции инволюты. "
"\n Используйте команду ROTINI для задания объектов, затем ROTGO или ROTREV "
"\n для визуализации зацепления зубьев отрисованной шестерни и производящей рейки;"
"\n Прежде наберите TOOTH для создания кривой профиля зуба шестерни."
);strcat
));princ

(princ)
