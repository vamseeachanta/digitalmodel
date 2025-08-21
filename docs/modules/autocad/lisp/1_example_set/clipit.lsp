;;;    CLIPIT.LSP - Written by Randy Kintzley
;;;
;;;    Copyright (C) 1997 by Autodesk, Inc.
;;;
;;;    Permission to use, copy, modify, and distribute this software
;;;    for any purpose and without fee is hereby granted, provided
;;;    that the above copyright notice appears in all copies and
;;;    that both that copyright notice and the limited warranty and
;;;    restricted rights notice below appear in all supporting
;;;    documentation.
;;;
;;;    AUTODESK PROVIDES THIS PROGRAM "AS IS" AND WITH ALL FAULTS.
;;;    AUTODESK SPECIFICALLY DISCLAIMS ANY IMPLIED WARRANTY OF
;;;    MERCHANTABILITY OR FITNESS FOR A PARTICULAR USE.  AUTODESK, INC.
;;;    DOES NOT WARRANT THAT THE OPERATION OF THE PROGRAM WILL BE
;;;    UNINTERRUPTED OR ERROR FREE.
;;;
;;;    Use, duplication, or disclosure by the U.S. Government is subject to
;;;    restrictions set forth in FAR 52.227-19 (Commercial Computer
;;;    Software - Restricted Rights) and DFAR 252.227-7013(c)(1)(ii)
;;;    (Rights in Technical Data and Computer Software), as applicable.
;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;GLOBAL INFO.;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Functions created as result of loading file: clipit.lsp
; C_CLIPIT
; IN_BOUNDS
; WIPEOUT_CLIPIT
;
;Variables created as result of loading file: clipit.lsp
;
;Functions created as a result of executing the commands in: clipit.lsp
;
;Variables created as a result of executing the commands in: clipit.lsp
; BONUS_ALIVE
; BONUS_OLD_ERROR
; IMAGEADJUST
; #CLIPIT_RES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;GLOBAL INFO.;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;;;
;;; ===========================================================================
;;; ===================== load-time error checking ============================
;;;

  (defun ai_abort (app msg) 
     (defun *error* (s)
      (if old_error (setq *error* old_error))
      (princ)
     );defun
     (if msg
       (alert (strcat " Application error: "
                      app
                      " \n\n  "
                      msg
                      "  \n"
              )
       );alert
     );if
     ;(*error* msg)
     (exit)
  );defun ai_abort

;;; Check to see if AI_UTILS is loaded, If not, try to find it,
;;; and then try to load it.
;;;
;;; If it can't be found or it can't be loaded, then abort the
;;; loading of this file immediately, preserving the (autoload)
;;; stub function.

;runs at load time - rk.
(cond
 (  (and ai_dcl (listp ai_dcl)))          ; it's already loaded.
 (  (not (findfile "ai_utils.lsp"))                     ; find it
    (ai_abort "CLIPIT"
              (strcat "Can't locate file AI_UTILS.LSP."
                      "\n Check support directory.")
    );ai_abort
 )
 (  (eq "failed" (load "ai_utils" "failed"))            ; load it
    (ai_abort "CLIPIT" "Can't load file AI_UTILS.LSP")
 )
);cond close

(if (not (ai_acadapp))               ; defined in AI_UTILS.LSP
    (ai_abort "CLIPIT" nil)          ; a Nil <msg> supresses
);if                                 ; ai_abort's alert box dialog.

;;; ==================== end load-time operations ===========================

(defun c:clipit ( / la dxf na na2 e1 e2 p1 a n lst lst2 zflag redraw_it)

(if (and (not init_bonus_error) 
         (equal -1 (load "ac_bonus.lsp"  -1)) 
    );and
    (progn (alert "Error:\n     Cannot find AC_BONUS.LSP.")(exit))
);if
(init_bonus_error (list
                   (list   "cmdecho" 0 
                         "highlight" 0
                         "regenmode" 1
                            "osmode" 0
                         "orthomode" 0
                   )
                   T     ;flag. True means use undo for error clean up.  
                  '(while redraw_it 
                     (redraw (car redraw_it) 4) 
                     (setq redraw_it (cdr redraw_it))
                   ) 
                  );list  
);init_bonus_error


 ;local function
 (defun dxf (a b / ) (cdr (assoc a b)));defun

(princ "\nPick a POLYLINE/CIRCLE/ARC/ELLIPSE/TEXT object for clipping edge.. ")
(setq na (single_select '((-4 . "<OR") 
                           (0 . "*POLYLINE")
                           (0 . "ARC")
                           (0 . "CIRCLE")
                           (0 . "ATTDEF")
                           (0 . "TEXT")
                           (0 . "MTEXT")
                           (0 . "ELLIPSE")
                          (-4 . "OR>") 
                         )
                         T ;enable locked layer selection for boundary
         );single_select 
);setq
(if na 
    (progn

     (setq e1 (entget na))
     (setq redraw_it (list na))
     (redraw na 3)
     (princ "\nPick an IMAGE, a WIPEOUT, or an XREF/BLOCK to clip... ")
     (setq na2 (single_select '((-4 . "<OR") 
                               (0 . "IMAGE")
                               (0 . "INSERT")
                               (0 . "WIPEOUT")
                               (-4 . "OR>") 
                              )
                              nil ;dis-allow locked layer selection for object to be clipped 
              );single_select 
     );setq
     (setq redraw_it nil)
     (redraw na 4)
  (if na2
      (progn  
       (setq e2 (entget na2))
       (setvar "cmdecho" 0)
       (setvar "highlight" 1)
       (command "_.select" na na2)

       (setq a (get_clipitres))
       (command "")

       (entupd na)
       (setq lst (ep_list na a))
       (if (or (not (equal "INSERT" (dxf 0 e2)))
               (not (p_isect lst nil))
           );or
           (progn
            (if (equal (car lst) (last lst) 0.001)
                (setq lst (cdr lst))
            );if
            (if (or (equal "INSERT" (dxf 0 e2))
                    (equal "IMAGE"  (dxf 0 e2))
                );or 
                (progn 
                 (if (setq zflag (zoom_4_select lst))
                     (command "_.zoom" "_w" (car zflag) (cadr zflag))
                 );if
                 (c_clipit na2 lst)
                 (if zflag (command "_.zoom" "_p"))
                );progn then
                (wipeout_clipit na2 lst)
            );if
           );progn then valid boundary selected
           (princ "\nInvalid. Bounding entity cannot self intersect for xclip.")
       );if     
      );progn then got na2
  );if
   );progn then got na
);if
(restore_old_error)
(princ)
);defun c:clipit

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun get_clipitres ( / a n)

(if (not #clipitres)
    (progn
     (setq #clipitres 0.0);setq 
     (setq n 0)
     (while (equal #clipitres 0.0)
      (setq #clipitres (pixel_unit)
            #clipitres (rtos #clipitres 2 
                             (+ (fix (/ (getvar "luprec") 2.0)) n)
                       )
            #clipitres (atof #clipitres)
      );setq 
      (setq n (+ n 1));setq 
     );while
    );progn then
);if
(initget 2)
(setq  a (getdist 
           (strcat "\nMax allowable error distance for resolution of arc segments <"
                   (ai_rtos #clipitres) ">: "
           );strcat
         );getdist
);setq
(if (not a) 
    (setq a #clipitres)
    (setq #clipitres a) 
);if

#clipitres
);defun get_clipitres

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c_clipit ( na lst / e1 n lst2 lst3 res)


(if (equal 'PICKSET (type na))
    (setq e1 (entget (ssname na 0)));setq then it's a selection set
    (setq e1 (entget na));setq else 
);if
(if (equal "IMAGE" (cdr (assoc 0 e1)))
    (progn

;(pline (list '(62 . 2) lst))
;(print lst)
;(getstring "hey here's the boundary before proccessing")
;(entdel (entlast))

     (setq lst2 (image_bounds na)
           lst3 (pre_proccess_boundary lst lst2)
     );setq

;(pline (list '(62 . 2) lst))
;(pline (list '(62 . 3) lst2))
;(print 'lst3)
;(print lst3)
;(print '(length lst3))
;(print (length lst3))
;(getstring "hey 2 and 3")
;(entdel (entlast))
;(entdel (entlast))

     (setq n 0);setq
     (repeat (length lst3)
     (setq lst (nth n lst3))

;(setvar "cecolor" "1")
;(pline (list lst))
;(getstring "hey")
;(entdel (entlast))

     (setq res (c_clipit_sub na lst))
     (command "_.copy" na "" "0,0,0" "0,0,0")
     (setq na (entlast))
     (setq n (+ n 1));setq
     );repeat
     (entdel na) 
    );progn
    (setq res (c_clipit_sub na lst)) 
);if

res
);defun c_clipit

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c_clipit_sub ( na lst / dxf e1 lst2 a n )

 ;local function
 (defun dxf ( a b / ) (cdr (assoc a b)));defun
 (if (equal 'PICKSET (type na))
     (setq e1 (entget (ssname na 0)));setq then it's a selection set
     (setq e1 (entget na));setq else
 );if
 (if (equal "INSERT" (cdr (assoc 0 e1)))
     (command "_.xclip" na "" "_n")
     (progn
      (setq lst2 (image_bounds na)
            ;lst3 (pre_proccess_boundary lst lst2) ;12:34 PM 8/18/97
      );setq
      (setq  ;lst lst3 ;12:34 PM 8/18/97
            lst2 nil
            ;lst3 nil
      );setq
      (command "_.imageclip" na "_n") 
     );progn else
 );if
 (if (or (and (equal "{ACAD_XDICTIONARY" (cdr (assoc 102 e1)))
              (assoc 360 e1)
         );and
         (and ;(equal 4 ;@rk Took this out.
              ;       (logand 4 (dxf 70 e1))
              ;);equal
              (equal 1 (dxf 280 e1))
         );and 
     );or
     (command "_y")
 );if
 (command "_p")
 (setq n 0);setq
 (repeat (length lst)
  (setq a (nth n lst))
  (if (and ;(not (member a lst2))
           (or (not lst2)
               (not (equal (nth n lst) (last lst2) 0.000001))
           );or
      );and
      (progn
       (command a)
       (setq lst2 (append lst2 (list a)))
      );progn
  );if
  (setq n (+ n 1));setq
 );repeat

;(print '(length lst2))
;(print (length lst2))

 (if (< (length lst2) 3)
     (command nil) ;just cancel out
     (command "")
 );if

(>= (length lst2) 3);return success or failure 
);defun c_clipit_sub


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun pre_proccess_boundary ( lst lst2 / sp_inters a lst3 dst p1 p2 p3 p4
                                          n b side d c lst4 cnt lst5 lst6 j
                                          dx dy
                             )

 (defun sp_inters ( a b c d / x )
  (if (setq x (inters a b c d))
      (setq x (list x))
  );if
  x
 );defun sp_inters

(if (not (equal (car lst) (last lst) 0.0001))
    (setq lst (append lst (list (car lst))));setq then
);if
(setq    a (car lst)   ;The first point of the first segment to check
      lst3 (list a)    ;
       dst (list a)    ;dst will hold the max and min points of lst later 
        p1 (nth 0 lst2);the rectang bounds
        p2 (nth 1 lst2)
        p3 (nth 2 lst2)
        p4 (nth 3 lst2)
);setq
(setq n 1);setq
(repeat (max 0 (- (length lst) 1))
(setq    b (nth n lst)
       dst (maxminpnt (append dst (list b))) ;max and min points of lst
      side 0
);setq
 (setq d nil)
 (if (setq c (sp_inters a b p1 p2)) ;check the a-b segment against the boundary rectang
     (setq    d (append d c)
           side (+ side 1)
     );setq
 );if
 (if (setq c (sp_inters a b p2 p3))
     (setq    d (append d c)
           side (+ side 2)
     );setq
 );if
 (if (setq c (sp_inters a b p3 p4))
     (setq    d (append d c)
           side (+ side 4)
     );setq
 );if
 (if (setq c (sp_inters a b p4 p1))
     (setq    d (append d c)
           side (+ side 8)
     );setq
 );if

(if d
    (progn
     (if (> (length d) 1)
         (progn

          (if ;(equal (length d) 4) ;rk commented out and added line below 7:41 PM 8/27/97
              (> (length d) 2)
              (progn
               ;(print "44444")
               (setq d (list (car d) (caddr d)));setq
              );progn
          );if
          ;find the closest intersection and append that one first 
          (if (< (distance (car d) a) (distance (cadr d) a))
              (setq lst3 (append lst3 d))
              (setq lst3 (append lst3 (reverse d)))
          );if

          ;lst3 (a list of the resulting boundary points)
          ;lst4 is a list of sublists. Each sub-list has the form: (index, flag, side)
          ;where index is an index to lst3
          ;and flag is 0 or 1 meaning enter and exit respectively.
          ;side is a bit coded value. The sum of the bits where:
          ; crossing segment 1 is bit 1
          ; crossing segment 2 is bit 2
          ; crossing segment 3 is bit 4
          ; crossing segment 4 is bit 8
          (if (and 
                (or 
                    (not (in_bounds a lst2))
                    (equal a (car d) 0.0001)
                    (equal a (cadr d) 0.0001)
                );or
                (not (equal 0 (cadr (last lst4))))
              );and  
              (setq lst4 (append lst4 
                                 (list (list (- (length lst3) 2) 
                                             0                    ;enter
                                             side                 ;
                                       );list
                                 );list
                         );append
              );setq
          );if
          (if (and (not (in_bounds b lst2))
                   (not (equal 1 (cadr (last lst4))))
              );and  
              (setq lst4 (append lst4 
                                 (list (list (- (length lst3) 1)
                                             1                  ;exit
                                             side
                                       );list
                                 );list
                         );append
              );setq
          );if 
         );progn then passing through the rectang bounds with segment a-b
         (progn
          (setq d (car d));setq

          (setq lst3 (append lst3 (list d)));setq
          (if (and (in_bounds b lst2)
                   (or 
                       (not (in_bounds a lst2))
                       (equal a d 0.0001) 
                   );or
                   (not (equal 0 (cadr (last lst4)))) 
              );and
              (progn
               ;(print "enter")
               (setq lst4 (append lst4
                                  (list (list (- (length lst3) 1) 
                                              0           ;entering
                                              side
                                        );list
                                  );list
                          );append
               );setq
              );progn then entering
              (progn
               (if (and (in_bounds a lst2)
                        (not (in_bounds b lst2))
                        (not (equal 1 (cadr (last lst4)))) 
                   );and
                   (progn
                    ;(print "exit")
                    (setq lst4 (append lst4
                                       (list (list (- (length lst3) 1)
                                                   1                 ;exiting
                                                   side
                                             );list
                                       );list
                               );append
                    );setq 
                   );progn then exiting 
               );if
              );progn else possibly exiting
          );if
         );progn else 
     );if
    );progn then we are entering or exiting the rectang boundary
);if
(if (not (equal b (last lst3) 0.0001))
    (setq lst3 (append lst3 (list b)));setq
);if

(setq a b)
(setq n (+ n 1));setq 
);repeat
                  
(setq dst (* 2.0 (distance (car dst) (cadr dst))));setq
(if lst4
    (progn ;then possibly need to add corners of image to clip boundary loop
      
     (if (not (equal 1 (cadr (car lst4))))
         (setq lst4 (append (cdr lst4) (list (car lst4))));setq
     );if
     (if (not (equal (length lst4) (* 2 (/ (length lst4) 2)))) ;odd number of exits/entries
         (progn
          (setq lst4 (reverse (cdr (reverse lst4))));setq
         );progn then remove the last one.     
     );if    
       
     (setq n 0)
     (repeat (/ (length lst4) 2)
     (setq   b (nth n lst4)
             c (nth (+ n 1) lst4)
           cnt (length lst5)
     );setq
     (setq lst5 (append lst5
                        (add_corners (car b) (car c) lst3 lst2)
                );append
     );setq
     
     (while (< (+ cnt 1) (length lst5))
      (setq lst6 (append lst6
                         (list cnt);list
                 );append
      );setq
     (setq cnt (+ cnt 1))
     );while save a list of index's to lst5 to use for boundary overlap checking later.

     (setq j (car c));setq 
     (if (equal (+ n 2) (length lst4))
         (setq c (car lst4))
         (setq c (nth (+ n 2) lst4));setq
     );if
     (while (/= j (car c))
      (if (not (equal (last lst5) (nth j lst3) 0.0001))
          (setq lst5 (append lst5 (list (nth j lst3))));setq
      );if
      (setq j (+ j 1));setq
      (if (equal j (length lst3)) (setq j 0))
     );while
      
     (setq n (+ n 2));setq
     );repeat
    );progn then lst4
);if

(if lst5
    (progn
     (setq dx 0.0
           dy 0.0
     );setq 
     (setq n 0)
     (while (and (< (+ n 1) (length lst5))  ;make sure lst5 has non-zero extents
                 (or (equal dx 0.0 0.00001) 
                     (equal dy 0.0 0.00001) 
                 );or
            );and
     (setq dx (+ (abs (- (car (nth (+ n 1) lst5))
                         (car (nth n lst5))
                      );minus
                 );abs
                 dx
              );plus
           dy (+ (abs (- (cadr (nth (+ n 1) lst5))
                         (cadr (nth n lst5))
                      );minus
                 );abs
                 dy
              );plus
     );setq
     (setq n (+ n 1))
     );while 
     (if (or (equal dx 0.0 0.00001)
             (equal dy 0.0 0.00001)
         );or
         (setq lst3 nil
               lst5 nil
         );setq
         (progn
          (setq lst3 lst5
                lst5 nil
          );setq
          (if (and lst3
                   lst6
              );and
              (progn
               ;(getstring "hey")
               (setq lst3 (split_at_pinch2 lst3 lst6))
               ;(setq lst3 (list lst3));setq
               ;(getstring "hey2")
              );progn
              (progn
               (if lst3
                   (setq lst3 (list lst3));setq
               );if
              );progn then
          );if
         );progn else lst5 has non-zero extents
     );if

    );progn
    (progn
     (if (not (in_bounds (car lst3) lst2))
         (progn
          (if (or (point_inside (nth 0 lst2) lst3 dst)
                  (point_inside (nth 1 lst2) lst3 dst)
                  (point_inside (nth 2 lst2) lst3 dst)
                  (point_inside (nth 3 lst2) lst3 dst)
              );or
              (setq lst3 lst2);then clip boundary totally encloses the image bounds
                              ;so use image_bounds as clip boundary
              (setq lst3 nil);else clip boundary has no overlap with image so return nil
          );if
         );progn then clip boundary is not within the image_bounds
     );if 
     (if lst3
         (setq lst3 (list lst3));setq
     );if
    );progn else either totally inside or totally outside with no intersections
);if


lst3
);defun pre_proccess_boundary

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun split_at_pinch2 ( lst lst2 / n k a b x1 flag j m c d x2 x3 lst3 lst4 lst5
                       )

(setq n 0)
(repeat (max (- (length lst2) 1) 0)
(setq    k (nth n lst2)
         a (nth k lst)
         b (nth (+ k 1) lst)
        x1 (maxminpnt (list a b))
);setq
 (setq flag nil)
 (setq j (+ n 1))
 (while (and (not flag)
             (< j (length lst2))
        );and
 (setq m (nth j lst2)
       c (nth m lst)
       d (nth (+ m 1) lst) 
      x2 (maxminpnt (list c d))
      x3 (maxminpnt (append x1 x2))
 );setq
 (if (and (or (and (equal (car x3) (car x1) 0.0001)
                   (equal (cadr x3) (cadr x1) 0.0001)
              );and
              (and (equal (car x3) (car x2) 0.0001)
                   (equal (cadr x3) (cadr x2) 0.0001)
              );and
          );or
          (equal 0.0 (what_side c a b))
          (equal 0.0 (what_side d a b))
     );and
     (setq lst3 (append lst3 
                        (list (list k m))
                );append
     );setq then
 );if  
 (setq j (+ j 1));setq
 );while

(setq n (+ n 1));setq
);repeat

(setq n 0);setq
(repeat (max 0 (- (length lst3) 1))
(setq a (nth n lst3)
      b (cadr a)
      a (car a)
      c (nth (+ n 1) lst3)
      d (cadr c)
      c (car c)
);setq
(setq lst4 (append lst4
                   (list (list (+ a 1)
                               (if (not (equal (min b c) a))
                                   (min b c)
                                   b
                               );if
                         );list
                   );list
           );append
);setq
(setq n (+ n 1));setq
);repeat
(if lst3
    (progn
     (setq a (nth n lst3)
           b (cadr a)
           a (car a)
           c (car lst3)
           d (cadr c)
           c (car c)
     );setq
     (setq lst4 (append lst4
                        (list (list (max (+ a 1)
                                         (if lst4
                                             (+ (cadr (last lst4)) 1)
                                             0
                                         );if
                                    );max
                                    b
                              );list
                        );list
                        (list (list (+ b 1)
                                    c
                              );list
                        );list
                );append
     );setq 
    );progn then
);if

(if (and lst3 lst4)
    (progn
     (setq n 0)
     (repeat (length lst4)
     (setq a (nth n lst4)
           j (car a)
     );setq
     (setq lst3 nil)
     (while (not (equal j (cadr a)))
      (setq lst3 (append lst3 (list (nth j lst))));setq
     (setq j (+ j 1))
     (if (= j (length lst))
         (setq j 0)
     );if
     );while
     (setq lst3 (append lst3 (list (nth j lst))));setq
     (if (> (length lst3) 2)
         (progn
          (if (> (length lst3) 3)
              (progn
               (if (not (equal (car lst3) (last lst3)))
                   (setq lst3 (append lst3 (list (car lst3))));setq
               );if
               (setq lst3 (split_at_pinch2 lst3    ;a little recursion excursion.
                                           (list 0 (- (length lst3) 2))
                          );split_at_pinch2
               );setq
              );progn then
              (setq lst3 (list lst3))
          );if
          (setq lst5 (append lst5 lst3));setq
          ;(setq lst5 (append lst5 (list lst3)));setq

         );progn
     );if
     (setq n (+ n 1));setq
     );repeat
     (if lst5 
         (setq lst lst5)
         (setq lst (list lst))
     );if 
    );progn then
    (setq lst (list lst));setq else
);if

lst
);defun split_at_pinch2

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun add_corners ( nn jj lst3 lst2 / mxmn len n a b lst lst4
                                       ;func flag flag2 mxmn len lst lst4 
                                       ;n a b d1 d2 d3 d4
                   )

;(pline (list (cons 62 1) lst3))
;(pline (list (cons 62 2) lst2))
;(getstring "hey1")
;(entdel (entlast))
;(entdel (entlast))

(setq mxmn (list (nth nn lst3))
       len (length lst3)
);setq

(setq n nn)
(while (not (equal n jj));build a list that's a loop of points from exit to entry point
(setq    a (nth n lst3)
      mxmn (maxminpnt (append (list a) mxmn))
);setq
(if (not (equal a (last lst) 0.0001))
    (setq lst (append lst (list a)));setq
);if
(setq n (+ n 1))
(if (= n len) (setq n 0));if
);while
(setq    a (nth n lst3)
      mxmn (maxminpnt (append (list a) mxmn))
       lst (append lst (list a))
);setq

(setq mxmn (* 4.0 (distance (car mxmn) (cadr mxmn)))
         b mxmn 
);setq
(if (not (equal (car lst) (last lst) 0.0001)) ;make sure the loop is fully closed
    (setq lst (append lst (list (car lst))));setq then
);if
(setq lst2 (cdr lst2))
(setq n 0)
(repeat (length lst2)
(setq a (nth n lst2))
(if (not (point_inside a lst mxmn))
    (setq a nil)
);if
(setq lst4 (append lst4 (list a)))
(setq n (+ n 1));setq
);repeat
(setq b (list b (car lst4)))
(if (member nil lst4)
    (progn
     (while (car lst4)
      (setq lst4 (append (cdr lst4) (list (car lst4))))
     );while
     (while (and lst4
                 (not (car lst4))
            );and
      (setq lst4 (cdr lst4))
     );while
     (if (not (last lst4))
         (progn
          (setq lst4 (reverse lst4))
          (while (and lst4
                      (not (car lst4))
                 );and
           (setq lst4 (cdr lst4))
          );while
          (setq lst4 (reverse lst4))
         );progn
     );if
    );progn
    (progn
     (if (equal (length lst4) 4)
         (progn
          (while (not (equal 0
                             (what_side (nth nn lst3) (car lst4) (last lst4))
                 )    )
           (setq lst4 (append (cdr lst4) (list (car lst4))));setq
          );while
         );progn
     );if   
    );progn 
);if

(if lst4
    (progn
     (if (< (distance (nth nn lst3) (last lst4))
            (distance (nth nn lst3) (car lst4))
         )
         (setq lst4 (reverse lst4))
     );if
     (if (and (< (distance (nth jj lst3) (car lst4))
                 (distance (nth nn lst3) (car lst4))
              )
              (< (distance (nth jj lst3) (car lst4))
                 (distance (nth jj lst3) (last lst4))
              )
         );and
         (setq lst4 (reverse lst4))
     );if
    );progn then
);if
(if (not (equal (nth nn lst3) (car lst4) 0.0001))
    (setq lst4 (append (list (nth nn lst3))
                       lst4
               );append
    );setq
);if                   
(if (not (equal (nth jj lst3) (last lst4) 0.0001))
    (setq lst4 (append lst4
                       (list (nth jj lst3))
               );append
    );setq
);if

lst4
);defun add_corners

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun in_bounds ( p1 lst / n a b c)
 (setq b (car lst)
       b (list (car b) (cadr b))
       c (caddr lst)
       c (list (car c) (cadr c)) 
       a (angle b (cadr lst))
 );setq
 (if (< a 0) (setq a (+ a (* 2.0 pi))))
 (setq  a (* -1.0 a)
        c (rotate_pnt c b a)
       p1 (rotate_pnt p1 b a)
      lst (maxminpnt (list b c p1)) 
 );setq
 (and (equal (car lst) b 0.000001)
      (equal (cadr lst) c 0.000001)
 );and
);defun in_bounds

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun wipeout_clipit ( na lst / na2 la n a)
(if na
    (entdel na)
);if
(if (setq la (b_layer_locked (getvar "clayer")))
    (command "_.layer" "_un" (getvar "clayer") "")
);if
 
(command "_.pline")
(setq n 0)
(repeat (length lst)
(setq a (nth n lst));setq
(command a)
(setq n (+ n 1));setq
);repeat
(command "")

(setq na2 (entlast))
(command "_.pedit" na2 "_cl" "_x")
(command "_.wipeout" "_n" na2 "_y")
(if la
    (command "_.layer" "_lock" (getvar "clayer") "")
);if
);defun wipeout_clipit

;(princ "\nCLIPIT")
;(princ (strcat
;"\nWith CLIPIT you can use ARCS, CIRCLES or POLYLINES "
;"\nto define \"clipping boundaries\" for BLOCKS, XREFS, "
; "\nIMAGES and WIPEOUTS."
;))
(princ)





