;;;
;;;    EXTRIM.LSP - Written by Randy Kintzley 
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
;Functions created as result of loading file: extrim.lsp
; ANOTHER_OFFSET
; ETRIM
; GET_FENCE_POINTS
; INTERSECT_CHECK
; TRUNCATE_2_VIEW
;
;Variables created as result of loading file: extrim.lsp
;
;Functions created as a result of executing the commands in: extrim.lsp
;
;Variables created as a result of executing the commands in: extrim.lsp
; BONUS_ALIVE
; BONUS_OLD_ERROR
;;;;;;;;;;;;;;;;;;;;;;;;;;;;GLOBAL INFO.;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Extended-TRIM - cookie-cutter routine
;
;Select a polyline, line, circle or arc and a side to trim on
;
(defun c:extrim ( / dxf na e1 p1 redraw_it lst n )

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
                           "ucsicon" 0
                        "offsetdist" 0
                            "attreq" 0 
                          "plinewid" 0
                         "plinetype" 1
                          "gridmode" 0
                           "celtype" "CONTINUOUS"
                   )
                   T     ;flag. True means use undo for error clean up.  
                   '(if redraw_it (redraw na 4))
                  );list  
);init_bonus_error

 ;local function
 (defun dxf (a b / ) (cdr (assoc a b)));defun

(princ "\nPick a POLYLINE, LINE, CIRCLE, ARC, ELLIPSE, IMAGE or TEXT for cutting edge..")
(setq na (single_select '((-4 . "<OR")
                           (0 . "CIRCLE")
                           (0 . "ARC")
                           (0 . "LINE")
                           (0 . "ELLIPSE")
                           (0 . "ATTDEF")
                           (0 . "TEXT")
                           (0 . "MTEXT")
                           (0 . "IMAGE")
                           (0 . "LWPOLYLINE")
                           (-4 . "<AND")
                            (0 . "POLYLINE")
                            (-4 . "<NOT")
                              (-4 . "&")  
                              (70 . 112)  
                            (-4 . "NOT>")
                           (-4 . "AND>")
                          (-4 . "OR>")
                         )
                         T
         );single_select
);setq
(if na 
    (progn
     (setq e1 (entget na));;setq
     (if (or (equal "TEXT" (cdr (assoc 0 e1)))
             (equal "MTEXT" (cdr (assoc 0 e1))) 
             (equal "ATTDEF" (cdr (assoc 0 e1)))
             (equal "IMAGE" (cdr (assoc 0 e1)))
         );or
         (progn
          (setq lst (ep_list na nil))
          (setq n 0)
          (command "_.pline")
          (repeat (length lst) 
          (command (nth n lst))
          (setq n (+ n 1));setq
          );repeat  
          (command "_cl")
          (setq na (entlast)
                e1 na
          );setq
         );progn then draw a temp pline to be the cutting edge.
         (setq e1 nil)
     );if 
     (redraw na 3)
     (setq redraw_it T)

     (setq p1 (getpoint "\nPick the side to trim on:"));setq
     (redraw na 4)
     (setq redraw_it nil)

     (if p1 (etrim na p1));if
     (if e1
         (progn
          (if (setq p1 (b_layer_locked (getvar "clayer")))
              (command "_.layer" "_un" (getvar "clayer") "")
          );if
          (entdel e1)
          (if p1
              (command "_.layer" "_lock" (getvar "clayer") "")
          );if
         );progn
     );if
    );progn
);if

(restore_old_error)
(princ)
);defun c:extrim

;!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;Entity-TRIM function
;takes: na - entity name
;	 a - a point, the side to trim on
;NOTE: This function does not allow for the possible miss of
;      non-continuous linetypes. 
;
(defun etrim ( na a / la dxf b d e1 lst lst2 n j k m ss na2 na3 na4 
                      x y z flag flag2 flag3 
             )

 ;local function
 (defun dxf (a b / ) (cdr (assoc a b)));defun

(setq e1 (entget na));setq
(if (or (setq flag (equal (dxf 0 e1) "POLYLINE"))
        (setq flag (equal (dxf 0 e1) "LWPOLYLINE"))
        (equal (dxf 0 e1) "LINE")
        (equal (dxf 0 e1) "CIRCLE")
        (equal (dxf 0 e1) "ARC")
        (equal (dxf 0 e1) "ELLIPSE")
        (equal (dxf 0 e1) "TEXT")
        (equal (dxf 0 e1) "ATTDEF")
        (equal (dxf 0 e1) "MTEXT")
    );or
    (progn
     (if (and flag
              (equal 8 (logand 8 (dxf 70 e1)))
         );and
         (setq flag nil)
     );if
     (setq a (trans a 1 0));setq
     (command "_.ucs" "_View")
                               
     (setq lst (ep_list na nil)  ;;;find extents of selected cutting edge object
           lst (maxminpnt lst)
             x (- (car (cadr lst)) (car (car lst)))
             y (- (cadr (cadr lst)) (cadr (car lst)))
             x (* 0.075 x)
             y (* 0.075 y)
             z (list x y)
             x (list (+ (car (cadr lst)) (car z))
                     (+ (cadr (cadr lst)) (cadr z))
               );list
             y (list (- (car (car lst)) (car z))
                     (- (cadr (car lst)) (cadr z))   
               );list
     );setq
     (command "_.zoom" "_w" x y)
     (entupd na)                  ;;;update the ent. so it's curves display smoothly

     (setq lst (ep_list na 
                       (/ (pixel_unit) 2.0)
               )
     );setq
     (if (or (not flag)
             (not (p_isect lst nil))
         );or 
         (progn             ;then the object is valid and not a self intersecting polyline.
          (if (and flag
                   (equal (car lst) (last lst) 0.0001)
              );and
              (setq flag3 T);then the polyline could potentialy need a second offset
          );if
          (if (setq la (b_layer_locked (getvar "clayer")))
              (command "_.layer" "_unl" (getvar "clayer") "")
          );if
     
          (command "_.pline")
          (setq b nil)
          (setq n 0);setq
          (repeat (length lst)
           (setq d (nth n lst))
           (if (not (equal d b 0.0001))
              (progn
               (command d)
               (setq lst2 (append lst2 (list d)));setq
               (setq b d);setq 
              );progn
           );if 
           (setq n (+ n 1))
          );repeat
          (command "")
          (setq  na2 (entlast)
                  ss (ssadd)
                  ss (ssadd na2 ss)
                 lst nil 
          );setq
          (ss_visible ss 1)
          (setq lst2 (get_fence_points na2 a lst2 flag3 flag));setq
     
          (if la
              (command "_.layer" "_lock" (getvar "clayer") "")
          );if
          (command "_.ucs" "_p") 
          ;Move the ents to force a display update of the ents to avoid viewres problems. 
          (setvar "highlight" 0)
          (if (setq ss (ssget "_f" (last lst2)))
              (command "_.move" ss "" "0,0,0" "0,0,0")
          );if
          (if flag
              (progn
               (if (setq la (b_layer_locked (dxf 8 e1)))
                   (command "_.layer" "_unl" (dxf 8 e1) "")
               );if 
               (ucs_2_ent (dxf 210 e1))
               (command "_.copy" na "" "0,0,0" "0,0,0")
               (entdel na)
               (setq na3 na
                      na (entlast)
               );setq
               (command "_.pedit" na "_w" "0.0" "_x")
               (command "_.ucs" "_p")
               (if la (command "_.layer" "_lock" (dxf 8 e1) ""));if 
              );progn
          );if
          (command "_.trim" na "") 
          (setq m (- (length lst2) 1));setq
          (setq k 0) 
          (repeat (length lst2)
           (setq lst (nth k lst2))  
           (setq a (trans (car lst) 0 1))
           (setq n 1)
           (repeat (- (length lst) 1) ;repeat each fence list
            (setq b (trans (nth n lst) 0 1))
            (if (equal a b 0.0001)
                (setq flag2 T)
                (setq flag2 nil)
            );if     
            (setq na4 nil);setq
            (setq j 0);setq
            (while (not flag2)       ;repeat each segment of the fence until no new ents are created. 
             (setq na4 (entlast));setq
             (command "_F" a b "")      
             (if (and (equal na4 (entlast))
                      (or (not (equal k m))
                          (> j 0)
                      );or
                 );and
                 (setq flag2 T)
             );if
             (setq j (+ j 1));setq 
            );while
            (setq a b);setq
            (setq n (+ n 1));setq
           );repeat 
             
           (setq k (+ k 1))
          );repeat 
          (command "") 
           
          (if flag 
              (progn
               (if (setq la (b_layer_locked (dxf 8 e1)))
                   (command "_.layer" "_unl" (dxf 8 e1) "")
               );if 
               (entdel na) ;get rid of the copy
               (entdel na3);bring back the original
               (if la (command "_.layer" "_lock" (dxf 8 e1) ""));if 
              );progn
          );if
         );progn
         (progn
          (command "_.ucs" "_p")
          (princ "\nSelf intersecting edges are not acceptable.") 
         );progn else invalid self intersecting polyline
     );if 
     (command "_.zoom" "_p")
    );progn then it's a most likely a valid entity.
);if
);defun etrim

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun another_offset ( pl1 pl2 a1 a2 b na2 lst2 a lst3 lst4 / na ss lst da1 da2)

(setq da1 (abs (- a2 a1)));setq
(setq da2 (- (* b (max pl2 pl1))
             (/ (* b (abs (- pl2 pl1))) 
                 2.0
             )
          )
);setq
(if (> (abs (- da2 da1))
       (* 0.01 (max a1 a2)) 
    )
    (progn

     (pline (list lst2))
     (setq  na (entlast)
           na2 (entlast)
            ss (ssadd)
            ss (ssadd na ss)
     );setq
     (ss_visible ss 1)
     (command "_.offset" b na2 a "") 
     (if (and (not (equal na (entlast)))
              (setq lst3 (vtlist (entlast)))
              (setq lst3 (intersect_check lst2 lst3 lst4))
         );and
         (progn
          (ss_visible (ssadd (entlast) (ssadd)) 1)
          (command "_.area" "_ob" (entlast))
          (setq pl2 (getvar "perimeter")
                 a2 (getvar "area")
          );setq
          (setq lst (list (vtlist (list (entlast) 0))));setq
          (entdel (entlast));then offset was a success so delete the ent after getting it's info
         );progn then
         (if (not (equal na (entlast))) (entdel (entlast)));if else
     );if
     (entdel na2)
    );progn then let's do that second offset 
);if

lst
);defun another_offset

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun get_fence_points ( na2 a lst2 flag plflag / a1 a2 pl1 pl2 b c d n 
                                                   lst lst2 lst3 lst4 na 
                        )

(if flag
    (progn
     (setq lst2 (cdr lst2));setq
     (repeat (fix (/ (length lst2) 2))
      (setq lst2 (append (cdr lst2) (list (car lst2)));append
      );setq
     );repeat
     (setq lst2 (append lst2 (list (car lst2))));setq
     (command "_.area" "_ob" na2)
     (setq pl1 (getvar "perimeter")
            a1 (getvar "area")
     );setq
    );progn
);if

(setq    a (trans a 0 1)
         b (* (getvar "viewsize") 0.05);initial offset distance
         n 3.0                         ;number of offsets
         d (/ b (- n 1))               ;delta offset
         c (pixel_unit)
      lst4 (viewpnts)
);setq

(while (> b c)
(setq na (entlast))
(command "_.offset" b na2 a "") 
(if (and (not (equal na (entlast)))
         (setq lst3 (vtlist (entlast)))
         (or (not plflag)
             (setq lst3 (intersect_check lst2 lst3 lst4))
         );or
    );and 
    (progn
     (setq lst3 (lsttrans lst3 1 0))
     (ss_visible (ssadd (entlast) (ssadd)) 1)
     (if flag
         (progn 
          (command "_.area" "_ob" (entlast))
          (setq pl2 (getvar "perimeter")
                 a2 (getvar "area")
          );setq
         );progn
     );if 
     (setq lst (append lst (list lst3)));setq
     (entdel (entlast))  ;delete the ent after getting it's vertex info
     (if flag
         (setq lst (append lst 
                           (another_offset pl1 pl2 a1 a2 b na2 lst2 a lst3 lst4)
                   );append
         );setq
     );if
    );progn then offset was a success
    (if (not (equal na (entlast))) (entdel (entlast)));if else
);if
(setq b (- b d));setq
);while
(setq na (entlast))
(command "_.offset" c na2 a "") 
(if (and (not (equal na (entlast)))
         (setq lst3 (vtlist (entlast)))
         (or (not plflag)
             (setq lst3 (intersect_check lst2 lst3 lst4))
         );or
    );and 
    (progn
     (setq lst3 (lsttrans lst3 1 0)) 
     (ss_visible (ssadd (entlast) (ssadd)) 1)
     (if flag
         (progn 
          (command "_.area" "_ob" (entlast))
          (setq pl2 (getvar "perimeter")
                 a2 (getvar "area")
          );setq
         );progn
     );if 
     (setq lst (append lst (list lst3)));setq
     (entdel (entlast));then offset was a success so delete the ent after getting it's info
     (if flag     
         (setq lst (append lst 
                           (another_offset pl1 pl2 a1 a2 c na2 lst2 a lst3 lst4)
                   );append
         );setq
     );if
    );progn then
    (if (not (equal na (entlast))) (entdel (entlast)));if else
);if
(entdel na2)

lst
);defun get_fence_points

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;returns a list of points on screen if the first two lists do not
;contain segments that intersect each other.
; 
(defun intersect_check ( lst lst2 lst3 / x x2 y y2 lst4 flag len len2 
                                         a aa b bb c d n j)

(setq  len (length lst)
      len2 (length lst2)
         x (car (car lst3))
        x2 (car (cadr lst3))
         y (cadr (car lst3))
        y2 (cadr (cadr lst3))
);setq

(setq n 0);setq
(while (and (not flag) 
            (< (+ n 1) len2)
       );and
(setq   aa (nth n lst2)
        bb (nth (+ n 1) lst2)
         a (truncate_2_view aa bb x y x2 y2)
         b (truncate_2_view bb aa x y x2 y2)
      lst4 (append lst4 (list a))
);setq
(if (or (not (equal a aa))
        (not (equal b bb))
    );or
    (setq lst4 (append lst4 (list b)))
);if
(setq j 0);setq 
 (while (and (not flag) 
             (< (+ j 1) len)
        );and
 (setq    c (nth j lst)
          d (nth (+ j 1) lst)
       flag (inters a b c d) 
 );setq

 (setq j (+ j 1));setq
 );while

(setq n (+ n 1));setq
);while
(if (not (equal b (last lst4)))
    (setq lst4 (append lst4 (list b)));setq
);if
(if (not flag) 
    (setq flag lst4)
    (setq flag nil)
);if
flag
);defun intersect_check

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun truncate_2_view ( a b x y x2 y2 / int)
 (if (and (< (car a) x)
          (setq int (inters a b (list x y 0.0) (list x y2 0.0)))
     );and
     (setq a int)
     (if (and (> (car a) x2)
              (setq int (inters a b (list x2 y 0.0) (list x2 y2 0.0)))
         );and
         (setq a int)
     );if else
 );if
 (if (and (< (cadr a) y)
          (setq int (inters a b (list x y 0.0) (list x2 y 0.0)))
     );and
     (setq a int)
     (if (and (> (cadr a) y2)
              (setq int (inters a b (list x y2 0.0) (list x2 y2 0.0)))
         );and
         (setq a int)
     );if else
 );if

a
);defun truncate_2_view


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(princ "\n \"EXTRIM\" loaded.")
(princ)
