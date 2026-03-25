;;;
;;;    MSTRETCH.LSP - Written by Randy Kintzley 
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
;Functions created as result of loading file: mstretch.lsp
; CP_LOOP
; LWPLINE
; RECT_POINTS
; RK_CP
; TMP_SEG
;
;Variables created as result of loading file: mstretch.lsp
;
;Functions created as a result of executing the commands in: mstretch.lsp
;
;Variables created as a result of executing the commands in: mstretch.lsp
; BONUS_ALIVE
; BONUS_OLD_ERROR
;;;;;;;;;;;;;;;;;;;;;;;;;;;;GLOBAL INFO.;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:mstretch ( / n ss ss2 a b lst lst2 lst3 lst4 flag p1 p2 p3 p4 zflag)

(if (and (not init_bonus_error) 
         (equal -1 (load "ac_bonus.lsp"  -1)) 
    );and
    (progn (alert "Error:\n     Cannot find AC_BONUS.LSP.")(exit))
);if
(init_bonus_error 
 (list
  (list   "cmdecho" 0
        "highlight" 0
         "dragmode" (getvar "dragmode")
           "osmode" 0
          "cecolor" "6"
          "celtype" "CONTINUOUS"
  )
  T 
  '(progn (tmp_seg nil nil nil) (command "_.redraw")) 
 )
)

(princ "\nDefine crossing windows or crossing polygons...") 
(setvar "highlight" 1)
(setq ss (ssadd))
(command "_.select")
(while (not flag)

(if (not lst)
    (progn   
     (initget 128 "CP C")
     (setq a (getpoint "\nCP(crossing polygon)/<Crossing First point>: "));setq
    );progn
    (progn   
     (initget 128 "CP C Undo")
     (setq a (getpoint "\nCP/Undo/<Crossing First point>: "));setq
    );progn
);if
(cond 
 ((or (and (= a "C")
           (progn (initget 1) (setq a (getpoint "\nFirst corner: ")))
           (progn (initget 1) (setq b (getcorner a "\nOther corner: ")))
           ;(setq lst2 (rect_points a b));setq 
      );and
      (and a
           (equal (type a) 'LIST)
           (progn (initget 1) (setq b (getcorner a "\nOther corner: ")))
           ;(setq lst2 (rect_points a b));setq
      );and
  );or
  (setq  lst (append lst (list (list a b)))
        lst4 (append lst4 
                     (list (ssget "_c" a b))
             )
          p3 (trans '(0.0 0.0 0.0) 1 0)
          p4 (trans (getvar "viewdir") 1 0 T);@rk added T 4:12 PM 8/12/97
          ;p4 (list (- (car p4) (car p3))
          ;         (- (cadr p4) (cadr p3))
          ;         (- (caddr p4) (caddr p3))
          ;   );list
  );setq

  (lwpline 
    (list 
        (cons 210 p4)
        ;(lsttrans (rect_points a b) 1 2)
        (rect_points (trans a 1 p4); p4 was 2
                     (trans b 1 p4)
        )
    );list
  );lwpline

  (command (entlast))
  (setq lst3 (append lst3 (list (entlast))))
 );cond #1
 ((= a "CP")
  (progn
   (if (setq lst2 (rk_cp))
       (progn
        (setq lst2 (append lst2 (list (car lst2)))
               lst (append lst (list lst2))
              lst4 (append lst4 
                           (list (ssget "_cp" (cdr lst2)))
                   )
                p3 (trans '(0.0 0.0 0.0) 1 0)
                p4 (trans (getvar "viewdir") 1 0 T)
                ;p4 (list (- (car p4) (car p3))
                ;         (- (cadr p4) (cadr p3))
                ;         (- (caddr p4) (caddr p3))
                ;   );list
        );setq
        (lwpline 
          (list 
           (cons 210 p4)
           (lsttrans 
                     lst2 
                     1 
                     p4 ;@rk 2 4:27 PM 8/12/97
           )
          );list
        );lwpline

        (command (entlast))
        (setq lst3 (append lst3 (list (entlast))))
       );progn
   );if 
  );progn
 )
 ((and lst                 ;;;;;Undo the last window definition 
       (= a "Undo")
  );and   
  (command "_r" (last lst3) "_a")
  (if (b_layer_locked (getvar "clayer"))
      (progn 
       (command "")
       (command "_.layer" "_unl" (getvar "clayer") "")
       (entdel (last lst3))
       (command "_.layer" "_lock" (getvar "clayer") "")
       (command "_.select")
       (if (> (length lst3) 1)
           (eval (append '(command) 
                          (cdr (reverse lst3))
                 );append
           );eval 
       );if
      );progn then the current layer is locked
      (entdel (last lst3))
  );if
  (setq lst3 (reverse (cdr (reverse lst3)))
        lst4 (reverse (cdr (reverse lst4)))
         lst (reverse (cdr (reverse lst)))
  );setq
 )
 ((or (= a "")
      (not a) 
  );or
  (setq flag T) 
 )
 (T
  (princ "\nInvalid")
 )
);cond

);while
(command "");end select
(setvar "highlight" 0)

(if lst
    (progn 
     (princ "\nDone defining windows for stretch...") 
     (if (b_layer_locked (getvar "clayer"))
         (progn 
          (command "_.layer" "_unl" (getvar "clayer") "")
          (setq lst (reverse lst))     
          (setq n 0);setq
          (repeat (length lst3)
           (entdel (nth n lst3))
           (setq n (+ n 1));setq
          );repeat
          (command "_.layer" "_lock" (getvar "clayer") "")
         );progn then the current layer is locked
         (progn
          (setq lst (reverse lst))     
          (setq n 0);setq
          (repeat (length lst3)
           (entdel (nth n lst3))
           (setq n (+ n 1));setq
          );repeat
         );progn else
     );if
     
     (setvar "highlight" 1)
     (command "_.select")
     (repeat (length lst4)
      (if (car lst4) (command (car lst4)))
      (setq lst4 (cdr lst4))
     );repeat          
     (command "")
     (setq ss (ssget "_p")) 
                 
     (if ss
         (progn
          
          (command "_.select" ss)
          (setvar "osmode" (cadr (assoc "OSMODE" (car bonus_varlist)))) 
          (initget 128 "Remove")
          (setq p1 (getpoint "\nRemove objects/<Base point>: "));setq
          (if (not p1) (setq p1 (car (viewpnts))));if
          (command "")
          (if (= p1 "Remove")
              (progn
               (setvar "highlight" 0)
               (clear_prev_ss) 
               ;(command "_.select" (entnext) "")
               ;(command "_.undo" "1")
               (setvar "highlight" 1) 
               
               (command "_.select" ss "_r" "_auto")
               (setvar "cmdecho" 1) 
               (while (wcmatch (getvar "cmdnames") "*SELECT*")
                (command pause)
               );while
               (if (setq ss2 (ssget "_P"));setq
                   (progn
                    (command "_.select" ss2)
                    (setq p1 (getpoint "\nBase point: "));setq 
                    (command "")
                    (if (not p1) (setq p1 (car (viewpnts))));if
                   );progn
               );if 
              );progn then
              (setq ss2 ss);else
          );if     
          (if ss2
              (progn  
               ;;get the extents of the crossing window definitions
               (setq lst2 lst)           

               (repeat (length lst2)
                (if (> (length (car lst2)) 2)
                    (setq lst2 (append (cdr lst2) (car lst2)))
                    (setq lst2 (cdr lst2))
                );if 
               );repeat

               (if (and (> (length (car lst)) 2)                 ;;;cp_off_screen?
                        (zoom_4_select (car lst))
                   );and                 
                   (progn
                    (setvar "cmdecho" 0) 
                    (command "_.select" ss2)
                    (setq p2 (getpoint p1 "\nSecond base point: "));setq
                    (command "")
                   );progn
                   (progn 
                    (setvar "cmdecho" 0) 
                    (command "_.stretch")
                    (cp_loop (car lst))
                    (command "_r" ss "_a" ss2 "" p1)
                    (setvar "cmdecho" 1)
                    (princ "\nSecond base point: ")
                    (command pause)
                    (setvar "cmdecho" 0) 
                    (setq p2 (getvar "lastpoint"))
                    (setq lst (cdr lst))
                   );progn
               );if    
                
               (if (setq zflag (zoom_4_select lst2))
                   (command "_.zoom" "_w" (car zflag) (cadr zflag))
               );if

               (setvar "highlight" 0)
               (setvar "dragmode"  0)
               (setvar "osmode"    0)
               (setq n 0);setq
               (repeat (length lst)
                (setq a (nth n lst));setq
                (command "_.stretch")
                (cp_loop a)
                (command "_r" ss "_a" ss2 "" p1 p2)
                (setq n (+ n 1));setq
               );repeat
               (if zflag (command "_.zoom" "_p"))
              );progn then ss2
              (princ "\nNothing selected")
          );if
         );progn then ss
         (princ "\nNothing selected")
     );if  
    );progn then lst
);if
(restore_old_error)
(princ)
);defun c:mstretch
                        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun cp_loop ( lst / n)
(if (equal (length lst) 2)
    (command "_c" (car lst) (cadr lst))
    (progn
     (command "_cp") 
     (setq n 0)
     (repeat (length lst)
     (command (nth n lst))
     (setq n (+ n 1));setq 
     );repeat
     (command "")
    );progn
);if
);defun cp_loop

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;rect_points
;takes a lower left corner and upper right corner and returns a
;list of 5 corner points (first and last are identical).
(defun rect_points ( a b / c d )

(setq c (list (min (car a) (car b))
              (min (cadr a) (cadr b))
              0.0
        );list
      d (list (max (car a) (car b))
              (max (cadr a) (cadr b))
              0.0
        );list
);setq

(list c 
      (list (car d) (cadr c) 0.0)
      d
      (list (car c) (cadr d) 0.0)
      c
);list
);defun


;;;;;;;;;;;;;;;;;;;;;;;;;;;crossing polygon emulator;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun rk_cp ( / p1 p2 flag lst lst2 n str)

(setq   p1 (getpoint "\nFirst polygon point: ")
      lst2 (list p1)
       str ""
);setq
(princ "\nUndo/<Endpoint of line>: ")

(while (not flag)
(setq p2 (grread T))
(cond
 ((equal 5 (car p2))
  (tmp_seg p1 
           (cadr p2) 
           (if (> (length lst2) 1)
               (car lst2)
               nil
           );if
  )
 )
 ((and (equal 3 (car p2)) 
       (not (p_isect (append lst2 (list (cadr p2))) 
                     "\nUndo/<Endpoint of line>: "
            )
       );not
  );and
  (progn
   (princ "\nUndo/<Endpoint of line>: ")

   (setq      lst (append lst 
                          (list (list (car p_tmp_seg)
                                      (cadr p_tmp_seg)
                          )     )
                  )
             lst2 (append lst2 
                          (list (cadr p_tmp_seg))
                  )
               p1 (cadr p_tmp_seg)
        p_tmp_seg (list nil p1 (caddr p_tmp_seg))
              str ""
   );setq
  );progn
 )
 ((or (equal p2 '(2 13)) ;return
      (equal p2 '(2 32)) ;space
  );or
  (progn
   (princ "\nUndo/<Endpoint of line>: ")
   
   (if (equal str "")  
       (progn 
        (setq flag T)
        
        (setq n 0)
        (tmp_seg nil nil nil)
        (repeat (length lst) 
         (setq p_tmp_seg (nth n lst)) 
         (tmp_seg nil nil nil)
        (setq n (+ n 1));setq 
        );repeat
       );progn
       (progn
        (if (and (equal str "U")
                 (> (length lst) 0)
            );and
            (progn
             (tmp_seg nil nil nil)
             (setq p_tmp_seg (last lst))
             (tmp_seg nil nil nil)   
             (setq  lst (reverse (cdr (reverse lst)))
                   lst2 (reverse (cdr (reverse lst2)))
                     p1 (last lst2)
             );setq
            );progn
            (progn
             (if (equal str "U")
                 (princ "\nAll segments undone.\n") 
                 (princ "\nInvalid.\n")
             );if
             (princ "\nUndo/<Endpoint of line>: ")

            );progn
        );if 
       );progn else
   );if 

   (setq str "")

  );progn
 )
 ((equal 2 (car p2))
  (progn

   (if (equal p2 '(2 8))
       (progn 
        (if (> (strlen str) 0)
            (progn 
             (princ (chr (cadr p2)))
             (princ " ")
             (princ (chr (cadr p2)))
             (setq str (substr str 1 
                               (max 0 (- (strlen str) 1))
                       )
             );setq
            );progn
        );if
       );progn
       (progn
        (princ (chr (cadr p2)))
        (setq str (strcat str (strcase (chr (cadr p2)))))
       );progn
   );if
  );progn
 )
);cond close

);while
(if (or (<= (length lst2) 2)
        (p_isect (append lst2 (list (car lst2))) "")
    );or
    (progn
      (setq lst2 nil);setq
      (princ "\nCrossing Polygon not defined.")
    );progn
);if 

lst2
);defun rk_cp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;tmp_seg
;uses grdraw to draw temporary segements bewteeen the 
;points provided as arguments.
(defun tmp_seg ( p1 p2 p3 / )

(if p_tmp_seg
    (progn
     (if (and (car p_tmp_seg) (cadr p_tmp_seg))
         (grdraw (car p_tmp_seg) (cadr p_tmp_seg) 0)
     );if
     (if (and (cadr p_tmp_seg) (caddr p_tmp_seg))
         (grdraw (cadr p_tmp_seg) (caddr p_tmp_seg) 0)
     );if
    );progn 
);if

(if p2
    (progn
     (if (and p1 p2)
         (grdraw p1 p2 7 -1)
     );if
     (if (and p2 p3)
         (grdraw p2 p3 7 -1)
     );if
     (setq p_tmp_seg (list p1 p2 p3))
    );progn
    (setq p_tmp_seg nil)
);if

);defun tmp_seg

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun lwpline ( lst / n a e1)
 
 (setq e1 (list '(0 . "LWPOLYLINE") 
                 '(100 . "AcDbEntity")
                 
                 '(100 . "AcDbPolyline")
                 (cons 90 (length (last lst)))
                 '(70 . 0)
                 '(43 . 0.0) 
                 (if (caddr (car (last lst)))
                     (cons 38 (caddr (car (last lst))))
                     '(38 . 0.0) 
                 );if
          );list
 );setq
 (if (> (length lst) 1)
     (setq  e1 (append e1 (reverse (cdr (reverse lst)))));setq
 );if
 (setq lst (last lst))
 
 (setq n 0);setq
 (repeat (length lst)
  (setq  a (nth n lst)
        e1 (append e1 (list (list 10 (car a) (cadr a)))) 
  );setq
  (setq n (+ n 1));setq
 );repeat
 
 (if (> n 0)
     (entmake e1)
 );if
 (princ)
);defun lwpline

(princ)


