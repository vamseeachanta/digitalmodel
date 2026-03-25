;;;
;;;    TREXBLK.LSP - Written by Randy Kintzley 
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;GLOBAL INFO.;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Functions created as result of loading file: trexblk.lsp
; - B_PURGE (removed and placed in ac_bonus.lsp)
; DO_VOODO
; EXTRACT_CLONE
; FIND_PLINE_HEADER
; IMAGE_MAKE
; MK_NOBODY - (removed and added to ac_bonus.lsp)
;  - NEWSEL (removed and placed in ac_bonus.lsp)
; REM_CODES
; REM_GROUP - (removed and added to ac_bonus.lsp)
; REMX_CODES
; SELECT_NESTED
;
;Variables created as result of loading file: trexblk.lsp
;
;Functions created as a result of executing the commands in: trexblk.lsp
;
;Variables created as a result of executing the commands in: trexblk.lsp
; BONUS_ALIVE
; BONUS_OLD_ERROR
;;;;;;;;;;;;;;;;;;;;;;;;;;;;GLOBAL INFO.;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun find_pline_header ( na na2 / na3 na4)

(setq na3 (tblobjname "block" (cdr (assoc 2 (entget na)))));setq
(while (and na3 
            (not (equal na2 na3))
       );and
(if (equal "POLYLINE" (cdr (assoc 0 (entget na3))))
    (setq na4 na3);a polyline header that could be the one we need. 
);if
(setq na3 (entnext na3))
);while

na4
);defun find_pline_header

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun extract_clone ( lst / flag flag2 bna bna2 bna3 lst lst3 j n na ss e1 )

 (setq   na (car lst)
         e1 (entget na) 
       lst3 (last lst);the ent family tree 
          j (- (length lst3) 1)
 );setq

 (if (setq flag2 (car lst3))
     (progn
       (ucs_2_ent (cdr (assoc 210 (entget (car lst3)))))
     );progn
 );if
 (if (equal "VERTEX" (cdr (assoc 0 e1)))
     (setq na (find_pline_header (car lst3) na)
           e1 (entget na) 
     );setq
 );if
 (if (> (length lst3) 0)
     (progn
      (if (not (assoc 60 e1))            ;set the invisibility flag           
          (setq e1 (append e1 (list (cons 60 1))));setq then
          (setq e1 (subst (cons 60 1) (assoc 60 e1) e1));setq else
      );if
     );progn
 );if

 (setq e1 (rem_codes e1));setq
 (if (equal "IMAGE" (cdr (assoc 0 e1)))
     (image_make na e1)                    ; Make the nested ent the way it was first drawn
     (entmake e1)                          ;except this time it's invisible!
 );if

 (if (equal '(66 . 1) (assoc 66 e1));just in case its some variety of polyline,
     (progn                         ;then better finish the entmake job. 
      (setq na (entnext na)
            e1 (entget na)
            e1 (rem_codes e1)
      );setq 
      (while (and na
                  (not (wcmatch (cdr (assoc 0 e1)) "*END*"))
             );and 
       (entmake e1)
       (setq na (entnext na)
             e1 (entget na)
             e1 (rem_codes e1)
       );setq 
      );while           
      (entmake e1)
     );progn then
 );if
 (setq   na (entlast)            
         ss nil
         ss (ssadd)              
         ss (ssadd na ss)
        bna (mk_nobody ss nil)      ;define an anonymous block 
       bna2 bna                     ;save the name of the original for comparison later
       bna3 ""                      ;a string for adding names of purge-ables 
 );setq 

 (setvar "cmdecho" 0)
 (setvar "highlight" 0)
 (if (> (length lst3) 0)
     (command "_.erase" na "")          ;and then remove the ent.
 );if
 (setq n 0);setq                         
 (repeat (length lst3)                   
  (setq  e1 (entget (nth n lst3))                ;get the insert.
         e1 (subst (cons 2 bna) (assoc 2 e1) e1) ; put in the name of the anonymous block 
         e1 (rem_codes e1)                       ;remove the entname and the handle 
         e1 (subst (cons 66 0) (assoc 66 e1) e1)
  );setq 
 
  (entmake e1)                                   ;make the insert
 
  (setq na (entlast));setq
  
  (command "_.explode" na)                       ;explode it 
  (while (wcmatch (getvar "cmdnames") "*EXPLODE*") 
   (command "")
  );while
  (if (not (entget na))      
      (progn
       (setq ss (newsel na));setq 
       (setq bna3 (strcat bna3 ",`" bna))        ;purge it out later
       (if (not (equal n j))
           (progn
            (ss_visible ss 1)                    ;Make the ents stealth.      
            (setq bna (mk_nobody ss nil)         ; Define the anonymous block
            );setq                               ;such that it's non-nested.  
           );progn
       );if
      );progn then the explode operation was a success.
      (progn
       (setq ss nil
             ss (ssadd)
             ss (ssadd na ss)
       );setq
       (if (not flag)
           (setq flag bna);then save the first non-exploded anonymous block name
       );if 
       (if (not (equal n j))
           (setq bna (mk_nobody ss nil))      ; Define a nested anonymous block cuz we 
       );if                                   ;couldn't explode the insert of the last one    
      );progn else                                  
  );if
  (if (not (equal n j))                      ;if this isn't the last one, 
      (command "_.erase" ss "")              ;then remove the ents used to create block.  
      (progn                                 
                                            ; Else turn the stuff back on cuz this is 
       (if (not flag)                       ;the last time through.
           (ss_visible ss 0);then       
           (progn
            (setq  na (tblobjname "block" flag)
                   e1 (entget na)
                  lst (list e1)
            );setq
            (while (setq na (entnext na))
             (setq  e1 (entget na) 
                    e1 (subst (cons 60 0) (assoc 60 e1) e1)
                   lst (append lst (list e1))
             );setq
            );while
            (while lst
             (entmake (car lst))
             (setq lst (cdr lst));setq 
            );while
            (entmake '((0 . "ENDBLK")))
            (command "_.move" ss "" "0,0" "0,0");Done for highlighting reasons.  
           );progn else need to redefine the first block
       );if 
      );progn else make the ents visible again
  );if                                           
 (setq n (+ n 1));setq 
 );repeat

 (if flag2 
     (command "_.ucs" "_p")
 );if 
 (if (not (equal bna3 ""))
     (progn 
      (setq bna3 (substr bna3 2))
      (b_purge bna3)
     );progn
 );if
ss
);defun extract_clone

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;Dictionary notes:
;na  IMAGE  
;na2  340 -> IMAGEDEF (missing in xref) 
;na3          330 -> DICTIONARY1
;na4                  330 -> DICTIONARY2 (3  AND 350)
;na5                          "ACAD_IMAGE_DICT" 350 -> DICTIONARY1
;na6                          "ACAD_IMAGE_VARS" 350 -> "RASTERVARIABLES"
;na7                                                      DICTIONARY2
;na8                  350 -> IMAGEDEF
;na9          330 -> IMAGEDEF_REACTOR
;na10                 330 -> IMAGE
;na11 360 -> IMAGEDEF_REACTOR
;
; group {102 330 102}
; leader 340
; mline  340
; hatch  330
;
;mline has a mlstyle of group 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun image_make ( na e1 / na2 e2 na3 e3 na4 e4 p1 p2 ina ina2)
 (setq   p1 (image_bounds na)
         p2 (trans 
             (cadr p1) 
             0 1)
         p1 (trans 
              (car p1)  
              0 1)
        na2 (cdr (assoc 340 e1))
         e2 (entget na2)                        ;the imagedef
        ina (cdr (assoc 1 e2))                  ;the image name w/full path
        na3 (cdr (assoc 330 e2))                ;owner dict
         e3 (entget na3)
       ina2 (cdr (assoc 3 (member (cons 350 na2) (reverse e3))));image name w/out path
        na4 (namedobjdict)                      ;current drawings' dictionary 
         e4 (dictsearch na4 "ACAD_IMAGE_DICT")  ;image dict
 );setq
 (if (member (cons 3 ina2) e4)
     (setq ina ina2);then use the short name instead.
 );if

 (setq na (entlast))
 (command "_.-image" "_a" ina p1 p2 p2);command

 (if (equal na (entlast))
     (progn
      (alert (strcat "Problem attaching image: " ina
                     "\nThe file may not be on AutoCAD's search path"
             );strcat
      );alert
      (exit)
     );progn
     (progn
      (setq e2 (entget (entlast))
            e3 (append (list (assoc -1 e2)) e1)
            e3 (subst (assoc 5 e2) (assoc 5 e1) e3)
            e3 (subst (assoc 340 e2) (assoc 340 e1) e3)
            e3 (subst (assoc 360 e2) (assoc 360 e1) e3)
      );setq
      (entmod e3)
      (entupd (cdr (assoc -1 e3)))
      (ss_visible (ssadd (entlast) (ssadd)) 1)     
     );progn else
 );if       
);defun image_make

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun select_nested ( msg / errno_sel lst ss ss2 na e1 n a lst2)

 ;local function
 (defun errno_sel ( msg / a)
  (setvar "errno" 0) 
  (while (and (not (setq a (nentsel msg)))
              (equal 7 (getvar "errno"))
         );and
    (setvar "errno" 0)     
  );while
  a
 );defun errno_sel 

 (setvar "cmdecho" 0) 
 (clear_prev_ss)
 (setvar "highlight" 1) 
 (command "_.select")
 (while (setq lst (errno_sel msg))
  (setq na (car lst)
        e1 (entget na)
  );setq

  (if (and (not (equal (length lst) 4))
           (not (equal "ATTRIB" (cdr (assoc 0 e1))))
      );and
      (progn 
       (if (equal "VERTEX" (cdr (assoc 0 e1)))
           (command (cadr lst));then get the polyline header by directly selecting
           (setq ss nil
                 ss (ssadd)
                 ss (ssadd (car lst) ss)
           );setq
       );if  
      );progn then it's non-nested.
      (progn
       (command "")
       (setq ss2 (ssget "_p"))

       (if (and (equal "ATTRIB" (cdr (assoc 0 e1)))
                (not (equal (length lst) 4))
           );and 
           (progn
            (setq lst (append lst 
                              (list nil) 
                              (list nil) ;;;(list (list (ssname (ssget "_p") 0)))
                      );append
            );setq
           );progn then do special stuff for the attrib
       );if

       (setq ss (extract_clone lst))

       (if (and ss
                (> (sslength ss) 0)
           );and
           (setq lst2 (append lst2 (list ss)))
       );if
       (setvar "cmdecho" 0) 
       (clear_prev_ss)
       (setvar "highlight" 1) 
       (command "_.select")
       (if (and ss2 
                (> (sslength ss2) 0)
           );and
           (command ss2)
       );if
      );progn else it's nested
  );if
  (if (and ss 
           (> (sslength ss) 0)
      );and
      (command ss)
  );if  
 );while

 (while (wcmatch (getvar "cmdnames") "*SELECT*") (command ""));while 
 (setq ss (ssget "_p"))

 (setvar "cmdecho" 0) 
 (setvar "highlight" 0) 
 (clear_prev_ss)
 (command "_.select")
 (setq n 0)
 (repeat (length lst2)
 (setq a (nth n lst2))
  (command a)
 (setq n (+ n 1));setq
 );repeat 
 (command "")
 (setq lst2 nil)
 (setq ss2 (ssget "_p"));nested ents

(list ss ss2)
);defun select_nested

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
(defun do_voodo ( msg do flt / flag la ss ss2 n) 
 (init_bonus_error (list (list   "cmdecho" 0
                               "highlight" 1  
                                 "ucsicon" (getvar "ucsicon")
                                  ;"expert" 5
                         );list
                         T 
                   );list 
 );init_bonus_error
 (if (setq   la (getvar "clayer")
           flag (b_layer_locked la)
     );setq
     (command "_.layer" "_unlock" la "")
 );if
 (setq  ss (select_nested msg)
       ss2 (cadr ss);nested only
        ss (car ss) ;all.
 );setq
 (if (and ss
          (> (sslength ss) 0)
          (or (not flt)
              (progn
               (setq n (sslength ss))
               (command "_.select" ss "")
               (if (and (setq ss (ssget "_p" flt))
                        (not (equal n (sslength ss)))
                   );and
                   (princ (strcat "\n" (itoa (- n (sslength ss))) 
                                  " objects were invalid for " do
                   )      );print strcat
               );if
               ss
              );progn
          );or
     );and 
     (progn 
      (setvar "cmdecho" 0)
      (setvar "highlight" 1)
      (command (strcat "_." do) ss)
      (setvar "cmdecho" 1)
      (if (wcmatch (getvar "cmdnames") 
                   (strcat "*" (strcase do) "*")
          )
          (command "")
      );if
      (while (wcmatch (getvar "cmdnames") 
                      (strcat "*" (strcase do) "*")
             )
      (command pause)
      );while 
     );progn
 );if
 (setvar "cmdecho" 0)
 (if (and ss2
          (> (sslength ss2) 0)
     );and 
     (progn
      (if (b_layer_locked (getvar "clayer")) ;just in case of transparent layer changes
          (command "_.layer" "_unlock" la "")
      );if
      (command "_.erase" ss2 "")
      (if (not ss)
          (princ (strcat "\nNo valid objects selected for " do ".\n"))
      );if
     );progn
 );if
 (if flag (command "_.layer" "_lock" la ""))
 (restore_old_error)

 (princ)
);defun do_voodo 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun rem_codes ( e1 / na2 na3 e2 e3)
 (setq e1 (rem_group -1 e1)
       e1 (rem_group 5 e1)
       e1 (rem_group 102 e1)
       ;e1 (rem_group 330 e1)
       ;e1 (rem_group 340 e1)
       e1 (rem_group 67 e1)
       e1 (remx_codes e1)
 );setq
 (cond 
  ((equal "ATTRIB" (cdr (assoc 0 e1)))
   (setq e1 (rem_group 3 e1)
         e1 (subst '(0 . "ATTDEF") '(0 . "ATTRIB") e1)
         e1 (append e1 (list (cons 3 (cdr (assoc 1 e1)))))
         e1 (rem_group 100 e1)
   );setq
  )
  ((equal "HATCH" (cdr (assoc 0 e1)))
   (setq e1 (subst '(71 . 0) (assoc 71 e1) e1)
         e1 (subst '(97 . 0) (assoc 97 e1) e1)
         e1 (rem_group 330 e1)
   );setq
  )
  ((equal "MLINE" (cdr (assoc 0 e1)))
   (setq   e2 (dictsearch (namedobjdict) "ACAD_MLINESTYLE")
          na2 (cdr (assoc -1 e2))
          na3 (cdr (assoc 350 (member (assoc 3 e2) e2)))
           e1 (subst (cons 340 na3) (assoc 340 e1) e1)
   )
  )
  ((equal "MTEXT" (cdr (assoc 0 e1)))
   (setq e1 (rem_group 50 e1)) 
  )
 );cond
 e1
);defun rem_codes

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun remx_codes ( e1 / lst a b c d n na2 e2)
;
(setq   d (cdr (assoc 0 e1))
      lst (list '(6 . "LTYPE") 
                '(7 . "STYLE")
                '(8  . "LAYER")
          );list
);setq
(if (or (equal "DIMENSION" d)
        (equal "TOLERANCE" d)
        (equal "LEADER" d)
    );or
    (setq lst (append lst (list '(3 . "DIMSTYLE"))));setq then deal with dimstyles
);if
(setq n 0);setq
(repeat (length lst)
(setq a (nth n lst)
      b (cdr a) 
      a (car a)
      c (cdr (assoc a e1))
);setq
(if (and c 
         (setq na2 (tblobjname b c))
         (setq e2 (entget na2))
         (equal 16 (logand 16 (cdr (assoc 70 e2))))
    );and 
    (progn
     
     ;yank it out for now.
     (setq e1 (rem_group a e1))

     ;(if (equal 32 (logand 32 (cdr (assoc 70 e2))))
     ;    (progn
     ;     ;bind it?
     ;    );progn
     ;    (setq e1 (rem_code a e1));yank it
     ;);if
    );progn then xref dependant symbol table entry.
);if
(setq n (+ n 1));setq
);repeat

e1
);defun remx_code

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:btrim ( / flt ) 
 (setq flt '((-4 . "<OR") (0 . "TEXT") (0 . "MTEXT") (0 . "TOLERANCE")
              (0 . "ARC") (0 . "CIRCLE") (0 . "LINE") (0 . "LWPOLYLINE")         
              (-4 . "<AND") 
               (0 . "POLYLINE")
               (-4 . "<NOT") (-4 . "&") (70 . 112) (-4 . "NOT>")
              (-4 . "AND>")
              (0 . "ELLIPSE") (0 . "SPLINE") (0 . "LEADER") (0 . "REGION")
              (0 . "IMAGE") (0 . "HATCH") (0 . "MLINE")
             (-4 . "OR>")
            )
 );setq
 (do_voodo "\nSelect cutting edges: " "trim" flt)
);defun c:btrim

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:bextend ( / flt ) 
 (setq flt '((-4 . "<OR") (0 . "TEXT") (0 . "MTEXT") (0 . "TOLERANCE")
              (0 . "ARC") (0 . "CIRCLE") (0 . "LINE") (0 . "LWPOLYLINE")         
              (-4 . "<AND") 
               (0 . "POLYLINE")
               (-4 . "<NOT") (-4 . "&") (70 . 112) (-4 . "NOT>")
              (-4 . "AND>")
              (0 . "ELLIPSE") (0 . "SPLINE") (0 . "LEADER") (0 . "REGION")
              (0 . "IMAGE") (0 . "HATCH") (0 . "MLINE")
             (-4 . "OR>")
            )
 );setq
 (do_voodo "\nSelect edges for extend: " "extend" flt)
);defun c:bextend

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:ncopy ( / )
 (do_voodo "\nSelect nested objects to copy:" "copy" nil)
);defun c:ncopy

(princ)
