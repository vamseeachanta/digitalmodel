

(defun c:cut ()
;;Freeware
;;Enhansed Multi-Trim Commands by Bob Jones With Dialog Box interface
;;added by Jim Arthur
;;This program is an enhansment of:
;;SECTION Release1.0
;;Copyright (C) 1996, Bob Jones
;;Email: bcjones@io.com
;;WWW: http://www.io.com/~bcjones

;;Permission to use, copy, modify, and distribute this software for any purpose
;;and without fee is hereby granted, provided that the above copyright notice
;;appears in all copies and that both that copyright notice and this permission
;;notice appear in all supporting documentation.

;;Bob Jones makes no warranty, including but not limited to any implied
;;warranties of merchantability or fitness for a particular purpose,
;;regarding the software and accompanying materials.  The software and
;;accompanying materials are provided solely on an "as-is" basis.

;;In no event shall Bob Jones be liable to any special, collateral, incidental,
;;or consequential damages in connection with or arising out of the use of
;;the software or accompanying materials.

;;This routine can be called by typing one of three commands at the command prompt.
;;All three commands will ask the user to select to corners of a rectangle.

;;The first command, SCB, will erase and trim all entities outside of the rectangle
;;and leave a polyline border.

;;The second command, SC, will erase and trim all entities outside of the rectangle
;;but will not leave a border.

;;The final command, SCD, will erase and trim all entities inside of the rectangle
;;and will not leave a border.


;;Please feel free to rename these commands as you desire.
(defun c:scb () (section t nil)); SECTION W/ BORDER
(defun c:sc () (section nil nil)); SECTION W/O BORDER
(defun c:scd () (section nil t)); DELETE INSIDE RECTANGLE
(defun c:bigtrim () (section nil nil)); SECTION W/O BORDER
(defun c:rectrim () (section nil t)); DELETE INSIDE RECTANGLE


* * * * * ERROR ROUTINE * * * * *
(defun newerr (msg)
 (prompt (strcat "\nSection cancelled: " msg)); PRINT ERROR
 (setvar "cmdecho" cmd); RESET COMMAND ECHO
 (setvar "highlight" hlt); RESET HIGHLIGHT
)


* * * * * MAIN FUNCTION * * * * *
;If the first argument has any value other than nil then the border will be left.  If it is nil
;then the border is erased.
;If the second argument is has any value other than nil then entities inside the border will be erased.
;If it is nil then entities outside the border are erase.
;For very large area drawings (maps or something), the DST variable may need to be changed.  If you
;find that not all entities are being trimmed properly try increasing the number higher than 1000.

(defun section (bdr n / olderr newerr cmd hlt p1 p2 p1x p1y p2x p2y p3 p4 dst plus minus p1a p2a p3a p4a lst)
 (graphscr); CHANGE TO GRAPHICS SCREEN
 (setq olderr *error* ; SET UP NEW
       *error* newerr ; ERROR ROUTINE
       cmd (getvar "cmdecho"); SAVE COMMAND ECHO SETTING
       hlt (getvar "highlight"); SAVE HIGHLIGHT SETTING
       p1 (getpoint "\nSelect first corner of rectangle: "); GET LL CORNER OF RECTANGLE
       p2 (getcorner p1 "\nSelect other corner: "); GET UR CORNER
       p1x (car p1)
       p1y (cadr p1)
       p2x (car p2)
       p2y (cadr p2)
       p3 (list p2x p1y); BUILD LR CORNER
       p4 (list p1x p2y); BUILD UL CORNER
       dst (/ (distance p1 p2) 2000.0); OFFSET FACTOR FOR TRIMMING
       plus (if n - +)
       minus (if n + -)
 );END SETQ
 (cond
  ((and (< p1x p2x) (< p1y p2y)); P1 IS LL CORNER
   (setq p1a (list (minus p1x dst) (minus p1y dst)); BUILD LL TRIM LINE POINT
         p2a (list (plus p2x dst) (plus p2y dst))); BUILD UR TRIM LINE POINT
  )
  ((and (> p1x p2x) (< p1y p2y)); P1 IS UL CORNER
   (setq p1a (list (plus p1x dst) (minus p1y dst)); BUILD LL TRIM LINE POINT
         p2a (list (minus p2x dst) (plus p2y dst))); BUILD UR TRIM LINE POINT
  )
  ((and (> p1x p2x) (> p1y p2y)); P1 IS UR CORNER
   (setq p1a (list (plus p1x dst) (plus p1y dst)); BUILD LL TRIM LINE POINT
         p2a (list (minus p2x dst) (minus p2y dst))); BUILD UR TRIM LINE POINT
  )
  ((and (< p1x p2x) (> p1y p2y)); P1 IS LR CORNER
   (setq p1a (list (minus p1x dst) (plus p1y dst)); BUILD LL TRIM LINE POINT
         p2a (list (plus p2x dst) (minus p2y dst))); BUILD UR TRIM LINE POINT
  )
 ); END COND
 (setq p3a (list (car p2a) (cadr p1a)); BUILD LR TRIM LINE POINT
       p4a (list (car p1a) (cadr p2a)); BUILD UL TRIM LINE POINT
 ); END SETQ
 (setvar "cmdecho" 0); TURN OFF COMMAND ECHO
 (setvar "highlight" 0); TURN OFF HIGHLIGHT
 (command "_.pline" p1 p3 p2 p4 "_c"); DRAW POLYLINE BORDER
 (setq lst (entlast)); SAVE POLYLINE ENTITY NAME
 (if n                                          ;ERASE ENTITIES
  (command "_.erase" "_w" p1 p2 "_r" lst "")    ;INSIDE RECTANGLE
  (command "_.erase" "_all" "_r" "_c" p1 p2 "") ;OUTSIDE RECTANGLE
 ); END IF
 (command "_.trim" lst "" "_f" p1a p3a ""     ;TRIM ENTITIES AROUND BORDER
                          "_f" p3a p2a ""     ;DO TO THE FINICKY NATURE OF TRIMMING
                          "_f" p2a p4a ""     ;WITH THE FENCE OPTION, I HAVE USED FOUR
                          "_f" p4a p1a "" ""  ;FENCE LINES INSTEAD OF ONE LONG ONE
 ); END COMMAND
 (if (not bdr) (entdel lst)); DELETE POLYLINE BORDER IF DESIRED
 (setq *error* olderr); RESTORE ORIGINAL ERROR ROUTINE
 (setvar "highlight" hlt); RESTORE HIGHLIGHT
 (setvar "cmdecho" cmd); RESTORE COMMAND ECHO
 (princ); EXIT CLEANLY
)
;;The following prompts are disabled when section.lsp is used with dialog box.
;(prompt "\nType SCB to create a section with a border.")
;(prompt "\nType SC to create a section without a border.")
;(prompt "\ntype SCD to delete entities inside rectangle.")
;(princ)

(defun cut_x ()
  (setq  C  0
        dcl_id (load_dialog "cut.dcl"))
  (if (not (new_dialog "cut" dcl_id))(exit))
    (action_tile "cut_outp" "(setq c 1)(done_dialog)")
    (action_tile "cut_out" "(setq c 2)(done_dialog)")
    (action_tile "cut_in" "(setq c 3)(done_dialog)")
    (action_tile "cancel" "(done_dialog)(exit)")

  (start_dialog)
  (unload_dialog dcl_id)
  (COND
       ((= C 1)(c:scb))
       ((= C 2)(c:sc))
       ((= C 3)(c:scd))
 )
(princ)
)
(cut_x)
)
;;__________________________________________________________________
;;messages
(prompt "\nCut.LSP loaded - Type Cut to begin.")
(princ)

