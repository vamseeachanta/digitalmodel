; PDIST.LSP
; 3/25/1987
; By Philip M. Kreiker, Looking Glass Microproducts
;
; The information contained herein is being donated to the public domain
; soley for the benefit of licensed AutoCAD END USERS everywhere.  You are
; free to modify, or reproduce and distribute only in the un-altered
; original form, the information contained herein for ANY NON-COMMERCIAL
; use, provided such reproduction/disribution is NOT FOR PROFIT, and
; provided that with any such distribution, this NOTICE (or any portion
; thereof) is NOT REMOVED OR ALTERED from it's original content in ANY
; manner, shape, or form.
;
; You ARE NOT free to reproduce and/or distribute the information
; contained herein (regardless of medium or form of reproduction) for
; profit, or to include it or any portion thereof, as a part of any
; commercially marketed goods regardless of thier intended nature, scope,
; or form, without the expressed written permission of the author.
;
; If you seek such permission, it will be granted provided you furnish the
; author with ONE (1) fully-functional, licenseable sample of the finished
; product(s) in final marketed form.  In return for which you may solict
; and will recieve the author's objective opinions, advice and insight
; regarding any and all aspects of said product(s), free of charge, with
; appropriate and complete written assurance of non-disclosure.
;
; Additional information can be obtained
; by contacting
;
;                 Philip M. Kreiker
;                 Looking Glass Microproducts
;                 4233 West Eisenhower Boulevard
;                 Loveland, CO  80537
;                 (303) 669-2681
;
; Or, via electronic medium thru
;
;                 Compuserve ID: 70261,234
;
;   PDIST.LSP is a AutoLISP 2.6-based user defined command that measures
;   the distance between two points along a polyline.  This is accomplished
;   by BREAKING out the polyline segment to be measured, using the
;   AutoLISP 2.6 AREA command to report the length of the polyline,
;   and using the UNDO command to repair the break.
;
;
;   Some of the features of LEADER are:
;
;   To use PDIST, follow these steps;
;
;      Add all the AutoLISP code included in this file to your ACAD.LSP,
;      or LOAD in from within AutoCAD.
;
;     To measure the distance along a polyline, enter "PDIST" at the AutoCAD
;     Command:  prompt.
;
;     You will then be prompted to select a polyline.  Pick a point on
;     the polyline BETWEEN the two points you wish to measure.
;
;     You will then be prompted for the two points that define
;     the segment you wish to measure.
;
;     -- Philip M. Kreiker, Looking Glass Microproducts, 70261,234
;
;-----------------------------------------------------------------------------
;
; PDIST -- Determine the distance between two points on a polyline
;
(defun c:pdist ( / e ename ent p0 p1 p2)
  (if (setq e (entsel "\nSelect polyline:"))
      (progn
         (setq
           p0    (cadr e)
           ename (car e)
           ent   (entget ename)
         )
         (if (= "POLYLINE" (cdr (assoc 0 ent)) )
           (progn
              (redraw ename 3)
              (initget 1)
              (setq p1 (getpoint "\nFrom point: "))
              (setq p2 (getpoint "\nTo Point: "))
              (redraw ename 4)
              (setvar "cmdecho" 0)
              (command
                "break" p0 "F" p1 "@"
                "break" p0 "F" p2 "@"
                "area" "E" p0
                "undo" 3
              )
              (princ "[done")
           )
           (princ "[not a polyline")
         )
      )
      (princ "[none selected")
  )
  ']
)
