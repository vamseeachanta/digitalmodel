;;;    mpedit.lsp
;;;    
;;;    Copyright 1997 by Autodesk, Inc.
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
;;;============================================================================
;;;
;;;    Multiple objects can be selected for the equivalent of the pedit operation
;;;	   on polylines.  Objects that are not polylines will be filtered out of the
;;;    selection set.
;;;
;;; ===================== load-time error checking ============================

(defun *merr* (msg)
  (setq *error* m:err m:err nil)
  (princ)
)


;;;--------------------------------------------------------------------------
;;; Command-line entry point for mpedit

(defun c:mpedit (/ cmde)
  (setq m:err *error* *error* *merr*
        cmde (getvar "CMDECHO")
  )
 (setq oldplinetype (getvar "PLINETYPE"))
 (setvar "PLINETYPE" 2)
 (setvar "CMDECHO" 0)
 (setq plines (ssget '(
				(-4 . "<OR")
				(0 . "POLYLINE")
				(0 . "LWPOLYLINE")
				(0 . "ARC")
				(0 . "LINE")
				(-4 . "OR>")
		              )
              )
 )
 (convert plines) 
 (if plines
   (mpedit ss) ;after conversion, plines sset is duplicated in ss
   (princ "No polylines selected.")
 )
  (setvar "CMDECHO" cmde)
  (setq *error* m:err m:err nil)
  (setq ss nil)
  (setq plines nil)
  (setvar "PLINETYPE" oldplinetype)
  (princ)
)

;;; mpedit functionality and switch cases based on kword input operation

(defun mpedit (plines / opt newWidth)
    (command "_.undo" "_begin")
  (while (/= opt "eXit")
  (progn
 (initget 0 "Open Close Ltype Decurve Fit Spline Width eXit")
 (setq opt (getkword "Open/Close/Width/Fit/Spline/Decurve/Ltype gen/eXit <X>: "))
 (if (not opt) (setq opt "eXit"))
	(if (= opt "Open")
		(chgplopen plines)
	);if Open
	(if (= opt "Close")
		(chgplclose plines)
	);if Close
	(if (= opt "Ltype")
		(chgltgen plines)
	);if Ltype gen
	(if (= opt "Decurve")
		(chgdecurve plines)
	);if Ltype gen
	(if (= opt "Fit")
		(chgfit plines)
	);if Ltype gen
	(if (= opt "Spline")
		(chgspline plines)
	);if Ltype gen
 	(if (= opt "Width")
  		(progn
    	(initget 69) 
  		(setq newWidth (getdist "Enter new width for all segments: "))
		(chgplwidths plines newWidth)
   		);progn
 	);if Width
	);progn
	);while
	(command "_.undo" "_end")
)

;;; Pline width change

(defun chgplwidths (plines newWidth / count ent subEntity currVertex)
 (setq count 0)
 (while (< count (sslength plines))
 	(setq ent (entget (ssname plines count)))
	 (if (= (cdr (assoc 0 ent)) "LWPOLYLINE")
	 (command "_.pedit" (ssname plines count) "_width" newWidth "_exit")	 
		(progn ;polylines
			(setq subEntity (entnext (ssname plines count)))
			(setq currVertex (entget subEntity))
			(while (not (equal "SEQEND" (cdr (assoc 0 currVertex))))
			(setq currVertex (subst (cons 40 NewWidth) (assoc 40 currVertex) currVertex))
			(setq currVertex (subst (cons 41 NewWidth) (assoc 41 currVertex) currVertex))
			(entmod currVertex)
			(setq subEntity (entnext (cdr (assoc -1 currVertex))))
            (setq currVertex (entget subEntity))
			);while
			(entupd (ssname plines count))			
		);progn
	 );if
 	(setq count (1+ count))
 );while
)

;;; Pline modification to close
(defun chgplclose (plines / count new70)
 (setq count 0)
 (setq count 0)
 (while (< count (sslength plines))
	(command "_.pedit" (ssname plines count) "_close" "_exit")
	(setq count (1+ count))
 );while
)

;;; Pline modification to open
(defun chgplopen (plines / count)
 (setq count 0)
 (while (< count (sslength plines))
	(command "_.pedit" (ssname plines count) "_open" "_exit")
	(setq count (1+ count))
 );while
)

;;; Pline vertex linetype generation switch

(defun chgltgen (plines / count new70)
 (setq count 0)
  (initget 0 "ON OFF Exit")
  (setq opt (getkword "Full PLINE linetype ON/OFF <Exit>: "))
  (if opt opt "eXit") 
   (if (= opt "ON")
 	(while (< count (sslength plines))
 		(setq ent (entget (ssname plines count)))
		(setq new70 (cons 70 (logior 128 (cdr (assoc 70 ent)))))
		(setq ent (subst new70 (assoc 70 ent) ent)) 
	 	(entmod ent)
	 	(setq count (1+ count))
	 );while
   );if on
   (if (= opt "OFF")
 	(while (< count (sslength plines))
 		(setq ent (entget (ssname plines count)))
		(setq new70 (cons 70 (boole 6 128 (cdr (assoc 70 ent)))))
		(setq ent (subst new70 (assoc 70 ent) ent)) 
	 	(entmod ent)
	 	(setq count (1+ count))
	 );while
   );if off
)

;;; Pline decurve
(defun chgdecurve (plines / count)
 (setq count 0)
 (while (< count (sslength plines))
	(command "_.pedit" (ssname plines count) "_decurve" "_exit")
	(setq count (1+ count))
 );while
)

;;; Pline curve fit

(defun chgfit (plines / count)
 (setq count 0)
 (while (< count (sslength plines))
	(command "_.pedit" (ssname plines count) "_fit" "_exit")
	(setq count (1+ count))
 );while
)

;;; Pline spline fit
(defun chgspline (plines / count)
 (setq count 0)
 (while (< count (sslength plines))
	(command "_.pedit" (ssname plines count) "_spline" "_exit")
	(setq count (1+ count))
 );while
)

;;; Convert arcs and lines to polylines
;;; ss is retained as a duplicate of the plines selection set because
;;; after conversion, new handles are assigned to what were arcs and lines
(defun convert (plines / count doconvert opt)
 (initget 0 "Yes No")
 (setq opt (getkword "Convert Lines and Arcs to polylines? <Yes>: "))
 (if (not opt) (setq opt "Yes"))
 (if (= opt "Yes")
     (progn	;if yes -- convert lines and arcs to polylines
	 (setq ss (ssadd))
	 (setq count 0)
	 (while (< count (sslength plines))
		(if  
		 	(or (equal (assoc 0 (entget (ssname plines count))) '(0 . "ARC"))
		     	(equal (assoc 0 (entget (ssname plines count))) '(0 . "LINE"))
		 	);or
		 (progn
		 (command "_.pedit" (ssname plines count) "_yes" "_exit")
		 (ssadd (entlast) ss)
		 );progn true
		 (ssadd (ssname plines count) ss)
		);if
		(setq count (1+ count))
	 );while
	 );progn yes
	 (progn ;if no -- do not convert
	 (setq ss plines)
	 (setq count 0)
	 (while (< count (sslength ss))
			(if  
		 	(or (equal (assoc 0 (entget (ssname ss count))) '(0 . "ARC"))
		     	(equal (assoc 0 (entget (ssname ss count))) '(0 . "LINE"))
		 	);or
		 (progn
		 (ssdel (ssname ss count) ss)
		 (setq count (1- count))
		 );progn true
		 (princ)
		);if
		(setq count (1+ count))
	 );while
	 );progn no
 );if
)
(princ)

