;;;---------------------------------------------------------------------------;
;;;
;;; bomlengths.lsp
;;;
;;; By Jimmy Bergmark
;;; Copyright (C) 1997-2007 JTB World, All Rights Reserved
;;; Website: www.jtbworld.com
;;; E-mail: info@jtbworld.com
;;;
;;; 1998-03-31 - First release
;;; 2000-05-11 - Fixed for LWPOLYLINES and for A2k
;;; 2003-06-10 - Tested on 2004 and fixed a minor bug
;;; 2004-03-18 - Added (vl-load-com)
;;; 2007-09-24 - Shows the result in the active unit
;;; Tested on AutoCAD 2000, 2004, 2005, 2008
;;; should be working on older versions too with minor modifications.
;;;  exchange bom-code-old with bom-code
;;;
;;;---------------------------------------------------------------------------;
;;;  DESCRIPTION
;;;
;;;  BILL OF LENGTHS. Get the accumulated sum length of multiple objects.
;;;  c:bomlengths - length of lines, arcs, polylines and splines and total.
;;;  c:bom_lines - length of lines and total.
;;;  c:bom_arcs - length of arcs, and total.
;;;  c:bom_polylines - length of polylines and total.
;;;  c:bom_splines - length of splines and total.
;;;---------------------------------------------------------------------------;

(defun dxf (n ed) (cdr (assoc n ed)))

(defun bom-code (ssfilter        /       errexit undox   restore
                 *error* olderr  oldcmdecho      %l      %t
                 sset    %i      en      ed      p1      p2
                 ot      a1      a2      r
                )
  (defun errexit (s)
    (princ)
    (restore)
  )

  (defun undox ()
    (command "._undo" "_E")
    (setvar "cmdecho" oldcmdecho)
    (setq *error* olderr)
    (princ)
  )

  (setq olderr  *error*
        restore undox
        *error* errexit
  )
  (setq oldcmdecho (getvar "cmdecho"))
  (setvar "cmdecho" 0)
  (command "._UNDO" "_BE")
  (setq %i 0
        %t 0
  )
  (vl-load-com)
  (setq sset (ssget ssfilter))
  (if sset
    (progn
      (princ "\nLengths:")
      (repeat (sslength sset)
	(setq en (ssname sset %i))
	(setq ed (entget en))
	(setq ot (dxf 0 ed))
	(setq curve (vlax-ename->vla-object en))
	(if (vl-catch-all-error-p
	      (setq len	(vl-catch-all-apply
			  'vlax-curve-getDistAtParam
			  (list	curve
				(vl-catch-all-apply
				  'vlax-curve-getEndParam
				  (list curve)
				)
			  )
			)
	      )
	    )
	  nil
	  len
	)
	(setq %l len)

	(setq %i (1+ %i)
	      %t (+ %l %t)
	)
	(terpri)
	;(princ %l )
	(princ (rtos %l (getvar "lunits")(getvar "luprec")))
      )
      (princ "\nTotal = ")
      ;(princ %t)
      (princ (rtos %t (getvar "lunits")(getvar "luprec")))
      (textpage)
    )
  )
  (restore)
)

(defun bom-code-old (ssfilter        /       errexit undox   restore
                 *error* olderr  oldcmdecho      %l      %t
                 sset    %i      en      ed      p1      p2
                 ot      a1      a2      r
                )
  (defun errexit (s)
    (princ)
    (restore)
  )

  (defun undox ()
    (command "._undo" "_E")
    (setvar "cmdecho" oldcmdecho)
    (setq *error* olderr)
    (princ)
  )

  (setq olderr  *error*
        restore undox
        *error* errexit
  )
  (setq oldcmdecho (getvar "cmdecho"))
  (setvar "cmdecho" 0)
  (command "._UNDO" "_BE")
  (setq %i 0
        %t 0
  )
  (setq sset (ssget ssfilter))
  (if sset
    (progn
      (princ "\nLengths:")
      (repeat (sslength sset)
        (setq en (ssname sset %i))
        (setq ed (entget en))
        (setq ot (dxf 0 ed))
        (cond
          ((= ot "LINE")
           (setq p1 (dxf 10 ed)
                 p2 (dxf 11 ed)
                 %l (distance p1 p2)
           )
          )
          ((= ot "ARC")
           (setq a1 (dxf 50 ed)
                 a2 (dxf 51 ed)
                 r  (dxf 40 ed)
                 %l (* r (abs (- a2 a1)))
           )
          )
          (t
           (command "._area" "_obj" en)
           (setq %l (getvar "perimeter"))

          )
        )
        (setq %i (1+ %i)
              %t (+ %l %t)
        )
        (terpri)
        (princ %l)
      )
      (princ "\nTotal = ")
      (princ %t)
      (textpage)
    )
  )
  (restore)
)

(defun c:bomlengths ()
  (initget "Lines Arcs Polylines Splines ALL")
  (setq ans (getkword
              "Enter an option [Lines/Arcs/Polylines/Splines] <ALL>: "
            )
  )
  (cond
    ((= ans "Lines") (c:bom_lines))
    ((= ans "Arcs") (c:bom_arcs))
    ((= ans "Polylines") (c:bom_polylines))
    ((= ans "Splines") (c:bom_splines))
    (t
     (bom-code '((-4 . "<OR")
                 (0 . "LINE")
                 (0 . "ARC")
                 (0 . "POLYLINE")
                 (0 . "LWPOLYLINE")
                 (0 . "SPLINE")
                 (-4 . "OR>")
                )
     )
    )
  )
  (princ)
)

(defun c:bom_lines ()
  (bom-code '((0 . "LINE")))
  (princ)
)

(defun c:bom_arcs ()
  (bom-code '((0 . "ARC")))
  (princ)
)

(defun c:bom_polylines ()
  (bom-code '((-4 . "<OR")
              (0 . "POLYLINE")
              (0 . "LWPOLYLINE")
              (-4 . "OR>")
             )
  )
  (princ)
)

(defun c:bom_splines ()
  (bom-code '((0 . "SPLINE")))
  (princ)
)


