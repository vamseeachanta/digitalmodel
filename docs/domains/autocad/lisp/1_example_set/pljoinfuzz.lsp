;;; PLJOINFUZZ.LSP
;;; Joins lines, arcs and polylines using a fuzz distance
;;; If only one object is selected it tries to join to all objects that are possible
;;; By Jimmy Bergmark
;;; Copyright (C) 2003-2004 JTB World, All Rights Reserved
;;; Website: www.jtbworld.com
;;; E-mail: info@jtbworld.com
;;; Tested on AutoCAD 2002,2004 and 2005
;;; Latest revision made 2004-11-11
;;; Minor code cosmetic change made 2004-11-13
;;; Bug corrected 2004-12-23

(defun c:jf () (c:pljoinfuzz)) ; this line can be commented out if there is an existing command called jf
(defun c:pljoinfuzz (/ ss1 entLine objType oldcmdecho oldpeditaccept fuzz okObjects)
  (setq oldcmdecho (getvar "cmdecho"))
  (setq oldpeditaccept (getvar "PEDITACCEPT"))
  (setvar "cmdecho" 0)
  (setq A2k4 (>= (substr (getvar "ACADVER") 1 2) "16"))
  (if A2k4 (setvar "PEDITACCEPT" 0))
  (setq	okObjects '((0 . "LINE,ARC,POLYLINE,LWPOLYLINE")))
  (princ "\nSelect object to join: ")
  (setq ss1 (ssget okObjects))
  (setq fuzz (getdist "\nFuzz distance <0>: "))
  (if (= fuzz nil) (setq fuzz 0))
  (if (/= ss1 nil)
      (progn
	(setq objType (cdr (assoc 0 (entget (setq entLine (ssname ss1 0))))))
	(if (= (sslength ss1) 1) (setq ss1 (ssget "X" okObjects)))
	(if (member objType '("LINE" "ARC"))
	  (command "_.pedit" "_M" ss1 "" "_Y" "_J" "_J" "_B" fuzz "")
	  (command "_.pedit" "_M" ss1 "" "_J" "_J" "_B" fuzz "")
	)
      )
  )
  (setvar "cmdecho" oldcmdecho)
  (if A2k4 (setvar "PEDITACCEPT" oldpeditaccept))
  (princ)
)