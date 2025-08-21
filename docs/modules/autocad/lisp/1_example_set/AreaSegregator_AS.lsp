;;; CADALYST 10/08  www.cadalyst.com/CADtips
;;; Tip 3016: AreaSegregator.lsp   Subdivide Lot into Specified Desired Areas  (c) 2008 Noel Gemilga
;;;
;;;AREA SEGREGATOR.LSP  by:Noel  A. Gemilga  date:     May-13-08
;
;|
; ----------------------------------------------------------------------------------------------
; DISCLAIMER:  I Disclaims any and all liability for any damages
; arising out of the use or operation, or inability to use the software.
; FURTHERMORE, User agrees to hold me harmless from such claims.
; I make no warranty, either expressed or implied, as to the
; fitness of this product for a particular purpose.  All materials are
; to be considered as-is, and use of this software should be considered as
; AT YOUR OWN RISK. God Bless and Happy drafting !!!...
; -----------------------------------------------------------------------------------------------
;(Thanks to some advises and guides from Mr. Jeffrey Sanders)
;
;NOTES:
;1. The initial Partition Boundary line shall be an XLINE.
;2. In NO CASE shall the "to be subdivided lot" was drawn using entities such as a point, an xline, polyline,
;   and closed polyline. Just have it drawn simply using a line. If it's drawn using bpoly, explode it.
;3. For Delta or Incremental Line for iteration, The smaller the length, the bigger its accuracy may be..
;     It is hereby suggested to use L=0.75units (but since it may loop several times to come-up to the required area,
;     Run first using longer horizontal incremental line, mark the nearest, and place the partition xline on the mark
;     so you can have fewer looping cycle).
;4. Since in this routine, our partition line travels horizontally, the author hereby deters you to use xline partition
;     lines whose inclination angle is near horizontal. It is better to rotate the figure first, subdivide using this
;     routine, and reposition the whose figure to its previous setup or location ( probably by using the ALIGN command).
;5. If your required area is located at the left, make the incremental start at from the left to right.
;   While, if your required area is located at the right, make the incremental start at from the right to left.
;6. The entire lot, that shall be divided with, shall be visible in 
;   the monitor to avoid possible error. It shall not be made-up of lines only.
|;
;^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
;This routine segregates the required area.
;
;START OF THE MAIN ROUTINE
;
;
(defun C:AS (/ errexit oldcmdecho olderr entlist stpt endpt dx ptA fx fy fx1 ss1 ptB areai nr en tot_area1 tot_areaa eset MMX MM ss2 ns em)
;------
  (defun errexit (s)
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

;------
   (setvar "cmdecho" 0)
(command "orthomode" 0 )
(prompt "\nSelect an increment-length line:"  )
(setq entList(entget (car (entsel))))
(setq stpt (cdr (assoc 10 entlist)))
(setq endPt (cdr (assoc 11 entlist)))
(setq dx (distance stpt endpt))
   (setq ptA (getpoint "\nPoint the REQUIRED to-be segregated LOT..."))
   (setq fx (car ptA))
   (setq fy (cadr ptA))
   (setq fx1 (+ dx fx))
   (setq ptB (list fx1 0 0 ))
(prompt "\nArea per lot...") 
   (setq areai (getreal))
;(command "xline" "v" ptB nil)
  (command "point" ptA nil)
  (command "-boundary" "a" "o" "p" "" ptA "")
  (command "._area" "o" (entlast) nil)
 (if (setq ss1 (ssget "X" (list (cons 0 "LWPOLYLINE")))
     )
    (progn
      (setq nr 0)
      (setq tot_area1 0.0)
      (setq en (ssname ss1 nr))
       (while en
        (command "._area" "_O" en)
        (setq tot_area1 (+ tot_area1 (getvar "area")))
        (setq nr (1+ nr))
        (setq en (ssname ss1 nr))
       )
(princ "\nNR=" )
(princ nr)
(princ "\nTotal Area1 = ")
(princ tot_area1)(terpri)
    )
 )
;--------
(setq tot_areaa 0)
(while (< tot_areaa areai)(< tot_areaa areai)
;--------
(progn (Command "erase" (ssget "X" (list (cons 0 "LWPOLYLINE"))) "" )
 (if(setq eset (ssget "X" (list (cons 0 "xline"))))
   (progn
        (command "move" eset "" stpt endpt)
  ))
;This next line can be omitted
;(prompt "\nThe location of MM is at... ")
(setq MMX (+ fx (* (+ nr 0.5) dx ) ))
(setq MM (list MMX fy 0 ))
  (command ".point" MM )
  (command "-boundary" "a" "o" "p" "" MM "")
  (if (setq ss2 (ssget "X" (list (cons 0 "lwpolyline")))
     )
     (progn
      (setq ns 0)
      (setq tot_areaa 0.0)
      (setq em (ssname ss2 ns))
       (while em
        (command "._area" "_O" em)
        (setq tot_areaa (+ tot_areaa (getvar "area")))
        (setq ns (1+ ns))
        (setq em (ssname ss2 ns))
       )
;The next 2 lines below can be omitted
;(princ "\nNs=" )
;(princ ns)
(princ "\nStill running, Area now = ")
(princ tot_areaa)(terpri)
    )
 )

)
)
;-------------------------------

(Command "erase" (ssget "X" (list (cons 0 "POint"))) "" )
;This next line can be inactivated depending on the user
(Command "erase" (ssget "X" (list (cons 0 "xline"))) "" )
(command "orthomode" 1 )
(setvar "cmdecho" 1)
(princ "\nThe Total Area in this run = ")
(princ tot_areaa)(terpri)
(princ "...The command AS of this routine is Completed. \n ")
(princ)
)
;END OF MAIN ROUTINE
;^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
;a.-This routine below makes a horizonal line, half of length with the picked xline. This will make the partition line
;   move backward 5 times. 
;b.-Suggeastion: Make the incremental line smaller, (preferably, command "scale" to make it smaller into 40% only of 
;                the previous.
;
(defun C:HHH (/ enlist stpt endpt dx dxh dhs dhe esett)
   (setvar "cmdecho" 0)
(prompt "\nSelect a line to be used as LINE-TRANSFER increment:"  )
(setq entList(entget (car (entsel))))
(setq stpt (cdr (assoc 10 entlist)))
(setq endPt (cdr (assoc 11 entlist)))
(setq dx (distance stpt endpt))
(setq dxh (* -0.5 dx))
(setq dhs (list 0 0 0 ))
(setq dhe (list dxh 0 0 ))
(repeat 5
  (if(setq esett (ssget "X" (list (cons 0 "xline"))))
   (progn
        (command "move" esett "" endpt stpt)
    )
  )
)
   (command "move" esett "" dhs dhe )
   (setvar "cmdecho" 1)
(princ ".....HHH.lsp Complete. \n ")
(princ)
)
;^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
