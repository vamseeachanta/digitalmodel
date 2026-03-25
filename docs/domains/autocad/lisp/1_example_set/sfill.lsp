; SFILL.LSP   1.0 July 14 1988
; By Jamie Clay
; [76703,4204]
;
; SFILL will construct an even division of SOLIDs between any 2 Arcs, 
; Lines or Polylines on any elevation plane.
;
; This routine was once RULESURF.lsp and much of the code was designed
; to collect and use 3dpoints for 3dfaces. It works well with the SOLID
; entity and I think you will find it very useful. Please feel free
; to modify and improve this code.
;
; Note: the entities must "read" in the same direction, else you will
; get a major bow tie effect.

(vmon)
(setq res "10")

; SPATHA Set up first path's point set
(defun spatha (/ path1) 
  (while (= entcheck nil) 
    (setq patha (entsel "\nSelect first surface path: ")) 
    (setq getit (cadr patha)) 
    (setq patha (car patha)) 
    (setq path1 (entget patha)) 
    (if (assoc 38 path1) 
      (setq elev1 (list (cdr (assoc 38 path1)))) 
      (setq elev1 '(0)) 
    ) 
    (setq entcheck (cdr (assoc 0 path1))) 
    (if (/= entcheck "POLYLINE") 
      (if (or (= entcheck "ARC") (= entcheck "LINE")) 
        (progn 
          (command "PEDIT" patha "Y" "") 
          (setq patha (ssget getit)) 
          (setq patha (ssname patha 0)) 
        ) 
        (progn 
          (Prompt "\nPlease use Arcs, Lines or Polylines!") 
          (setq entcheck nil) 
        ) 
      ) 
    ) 
  ) 
  (setq entcheck nil) 
  (setq pathlist path1) 
  (setq pathent patha) 
  (pathout) 
  (setq patha pnts) 
) 
 
; SPATHB set up second path's point set
(defun spathb (/ path2) 
  (while (= entcheck nil) 
    (setq pathb (entsel "\nSelect second surface path: ")) 
    (setq getit (cadr pathb)) 
    (setq pathb (car pathb)) 
    (setq path2 (entget pathb)) 
    (if (assoc 38 path2) 
      (setq elev2 (list (cdr (assoc 38 path2)))) 
      (setq elev2 '(0)) 
    ) 
    (setq entcheck (cdr (assoc 0 path2))) 
    (if (/= entcheck "POLYLINE") 
      (if (or (= entcheck "ARC") (= entcheck "LINE")) 
        (progn 
          (command "PEDIT" pathb "Y" "") 
          (setq pathb (ssget getit)) 
          (setq pathb (ssname pathb 0)) 
        ) 
        (progn 
          (Prompt "\nPlease use Arcs, Lines or Polylines!") 
          (setq entcheck nil) 
        ) 
      ) 
    ) 
  ) 
  (setq entcheck nil) 
  (setq pathlist path2) 
  (setq pathent pathb) 
  (pathout) 
  (setq pathb pnts) 
) 
 
 
; PATHOUT - Divide and collect points
(defun pathout (/ 1stpt) 
  (while (/= stop "SEQEND") 
    (progn 
      (setq lastpl pathlist) 
      (if (/= stop "SEQEND") 
        (progn 
          (setq pathlist (entget (setq pathent (entnext pathent)))) 
          (if (= 1stpt nil) 
            (setq 1stpt (cdr (assoc 10 pathlist))) 
          ) 
        ) 
      ) 
    ) 
    (setq stop (cdr (assoc 0 pathlist))) 
  ) 
  (setq pnts 1stpt) 
  (setq lastpt (cdr (assoc 10 lastpl))) 
  (command "layer" "M" "$nodes" "") 
  (command "divide" getit res) 
  (setq points (ssget "x" '((8 . "$NODES")(0 . "POINT")))) 
  (setq index (1- (sslength points))) 
  (while (/= index -1) 
    (progn 
      (setq XY (ssname points index)) 
      (setq XY (cdr (assoc 10 (entget XY)))) 
      (setq pnts (append pnts XY)) 
      (setq index (1- index)) 
    ) 
  ) 
  (setq pnts (append pnts lastpt)) 
  (command "undo" "1") 
  (setq stop '()) 
) 
 
; 3DPOINTS - build 3dpoints for surfaces 
(defun 3dpoints () 
  (if (= firstpass nil) 
    (progn 
      (setq pt1 (list (nth next1 patha))) 
      (setq next1 (1+ next1)) 
      (setq pt1 (append pt1 (list (nth next1 patha)))) 
      (setq pt1 (append pt1 elev1)) 
      (setq pt2 (list (nth next2 pathb))) 
      (setq next2 (1+ next2)) 
      (setq pt2 (append pt2 (list (nth next2 pathb)))) 
      (setq pt2 (append pt2 elev2)) 
      (setq firstpass 1) 
    ) 
    (progn 
      (setq pt1 pt3) 
      (setq pt2 pt4) 
    ) 
  ) 
  (setq next1 (1+ next1)) 
  (setq pt3 (list (nth next1 patha))) 
  (setq next1 (1+ next1)) 
  (setq pt3 (append pt3 (list (nth next1 patha)))) 
  (setq pt3 (append pt3 elev1)) 
  (setq next2 (1+ next2)) 
  (setq pt4 (list (nth next2 pathb))) 
  (setq next2 (1+ next2)) 
  (setq pt4 (append pt4 (list (nth next2 pathb)))) 
  (setq pt4 (append pt4 elev2)) 
) 
 
;SFILL - The big cheese
(defun c:SFILL (/ patha pathb elev1 elev2 next1 next2 firstpass) 
  (setq res# (getstring (strcat "Resolution <"res">: ")))
  (if (/= res# "")
    (setq res res#)
  )
  (setvar "cmdecho" 0)
  (spatha) 
  (spathb) 
  (setq next1 (setq next2 0)) 
  (while (nth next2 pathb) 
    (progn 
      (3dpoints) 
      (if (nth next2 pathb) 
        (command "solid" pt1 pt2 pt3 pt4 "") 
      ) 
    ) 
  ) 
  (setq firstpass 0)
  (setvar "cmdecho" 1) 
) 
