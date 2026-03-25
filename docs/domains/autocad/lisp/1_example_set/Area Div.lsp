;;;=======================================================
;;;=======================================================
;;;
;;;  FUNCTION: Area Division (AreaDiv.lsp)
;;;  Calculates the area of a partitioned region.
;;;
;;;  AUTHOR
;;;  Copyright © 2009 Lee McDonnell
;;;    (contact ~ Lee Mac, CADTutor.net)
;;;
;;;  VERSION
;;;  1.0  ~  23.03.2009
;;;
;;;=======================================================
;;;=======================================================


(defun c:ADiv  (/ *error* vlst ovar spc cEnt vpt cCur
          cAng clen grlist arpt spt pt1 pt2 iLin
          iArr iLst ptLst plst stpar vpts aPly)
  
  (vl-load-com)

  (defun *error*  (msg)
    (grtext) (redraw)
    (if    ovar (mapcar 'setvar vlst ovar))
    (if    (not (member msg '("Function cancelled" "quit / exit abort")))
      (princ (strcat "\n<!> Error: " (strcase msg) " <!>")))
    (princ))

  (setq    vlst '("CMDECHO" "OSMODE")
    ovar (mapcar 'getvar vlst))
  (mapcar 'setvar vlst '(0 0))

  (setq    spc (vla-get-ModelSpace
          (vla-get-ActiveDocument
        (vlax-get-Acad-Object))))

  (if (and (setq cEnt (entsel "\nSelect Edge for Segregation: "))
       (eq "LWPOLYLINE" (cdadr (entget (car cEnt)))))        
    (progn
      (setq vpt (osnap (cadr cEnt) "_nea")
        cCur (vlax-ename->vla-object (car cEnt))
        cAng (angle    '(0 0 0) (vlax-curve-getFirstDeriv cCur
                   (vlax-curve-getParamAtPoint cCur vpt))))
      (setq clen (distance (vlax-curve-getPointatParam cCur
                 (fix (vlax-curve-getParamAtPoint cCur vpt)))
               (vlax-curve-getPointatParam cCur
                 (1+ (fix (vlax-curve-getParamAtPoint cCur vpt))))))
      (grtext -1 "Select Area Segregation...")
      (while (= 5 (car (setq grlist (grread t 1))))
    (redraw)
    (if (= 'list (type (setq arpt (cadr grlist))))
      (progn
        (setq spt (vlax-curve-getClosestPointto cCur arpt)
          pt1 (polar spt cAng (/ clen 3.0))
          pt2 (polar spt cAng (/ clen -3.0)))
        (grdraw pt1 pt2 3))))
      (setq iLin (vla-Addline spc (vlax-3D-point spt)
           (vlax-3D-point (polar spt cAng clen)))
        iArr (vlax-variant-value
           (vla-IntersectWith iLin cCur acExtendThisEntity)))
      (if (> (vlax-safearray-get-u-bound iArr 1) 0)
    (progn
      (setq iLst (vlax-safearray->list iArr))
      (while (not (zerop (length iLst)))
        (setq ptLst    (cons (list (car iLst) (cadr iLst) (caddr iLst)) ptLst)
          iLst    (cdddr iLst)))
      (and (vla-delete iLin) (setq iLin nil))
      (if (> (length ptlst) 1)
        (progn
          (setq plst  (vl-sort (list (vlax-curve-getParamAtPoint cCur (car ptLst))
                (vlax-curve-getParamAtPoint cCur (cadr ptLst))) '<)
            stpar (1+ (fix (car plst))))
          (while (< stpar (cadr plst))
        (setq plst (append plst (list stpar))
              stpar (1+ stpar)))
          (setq plst (vl-sort plst '<)
            vpts (mapcar '(lambda (p) (vlax-curve-getPointatParam cCur p)) plst))
          (command "_pline") (foreach x vpts (command x)) (command "_C")
          (vla-put-color (setq aPly (vlax-ename->vla-object (entlast))) acRed)
          (princ (strcat "\n<<<  Area of Enclosed Region: " (rtos (vla-get-Area aPly)) " >>>")))
        (princ "\n<!> Selected Segregation not Closed <!>")))
    (princ "\n<!> Area Not Segregated Properly <!>")))
    (princ "\n<!> Nothing Selected or this isn't an LWPLINE <!>"))
  (mapcar 'setvar vlst ovar)
  (grtext) (redraw)
  (princ))

(princ "\n** AreaDiv.lsp Successfully Loaded - type \"ADiv\" to invoke **") (princ)