; Next available MSG number is    12 
; MODULE_ID EDGE_LSP_
;;;
;;;    edge.lsp            
;;;    
;;;    Copyright 1988-2003 by Autodesk, Inc.
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
;;;.
;;;
;;; --------------------------------------------------------------------------;
;;; DESCRIPTION
;;;
;;;   Interactive editor for changing the visibility of 3DFACE edges.           
;;;   Prompt:         "Display/<Select edge>: "                                 
;;;
;;;   Features:                                                                 
;;;    > "Display" -  Allows selective regeneration of 3DFACE's highlighting    
;;;                   invisible edges.                                          
;;;    >  Select   -  Reverses the visibility of each edge found.               
;;;
;;;   Note:                                                                     
;;;    >  EDGE uses Osnap MIDpoint as the center of a small crossing box        
;;;       when selecting edges.                                                 
;;;    >  Only edges displayed (or highlighted) can be modified.  (Use          
;;;       "Display" to display a 3DFACE.)                                       
;;;    >  Invisible edges will always be displayed if the system variable       
;;;       SPLFRAME is set to 1.                                                 
;;;
;;; --------------------------------------------------------------------------;


;;; Set CMDECHO without undo recording
(defun edge_setCmdEcho ( newVal / _oldEnvVal)
  ; Only do it if the value is different than the current value
  (if (/= newVal (getvar "CMDECHO"))
    (progn
      (setq _oldEnvVal (getenv "acedChangeCmdEchoWithoutUndo"))
      ; If not set yet, use 0 for default
      (if (not _oldEnvVal)
          (setq _oldEnvVal "0"))
      (setenv "acedChangeCmdEchoWithoutUndo" "1")
      (setvar "cmdecho" newVal)
      (setenv "acedChangeCmdEchoWithoutUndo" _oldEnvVal)
    )
  )
)

;;; ----------------------------- CREATE NEW *ERROR* -------------------------;

(defun edge-er (n) 
  (if (/= s "Function cancelled") 
    (princ (strcat "\nError: " n))
  ) 
  (command) 
  (command "_.UCS" "_P") 
  (setvar "osmode" o1)
  (setvar "gridmode" g1)
  (setvar "aperture" a1)
  (setvar "splframe" v1)
  (setvar "ucsfollow" u1)
  (setq n1 -1)
  (repeat (sslength faclst) 
    (redraw (entupd (ssname faclst (setq n1 (1+ n1)))))
  )  
  (command "_.UNDO" "_E")
  ;; Restore CMECHO without undo recording
  (edge_setCmdEcho _edge_oldCmdEcho)
  (setq *error* lisp-er)
  (prin1)
) 
;;; ---------------------------- COMMONLY USED MACROS ------------------------;

(defun getval (n e) 
  (cdr (assoc n e))
) 

(defun fltfac (ss / n1) 
  (setq n1 0)
  (if ss 
    (repeat (sslength ss) 
      (if (/= (getval 0 (entget (setq e1 (ssname ss n1)))) "3DFACE") 
        (ssdel e1 ss) 
        (setq n1 (1+ n1))
      )
    )
  ) 
  ss
) 

;;; ------------------------- FORCE DISPLAY OF ALL EDGES ---------------------;

(defun dsply (/ ss n1 t1) 
  (setvar "osmode" 0)
  (initget "All Select") 
  (setq ss (if (eq (getkword "\nEnter selection method for display of hidden edges [Select/All] <All>: ") "Select") 
             (fltfac (ssget)) 
             (ssget "_x" '((0 . "3dface")))
           ) 
        n1 -1)
  (setvar "osmode" 2)
  (cond (ss (princ "\n** Regenerating 3DFACE objects...") 
            (repeat (sslength ss) 
              (ssadd (setq t1 (ssname ss (setq n1 (1+ n1)))) faclst) 
              (shohdn (entget (entupd t1)))) 
            (princ "done.") T) 
    (T (princ "\nNo 3DFACE objects found.") nil)
  )
) 
;;; ----------------------------- SHOW HIDDEN EDGES --------------------------;

(defun shohdn (e / b1 p1 p2 p3 p4) 
  (setq b1 (getval 70 e))
  (mapcar '(lambda (j k) (set j (getval k e))) 
          '(p1 p2 p3 p4) 
          '(10 11 12 13)) 
  (if (= (logand b1 1) 1) 
    (grdraw p1 p2 c1 1)
  ) 
  (if (= (logand b1 2) 2) 
    (grdraw p2 p3 c1 1)
  ) 
  (if (= (logand b1 4) 4) 
    (grdraw p3 p4 c1 1)
  ) 
  (if (= (logand b1 8) 8) 
    (grdraw p4 p1 c1 1)
  )
) 

;;; --------------------------- GET ENTITY TO EDIT ---------------------------;

(defun getfce (pt / ll ur n1 ss e1 p1) 
  (setq p1 (trans pt 0 2) 
        ll (trans (polar P1 (/ (* pi 5) 4) h1) 2 0) 
        ur (trans (polar P1 (/ pi 4) h1) 2 0) 
        n1 0)
  (setvar "osmode" 0)
  (if (setq ss (ssget "_c" ll ur))
    (setq ss (fltfac ss))
  ) 
  (setvar "osmode" 2)
  ss
) 

;;; --------------------- MODIFY 3DFACE EDGE VISIBILITY FLAG -----------------;

(defun modfce (ss pt / n1 e1 e0 p0 b1 b2 b3) 
  (setq n1 0)
  (repeat (sslength ss) 
    (setq e1 (entget (ssname ss n1)) 
          e0 (getval -1 e1))
    (ssadd e0 faclst) 
    (mapcar '(lambda (j k) (set j (getval k e1))) 
            '(p1 p2 p3 p4) 
            '(10 11 12 13)) 
    (setq p0 (if (equal (distance p3 p4) 0 1e-8) 
               (mapcar '(lambda (j k l) (/ (+ j k l) 3)) p1 p2 p3) 
               (mapcar '(lambda (j k l m) (/ (+ j k l m) 4)) p1 p2 p3 p4)
             ))
    (setq b1 (getval 70 e1) 
          b2 (cond ((equal pt (inters p0 pt p1 p2) h1) 1) 
               ((equal pt (inters p0 pt p2 p3) h1) 2) 
               ((equal pt (inters p0 pt p3 p4) h1) 4) 
               ((equal pt (inters p0 pt p4 p1) h1) 8) 
               (T 0)
             ) 
          b3 (+ b1 (if (= (logand b1 b2) b2) 
                     (- b2) 
                     b2
                   )
             ) 
          e1 (shohdn (entmod (subst (cons 70 b3) (assoc 70 e1) e1))) 
          n1 (1+ n1))
  ) 
  T
) 
;;; ------------------------------ MAIN PROGRAM ------------------------------;

(defun c:EDGE (/ lisp-er s1 o1 g1 a1 v1 h1 u1 faclst c1 r1 t1 ss pt n e n1) 
  (setq lisp-er *error* 
        *error* edge-er 
        s1 (getvar "cmdecho") 
        _edge_oldCmdEcho s1
        o1 (getvar "osmode") 
        g1 (getvar "gridmode") 
        a1 (getvar "aperture") 
        v1 (getvar "splframe") 
        u1 (getvar "ucsfollow") 
        h1 (/ (getvar "viewsize") 100) 
        faclst (ssadd) 
        c1 7 
        r1 T)
   ; Set CMDECHO without undo recording
  (edge_setCmdEcho 0)
  (command "_.UNDO" "_GROUP") 
  (setvar "osmode" 2)
  (setvar "gridmode" 0)
  (setvar "aperture" 5)
  (setvar "splframe" 1)
  (setvar "ucsfollow" 0)
  (command "_.UCS" "_W") 
  (while r1 
    (initget "Display") 
    (setq t1 (getpoint "\nSpecify edge of 3dface to toggle visibility or [Display]: ") 
          r1 (cond ((eq t1 "Display") (dsply)) 
               ((eq (type t1) 'LIST) (setq ss (getfce t1))
                (cond ((null ss) (princ " No 3DFACE edges found.")) 
                  ((= (sslength ss) 0) (princ " Object is not a 3DFACE.")) 
                  (t (modfce ss t1))
                )
               ) 
               (T nil)
             ))
  ) 
  (command "_.UCS" "_P") 
  (setvar "osmode" o1)
  (setvar "gridmode" g1)
  (setvar "aperture" a1)
  (setvar "splframe" v1)
  (setvar "ucsfollow" u1)
  (setq n1 -1)
  (repeat (sslength faclst) 
    (redraw (entupd (ssname faclst (setq n1 (1+ n1)))))
  ) 
  (redraw) ; to remove highlighted screen graphics
  (command "_.UNDO" "_E") 
  ;; Restore CMECHO without undo recording
  (edge_setCmdEcho _edge_oldCmdEcho)

  (setq *error* lisp-er)
  (prin1)
) 
