;;; -*-  Mode: LISP -*- Syntax: AutoLISP (C) Benjamin Olasov 1988
;;;      Functions to generate recursively defined drawings.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; File: DRAGON.LSP Copyright (C) Benjamin Olasov    Graphic Systems, Inc. ;;;
;;; Inquiries:                                                              ;;;
;;;                                                                         ;;;
;;;     Benjamin Olasov                                                     ;;;
;;;     Graphic Systems, Inc.:                                              ;;;
;;;                                                                         ;;;
;;;                    New York, NY:   PH (212) 725-4617                    ;;;
;;;                    Cambridge, MA:  PH (617) 492-1148                    ;;;
;;;                    MCI-Mail:       GSI-NY   344-4003                    ;;;
;;;                    Arpanet:        olasov@cs.columbia.edu               ;;;
;;;                                                                         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This program is provided 'as is' without warranty of any kind, either 
;; expressed or implied, including, but not limited to the implied warranties of
;; merchantability and fitness for a particular purpose.  The entire risk as to
;; the quality and performance of the program is with the user.  Should the 
;; program prove defective, the user assumes the entire cost of all necessary 
;; servicing, repair or correction. 
;; AutoLisp and AutoCad are registered trademarks of AutoDesk, Inc.


(VMON)
(GC)
(TEXTSCR)
(PROMPT "\nC:DRAGON and C:C-CURVE generate recursively defined drawings.")

; Random number generator
(DEFUN RAN ()
       (SETQ SEED (IF SEED (REM (+ (* SEED 15625.7) 0.21137152) 1) 0.3171943)))

(DEFUN C:DEMO () 
       (GRAPHSCR) 
       (DEFUN *ERROR* (MSG)
              (PRINC MSG))
       (SETQ CEN (GETVAR "VIEWCTR")
             HGT (GETVAR "VIEWSIZE")
             DIST1 HGT)
       (WHILE (/= 1 2)
              (SETQ DIST2 (* (RAN) DIST1)
                    DIST3 (* (RAN) DIST1)
                    A1 (* (/ PI 2.0) (RAN))
                    A2 (* (/ PI 2.0) (RAN))
                    P1 (POLAR CEN A1 DIST2)
                    P2 (POLAR CEN A2 DIST3)
                    LENG (DISTANCE P1 P2)
                    MIN-LENG (MAX (/ HGT 161.8) (/ LENG 160.0)) 
                    ANGL (ANGLE P1 P2)
                    LEVEL 0)
           (DRAGON LENG ANGL (+ 0.0 1.0) MIN-LENG) 
           (REDRAW))
(PRINC))

(DEFUN PLOT-LINE (L A)
       (GRDRAW BASE (POLAR BASE A L) LEVEL)
       (SETQ BASE (POLAR BASE A L)))

(DEFUN C-CURVE (LEN ANG MIN-LEN)
       (SETQ LEVEL (1+ LEVEL))
       (PRINC "\nLevel ")
       (PRINC LEVEL)
       (COND ((< LEN MIN-LEN)
              (PRINC "\nPLOT-LINE at level ")
              (PRINC LEVEL)
              (PLOT-LINE LEN ANG))
             (T (C-CURVE (/ LEN (SQRT 2.0))
                         (+ ANG (/ PI 4.0))
                          MIN-LEN)
                (C-CURVE (/ LEN (SQRT 2.0))
                         (- ANG (/ PI 4.0))
                         MIN-LEN)))
       (PROGN (SETQ LEVEL (1- LEVEL))
              (PRINC "\nLevel ")
              (PRINC LEVEL)))

(DEFUN C:C-CURVE (/ BASE PT2 ANGL MIN-LENG CURVE-VAR)
       (GRAPHSCR)
       (SETQ LENG (DISTANCE (SETQ BASE (GETPOINT "\nStarting point: "))
                            (SETQ PT2 (GETPOINT BASE
                                               "\nDigitize length/ angle: "))) 
             ANGL (ANGLE BASE PT2) 
             MIN-LENG (MAX (/ (GETVAR "VIEWSIZE") 100.0) (/ LENG 160.0)) 
             LEVEL 0) 
       (C-CURVE LENG ANGL MIN-LENG) 
       (GETSTRING "\nPress Return to clear: ") 
       (REDRAW))

(DEFUN DRAGON (LEN ANG SIGN MIN-LEN)
       (SETQ LEVEL (1+ LEVEL))
       (PRINC "\nLevel ")
       (PRINC LEVEL)
       (COND ((< LEN MIN-LEN)
              (PRINC "\nPLOT-LINE called at level ")
              (PRINC LEVEL)
              (PLOT-LINE LEN ANG))
             (T (DRAGON (/ LEN (SQRT 2.0))
                         (+ ANG (* SIGN (/ PI 4.0)))
                          +1.0 MIN-LEN)
                (DRAGON (/ LEN (SQRT 2.0))
                         (- ANG (* SIGN (/ PI 4.0)))
                         -1.0 MIN-LEN)))
       (PROGN (SETQ LEVEL (1- LEVEL))
              (PRINC "\nLevel ")
              (PRINC LEVEL)))

(DEFUN C:DRAGON (/ BASE PT2 ANGL MIN-LENG CURVE-VAR) 
       (GRAPHSCR) 
       (SETQ LENG (DISTANCE (SETQ BASE (GETPOINT "\nStarting point: ")) 
                            (SETQ PT2 (GETPOINT BASE 
                                                "\nDigitize length/ angle: "))) 
              ANGL (ANGLE BASE PT2) 
              MIN-LENG (MAX (/ (GETVAR "VIEWSIZE") 100.0) (/ LENG 160.0)) 
              LEVEL 0) 
       (DRAGON LENG ANGL (+ 0.0 1.0) MIN-LENG) 
       (GETSTRING "\nPress RETURN to clear: ") 
       (REDRAW))

(PROMPT "\n\nType DRAGON or C-CURVE to generate a recursively defined drawing.")
(PRINC)


