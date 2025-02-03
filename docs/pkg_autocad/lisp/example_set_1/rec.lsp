;;; -*-  Mode: LISP -*- Syntax: AutoLISP (C) Benjamin Olasov 1988, 1989
;;;      rectangle drawing

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; File: RECT.LSP Copyright (C) Benjamin Olasov    Graphic Systems, Inc.   ;;;
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


;; This program is provided 'as is' without warranty of any kind, either 
;; expressed or implied, including, but not limited to the implied warranties of
;; merchantability and fitness for a particular purpose.  The entire risk as to
;; the quality and performance of the program is with the user.  Should the 
;; program prove defective, the user assumes the entire cost of all necessary 
;; servicing, repair or correction. 
;; AutoLisp and AutoCad are registered trademarks of AutoDesk, Inc.

(gc)
(vmon)
(princ "\nPlease wait- loading.")

;;REC draws a rectangle based on digitizing two corners and prompts for
;;the corner rounding radius.  The rounding radius can be digitized.
;;The default rounding radius is always 0.

(defun C:REC (/ ll lr ul ur rad)
        (graphscr)
        (setvar "blipmode" 1)
        (setq ur (getcorner (setq ll (getpoint "\n\n\nFirst corner: "))
                 "\nOther corner: "))
        (setq ul (list (car ll) (cadr ur))
              lr (list (car ur) (cadr ll))
              rad (getdist "\nCorner round radius <0>: "))
        (setvar "blipmode" 0)
        (setvar "cmdecho" 0)
        (if (or (null rad) (= rad 0))
            (command "pline" ll "w" "0" "0" ul ur lr "c")
            (command "pline" (polar ll (angle ll ul) rad) "w" "0" "0"
                             (polar ul (angle ul ll) rad) "a" "d" ul
                             (polar ul (angle ul ur) rad) "l"
                             (polar ur (angle ur ul) rad) "a" "d" ur
                             (polar ur (angle ur lr) rad) "l"
                             (polar lr (angle lr ur) rad) "a" "d" lr
                             (polar lr (angle lr ll) rad) "l"
                             (polar ll (angle ll lr) rad) "a" "d" ll
                             (polar ll (angle ll ul) rad) "cl"))
        (setvar "cmdecho" 1) 'done)


;;RECT draws a rectangle based on digitizing the lower left corner and 
;;supplying X and Y dimensions.
;;The widths and rounding radius can either digitized or entered in dimension
;;format from the keyboard.

(defun C:RECT (/ ll lr ul ur rad)
       (graphscr)
       (setvar "cmdecho" 0)
       (setvar "blipmode" 1)
       (setq ll (getpoint "\nLower left corner of rectangle: "))
       (setq width (getdist "\nEnter X dimension: ")
             height (getdist "\nEnter Y dimension: "))
       (setq lr (list (+ (car ll) width) (cadr ll))
             ul (list (car ll) (+ (cadr ll) height))
             ur (list (+ (car ll) width) (+ (cadr ll) height))
             rad (getdist "\nCorner round radius <0>: "))
       (setvar "blipmode" 0)
       (if (or (null rad) (= rad 0))
           (command "pline" ll "w" "0" "0" ul ur lr "c")
           (command "pline" (polar ll (angle ll ul) rad) "w" "0" "0"
                            (polar ul (angle ul ll) rad) "a" "d" ul
                            (polar ul (angle ul ur) rad) "l"
                            (polar ur (angle ur ul) rad) "a" "d" ur
                            (polar ur (angle ur lr) rad) "l"
                            (polar lr (angle lr ur) rad) "a" "d" lr
                            (polar lr (angle lr ll) rad) "l"
                            (polar ll (angle ll lr) rad) "a" "d" ll
                            (polar ll (angle ll ul) rad) "cl"))
       (setvar "cmdecho" 1)      
       (setvar "blipmode" 1)
       (princ))

(defun butlast (lst)
       (if (and (listp lst) (cdr lst))
           (reverse (cdr (reverse lst))) nil))

(defun blobdraw (plist rad / p1 *p* *plist*)
        (setvar "blipmode" 0)
        (setvar "cmdecho" 0)
        (g-draw plist)
        (setq len (length plist)
              p1 (car plist)
              *p1* (polar p1 (angle p1 (cadr plist)) rad))
        (if (/= p1 (last plist))
            (setq *plist* (append (cdr plist) (list p1)))
            (setq *plist* (cdr plist)))
        (command "pline" *p1* 
                 (polar (car *plist*) (angle (car *plist*) p1) rad)
                 "a" "d" (car *plist*))
        (foreach p (butlast (butlast *plist*))
                 (setq *p* (cadr (member p *plist*)))
                 (command (polar p (angle p *p*) rad) "l"
                          (polar *p* (angle *p* p) rad) "a" "d" *p*))
        (command *p1* "cl"))


;; BLOB draws n-sided polygons with user specified corner rounding
(defun C:BLOB (/ plist rad pt)
       (graphscr)
       (setvar "coords" 2)
       (setvar "blipmode" 1)
       (setq pt nil
             rad (getdist "\nCorner round radius: "))
       (if rad
           (progn (setq pt (getpoint "\nOrigin point: "))
                  (cond ((not (null pt))
                         (setvar "blipmode" 0)
                         (setq plist (cons pt plist))
                         (setq pt (getpoint (car plist) "\nNext point: "))
                         (grdraw (car plist) pt -1)
                         (setq plist (cons pt plist))
                         (while (and (not (equal (car plist) (car (reverse plist))))
                                (setq pt (getpoint (car plist) "Next point: ")))
                                (grdraw (car plist) pt -1)
                                (setq plist (cons pt plist)))
                         (if (and plist (listp plist) (> (length plist) 2))
                             (blobdraw plist rad)
                             (progn (g-draw plist)
                                    (princ "\nBlob must have at least 3 vertices."))))
                        (T (princ "\nBad point list"))))
            (princ "\nNull radius invalid"))
       (princ))

(defun g-draw (pt-list)
       (cond ((< (length pt-list) 2) nil) ;;termination condition
             (T (grdraw (car pt-list) (cadr pt-list) -1)
                (g-draw (cdr pt-list))))) ;;tail recursion

(princ "\nType REC, RECT or BLOB to begin.")
(princ)

