;;---------------------------=={ Align Text }==-------------------------;;
;;                                                                      ;;
;;  This program enables the user to reposition a selection of          ;;
;;  single-line text objects to be aligned by their text alignment      ;;
;;  points in a direction perpendicular to the rotation of the text,    ;;
;;  optionally equispaced by a factor of the text height.               ;;
;;                                                                      ;;
;;  The program assumes all text objects in the selection have the      ;;
;;  same rotation and will align each text object using the coordinates ;;
;;  of the text alignment point.                                        ;;
;;                                                                      ;;
;;  The program will perform successfully with text constructed in      ;;
;;  any UCS plane.                                                      ;;
;;----------------------------------------------------------------------;;
;;  Author:  Lee Mac, Copyright © 2013  -  www.lee-mac.com              ;;
;;----------------------------------------------------------------------;;
;;  Version 1.4    -    2016-01-16                                      ;;
;;----------------------------------------------------------------------;;

(defun c:at ( / *error* ang bp1 bp2 enx inc ins lst ocs sel spf vc1 vc2 )

    (defun *error* ( msg )
        (LM:endundo (LM:acdoc))
        (if (not (wcmatch (strcase msg t) "*break,*cancel*,*exit*"))
            (princ (strcat "\nError: " msg))
        )
        (princ)
    )
    
    (if (setq sel (ssget "_:L" '((0 . "TEXT"))))
        (progn
            (initget 6)
            (setq spf (cond ((getdist "\nSpecify line spacing factor <use existing>: ")))
                  inc (sslength sel)
                  enx (entget (ssname sel (1- inc)))
                  ang (cdr (assoc 50 enx))
                  ocs (trans '(0.0 0.0 1.0) 1 0 t)
                  vc1 (trans (list    (cos ang)  (sin ang)) ocs 0)
                  vc2 (trans (list (- (sin ang)) (cos ang)) ocs 0)
                  spf (if spf (* (cdr (assoc 40 enx)) spf))
            )
            (repeat inc
                (setq enx (entget (ssname sel (setq inc (1- inc))))
                      lst (cons (list  (trans (aligntext:gettextinsertion enx) (cdr (assoc -1 enx)) 0) enx) lst)
                      ins (cons (caddr (trans (caar lst) 0 vc2)) ins)
                )
            )
            (setq lst (mapcar '(lambda ( n ) (nth n lst)) (vl-sort-i ins '>))
                  bp1 (caddr (trans (caar lst) 0 vc1))
                  bp2 (caddr (trans (caar lst) 0 vc2))
            )
            (LM:startundo (LM:acdoc))
            (foreach  itm (cdr lst)
                (if spf
                    (setq ins (trans (car itm) 0 vc2)
                          ins (trans (list (car ins) (cadr ins) (- bp2 spf)) vc2 vc1)
                          bp2 (- bp2 spf)
                    )
                    (setq ins (trans (car itm) 0 vc1))
                )
                (aligntext:puttextinsertion
                    (trans (list (car ins) (cadr ins) bp1) vc1 (cdr (assoc -1 (cadr itm))))
                    (cadr itm)
                )
            )
            (LM:endundo (LM:acdoc))
        )
    )
    (princ)
)

(defun aligntext:getdxfkey ( enx )
    (if
        (and
            (zerop (cdr (assoc 72 enx)))
            (zerop (cdr (assoc 73 enx)))
        )
        10 11
    )
)

(defun aligntext:gettextinsertion ( enx )
    (cdr (assoc (aligntext:getdxfkey enx) enx))
)

(defun aligntext:puttextinsertion ( ins enx )
    (   (lambda ( key )
            (if (entmod (subst (cons key ins) (assoc key enx) enx))
                (entupd (cdr (assoc -1 enx)))
            )
        )
        (aligntext:getdxfkey enx)
    )
)

;; Start Undo  -  Lee Mac
;; Opens an Undo Group.

(defun LM:startundo ( doc )
    (LM:endundo doc)
    (vla-startundomark doc)
)

;; End Undo  -  Lee Mac
;; Closes an Undo Group.

(defun LM:endundo ( doc )
    (while (= 8 (logand 8 (getvar 'undoctl)))
        (vla-endundomark doc)
    )
)

;; Active Document  -  Lee Mac
;; Returns the VLA Active Document Object

(defun LM:acdoc nil
    (eval (list 'defun 'LM:acdoc 'nil (vla-get-activedocument (vlax-get-acad-object))))
    (LM:acdoc)
)

(vl-load-com) (princ)

;;----------------------------------------------------------------------;;
;;                             End of File                              ;;
;;----------------------------------------------------------------------;;