;;--------------------=={ Object Align }==--------------------;;
;;                                                            ;;
;;  This program will enable the user to dynamically align a  ;;
;;  selection of objects to a selected curve, with intuitive  ;;
;;  placement controls.                                       ;;
;;                                                            ;;
;;  Upon starting the program with the command syntax 'OA',   ;;
;;  the user is prompted to make a selection of objects to    ;;
;;  be aligned. Following a valid selection, the user is      ;;
;;  prompted to specify a base point to use during alignment; ;;
;;  at this prompt, the program will use the center of the    ;;
;;  bounding box of the selection of objects by default.      ;;
;;                                                            ;;
;;  The user is then prompted to select a curve object        ;;
;;  (this may be a Line, Polyline, Arc, Circle, Ellipse,      ;;
;;  XLine, Spline etc.) to which the objects are to be        ;;
;;  aligned. The selected curve may be a primary object, or   ;;
;;  nested with a Block or XRef to any level. After           ;;
;;  selection, the program offers several controls to aid     ;;
;;  with object placement displayed at the command line:      ;;
;;                                                            ;;
;;  [+/-] for [O]ffset | [</>] for [R]otation | <[E]xit>:     ;;
;;                                                            ;;
;;  The offset of the objects from the curve may be           ;;
;;  controlled incrementally by a tenth of the object height  ;;
;;  using the '+' / '-' keys, or a specific offset may be     ;;
;;  entered upon pressing the 'O' or 'o' key.                 ;;
;;                                                            ;;
;;  The set of objects may be rotated anti-clockwise or       ;;
;;  clockwise by 45 degrees relative to the curve by pressing ;;
;;  the '<' or '>' keys respectively; alternatively, the user ;;
;;  may enter a specific rotation by pressing the 'R' or 'r'  ;;
;;  key.                                                      ;;
;;                                                            ;;
;;  Finally, the user may place the objects and exit the      ;;
;;  program by either clicking the left or right mouse        ;;
;;  buttons, pressing Enter or Space, or by pressing the 'E'  ;;
;;  or 'e' keys.                                              ;;
;;                                                            ;;
;;  The program should perform successfully in all UCS &      ;;
;;  Views, and in all versions of AutoCAD that have Visual    ;;
;;  LISP functions available (AutoCAD 2000 onwards).          ;;
;;                                                            ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2012 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;
;;  Version 1.3    -    14-12-2012                            ;;
;;------------------------------------------------------------;;

(defun c:oa

    (
        /
        *error*
        _copynested
        _curveobject-p
        _fixdxfdata
        _locked-p
        _selectif

        bb1 bb2 blk bnm bpt
        def dis
        enl ent
        fac
        gr1 gr2
        inc
        llp lst
        mat msg
        nrm
        obj
        pi2 pt1 pt2
        sel
        tmp
        urp uxa
    )

    (defun *error* ( msg )
        (if (and
                (= 'list  (type mat))
                (= 'ename (type ent))
            )
            (entdel ent)
        )
        (if (and (= 'vla-object (type blk)) (not (vlax-erased-p blk)))
            (vl-catch-all-apply 'vla-delete (list blk))
        )
        (if (and (= 'vla-object (type def)) (not (vlax-erased-p def)))
            (vl-catch-all-apply 'vla-delete (list def))
        )
        (foreach obj lst
            (if (not (vlax-erased-p obj))
                (vl-catch-all-apply 'vla-delete (list obj))
            )
        )
        (LM:endundo (LM:acdoc))
        (if (and msg (not (wcmatch (strcase msg) "*BREAK,*CANCEL*,*EXIT*")))
            (princ (strcat "\nError: " msg))
        )
        (princ)
    )
    
    (defun _curveobject-p ( ent )
        (null
            (vl-catch-all-error-p
                (vl-catch-all-apply 'vlax-curve-getendparam (list ent))
            )
        )
    )

    (defun _fixdxfdata ( elst )
        (vl-remove-if '(lambda ( pair ) (member (car pair) '(5 6 8 102 330))) elst)
    )

    (defun _copynested ( ent mat / enx )
        (if
            (setq ent
                (cond
                    (   (= "VERTEX" (cdr (assoc 0 (setq enx (entget ent)))))
                        (entmakex (_fixdxfdata (entget (setq ent (cdr (assoc 330 enx))))))
                        (setq ent (entnext ent)
                              enx (entget  ent)
                        )
                        (while (/= "SEQEND" (cdr (assoc 0 enx)))
                            (entmakex (_fixdxfdata enx))
                            (setq ent (entnext ent)
                                  enx (entget  ent)
                            )
                        )
                        (cdr (assoc 330 (entget (entmakex (_fixdxfdata enx)))))
                    )
                    (   (entmakex (_fixdxfdata enx))   )
                )
            )
            (if mat (vla-transformby (vlax-ename->vla-object ent) (vlax-tmatrix mat)))
        )
        ent
    )

    (defun _selectif ( msg pred )
        (
            (lambda ( pred / sel )
                (while
                    (progn (setvar 'errno 0) (setq sel (nentselp msg))
                        (cond
                            (   (= 7 (getvar 'errno))
                                (princ "\nMissed, try again.")
                            )
                            (   (= 'ename (type (car sel)))
                                (if (null (pred (car sel)))
                                    (princ "\nInvalid Object Selected.")
                                )
                            )
                        )
                    )
                )
                sel
            )
            (eval pred)
        )
    )
    
    (defun _locked-p ( layer )
        (= 4 (logand 4 (cdr (assoc 70 (tblsearch "LAYER" layer)))))
    )

    (if (null oa|rot)
        (setq oa|rot 0.0)
    )
    (if (null oa|off)
        (setq oa|off 0.0)
    )
    (cond
        (   (or
                (_locked-p (getvar 'clayer))
                (_locked-p "0")
            )
            (princ "\nCurrent Layer or Layer \"0\" locked.")
        )
        (   (null (setq sel (LM:ssget "\nSelect Objects to Align: " '("_:L" ((0 . "~VIEWPORT"))))))
            (princ "\n*Cancel*")
        )
        (   (progn
                (setq mat
                    (vlax-tmatrix
                        (append
                            (mapcar
                               '(lambda ( a b ) (append (trans a 1 0 t) (list b)))
                               '(
                                    (1.0 0.0 0.0)
                                    (0.0 1.0 0.0)
                                    (0.0 0.0 1.0)
                                )
                                (trans '(0.0 0.0 0.0) 0 1)
                            )
                           '((0.0 0.0 0.0 1.0))
                        )
                    )
                )
                (LM:startundo (LM:acdoc))
                (repeat (setq inc (sslength sel))
                    (setq obj (vla-copy (vlax-ename->vla-object (ssname sel (setq inc (1- inc)))))
                          lst (cons obj lst)
                    )
                    (vla-transformby obj mat)
                    (if (and (vlax-method-applicable-p obj 'getboundingbox)
                            (not
                                (vl-catch-all-error-p
                                    (vl-catch-all-apply 'vla-getboundingbox (list obj 'llp 'urp))
                                )
                            )
                        )
                        (setq bb1 (cons (vlax-safearray->list llp) bb1)
                              bb2 (cons (vlax-safearray->list urp) bb2)
                        )
                    )
                    (vla-put-visible obj :vlax-false)
                )
                (setq bb1 (apply 'mapcar (cons 'min bb1))
                      bb2 (apply 'mapcar (cons 'max bb2))
                )
                (cond
                    (   (setq bpt (getpoint "\nSpecify Base Point <Center>: "))
                        (setq bpt (trans bpt 1 0))
                    )
                    (   (setq bpt (mapcar (function (lambda ( a b ) (/ (+ a b) 2.0))) bb1 bb2)))
                )
                (null
                    (setq enl
                        (_selectif "\nSelect Curve: "
                            (function
                                (lambda ( x )
                                    (or (= "VERTEX" (cdr (assoc 0 (entget x)))) (_curveobject-p x))
                                )
                            )
                        )
                    )
                )
            )
            (*error* nil)
        )
        (   (not
                (or
                    (and
                        (setq mat (caddr enl))
                        (setq ent (_copynested (car enl) mat))
                    )
                    (and
                        (= "VERTEX" (cdr (assoc 0 (entget (car enl)))))
                        (setq ent (cdr (assoc 330 (entget (car enl)))))
                    )
                    (setq ent (car enl))
                )
            )
            (*error* nil)
            (princ "\nUnable to Recreate Nested Entity.")
        )
        (   t         
            (setq pt1 (cadr (grread t 9))
                  fac (/ (- (cadr bb2) (cadr bb1)) 2.0)
                  nrm (trans '(0.0 0.0 1.0) 1 0 t)
                  uxa (angle '(0.0 0.0 0.0) (trans (getvar 'ucsxdir) 0 nrm t))
                  pi2 (/ pi -2.0)
            )
            (setq inc 0)
            (while (tblsearch "BLOCK" (setq bnm (strcat "$tmp" (itoa (setq inc (1+ inc)))))))
            (foreach obj lst (vla-put-visible obj :vlax-true))
            (vla-copyobjects (LM:acdoc)
                (vlax-make-variant
                    (vlax-safearray-fill
                        (vlax-make-safearray vlax-vbobject (cons 0 (1- (length lst))))
                        lst
                    )
                )
                (setq def (vla-add (vla-get-blocks (LM:acdoc)) (vlax-3D-point bpt) bnm))
            )
            (foreach obj lst (vla-delete obj))
            (setq lst nil)
         
            (setq blk
                (vla-insertblock
                    (vlax-get-property (LM:acdoc) (if (= 1 (getvar 'cvport)) 'paperspace 'modelspace))
                    (vlax-3D-point (trans pt1 1 0))
                    bnm
                    1.0 1.0 1.0 0.0
                )
            )
            (vla-put-layer blk "0")
            (vla-put-normal blk (vlax-3D-point nrm))
            (setq msg (princ "\n[+/-] for [O]ffset | [</>] for [R]otation | <[E]xit>: "))

            (while
                (progn
                    (setq gr1 (grread t 15 0)
                          gr2 (cadr gr1)
                          gr1 (car  gr1)
                    )
                    (cond
                        (   (member gr1 '(3 5))
                            (setq pt2 (trans gr2 1 0)
                                  pt1 (vlax-curve-getclosestpointto ent pt2)
                            )
                            (if (not (equal pt1 pt2 1e-8))
                                (progn
                                    (setq dis (/ (* fac oa|off) (distance pt1 pt2)))
                                    (vla-put-insertionpoint blk (vlax-3D-point (mapcar '(lambda ( a b ) (+ a (* (- b a) dis))) pt1 pt2)))
                                    (vla-put-rotation blk (+ (angle (trans pt1 0 1) gr2) uxa oa|rot pi2))
                                )
                            )
                            (= 5 gr1)
                        )
                        (   (= 2 gr1)
                            (cond
                                (   (member gr2 '(043 061))
                                    (setq oa|off (+ oa|off 0.1))
                                )
                                (   (member gr2 '(045 095))
                                    (setq oa|off (- oa|off 0.1))
                                )
                                (   (member gr2 '(044 060))
                                    (setq oa|rot (+ oa|rot (/ pi 4.0)))
                                )
                                (   (member gr2 '(046 062))
                                    (setq oa|rot (- oa|rot (/ pi 4.0)))
                                )
                                (   (member gr2 '(013 032 069 101))
                                    nil
                                )
                                (   (member gr2 '(082 114))
                                    (if (setq tmp (getangle (strcat "\nSpecify Rotation <" (angtos oa|rot) ">: ")))
                                        (setq oa|rot tmp)
                                    )
                                    (princ msg)
                                )
                                (   (member gr2 '(079 111))
                                    (if (setq tmp (getdist (strcat "\nSpecify Offset <" (rtos (* fac oa|off)) ">: ")))
                                        (setq oa|off (/ tmp fac))
                                    )
                                    (princ msg)
                                )
                                (   t   )
                            )
                        )
                        (   (member gr1 '(011 025))
                            nil
                        )
                        (   t   )
                    )
                )
            )
            (if mat (entdel ent))
            (vla-explode blk)
            (vla-delete  blk)
            (vla-delete  def)
            (LM:endundo (LM:acdoc))
        )
    )
    (princ)
)

;; ssget  -  Lee Mac
;; A wrapper for the ssget function to permit the use of a custom selection prompt
;;
;; Arguments:
;; msg    - selection prompt
;; params - list of ssget arguments

(defun LM:ssget ( msg params / sel )
    (princ msg)
    (setvar 'nomutt 1)
    (setq sel (vl-catch-all-apply 'ssget params))
    (setvar 'nomutt 0)
    (if (not (vl-catch-all-error-p sel)) sel)
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
    
;;------------------------------------------------------------;;

(vl-load-com)
(princ
    (strcat
        "\n:: ObjectAlign.lsp | Version 1.3 | © Lee Mac "
        (menucmd "m=$(edtime,$(getvar,DATE),YYYY)")
        " www.lee-mac.com ::"
        "\n:: Type \"OA\" to Invoke ::"
    )
)
(princ)

;;------------------------------------------------------------;;
;;                         End of File                        ;;
;;------------------------------------------------------------;;