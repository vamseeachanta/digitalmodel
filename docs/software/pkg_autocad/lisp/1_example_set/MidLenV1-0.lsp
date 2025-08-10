;;----------------------=={ Length at Midpoint }==----------------------;;
;;                                                                      ;;
;;  This program prompts the user for a selection of objects to be      ;;
;;  labelled and proceeds to generate an MText object located at        ;;
;;  the midpoint of each object displaying a Field Expression           ;;
;;  referencing the length of the object.                               ;;
;;                                                                      ;;
;;  The program is compatible for use with Arcs, Circles, Lines,        ;;
;;  LWPolylines, 2D & 3D Polylines, and under all UCS & View settings.  ;;
;;                                                                      ;;
;;  The program will generate MText objects positioned directly over    ;;
;;  the midpoint of each object, and aligned with the object whilst     ;;
;;  preserving text readability. The MText will have a background mask  ;;
;;  enabled and will use the active Text Style and Text Height settings ;;
;;  at the time of running the program.                                 ;;
;;----------------------------------------------------------------------;;
;;  Author:  Lee Mac, Copyright © 2013  -  www.lee-mac.com              ;;
;;----------------------------------------------------------------------;;
;;  Version 1.0    -    12-11-2013                                      ;;
;;                                                                      ;;
;;  First release.                                                      ;;
;;----------------------------------------------------------------------;;

(defun c:midlen ( / *error* ent fmt idx ins ocs par sel spc txt typ uxa )

    (setq fmt "%lu6") ;; Field Formatting

    (defun *error* ( msg )
        (LM:endundo (LM:acdoc))
        (if (not (wcmatch (strcase msg t) "*break,*cancel*,*exit*"))
            (princ (strcat "\nError: " msg))
        )
        (princ)
    )
    
    (if
        (setq sel
            (ssget
                (list
                   '(0 . "ARC,CIRCLE,LINE,*POLYLINE")
                   '(-4 . "<NOT")
                       '(-4 . "<AND")
                           '(0 . "POLYLINE")
                           '(-4 . "&")
                           '(70 . 80)
                       '(-4 . "AND>")
                   '(-4 . "NOT>")
                    (if (= 1 (getvar 'cvport))
                        (cons 410 (getvar 'ctab))
                       '(410 . "Model")
                    )
                )
            )
        )
        (progn
            (setq spc
                (vlax-get-property (LM:acdoc)
                    (if (= 1 (getvar 'cvport))
                        'paperspace
                        'modelspace
                    )
                )
            )
            (setq ocs (trans '(0.0 0.0 1.0) 1 0 t)
                  uxa (angle '(0.0 0.0) (trans (getvar 'ucsxdir) 0 ocs t))
            )
            (LM:startundo (LM:acdoc))
            (repeat (setq idx (sslength sel))
                (setq ent (ssname sel (setq idx (1- idx)))
                      par (vlax-curve-getparamatdist ent (/ (vlax-curve-getdistatparam ent (vlax-curve-getendparam ent)) 2.0))
                      ins (vlax-curve-getpointatparam ent par)
                      typ (cdr (assoc 0 (entget ent)))
                )
                (setq txt
                    (vlax-invoke spc 'addmtext ins 0.0
                        (strcat
                            "%<\\AcObjProp Object(%<\\_ObjId " (LM:objectid (vlax-ename->vla-object ent)) ">%)."
                            (cond
                                (   (= "CIRCLE" typ) "Circumference")
                                (   (= "ARC"    typ) "ArcLength")
                                (   "Length"   )
                            )
                            " \\f \"" fmt "\">%"
                        )
                    )
                )
                (vla-put-backgroundfill  txt :vlax-true)
                (vla-put-attachmentpoint txt acattachmentpointmiddlecenter)
                (vla-put-insertionpoint  txt (vlax-3D-point ins))
                (vla-put-rotation txt (LM:readable (- (angle '(0.0 0.0 0.0) (trans (vlax-curve-getfirstderiv ent par) 0 ocs t)) uxa)))
            )
            (LM:endundo (LM:acdoc))
        )
    )
    (princ)
)

;; Readable  -  Lee Mac
;; Returns an angle corrected for text readability.

(defun LM:readable ( a )
    (   (lambda ( a )
            (if (and (< (* pi 0.5) a) (<= a (* pi 1.5)))
                (LM:readable (+ a pi))
                a
            )
        )
        (rem (+ a pi pi) (+ pi pi))
    )
)

;; ObjectID  -  Lee Mac
;; Returns a string containing the ObjectID of a supplied VLA-Object
;; Compatible with 32-bit & 64-bit systems

(defun LM:objectid ( obj )
    (eval
        (list 'defun 'LM:objectid '( obj )
            (if
                (and
                    (vl-string-search "64" (getenv "PROCESSOR_ARCHITECTURE"))
                    (vlax-method-applicable-p (vla-get-utility (LM:acdoc)) 'getobjectidstring)
                )
                (list 'vla-getobjectidstring (vla-get-utility (LM:acdoc)) 'obj ':vlax-false)
               '(itoa (vla-get-objectid obj))
            )
        )
    )
    (LM:objectid obj)
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

(vl-load-com)
(princ
    (strcat
        "\n:: MidLen.lsp | Version 1.0 | \\U+00A9 Lee Mac "
        (menucmd "m=$(edtime,0,yyyy)")
        " www.lee-mac.com ::"
        "\n:: Type \"midlen\" to Invoke ::"
    )
)
(princ)

;;----------------------------------------------------------------------;;
;;                             End of File                              ;;
;;----------------------------------------------------------------------;;