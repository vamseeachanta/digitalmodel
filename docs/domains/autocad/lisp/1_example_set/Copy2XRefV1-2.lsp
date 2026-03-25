;;-------------------------=={ Copy to XRef }==-------------------------;;
;;                                                                      ;;
;;  This program enables the user to copy a selection of objects to a   ;;
;;  selected xref, without opening the xref source drawing.             ;;
;;                                                                      ;;
;;  Upon calling the program with 'c2x' at the command-line, the user   ;;
;;  is prompted to make a selection of objects to copy. Following a     ;;
;;  valid response, the user is then prompted to select an External     ;;
;;  Reference (xref) to which the objects are to be copied.             ;;
;;                                                                      ;;
;;  The program will then proceed to copy the selected objects to the   ;;
;;  source drawing of the selected xref using a deep-clone method,      ;;
;;  coupled with an ObjectDBX interface should the xref source drawing  ;;
;;  be unopened in the current drawing session.                         ;;
;;                                                                      ;;
;;  Upon copying the selection, the xref source drawing is saved and    ;;
;;  the xref is reloaded in the current drawing; the selected objects   ;;
;;  are then deleted from the current drawing.                          ;;
;;                                                                      ;;
;;  The program will account for the position, scale, rotation &        ;;
;;  orientation of the xref relative to the selection of objects and    ;;
;;  will perform successfully under all UCS & View settings.            ;;
;;                                                                      ;;
;;  Please Note:                                                        ;;
;;  ------------------------------                                      ;;
;;  The act of copying objects to the xref source drawing involves      ;;
;;  saving the external drawing remotely - this action cannot be        ;;
;;  undone within the current drawing and changes to the external       ;;
;;  drawing must be reset manually.                                     ;;
;;                                                                      ;;
;;  Note that when saving drawings through ObjectDBX, drawing file      ;;
;;  thumbnails will be lost until the next manual save.                 ;;
;;----------------------------------------------------------------------;;
;;  Author:  Lee Mac, Copyright © 2014  -  www.lee-mac.com              ;;
;;----------------------------------------------------------------------;;
;;  Version 1.2    -    2014-06-21                                      ;;
;;----------------------------------------------------------------------;;

(defun c:c2x ( / *error* acd app dbx def doc dwg dwl ent enx err inc lst mat obj sel vrs xrl )

    (defun *error* ( msg )
        (if (and (= 'vla-object (type dbx)) (not (vlax-object-released-p dbx)))
            (vlax-release-object dbx)
        )
        (if (not (wcmatch (strcase msg t) "*break,*cancel*,*exit*"))
            (princ (strcat "\nError: " msg))
        )
        (princ)
    )

    (while (setq def (tblnext "block" (null def)))
        (if (= 4 (logand 4 (cdr (assoc 70 def))))
            (setq xrl (vl-list* "," (cdr (assoc 2 def)) xrl))
        )
    )
    (cond
        (   (= 1 (getvar 'xloadctl))
            (princ "\nXLOADCTL system variable is set to 1, xref source drawings are locked.")
        )
        (   (not
                (and
                    (setq sel
                        (LM:ssget "\nSelect objects to copy to xref: "
                            (list "_:L"
                                (list
                                   '(0 . "~VIEWPORT")
                                   '(-4 . "<NOT")
                                       '(-4 . "<AND")
                                           '(0 . "INSERT") (cons 2 (apply 'strcat (cdr xrl)))
                                       '(-4 . "AND>")
                                   '(-4 . "NOT>")
                                    (if (= 1 (getvar 'cvport))
                                        (cons 410 (getvar 'ctab))
                                       '(410 . "Model")
                                    )
                                )
                            )
                        )
                    )
                    (progn
                        (while
                            (progn (setvar 'errno 0) (setq ent (car (entsel "\nSelect xref: ")))
                                (cond
                                    (   (= 7 (getvar 'errno))
                                        (princ "\nMissed, try again.")
                                    )
                                    (   (= 'ename (type ent))
                                        (if (or (/= "INSERT" (cdr (assoc 0 (setq enx (entget ent)))))
                                                (not (member (cdr (assoc 2 enx)) xrl))
                                            )
                                            (princ "\nSelected object is not an xref.")
                                        )
                                    )
                                )
                            )
                        )
                        ent
                    )
                )
            )
        )
        (   (progn
               (setq dbx
                   (vl-catch-all-apply 'vla-getinterfaceobject
                       (list (setq app (vlax-get-acad-object))
                           (if (< (setq vrs (atoi (getvar 'acadver))) 16)
                               "objectdbx.axdbdocument" (strcat "objectdbx.axdbdocument." (itoa vrs))
                           )
                       )
                   )
               )
               (or (null dbx) (vl-catch-all-error-p dbx))
            )
            (prompt "\nUnable to interface with ObjectDBX.")
        )
        (   (not
                (and
                    (setq dwg (cdr (assoc 1 (tblsearch "block" (cdr (assoc 2 enx))))))
                    (setq dwg (findfile dwg))
                )
            )
            (prompt "\nUnable to locate xref source drawing.")
        )
        (   (progn
                (vlax-for doc (vla-get-documents app)
                    (setq dwl (cons (cons (strcase (vla-get-fullname doc)) doc) dwl))
                )
                (not
                    (or (setq doc (cdr (assoc (strcase dwg) dwl)))
                        (and (not (vl-catch-all-error-p (setq err (vl-catch-all-apply 'vla-open (list dbx dwg)))))
                             (setq doc dbx)
                        )
                    )
                )
            )
            (prompt (strcat "\nUnable to interface with xref source drawing:\n" (vl-catch-all-error-message err)))
        )
        (   (setq mat (revrefgeom ent)
                  mat (vlax-tmatrix (append (mapcar 'append (car mat) (mapcar 'list (cadr mat))) '((0.0 0.0 0.0 1.0))))
                  acd (vla-get-activedocument app)
            )
            (repeat (setq inc (sslength sel))
                (setq obj (vlax-ename->vla-object (ssname sel (setq inc (1- inc))))
                      lst (cons obj lst)
                )
                (vla-transformby obj mat)
            )
            (vlax-invoke acd 'copyobjects lst (vla-get-modelspace doc))
            (vla-saveas doc dwg)
            (vla-reload (vla-item (vla-get-blocks acd) (cdr (assoc 2 enx))))
            (foreach obj lst (vla-delete obj)) ;; Comment this line to retain original objects
        )
    )
    (if (and (= 'vla-object (type dbx)) (not (vlax-object-released-p dbx)))
        (vlax-release-object dbx)
    )
    (princ)
)

;; ssget  -  Lee Mac
;; A wrapper for the ssget function to permit the use of a custom selection prompt

(defun LM:ssget ( msg params / sel )
    (princ msg)
    (setvar 'nomutt 1)
    (setq sel (vl-catch-all-apply 'ssget params))
    (setvar 'nomutt 0)
    (if (not (vl-catch-all-error-p sel)) sel)
)

;; RevRefGeom (gile)
;; The inverse of RefGeom

(defun revrefgeom ( ent / ang enx mat ocs )
    (setq enx (entget ent)
          ang (cdr (assoc 050 enx))
          ocs (cdr (assoc 210 enx))
    )
    (list
        (setq mat
            (mxm
                (list
                    (list (/ 1.0 (cdr (assoc 41 enx))) 0.0 0.0)
                    (list 0.0 (/ 1.0 (cdr (assoc 42 enx))) 0.0)
                    (list 0.0 0.0 (/ 1.0 (cdr (assoc 43 enx))))
                )
                (mxm
                    (list
                        (list (cos ang)     (sin ang) 0.0)
                        (list (- (sin ang)) (cos ang) 0.0)
                       '(0.0 0.0 1.0)
                    )
                    (mapcar '(lambda ( v ) (trans v ocs 0 t))
                        '(
                             (1.0 0.0 0.0)
                             (0.0 1.0 0.0)
                             (0.0 0.0 1.0)
                         )
                    )
                )
            )
        )
        (mapcar '- (cdr (assoc 10 (tblsearch "block" (cdr (assoc 2 enx)))))
            (mxv mat (trans (cdr (assoc 10 enx)) ocs 0))
        )
    )
)

;; Matrix Transpose  -  Doug Wilson
;; Args: m - nxn matrix

(defun trp ( m )
    (apply 'mapcar (cons 'list m))
)

;; Matrix x Matrix  -  Vladimir Nesterovsky
;; Args: m,n - nxn matrices

(defun mxm ( m n )
    ((lambda ( a ) (mapcar '(lambda ( r ) (mxv a r)) m)) (trp n))
)

;; Matrix x Vector  -  Vladimir Nesterovsky
;; Args: m - nxn matrix, v - vector in R^n

(defun mxv ( m v )
    (mapcar '(lambda ( r ) (apply '+ (mapcar '* r v))) m)
)

;;----------------------------------------------------------------------;;

(vl-load-com)
(princ
    (strcat
        "\n:: Copy2XRef.lsp | Version 1.2 | \\U+00A9 Lee Mac "
        (menucmd "m=$(edtime,0,yyyy)")
        " www.lee-mac.com ::"
        "\n:: Type \"c2x\" to Invoke ::"
    )
)
(princ)

;;----------------------------------------------------------------------;;
;;                             End of File                              ;;
;;----------------------------------------------------------------------;;