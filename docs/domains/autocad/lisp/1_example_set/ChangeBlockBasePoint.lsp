;;--------------------=={ Change Block Base Point }==-------------------;;
;;                                                                      ;;
;;  This program allows the user to change the base point for all       ;;
;;  block references of a block definition in a drawing.                ;;
;;                                                                      ;;
;;  The program offers two commands:                                    ;;
;;                                                                      ;;
;;  ------------------------------------------------------------------  ;;
;;  CBP (Change Base Point)                                             ;;
;;  ------------------------------------------------------------------  ;;
;;                                                                      ;;
;;  This command will retain the insertion point coordinates for all    ;;
;;  references of the selected block. Hence visually, the block         ;;
;;  components will be moved around the insertion point when the        ;;
;;  base point is changed.                                              ;;
;;                                                                      ;;
;;  ------------------------------------------------------------------  ;;
;;  CBPR (Change Base Point Retain Reference Position)                  ;;
;;  ------------------------------------------------------------------  ;;
;;                                                                      ;;
;;  This command will retain the position of the each block reference   ;;
;;  of the selected block. Hence, each block reference will be moved    ;;
;;  to retain the visual position when the base point is changed.       ;;
;;                                                                      ;;
;;  ------------------------------------------------------------------  ;;
;;                                                                      ;;
;;  Upon issuing a command syntax at the AutoCAD command-line, the      ;;
;;  program will prompt the user to select a block for which to change  ;;
;;  the base point.                                                     ;;
;;                                                                      ;;
;;  Following a valid selection, the user is then prompted to specify   ;;
;;  a new base point relative to the selected block.                    ;;
;;                                                                      ;;
;;  The block definition (and block reference depending on the command  ;;
;;  used) will then be modified to reflect the new block base point.    ;;
;;                                                                      ;;
;;  If the selected block is attributed, an ATTSYNC operation will      ;;
;;  also be performed to ensure all attributes are in the correct       ;;
;;  positions relative to the new base point.                           ;;
;;                                                                      ;;
;;  Finally, the active viewport is regenerated to reflect the changes  ;;
;;  throughout all references of the block.                             ;;
;;                                                                      ;;
;;  The program will furthermore perform successfully with rotated &    ;;
;;  scaled block references, constructed in any UCS plane.              ;;
;;                                                                      ;;
;;  ------------------------------------------------------------------  ;;
;;  Please Note:                                                        ;;
;;  ------------------------------------------------------------------  ;;
;;                                                                      ;;
;;  A REGEN is required if the UNDO command is used to undo the         ;;
;;  operations performed by this program.                               ;;
;;                                                                      ;;
;;----------------------------------------------------------------------;;
;;  Author:  Lee Mac, Copyright © 2013  -  www.lee-mac.com              ;;
;;----------------------------------------------------------------------;;
;;  Version 1.5    -    20-10-2013                                      ;;
;;----------------------------------------------------------------------;;

;; Retains Insertion Point Coordinates
(defun c:cbp  nil (LM:changeblockbasepoint nil))

;; Retains Block Reference Position
(defun c:cbpr nil (LM:changeblockbasepoint t))

;;----------------------------------------------------------------------;;

(defun LM:changeblockbasepoint ( flg / *error* bln cmd ent lck mat nbp vec )

    (defun *error* ( msg )
        (foreach lay lck (vla-put-lock lay :vlax-true))
        (if (= 'int (type cmd)) (setvar 'cmdecho cmd))
        (LM:endundo (LM:acdoc))
        (if (not (wcmatch (strcase msg t) "*break,*cancel*,*exit*"))
            (princ (strcat "\nError: " msg))
        )
        (princ)
    )

    (while
        (progn (setvar 'errno 0) (setq ent (car (entsel "\nSelect Block: ")))
            (cond
                (   (= 7 (getvar 'errno))
                    (princ "\nMissed, try again.")
                )
                (   (= 'ename (type ent))
                    (if (/= "INSERT" (cdr (assoc 0 (entget ent))))
                        (princ "\nSelected object is not a block.")
                    )
                )
            )
        )
    )
    (if (and (= 'ename (type ent)) (setq nbp (getpoint "\nSpecify New Base Point: ")))
        (progn
            (setq mat (car (revrefgeom ent))
                  vec (mxv mat (mapcar '- (trans nbp 1 0) (trans (cdr (assoc 10 (entget ent))) ent 0)))
                  bln (LM:blockname (vlax-ename->vla-object ent))
            )
            (LM:startundo (LM:acdoc))
            (vlax-for lay (vla-get-layers (LM:acdoc))
                (if (= :vlax-true (vla-get-lock lay))
                    (progn
                        (vla-put-lock lay :vlax-false)
                        (setq lck (cons lay lck))
                    )
                )
            )
            (vlax-for obj (vla-item (vla-get-blocks (LM:acdoc)) bln)
                 (vlax-invoke obj 'move vec '(0.0 0.0 0.0))
            )
            (if flg
                (vlax-for blk (vla-get-blocks (LM:acdoc))
                    (if (= :vlax-false (vla-get-isxref blk))
                        (vlax-for obj blk
                            (if
                                (and
                                    (= "AcDbBlockReference" (vla-get-objectname obj))
                                    (= bln (LM:blockname obj))
                                    (vlax-write-enabled-p obj)
                                )
                                (vlax-invoke obj 'move '(0.0 0.0 0.0) (mxv (car (refgeom (vlax-vla-object->ename obj))) vec))
                            )
                        )
                    )
                )
            )
            (if (= 1 (cdr (assoc 66 (entget ent))))
                (progn
                    (setq cmd (getvar 'cmdecho))
                    (setvar 'cmdecho 0)
                    (vl-cmdf "_.attsync" "_N" bln)
                    (setvar 'cmdecho cmd)
                )
            )
            (foreach lay lck (vla-put-lock lay :vlax-true))
            (vla-regen  (LM:acdoc) acallviewports)
            (LM:endundo (LM:acdoc))
        )
    )
    (princ)
)

;; RefGeom (gile)
;; Returns a list whose first item is a 3x3 transformation matrix and
;; second item the object insertion point in its parent (xref, block or space)

(defun refgeom ( ent / ang enx mat ocs )
    (setq enx (entget ent)
          ang (cdr (assoc 050 enx))
          ocs (cdr (assoc 210 enx))
    )
    (list
        (setq mat
            (mxm
                (mapcar '(lambda ( v ) (trans v 0 ocs t))
                   '(
                        (1.0 0.0 0.0)
                        (0.0 1.0 0.0)
                        (0.0 0.0 1.0)
                    )
                )
                (mxm
                    (list
                        (list (cos ang) (- (sin ang)) 0.0)
                        (list (sin ang) (cos ang)     0.0)
                       '(0.0 0.0 1.0)
                    )
                    (list
                        (list (cdr (assoc 41 enx)) 0.0 0.0)
                        (list 0.0 (cdr (assoc 42 enx)) 0.0)
                        (list 0.0 0.0 (cdr (assoc 43 enx)))
                    )
                )
            )
        )
        (mapcar '- (trans (cdr (assoc 10 enx)) ocs 0)
            (mxv mat (cdr (assoc 10 (tblsearch "block" (cdr (assoc 2 enx))))))
        )
    )
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

;; Matrix x Vector  -  Vladimir Nesterovsky
;; Args: m - nxn matrix, v - vector in R^n

(defun mxv ( m v )
    (mapcar '(lambda ( r ) (apply '+ (mapcar '* r v))) m)
)

;; Matrix x Matrix  -  Vladimir Nesterovsky
;; Args: m,n - nxn matrices

(defun mxm ( m n )
    ((lambda ( a ) (mapcar '(lambda ( r ) (mxv a r)) m)) (trp n))
)

;; Matrix Transpose  -  Doug Wilson
;; Args: m - nxn matrix

(defun trp ( m )
    (apply 'mapcar (cons 'list m))
)

;; Block Name  -  Lee Mac
;; Returns the true (effective) name of a supplied block reference
                        
(defun LM:blockname ( obj )
    (if (vlax-property-available-p obj 'effectivename)
        (defun LM:blockname ( obj ) (vla-get-effectivename obj))
        (defun LM:blockname ( obj ) (vla-get-name obj))
    )
    (LM:blockname obj)
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

;;----------------------------------------------------------------------;;

(vl-load-com)
(princ
    (strcat
        "\n:: ChangeBlockBasePoint.lsp | Version 1.5 | \\U+00A9 Lee Mac "
        (menucmd "m=$(edtime,0,yyyy)")
        " www.lee-mac.com ::"
        "\n:: Available Commands:"
        "\n::    \"CBP\"  -  Retain Insertion Point Position"
        "\n::    \"CBPR\" -  Retain Block Reference Position"
    )
)
(princ)

;;----------------------------------------------------------------------;;
;;                             End of File                              ;;
;;----------------------------------------------------------------------;;