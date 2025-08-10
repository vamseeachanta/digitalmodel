;;-----------------=={ AutoLabel Attributes }==---------------;;
;;                                                            ;;
;;  Automatically labels a specific attribute in a set of     ;;
;;  blocks, renumbering if blocks are added, copied or        ;;
;;  erased.                                                   ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2011 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;
;;  Version 1.0    -    14-09-2011                            ;;
;;------------------------------------------------------------;;

;;------------------------------------------------------------;;
;;                         Settings                           ;;
;;------------------------------------------------------------;;

;; [Note: Block names and Attribute Tags are *not* case-sensitive]

(setq *blockname* "block"  ;; Name of Block to be Updated
      *blocktag*  "tag"    ;; Attribute Tag to be Updated
)

;;------------------------------------------------------------;;
;;                      Main Program                          ;;
;;------------------------------------------------------------;;

(defun ObjectReactorCallback:RenumberBlocks ( object reactor params )
    (setq *reactor* reactor)
    (vlr-command-reactor "temp" '((:vlr-commandended . CommandReactorCallback:RenumberBlocks)))
    (vlr-remove  reactor)
    (princ)
)

;;------------------------------------------------------------;;

(defun CommandReactorCallback:RenumberBlocks ( reactor params / e f i l n s )
    (if reactor (vlr-remove reactor))
    (if
        (and
            (not *undoflag*)
            (setq s (ssget "_X" *filter*))
        )
        (progn
            (setq n 0)
            (repeat (setq i (sslength s))
                (if (eq *blockname*
                        (AutoLabel:EffectiveName
                            (setq o (vlax-ename->vla-object (setq e (ssname s (setq i (1- i))))))
                        )
                    )
                    (progn
                        (setq e (entnext e)
                              l (entget  e)
                              f nil
                        )
                        (while (and (not f) (eq "ATTRIB" (cdr (assoc 0 l))))
                            (if (eq *blocktag*  (strcase (cdr (assoc 2 l))))
                                (setq f (entmod (subst (cons 1 (itoa (setq n (1+ n)))) (assoc 1 l) l)))
                            )
                            (setq e (entnext e)
                                  l (entget  e)
                            )
                        )
                        (if (and *reactor* (not (member o (vlr-owners *reactor*))))
                            (vlr-owner-add *reactor* o)
                        )
                    )
                )
            )
        )
    )
    (if *reactor*
        (progn (vlr-add *reactor*) (setq *reactor* nil))
    )
    (princ)
)

;;------------------------------------------------------------;;

(defun CommandReactorCallback:UndoCheck ( reactor params )
    (setq *undoflag* (wcmatch (strcase (car params)) "*U,*UNDO"))
    (princ)
)

;;------------------------------------------------------------;;

(defun CommandReactorCallback:BlockInserted ( reactor params / e l )
    (if
        (and
            (not *undoflag*)
            (wcmatch (strcase (car params)) "*I,*INSERT,*EXECUTETOOL")
            (setq e (entlast))
            (setq l (entget e))
            (eq "INSERT" (cdr (assoc 0 l)))
            (= 1 (cdr (assoc 66 l)))
            (eq *blockname* (AutoLabel:EffectiveName (vlax-ename->vla-object e)))
        )
        (AutoLabel:GetNewNumber e)
    )
    (princ)
)

;;------------------------------------------------------------;;

(defun AutoLabel:GetNewNumber ( ent / e f i l n r s )
    (if (setq s (ssget "_X" *filter*))
        (progn
            (setq n 0)
            (repeat (setq i (sslength s))
                (if (eq *blockname*
                        (AutoLabel:Effectivename
                            (vlax-ename->vla-object (ssname s (setq i (1- i))))
                        )
                    )
                    (setq n (1+ n))
                )
            )
            (setq e (entnext ent)
                  l (entget e)
            )
            (while (and (not f) (eq "ATTRIB" (cdr (assoc 0 l))))
                (if (eq *blocktag*  (strcase (cdr (assoc 2 l))))
                    (setq f (entmod (subst (cons 1 (itoa n)) (assoc 1 l) l)))
                )
                (setq e (entnext e)
                      l (entget  e)
                )
            )
            (if
                (setq r
                    (vl-some
                        (function
                            (lambda ( r ) (if (eq *reacdata* (vlr-data r)) r))
                        )
                        (cdar (vlr-reactors :vlr-object-reactor))
                    )
                )
                (vlr-owner-add r (vlax-ename->vla-object ent))
            )                            
        )
    )
    (princ)
)

;;------------------------------------------------------------;;

(defun AutoLabel:EffectiveName ( obj )
    (strcase
        (if (vlax-property-available-p obj 'effectivename)
            (vla-get-effectivename obj)
            (vla-get-name obj)
        )
    )
)

;;------------------------------------------------------------;;
;;                   Loading Expressions                      ;;
;;------------------------------------------------------------;;

(vl-load-com)

(
    (lambda ( / i s l o )
        (setq
            *blocktag*  (strcase *blocktag*)
            *blockname* (strcase *blockname*)
            *reacdata*  "AutoBlockLabel"
            *reactor*   nil
            *undoflag*  nil
        )
        (foreach r1 (vlr-reactors)
            (foreach r2 (cdr r1)
                (if (eq *reacdata* (vlr-data r2)) (vlr-remove r2))
            )
        )
        (if
            (setq s
                (ssget "_X"
                    (setq *filter*
                        (list
                           '(0 . "INSERT")
                           '(66 . 1)
                            (cons 2 (strcat "`*U*," *blockname*))
                            (cons 410 (getvar 'CTAB))
                        )
                    )
                )
            )
            (progn
                (repeat (setq i (sslength s))
                    (if (eq *blockname*
                            (AutoLabel:EffectiveName
                                (setq o (vlax-ename->vla-object (ssname s (setq i (1- i)))))
                            )
                        )
                        (setq l (cons o l))
                    )
                )
                (CommandReactorCallback:RenumberBlocks nil nil)
                (vlr-object-reactor l *reacdata*
                    (list
                        (cons :vlr-erased   'ObjectReactorCallback:RenumberBlocks)
                        (cons :vlr-copied   'ObjectReactorCallback:RenumberBlocks)
                        (cons :vlr-unerased 'ObjectReactorCallback:RenumberBlocks)
                    )
                )
                (vlr-command-reactor *reacdata*
                    (list
                        (cons :vlr-commandwillstart 'CommandReactorCallback:UndoCheck)
                        (cons :vlr-commandended     'CommandReactorCallback:BlockInserted)
                    )
                )
            )
        )
    )
)

(princ)

;;------------------------------------------------------------;;
;;                         End of File                        ;;
;;------------------------------------------------------------;;