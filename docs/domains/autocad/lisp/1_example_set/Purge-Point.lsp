;;; purge-point.LSP
;;;
;;; These commands shall be used with caution since they destroys
;;; the intelligence in the objects. But it can reduce the size very much
;;; if that is what is needed.
;;; A test with an Architectural drawing reduced the size from 1061kB to 172kB.
;;;
;;; c:purge-vent Kills all POINT 5 vent-objects and V50-dictionaries
;;; c:purge-aec Kills all POINT 5 architect-objects and dictionaries
;;; the above command (purge-aec) shall be run when POINT A is unloaded to work correct
;;; c:purge-point5 Kills all POINT 5 general objects in a drawing
;;; c:purge-point Kills all POINT general objects in a drawing
;;; c:purge-all-point runs all of the above commands
;;;
;;; By Jimmy Bergmark
;;; Copyright (C) 1997-2006 JTB World, All Rights Reserved
;;; Website: www.jtbworld.com
;;; E-mail: info@jtbworld.com
;;; 2000-04-05 - First release
;;; 2000-06-05 - Strcase on wcmatch of dict. on entities
;;; 2000-06-19 - Buggfix on strcase of nil
;;; Tested on AutoCAD 2000 and POINT 5

(defun deldict (dictName)
  (dictremove (namedobjdict) dictName)
)

(defun listdictionaries ()
  (massoc 3 (entget (namedobjdict)))
)

(defun massoc (key alist / x nlist)
  (foreach x alist
    (if (eq key (car x))
      (setq nlist (cons (cdr x) nlist))
    )
  )
  (reverse nlist)
)

(defun kill-dict (typ / olderr oldcmdecho errexit undox restore en more ed no repl ed360 ed3)
  (defun errexit (s)
    (princ "\nError:  ")
    (princ s)
    (restore)
  )

  (defun undox ()
    (command "._undo" "_E")
    (setvar "cmdecho" oldcmdecho)
    (setq *error* olderr)
    (princ)
  )

  (setq olderr  *error*
        restore undox
        *error* errexit
  )
  (setq oldcmdecho (getvar "cmdecho"))
  (setvar "cmdecho" 0)
  (command "._UNDO" "_BE")
  (initget 0 "Yes No")
  (setq repl (getkword "\nAre you sure? [Yes/No] <No>: "))
  (if repl
    (progn
      (setq en (entnext))
      (setq more (not (not en)))
      (setq no 0)
      (while more
        (setq ed (entget en))
        (if (and
              (/= (member '(102 . "{ACAD_XDICTIONARY") ed) nil)
              (setq ed360 (assoc 360 ed))
              (setq ed3 (assoc 3 (entget (cdr ed360))))
              (wcmatch (strcase (cdr ed3)) (strcase typ))
            )
          (progn
            (entdel en)
            (setq ed
                   (append
                     (reverse
                       (cdr (member '(102 . "{ACAD_XDICTIONARY") (reverse ed)))
                     )
                     (cdr (member '(102 . "}") ed))
                   )
            )
            (if (not (entmake ed))
              (progn
                (entdel en)
                (princ "\nError deleting: ")
                (princ en)
              )
              (setq no (1+ no))
            )
          )
          (if (= (setq en (entnext en)) nil)
            (setq more nil)
          )
        )
      )
      (foreach dict (listdictionaries)
        (if (wcmatch (strcase dict) (strcase typ)) (deldict dict))
      )
    )
  )
  (princ "\nNumber of deleted objects: ")
  (princ no)
  (restore)
)

(defun c:purge-vent() (kill-dict "V50*"))
(defun c:purge-point5() (kill-dict "Point5*"))
(defun c:purge-point() (kill-dict "Point"))
(defun c:purge-aec() (kill-dict "PointAec*"))
(defun c:purge-all-point() (kill-dict "Point*") (kill-dict "V50*"))
(princ)




