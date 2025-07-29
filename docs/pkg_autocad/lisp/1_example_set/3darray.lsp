; Next available MSG number is    29
; MODULE_ID LSP_3DARRAY_LSP_
;;;
;;;    3darray.lsp
;;;
;;;    Copyright 1987, 1988, 1990, 1992, 1994, 1996 by Autodesk, Inc.
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
;;;============================================================================
;;;  Functions included:
;;;       1) Rectangular ARRAYS (rows, columns & levels)
;;;       2) Circular ARRAYS around any axis
;;; 
;;;  All are loaded by: (load "3darray")
;;; 
;;;  And run by:
;;;       Command: 3darray
;;;                Select objects:
;;;                Rectangular or Polar array (R/P): (select type of array)


;;; ===================== load-time error checking ============================

  (defun ai_abort (app msg)
     (defun *error* (s)
        (if old_error (setq *error* old_error))
        (princ)
     )
     (if msg
       (alert (strcat " Application error: "
                      app
                      " \n\n  "
                      msg
                      "  \n"
              )
       )
     )
     (exit)
  )

;;; Check to see if AI_UTILS is loaded, If not, try to find it,
;;; and then try to load it.
;;;
;;; If it can't be found or it can't be loaded, then abort the
;;; loading of this file immediately, preserving the (autoload)
;;; stub function.

  (cond
     (  (and ai_dcl (listp ai_dcl)))          ; it's already loaded.

     (  (not (findfile "ai_utils.lsp"))                     ; find it
        (ai_abort "3DARRAY"
                  (strcat "Can't locate file AI_UTILS.LSP."
                          "\n Check support directory.")))

     (  (eq "failed" (load "ai_utils" "failed"))            ; load it
        (ai_abort "3DARRAY" "Can't load file AI_UTILS.LSP"))
  )

  (if (not (ai_acadapp))               ; defined in AI_UTILS.LSP
      (ai_abort "3DARRAY" nil)         ; a Nil <msg> supresses
  )                                    ; ai_abort's alert box dialog.

;;; ==================== end load-time operations ===========================
;;; 
;;;******************************** MODES ********************************
;;; 
;;; System variable save

(defun MODES (a)
  (setq MLST '())
  (repeat (length a)
    (setq MLST (append MLST (list (list (car a) (getvar (car a))))))
    (setq a (cdr a))
  )
)

;;;******************************** MODER ********************************
;;; 
;;; System variable restore

(defun MODER ()
  (repeat (length MLST)
    (setvar (caar MLST) (cadar MLST))
    (setq MLST (cdr MLST))
  )
)

;;;******************************** 3DAERR *******************************
;;; 
;;; Standard error function

(defun 3DAERR (st)                    ; If an error (such as CTRL-C) occurs
                                      ; while this command is active...
  (if (/= st "Function cancelled")
      (princ (strcat "\nError: " s))
  )
  (command "_.UNDO" "_E")
  (ai_undo_off)
  (moder)                             ; Restore system variables
  (setq *error* olderr)               ; Restore old *error* handler
  (princ)
)

;;;******************************* P-ARRAY *******************************
;;; 
;;; Perform polar (circular) array around any axis

(defun P-ARRAY (/ n af yn cen c ra)

  ;; Define number of items in array
  (setq n 0)
  (while (<= n 1)
    (initget (+ 1 2 4))
    (setq n (getint "\nNumber of items: "))
    (if (= n 1)
      (prompt "\nNumber of items must be greater than 1")
    )
  )

  ;; Define angle to fill
  (initget 2)
  (setq af (getreal "\nAngle to fill <360>: "))
  (if (= af nil) (setq af 360))

  ;; Are objects to be rotated?
  (initget "Yes No")
  (setq yn (getkword "\nRotate objects as they are copied? <Y>: "))
  (if (null yn)
    (setq yn "Yes")
  )
  (setq yn (if (= yn "Yes") "_Y" "_N"))

  ;; Define center point of array
  (initget 17)
  (setq cen (getpoint "\nCenter point of array: "))
  (setq c (trans cen 1 0))

  ;; Define rotational axis
  (initget 17)
  (setq ra (getpoint cen "\nSecond point on axis of rotation: "))
  (while (equal ra cen)
    (princ "\nInvalid point. Second point cannot equal center point.")
    (initget 17)
    (setq ra (getpoint cen "\nPlease try again: "))
  )
  (setvar "UCSFOLLOW" 0)
  (setvar "GRIDMODE" 0)
  (command "_.UCS" "_ZAXIS" cen ra)
  (setq cen (trans c 0 1))

  ;; Draw polar array
  (command "_.ARRAY" ss "" "_P" cen n af yn)
  (command "_.UCS" "_p")
)

;;;******************************* R-ARRAY *******************************
;;; 
;;; Perform rectangular array

(defun R-ARRAY (/ nr nc nl flag x y z c el en ss2 e)

  ;; Set array parameters
  (while (or (= nr nc nl nil) (= nr nc nl 1))
    (setq nr 1)
    (initget (+ 2 4))
    (setq nr (getint "\nNumber of rows (---) <1>: "))
    (if (null nr) (setq nr 1))
    (initget (+ 2 4))
    (setq nc (getint "\nNumber of columns (|||) <1>: "))
    (if (null nc) (setq nc 1))
    (initget (+ 2 4))
    (setq nl (getint "\nNumber of levels (...) <1>: "))
    (if (null nl) (setq nl 1))
    (if (= nr nc nl 1)
      (princ "\nOne-element array, nothing to do.\nPlease try again")
    )
  )
  ;;
  ;; get environment variable "MaxArray", If unable to get, use
  ;; the default value of 100000. Value of 100000 is taken from
  ;; the value of MAX_ARRAY_DEFAULT  #defined in coresrc\array.c
  (if (= (getenv "MaxArray") nil)
    (progn 
	   (setq maxlimit 100000)
	)
	(progn
	  (setq maxlimit (atoi(getenv "MaxArray")))
	)
  )
  ;; ne - number of elements/entity.
  (setq ne (sslength ss))

  (if (< maxlimit (* nr nc nl ne))
  (progn
   (princ "\nThis would create ")
   (princ  (- (* nc nr nl ne) 1))
   (princ " objects, exceeding the limit of ")
   (princ maxlimit )
   (princ " objects imposed by the MaxArray environment setting.\n")
  )
  (progn
  (setvar "ORTHOMODE" 1)
  (setvar "HIGHLIGHT" 0)
  (setq flag 0)                       ; Command style flag
  (if (/= nr 1)
    (progn
    (initget (+ 1 2))
    (setq y (getdist "\nDistance between rows (---): "))
    (setq flag 1)
    )
  )
  (if (/= nc 1)
    (progn
    (initget (+ 1 2))
    (setq x (getdist "\nDistance between columns (|||): "))
    (setq flag (+ flag 2))
    )
  )
  (if (/= nl 1)
    (progn
    (initget (+ 1 2))
    (setq z (getdist "\nDistance between levels (...): "))
    )
  )
  (setvar "BLIPMODE" 0)

  (setq c 1)
  (setq el (entlast))                 ; Reference entity
  (setq en (entnext el))
  (while (not (null en))
    (setq el en)
    (setq en (entnext el))
  )

  ;; Copy the selected entities one level at a time
  (while (< c nl)
    (command "_.COPY" ss "" "0,0,0" (append (list 0 0) (list (* c z)))
    )
    (setq c (1+ c))
  )

  (setq ss2 (ssadd))                  ; create a new selection set
  (setq e (entnext el))               ; of all the new entities since
  (while e                            ; the reference entity.
    (ssadd e ss2)
    (setq e (entnext e))
  )

  ;; Array original selection set and copied entities
  (cond
    ((= flag 1) (command "_.ARRAY" ss ss2 "" "_R" nr "1" y))
    ((= flag 2) (command "_.ARRAY" ss ss2 "" "_R" "1" nc x))
    ((= flag 3) (command "_.ARRAY" ss ss2 "" "_R" nr nc y x))
  )
  ) ;;; matching progn
  ) ;;; matching '(if (< maxlimit (* nr nc nl ne))'
)

;;;***************************** MAIN PROGRAM ****************************

(defun C:3DARRAY (/ olderr ss xx undo_setting)
  (if (and (= (getvar "cvport") 1) (= (getvar "tilemode") 0))
    (progn
      (prompt "\n *** Command not allowed in Paper space ***\n")
      (princ)
    )
    (progn
      (setq olderr *error*
            *error* 3daerr
      )
      (modes '("cmdecho" "blipmode" "highlight" "orthomode" 
               "ucsfollow" "gridmode")
      )
      (setvar "CMDECHO" 0)

      (ai_undo_on)                    ; Turn UNOD on

      (command "_.UNDO" "_GROUP")
      (graphscr)

      (setq ss nil)
      (while  (null ss)               ; Ensure selection of entities
        (setq ss (ai_ssget (ssget)))
      )
    
      (initget 1 "Rectangular Polar Circular")
      (setq xx (getkword "\nRectangular or Polar array (R/P): "))
      (cond 
        ((eq xx "Rectangular") 
          (r-array)
        )
        (T 
          (p-array)
        )
      )
      (command "_.UNDO" "_E")
      (ai_undo_off)                   ; Return UNDO to initial state
      (moder)                         ; Restore system variables
      (setq *error* olderr)           ; Restore old *error* handler
      (princ)
    )
  )
)

(princ "  3DARRAY loaded.")
(princ)
