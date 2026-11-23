;;; OFFSET.LSP Version 1.0, Copyright (C) (1997) 
;;; ============================================================================
;;; ====================== load-time error checking ===========================
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
        (ai_abort "offset"
                  (strcat "Can't locate file AI_UTILS.LSP."
                          "\n Check support directory.")))

     (  (eq "failed" (load "ai_utils" "failed"))            ; load it
        (ai_abort "offset" "Can't load file AI_UTILS.LSP"))
  )

  (if (not (ai_acadapp))               ; defined in AI_UTILS.LSP
      (ai_abort "offset" nil)         ; a Nil <msg> supresses
  )                                    ; ai_abort's alert box dialog.

;;; ====================== end load-time operations ===========================
;;; ===========================================================================
(defun C:O (/ dist odist sdist
              object point)

 ;; Set up error function.
 (setq  old_error  *error*            ; save current error function
        *error* ai_error              ; new error function
 )
 ;; ===========================================================================
 (setq dist (getvar "OFFSETDIST"))
 (setq odist dist)
 (if (< dist 0)
     (setq sdist "Through")
     (setq sdist (rtos dist))
 )
 (initget "Through" 6)
 (setq dist (getdist (strcat "\nOffset distance or Through <" sdist ">:")))
 (cond
  ( (= dist "Through") (setq dist (- 1)))
  ( (= dist nil)       (setq dist odist))
  ( (= dist "")        (setq dist odist))
  ( (= dist " ")       (setq dist odist))
  ( (= dist "Enter")   (setq dist odist))
 )
 (setvar "OFFSETDIST" dist)
 (setq object 0)
 (setq point  0)
 (while (and (/= object nil) (/= point nil)) do
  (setq object (entsel "\nSelect object to offset:"))
  (if (/= object nil)
   (progn
    (cond
     ( (< dist 0) (setq point (getpoint "\nThrough point: ")) )
     ( (> dist 0) (setq point (getpoint "\nSide to offset? ")) )
    )
    (if (/= point nil)
     (progn
      (command "OFFSET" "" object point "")
      (command "CHPROP" "last" "" "LA" (GETVAR "CLAYER") "")
     )
    )
   )
  )
 )
 ;; ===========================================================================
 (setq *error* old_error)
 (princ)
)