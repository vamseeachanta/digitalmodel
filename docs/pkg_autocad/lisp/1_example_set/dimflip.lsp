;;; FUNCTION 
;;; allows the user to select numerous dimensions
;;; and proceeds to rotate the ucs origin by 180 degrees
;;; 
;;; ARGUMENTS 
;;; none 
;;; 
;;; USAGE 
;;; DIMFLIP
;;; 
;;; PLATFORMS 
;;; 2000+ 
;;; 
;;; AUTHOR 
;;; Copyright© 2003 Ron Heigh
;;; 
;;; VERSION 
;;; 1.0 January 01, 2003

(DEFUN c:dad ()

  '(acet-ss-clear-prev)

  (PRINC "\nPress Enter to select all or...")

  (IF (SETQ ss:dims (SSGET '((0 . "DIMENSION"))))
      (SETQ ss:dims (SSGET "P"))
      (SETQ ss:dims (SSGET "X" '((0 . "DIMENSION"))))
  ) ;_ end of if

  (IF ss:dims
    (main:flipper)
    (PRINC "\nNo valid objects selected.")
  ) ;_ end of if
  (PRINC)

) ;_ end of defun


  ;***CALL THE MODULES***

(DEFUN main:flipper ()
  (SETQ index 0
        cnt 0)
  (REPEAT (SSLENGTH ss:dims)
    (SETQ en (SSNAME ss:dims index))
    (test:code51)
    (SETQ index (+ index 1))
  ) ;_ end of REPEAT

  (PRINC (STRCAT "\n***Process Complete***"))
) ;_ end of defun


(DEFUN test:code51 ()
  (SETQ en:data (ENTGET en)
        val:51 (CDR (ASSOC 51 en:data))
        nval:51 (+ val:51 PI))
  (ENTMOD (SUBST (CONS 51 nval:51) (ASSOC 51 en:data) en:data))

) ;_ end of defun