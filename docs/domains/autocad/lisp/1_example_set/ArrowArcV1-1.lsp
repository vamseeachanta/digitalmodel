;;----------------------=={ Arrow Arc }==---------------------;;
;;                                                            ;;
;;  Enables the user to construct an arc with arrowheads at   ;;
;;  each endpoint. The size of the arrowheads may be altered  ;;
;;  using the 'aarcsettings' command.                         ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2012 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;
;;  Version 1.0    -    17-07-2012                            ;;
;;                                                            ;;
;;  First release.                                            ;;
;;------------------------------------------------------------;;
;;  Version 1.1    -    26-05-2013                            ;;
;;                                                            ;;
;;  Added 'aarcsettings' command to enable the user to alter  ;;
;;  the arrow length & width without modifying the code.      ;;
;;------------------------------------------------------------;;

(defun c:aarc ( / *error* an1 an2 an3 ang arl arw cen ent enx rad )
    
    (defun *error* ( msg )
        (if (not (wcmatch (strcase msg t) "*break,*cancel*,*exit*"))
            (princ (strcat "\nError: " msg))
        )
        (princ)
    )

    (if (not (and (setq arl (getenv "LMac\\aarcl")) (setq arl (distof arl 2))))
        (setq arl 1.0)
    )
    (if (not (and (setq arw (getenv "LMac\\aarcw")) (setq arw (distof arw 2))))
        (setq arw 0.5)
    )
    (princ
        (strcat
            "\nArrow Length: " (rtos arl 2) " | Arrow Width: " (rtos arw 2)
            "\nType \"aarcsettings\" to alter settings.\n"
        )
    )
    (setq ent (entlast))
    (command "_.arc")
    (while (= 1 (logand 1 (getvar 'cmdactive)))
        (command "\\")
    )
    (if (not (eq ent (setq ent (entlast))))
        (progn
            (setq enx (entget ent)
                  cen (cdr (assoc 10 enx))
                  rad (cdr (assoc 40 enx))
                  an1 (cdr (assoc 50 enx))
                  an2 (cdr (assoc 51 enx))
                  an3 (/ arl rad)
                  ang (rem (+ pi pi (- an2 an1)) (+ pi pi))
            )
            (if (< arl (* 0.5 rad ang))
                (if (entmake
                        (list
                           '(000 . "LWPOLYLINE")
                           '(100 . "AcDbEntity")
                           '(100 . "AcDbPolyline")
                           '(090 . 4)
                           '(070 . 0)
                            (cons 010 (polar cen an1 rad))
                           '(040 . 0.0)
                            (cons 041 arw)
                            (cons 042 (tan (/ an3 4.0)))
                            (cons 010 (polar cen (+ an1 an3) rad))
                           '(040 . 0.0)
                           '(041 . 0.0)                         
                            (cons 042 (tan (/ (- ang an3 an3) 4.0)))
                            (cons 010 (polar cen (- an2 an3) rad))
                            (cons 040 arw)
                           '(041 . 0.0)
                            (cons 042 (tan (/ an3 4.0)))
                            (cons 010 (polar cen an2 rad))
                            (cons 210 (trans '(0.0 0.0 1.0) 1 0 t))
                        )
                    )
                    (entdel ent)
                )
                (princ "\nArc too short to accommodate arrows.")
            )
        )
    )
    (princ)
)

;; Arrow Arc Settings Command

(defun c:aarcsettings ( / tmp )
    (initget 6)
    (if (setq tmp (getdist (strcat "\nSpecify Arrow Length <" (cond ((getenv "LMac\\aarcl")) ("1.0")) ">: ")))
        (setenv "LMac\\aarcl" (rtos tmp 2))
    )
    (initget 6)
    (if (setq tmp (getdist (strcat "\nSpecify Arrow Width <" (cond ((getenv "LMac\\aarcw")) ("0.5")) ">: ")))
        (setenv "LMac\\aarcw" (rtos tmp 2))
    )
    (princ)
)    
  
;; Tangent  -  Lee Mac
;; Args: x - real
  
(defun tan ( x )
    (if (not (equal 0.0 (cos x) 1e-10))
        (/ (sin x) (cos x))
    )
)
(princ)