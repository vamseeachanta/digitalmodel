;;;   ALIAS.lsp
;;;   (C) Copyright 1988-1992 by Autodesk, Inc.
;;;  
;;;   This program is copyrighted by Autodesk, Inc. and is  licensed
;;;   to you under the following conditions.  You may not distribute
;;;   or  publish the source code of this program in any form.   You
;;;   may  incorporate this code in object form in derivative  works
;;;   provided  such  derivative  works  are  (i.) are  designed and
;;;   intended  to  work  solely  with  Autodesk, Inc. products, and
;;;   (ii.)  contain  Autodesk's  copyright  notice  "(C)  Copyright
;;;   1988-1992 by Autodesk, Inc."
;;;
;;;   AUTODESK  PROVIDES THIS PROGRAM "AS IS" AND WITH  ALL  FAULTS.
;;;   AUTODESK  SPECIFICALLY DISCLAIMS ANY IMPLIED WARRANTY OF  MER-
;;;   CHANTABILITY OR FITNESS FOR A PARTICULAR USE.  AUTODESK,  INC.
;;;   DOES  NOT  WARRANT THAT THE OPERATION OF THE PROGRAM  WILL  BE
;;;   UNINTERRUPTED OR ERROR FREE.
;;; 
;;;   by Jan S. Yoder
;;;   29 January 1990

;;;-------------------------------------------------------------------------;
;;; DESCRIPTION
;;;
;;; List all of the aliases found in the file "acad.pgp" if it can be found 
;;; in AutoCAD's search path, which means that they are available in AutoCAD.
;;; This routine lists them in a two column format, pausing after every 20 
;;; lines.
;;;
;;;-------------------------------------------------------------------------;

;;;
;;; Look for an external definition file in AutoCAD's search path
;;; al_lfx == Alias_Look_For_Xfile
;;;
(defun al_lfx (f_name r_or_w /)
  ;; Look for f_name in AutoCAD's search paths.
  (if (= r_or_w "w")
    (if (setq temp (open f_name r_or_w))
      temp                            ; Return file descriptor
      (progn
        (princ (strcat "\n\tCouldn't open " f_name " for writing. "))
        (exit)
      )
    )
    (if (setq lfile (findfile f_name))
      (if (setq temp (open lfile r_or_w))
        temp                          ; Return file descriptor
        (progn
          (princ (strcat "\n\tCouldn't open " f_name " for reading. "))
          (exit)
        )
      )
      nil                             ; or nil
    )
  )
)
;;;
;;; Print the alias in a two column format, pausing after twenty lines.
;;;
(defun al_pr1 (/ j al_str c_str tmpstr )
  (setq j      0
        al_str "" 
        c_str  ""
  )
  (while (/= (setq tmpstr (substr line (setq j (1+ j)) 1)) ",")
    (if (/= tmpstr " ")
      (setq al_str (strcat al_str tmpstr))
    )
  )
  (while (and (/= (setq tmpstr (substr line (setq j (1+ j)) 1)) "")
              (/= tmpstr ";") 
              (/= tmpstr ",")
              (/= tmpstr "\n")
         )
    (if (and (/= tmpstr " ") (/= tmpstr "*") (/= tmpstr "\t"))
      (setq c_str (strcat c_str tmpstr))
    )
  )
  (if (= (strlen c_str) 0) (setq c_str "<Null>"))
  (if (< (strlen al_str) 12) 
    (progn
      (repeat (- 12 (strlen al_str)) (setq al_str (strcat al_str  " ")))
    )
  )
  (if (< (strlen c_str) 10) 
    (repeat (- 10 (strlen c_str)) (setq c_str (strcat c_str  " ")))
    (setq first T)
  )
  (if first 
    (princ (strcat "\n    " al_str "= " c_str ))
    (princ (strcat "\t\t    " al_str "= " c_str ))
  )
  (if (and first (< (strlen c_str) 11))
    (setq lineno (1+ lineno)
           first nil)
    (setq first T)
  )
  (if (and (= lineno 20) first)
    (progn
      (princ "\n\n\t<More> ")
      (grread)
      (terpri)
      (setq lineno 0)
    )
  )
)
;;;
;;; Main function
;;;
(defun c:alias (/ al_err al_oe s al_oce aliasi lineno first line lfile)

  (setq al_ver "1.00a")               ; Reset this local if you make a change.
  ;;
  ;; Internal error handler defined locally
  ;;
  (defun al_err (s)                   ; If an error (such as CTRL-C) occurs
                                      ; while this command is active...
    (if (/= s "Function cancelled")
      (princ (strcat "\nError: " s))
    )
    (if al_oe                         ; If an old error routine exists
      (setq *error* al_oe)            ; then, reset it 
    )
    (setq aliasi (close aliasi))
    (setvar "cmdecho" al_oce)         ; Reset command echoing on error
    (princ)
  )
  ;;
  ;; Body of alias function
  ;;
  (if *error*                         ; Set our new error handler
    (setq al_oe *error* *error* al_err) 
    (setq *error* al_err) 
  )

  ;;  
  ;; Set command echoing off.
  ;;
  (setq al_oce (getvar "cmdecho"))
  (setvar "cmdecho" 0)
  ;;
  ;; The default file name is "acad.pgp".
  ;; Look for it in AutoCAD's search paths.
  ;;

  (setq aliasi (al_lfx "acad.pgp" "r"))

  (if aliasi
    (progn
      (setq lineno 0
            first T
      )
      (if textpage (textpage) (textscr))    ; For Release 10
      (princ (strcat 
        "\n    Alias  Version " al_ver ", (c) 1990 by Autodesk, Inc. "))
      (princ (strcat 
        "\n    This is a list of the aliases and external commands found in"
        "\n    " 
                     lfile ". \n"))
      (while (setq line (read-line aliasi))
        ;; Check each line for leading semi-colon
        (if (and (/= (substr line 1 1) ";")     ; Leading semi-colon?
                 (/= (substr line 1 1) "")      ; Blank line?
                 (/= (substr line 1 1) "\n"))   ; Carriage return?
          
          (al_pr1)
        )
      )
      (princ "\n")
      (princ "\nPress any key to return to your drawing.")
      (grread)
      (princ "\r                                          ")
      (graphscr)
    )
    ;; else
    (progn
      (princ "\nCouldn't find or read the file \"acad.pgp\". ")
    )
  )
  (setq aliasi (close aliasi))
  (if al_oce (setvar "cmdecho" al_oce))
  (princ)
)

(princ "\n\tC:ALIAS loaded.  Start command with ALIAS.")
(princ)
