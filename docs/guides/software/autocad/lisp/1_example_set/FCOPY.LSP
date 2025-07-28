;;; --------------------------------------------------------------------------;
;;; FCOPY.LSP
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
;;; --------------------------------------------------------------------------;
;;; DESCRIPTION
;;;
;;;   This is a programming example.
;;;
;;;   This program takes two ASCII files as arguments, and copies the first   
;;;   file into the second.  If the first file does not exist, an error message 
;;;   is printed, however, if the second file does not exist, it is created.
;;;
;;;   Note that if the second file exists, its data will be overwritten.
;;;
;;;   Usage: (fcopy "infile.ext" "outfile.ext")
;;;
;;; --------------------------------------------------------------------------;

(defun fcopy (in out / ifp ofp l) 
  (cond ((null (setq ifp (open in "r"))) ; try to open in for reading
     (princ "Can't open ")            ; if nil print error message
     (princ in) 
     (princ " for reading.")
    ) 
    (if ifp 
      (progn
        (setq ofp (open out "w"))     ; else open out for writing
        (while (setq l (read-line ifp)) ; read each line from in
          (write-line l ofp)          ; and write it to out
        ) 
        (close ofp)                   ; close both files
        (close ifp)
      )
    )
  ) 
  (princ)
) 

;;; --------------------------------------------------------------------------;



