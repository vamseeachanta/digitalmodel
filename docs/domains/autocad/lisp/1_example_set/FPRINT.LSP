;;; --------------------------------------------------------------------------;
;;; FPRINT.LSP
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
;;;   This function prints (lists) an ASCII text file on the screen.
;;;
;;;   Usage:   (fprint "filename.ext")
;;;
;;; --------------------------------------------------------------------------;

(defun fprint (s / c i) 
  (setq i (open s "r"))               ; open the file for reading
  (if i 
    (progn
      (while (setq c (read-line i))   ; read one line of text from the file
        (princ c)                     ; and print it on the screen
        (princ "\n")
      ) 
      (close i)                       ; close the file
    ) 
    (princ (strcat "Cannot open file " s))
  ) 
  (princ)
) 

;;; --------------------------------------------------------------------------;

