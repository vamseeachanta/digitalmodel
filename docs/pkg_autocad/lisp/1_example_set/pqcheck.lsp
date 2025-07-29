; PQCHECK
;
;     Copyright (C) 1992 by Autodesk, Inc.
;
;     Permission to use, copy, modify, and distribute this software 
;     for any purpose and without fee is hereby granted, provided 
;     that the above copyright notice appears in all copies and that 
;     both that copyright notice and this permission notice appear in 
;     all supporting documentation.
;
;     THIS SOFTWARE IS PROVIDED "AS IS" WITHOUT EXPRESS OR IMPLIED
;     WARRANTY.  ALL IMPLIED WARRANTIES OF FITNESS FOR ANY PARTICULAR
;     PURPOSE AND OF MERCHANTABILITY ARE HEREBY DISCLAIMED.
;     ****************************************************************

;        --This routine checks lisp programs for mismatched parentheses          
;          and closing quotes.  If a missing quote is found, it tells you 
;          on what line to look for the error.  If a missing closing            
;          parenthesis is found, it tells you on what line the last set of
;          matched parentheses were found.  In this case, the mismatched        
;          parentheses ocurr after that line.  If an extra closing parenthesis
;          is found, the routine reports the line number where it was found.
;          In this case, the extra closing parenthesis that's identified is
;          usually the closing parenthesis to a defun, and the real extra 
;          closing parenthesis is somewhere above this point.
;
;          This program is not very smart.  It was written as a quick and      
;          dirty solution to providing slightly more information than
;          AutoLISP provides when it finds mismatched parentheses or missing
;          closing quotes. 
; 
;          To invoke:  Command: (load "pqcheck")
;                      C:PQCHECK
;                      Command: pqcheck
;                      Enter filename: test.lsp
;                      Checking..............
;                      Ok.

(defun update()
   (princ ".")
   (cond ((= popen pclose) (setq lastline line)))
   (cond ((> pclose popen)
          (princ (strcat "\nExtra closing parenthesis \042)\042 on line: "
                 (itoa line) "\nFix and rerun PQCHECK."))
          (setq quit t))
   );
   (cond ((/= qopen 0)
          (princ (strcat "\nNo closing quote on line: " (itoa line)
                 "\nFix and rerun PQCHECK."))
          (setq quit t))
   )
)

(defun parse()
   (setq len (strlen data))
   (setq i 1)
   (while (<= i len)
      (setq c (substr data i 1))
            ;if semicolon and not in middle of quote, ignore rest of line.
      (cond ((= c ";") (cond ((= qopen 0)
                              (setq i (+ len 1)))))
            ; if open parenthesis and not in middle of quote, increment popen.
            ((= c "(") (cond ((= qopen 0)
                               (setq popen (1+ popen)))))
            ; if close parenthesis and not in middle of quote, increment pclose.
            ((= c ")") (cond ((= qopen 0)
                              (setq pclose (1+ pclose)))))
            ((= c "\042") 
             (cond ((= qopen 0) (setq qopen 1))
                   ((= qopen 1) (setq qopen 0))
             );cond
            )
      );cond
      (setq i (1+ i))
   )
   (update)
)

(defun c:pqcheck()
   (setq name (getstring "\nEnter file name: "))
   (setq inf  (open name "r"))
   (cond ((not (null inf))
     (setq line 0 popen 0 pclose 0 qopen 0 lastline 0 quit nil)
     (setq data (read-line inf))
     (setq line (1+ line))
     (princ "\nChecking")
     (while (and (not (null data)) (not quit))
         (parse)
         (setq data (read-line inf))
         (setq line (1+ line))
     )
     (close inf)
     (cond ((null quit) 
            (cond ((/= popen pclose) 
                   (princ (strcat "\nMismatched parentheses.  Last matched "
                           "parentheses were on line: " (itoa lastline) "\n"))
                  )
                  (t (princ "\nOK.\n"))
            );cond
           )
     ));cond
     (t (princ (strcat "\nError:  Can't open \042" name "\042\n")))
   );cond
   (princ)
)
