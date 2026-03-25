;PROGRAM: 1-2&A-Z.LSP
;AUTHOR : Patrick Cragnolini
(princ "PURPOSE: creates a suite of numbers starting from user defined\n")
(princ "         start to user defined end, spaced by user defined displacement\n")
(princ "         NOTE: Prior to use ,set the STYLE and UCS.\n")
;
(defun 1-2&A-Z(/ inc alpnum rot count displ start ang endv)
(setq count 0 endv 0 inc 1)
(initget 1 "Alpha Num")
(setq alpnum (getkword "Alphabetic or Numeric: "))
(if (= alpnum "Num")
    (progn
         (while (>= count endv)
                (initget)
                (setq count (getint "Starting value: "))
                (initget 1)
                (setq endv (getint "Ending value: "))
                (initget 1)
                (setq inc (getint "Increment: "))
         );end of while
    );end of progn
    (progn
         (while (>= count endv)
                (setq count (ascii (getstring "Starting character: "))
                      endv (ascii (getstring "Ending character: ")))
         );end of while
    );end of progn
);endif 
(initget 1)
(setq start (getpoint "Starting point: "))
(initget (+ 1 2 4))
(setq displ (getdist start "Offset distance: "))
(setq ang (getangle start "Offset direction:<0> ")
      rot (getangle "Text rotation angle:<0> "))
(if (null ang) (setq ang 0))
(if (null rot) (setq rot 0) (setq rot (rtd rot)))
      (while (<= count endv)
             (if (= alpnum "Num")
                 (command "text" start rot count)
                 (command "text" start rot (chr count))
             );endif
             (setq start (polar start ang displ))
             (setq count (+ count inc))
      );end of while
);end of defun
(princ)
