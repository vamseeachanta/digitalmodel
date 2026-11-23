(defun c:ADDTEXT (/ CS CSZ CC CL CLT
                    EG ES TY NL)
              (setvar "CMDECHO" 0)
              (setq CS (getvar "TEXTSTYLE")
                   CSZ (getvar "TEXTSIZE")
                    CC (getvar "CECOLOR")
                    CL (getvar "CLAYER")
                   CLT (getvar "CELTYPE"))
              (setq EG (entget (setq ES (car (entsel
                  "\nSelect preceeding text: ")))))
              (if (= "TEXT" (cdr (assoc 0 EG)))
                (progn
                  (setq TY (cdr (assoc 72 EG))
                        NL (cdr (assoc 8 EG)))
              (princ "\nText: ")
              (command "COLOR" (cond ((not
                (assoc 62 EG)) "bylayer")
              (T (assoc 62 EG)))
                 "LINETYPE" "s" "bylayer" ""
                 "LAYER" "on" NL "t" NL "s" NL ""
                 "ERASE" ES ""
                 "TEXT" "s" (cdr (assoc 7 EG))
                 (cond ((= TY 0) "none")
                       ((= TY 1) "c")
                       ((= TY 2) "r")
                       ((= TY 3) "none")
                       ((= TY 4) "m")
                       ((= TY 5) "f"))
                 (cond ((or (= TY 1) (= TY 2)
                   (= TY 4)) (cdr (assoc 11 EG)))
                   (t (cdr (assoc 10 EG))))
                 (cond ((= TY 5) (cdr (assoc 11 EG)))
                   (t (cdr (assoc 40 EG))))
                 (cond ((= TY 5) (cdr (assoc 40 EG)))
                   (t (* (cdr (assoc 50 EG))
                   57.29578)))
                   (cdr (assoc 1 EG))
               "DTEXT" ""
)
               (setvar "TEXTSIZE" CSZ)
               (command "COLOR" CC
                        "LINETYPE" "s" CLT ""
                        "LAYER" "s" CL "")
               (if (/= CS (cdr (assoc 7 EG)))
                 (command "TEXT" "s" CS
                   "0,0" "" "" "")) )
               (princ "\nNot that you dummy-TEXT!")
               )
               (princ)
)
