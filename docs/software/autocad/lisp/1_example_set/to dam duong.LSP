;                             CHW.LSP

(defun echff ()
       (setq scmde (getvar "cmdecho"))
       (setvar "cmdecho" 0)
)
(defun echrs ()
       (setvar "cmdecho" scmde)
)

(defun chwerr (s)
   (if (/= s "Function cancelled")   ; If an error (such as CTRL-C) occurs
      (princ (strcat "\nError: " s)) ; while this command is active...
   )
   (echrs)
   (setq p nil)                      ; Free selection set
   (setq *error* olderr)             ; Restore old *error* handler
   (princ)
)

(defun C:CHW (/ p rw w n entx ename entg pt r olderr)
   (setq olderr  *error*             ; Initialize variables
         *error* chwerr)

       (initget 1)
       (setq p (ssget))
       (setq n 0 entx (ssname p n))
       (if (null w$$)
           (progn
                 (initget 1)
                 (prompt "\nNew width: ")
           )
           (progn
                 (prompt "\nNew width /<")
                 (princ (strcat w$$ ">: "))
           )
       )
       (setq rw (getstring))
       (if (and (= rw "") (/= nil w$$))
           (setq rw w$$)
           (setq w$$ rw)
       )
       (setq w (abs (atof rw)))
       (echff)
       (repeat (sslength p)
               (setq entg (entget entx))
               (setq ename (cdr (assoc 0 entg)))
               (cond ((= ename "POLYLINE")
                      (command "pedit" entx "w" w ""))
                     ((member ename '("LINE" "ARC"))
                      (command "pedit" entx "" "w" w ""))
                     ((= ename "CIRCLE")
                      (setq pt (cdr (assoc 10 entg)))
                      (setq r (cdr (assoc 40 entg)))
                      (command "DONUT" (- (* 2.0 r) w)
                                       (+ (* 2.0 r) w) pt ""
                      )
                      (entdel entx)
                     )
                     (T nil)
               )
               (setq n (1+ n) entx (ssname p n))
       )
       (echrs)
       (setq *error* olderr)             ; Restore old *error* handler
       (prin1)
)
