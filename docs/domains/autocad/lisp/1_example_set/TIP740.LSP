; TIP740.LSP   Change Elevations   (c)1992, Jim Neelands

;NUM.LSP
(defun C:NUM ()
(setq ss1 (ssget)
      len1 (sslength ss1)
      cnt 0
      add (getreal "\nEnter number to add to elevations:  ")
      )
(while (< cnt len1)
  (setq dent (entget (ssname ss1 cnt))
        txt (cdr (assoc 1 dent))
        len (strlen txt)
        cnt1 1
        )
   (while (<= cnt1 len)
    (if (/= (substr txt cnt1 1) " ")
     (progn
      (setq num (atof (substr txt cnt1 len))
            numlen (strlen (rtos num 2 2)))
       (if (/= num 0)
         (progn    
          (cond ((= (substr txt cnt1 numlen) (rtos num 2 2)) (setq sp 0))
                ((= (substr txt cnt1 (- numlen 1)) (rtos num 2 1)) (setq sp 1))
                ((= (substr txt cnt1 (- numlen 3)) (rtos num 2 0)) (setq sp 3))
            );cond      
            (setq rtxt (substr txt (- (+ cnt1 numlen) sp) len)
                  num (+ add num)
                  numt (rtos num 2 2)
                  ntx  (strcat (substr txt 1 (-  cnt1 1)) numt rtxt)
                  old (assoc 1 dent)
                  new (cons 1 ntx)
                  dent (subst new old dent)    
                  cnt1 (+ 1 len)
               );setq
                (entmod dent)
               );progn
             )));if,progn,if
        (setq cnt1 (1+ cnt1))      
      );end while
  (setq cnt (1+ cnt))    
 );while
);defun
