
(defun C:PSTEXT ( / pre suf sset n index el c old new d b2)
   (setq count 0)
   (setq pre (getstring  1 "\nEnter text prefix: "))
   (setq suf (getstring 1 "\nEnter text suffix: "))
   (princ "\nSelect TEXT to change.")
   (if (setq sset (ssget))
      (progn
         (setq n (sslength sset))
         (setq index 0)
         (repeat n
            (setq el (entget (ssname sset index)))
            (setq index (1+ index))
            (if (= "TEXT" (cdr (assoc 0 el)))
               (progn
                  (setq c (assoc 1 el)
                      old (cdr c)
                      new (strcat pre old suf)
                        d (cons (car c) new)
                       b2 (subst d c el)
                    count (1+ count)
                  );setq
                  (entmod b2)
               );progn
               nil
            );if
         );repeat
      );progn
      (princ "\nNo entities were selected.")
   );if
   (princ (strcat "\n" (itoa count) " TEXT entities were processed."))
   (princ)
);defun C:ADDTEXT

