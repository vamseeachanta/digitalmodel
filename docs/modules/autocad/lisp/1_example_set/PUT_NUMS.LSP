  (defun c:put_nums (
                    / oldecho tmp numHt numValStr
                    )
    (setq oldecho (getvar "cmdecho"))
    (setvar "cmdecho" 0)
    (if (not numVal) (setq numVal 1))
    (if (not numInc) (setq numInc 1))
    (setq tmp (getint (strcat "\nStarting number <" (itoa numVal) ">: ")))
    (if tmp (setq numVal tmp))
    (setq tmp (getint (strcat "\nIncrement by <" (itoa numInc) ">: ")))
    (if tmp (setq numInc tmp))
    (if (= 0.0 (cdr (assoc 40 (tblsearch "style" (getvar "textstyle")))))
      (progn
       (initget (+ 1 2))
       (setq numHt (getdist "\nText height: "))
      )
    )
    (while (setq pikPnt (getpoint
                          (strcat
                            "\nInsertion point for "
                            (itoa numVal)
                            ": "
                          )
                        )
           )
      (setq
        numValStr (itoa numVal)
        numVal (+ numVal numInc)
      )
      (command "._text" "mc" pikPnt)
      (if numHt (command numHt))
      (command "" numValStr)
    )
    (setvar "cmdecho" oldecho)
    (princ)
  )
  
  
  
  (defun C:PUT_NUMS2 ( / oldecho tmp numHt numValStr)
    (setq oldecho (getvar "cmdecho"))
    (setvar "cmdecho" 0)
    (if (not numVal) (setq numVal 1))
    (if (not numInc) (setq numInc 1))
    (setq tmp (getint (strcat "\nStarting number <" (itoa numVal) ">: ")))
    (if tmp (setq numVal tmp))
    (setq numValStr (itoa numVal))
    (setq tmp (getint (strcat "\nIncrement by <" (itoa numInc) ">: ")))
    (if tmp (setq numInc tmp))
    (if (= 0.0 (cdr (assoc 40 (tblsearch "style" (getvar "textstyle")))))
      (progn
       (initget (+ 1 2))
       (setq numHt (getdist "\nText height: "))
      )
    );end if
    (while (setq pikPnt (getpoint (strcat "\nInsertion point for " numValStr ": ")))
      (command "._text" "_mc" pikPnt)
      (if numHt (command numHt))
      (command "" numValStr)
      (setq
       numVal (+ numVal numInc)
       numValStr (itoa numVal)
      )
    );end while
    (setvar "cmdecho" oldecho)
    (princ)
  );end C:PUT_NUMS

