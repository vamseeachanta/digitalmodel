;;; By Jimmy Bergmark
;;; Copyright (C) 1997-2006 JTB World, All Rights Reserved
;;; Website: www.jtbworld.com
;;; E-mail: info@jtbworld.com
 ;
 ;     1999-12-22  First release
 ;     2000-03-02  Simplified command entry and code
 ;
 ;    This program moves the startpoint/snapbase for 
 ;    selected hatches base points independently.
 ;    Or it can be used to set the base point same for all selected hatches.
 ;    The program stops when pressing Enter/Rightclick at any point.
 ;
 ;    Tested for AutoCAD 2000

(defun c:hm () (c:hatch_move))
(defun c:hatch_move (/         ss1       fp        tp        loop
                     ent       oldsnapbase         oldosmode oldsp
                     newsp     contin    oldgrips  oldcmdecho
                     errexit   restx     restore
                    )
  (defun errexit (msg)
    (restore)
  ) ;_ end of defun
  (defun restx ()
    (setvar "snapbase" oldsnapbase)
    (setvar "osmode" oldosmode)
    (setvar "grips" oldgrips)
    (command "_.UNDO" "_E")
    (setvar "cmdecho" oldcmdecho)
    (setq *error* olderr)
    (princ)
  ) ;_ end of defun
  (setq olderr  *error*
        restore restx
        *error* errexit
  ) ;_ end of setq
  (setq oldcmdecho (getvar "cmdecho"))
  (setvar "cmdecho" 0)
  (setq oldgrips (getvar "grips"))
  (setvar "grips" 0)
  (setq oldsnapbase (getvar "snapbase"))
  (setq oldosmode (getvar "osmode"))
  (setq ss1 (cadr (sssetfirst nil (cadr (ssgetfirst)))))
 ; get ssgetfirst before UNDO
  (command "_.UNDO" "_BE")
  (graphscr)
  (if (setq ss1 (if ss1
                  (if (setq ss1 (ssget "P" '((0 . "HATCH"))))
                    ss1 ; hatches found in previous sel set
                    (ssget '((0 . "HATCH")))
 ; no hatches previously selected
                  ) ;_ end of if
                  (ssget '((0 . "HATCH"))) ; no previous sel set
                ) ;_ end of if
      ) ;_ end of setq
    (while
      (and
        (not (initget 228 "Set"))
        (if ss1
          (sssetfirst nil ss1)
          t
        ) ; show what is selected
        (setq fp
               (getpoint
                 "\nBase point for displacement or set same base point for all [Set] <exit>: "
               ) ;_ end of getpoint
        ) ;_ end of setq
        (progn
          (if (= fp "Set")
            (progn (initget 98)
                   (setq
                     fp (getpoint "\nSet same base point for all <exit>: ")
                   ) ;_ end of setq
                   (setq contin nil)
            ) ;_ end of progn
            (progn (initget 98)
                   (setq tp
                          (getpoint fp
                                    "\nSecond point for displacement <exit>: "
                          ) ;_ end of getpoint
                   ) ;_ end of setq
                   (setq contin t)
            ) ;_ end of progn
          ) ; end if 
          fp
        ) ; end progn
      ) ; end and
       (setvar "osmode" 0)
       (setq loop 0)
       (while (setq ent (ssname ss1 loop))
         (if contin
           (progn (setq oldsp (list (cdr (assoc 43 (entget ent)))
                                    (cdr (assoc 44 (entget ent)))
                              ) ;_ end of list
                  ) ;_ end of setq
                  (setq newsp (list (- (+ (car oldsp) (car tp)) (car fp))
                                    (- (+ (cadr oldsp) (cadr tp)) (cadr fp))
                              ) ;_ end of list
                  ) ;_ end of setq
           ) ;_ end of progn
           (setq newsp (list (car fp) (cadr fp)))
         ) ; end if
         (setvar "snapbase" newsp)
         (setvar "osmode" oldosmode)
         (sssetfirst nil)
         (command ".hatchedit" ent "" "" "" "")
         (setq loop (1+ loop))
       ) ; end while
    ) ; end while
  ) ; end if
  (restore)
) ;_ end of defun




