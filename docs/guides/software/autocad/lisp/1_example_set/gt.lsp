;GT.LSP V.2.1 10/18/99
;Provided "AS IS" by Bill DeShawn. bdeshawn@prodigy.net
;Permission granted to change or distrubute
;Bill DeShawn will not be held liable for any damages as a result of this code.
;Use this code at your own risk.
;There's a better way.  I just don't know how.  If you don't like it, fix it yourself!
(defun gterr (msg)
   (setq msg "Done.  ")
   (setq *error* olderr)
   (setq chg nil)
   (setq sset nil)
   (terpri)(terpri)
   (princ msg)
   (command "_.undo" "_end")
   (princ)
);

(DEFUN RTD (A)
   (/ (* A 180.0) PI)
)

(DEFUN DTR (A)
   (* PI (/ A 180.0))
)

(defun case ();(/ sset sslen index newstr ename elist oldstr newelist)
   (setq sslen (sslength sset))
   (setq index 0)
   (prompt "\nUpper or Lower case <U/L>")
   (setq newstr (getstring))
   (repeat sslen
      (setq ename (ssname sset index))
      (setq elist (assoc 1 (entget ename)))
      (if
         (or 
            (eq (cdr (assoc 0 (entget ename))) "TEXT")
            (eq (cdr (assoc 0 (entget ename))) "ARCALIGNEDTEXT")
         )
         (progn
            (setq oldstr (cdr (assoc 1 (entget ename)))); return the text content and call it oldstr
            (if (or (= newstr "X")(= newstr "")) (exit))
            (if (equal (strcase newstr) "U");                          if upper case is chosen 
               (setq newelist (cons 1 (strcase oldstr)));            then construct a list starting with 1 
               ;                                                                          & ending with upper case string
            )
            (if (equal (strcase newstr) "L") ;                          if lower case is chosen
               (setq newelist (cons 1 (strcase oldstr t)));           then construct a list starting with 1
            );                                                                              & ending with lower case string
            (setq changed (subst newelist elist (entget ename)));   subst new list for old
            (entmod changed)
            ;         (redraw ename 4)
         )
      )
      (setq index (+ index 1))
   )
   (if (eq (cdr (assoc 0 (entget ename))) "TEXT")
      (progn
         (setq chg (strcase (getstring "\nChange: Style/Height/Oblq angle/Rotation/Width/Content/cAse/eXit <X>: ")))
         (while (= chg "S")(styl))
         (while (= chg "C")(content))
         (while (= chg "H")(hite))
         (while (= chg "O")(oblique))
         (while (= chg "R")(rot))
         (while (= chg "W")(wid))
         (while (= chg "A")(case))
         (if (= chg "X")(gterr msg))
      )
   )
   (if (eq (cdr (assoc 0 (entget ename))) "ARCALIGNEDTEXT")
      (progn
         (setq chg (strcase (getstring "\nChange: Height/Style/Dist from center/Ending angle/sTarting angle/Width/Content/cAse/eXit <X>: ")))
         (while (= chg "H")(athite))
         (while (= chg "S")(styl))
         (while (= chg "C")(content))
         (while (= chg "D")(hite))
         (while (= chg "E")(oblique))
         (while (= chg "S")(rot))
         (while (= chg "W")(wid))
         (while (= chg "A")(case))
         (if (= chg "X")(gterr msg))
      )
   )
);

(defun styl ();(/ sset sslen index newstr ename elist oldstr newelist)
   (setq sslen (sslength sset))
   (setq index 0)
   (terpri)(terpri)
   (prompt "\nStyle name: ")
   (setq newstl (strcase (getstring)))
   (if (eq newstl "STANDARD")(setq newfont "TXT")(setq newfont newstl) )
   (repeat sslen
      (setq ename (ssname sset index))
      (setq elist (assoc 7 (entget ename)))
      (if 
         (or
            (eq (cdr (assoc 0 (entget ename))) "TEXT")
            (eq (cdr (assoc 0 (entget ename))) "ARCALIGNEDTEXT")
         )
         (progn
            (setq oldstl (cdr (assoc 7 (entget ename))));
            (if (or (= newstl "X")(= newstl "")) (gterr msg))
            (setq newelist (cons 7 (strcase newstl))); 
            (if (eq (cdr (assoc 0 (entget ename))) "ARCALIGNEDTEXT")
               (progn
                  (setq elist (assoc 2 (entget ename)))
                  (setq newelist (cons 2 (strcase newfont))); This is here because of a bug in this object.type.
               )
            ) ;Bug is that the font file should be defined in the style.  Not here in the elist.
            (setq changed (subst newelist elist (entget ename)));   subst new list for old
            (entmod changed)
         )
      )
      (setq index (+ index 1))
   ); end repeat
   (if (eq (cdr (assoc 0 (entget ename))) "TEXT")
      (progn
         (setq chg (strcase (getstring "\nChange: Style/Height/Oblq angle/Rotation/Width/Content/cAse/eXit <X>: ")))
         (while (= chg "S")(styl))
         (while (= chg "C")(content))
         (while (= chg "H")(hite))
         (while (= chg "O")(oblique))
         (while (= chg "R")(rot))
         (while (= chg "W")(wid))
         (while (= chg "A")(case))
         (if (= chg "X")(gterr msg))
      )
   )
   (if (eq (cdr (assoc 0 (entget ename))) "ARCALIGNEDTEXT")
      (progn
         (setq chg (strcase (getstring "\nChange: Height/Style/Dist from center/Ending angle/sTarting angle/Width/Content/cAse/eXit <X>: ")))
         (while (= chg "H")(athite))
         (while (= chg "S")(styl))
         (while (= chg "C")(content))
         (while (= chg "D")(hite))
         (while (= chg "E")(oblique))
         (while (= chg "S")(rot))
         (while (= chg "W")(wid))
         (while (= chg "A")(case))
         (if (= chg "X")(gterr msg))
      )
   )
);

(defun hite ();(/ sset sslen index newht ename elist oldstr newelist)
   (setq sslen (sslength sset))
   (setq index 0)
   (if (eq (cdr (assoc 0 (entget ename))) "TEXT")
      (setq newht (getdist "\nHow high?  "))
   )
   (if (eq (cdr (assoc 0 (entget ename))) "ARCALIGNEDTEXT")
      (setq newht (getdist "\nDistance from center:  "))
   )
   (repeat sslen
      (setq ename (ssname sset index))
      (setq alist (assoc 40 (entget ename)))
      (if 
         (or
            (eq (cdr (assoc 0 (entget ename))) "TEXT")
            (eq (cdr (assoc 0 (entget ename))) "ARCALIGNEDTEXT")
         )
         (progn
            (setq oldht (cdr (assoc 40 (entget ename)))); return the old height
            (setq newalist (cons 40 newht)); then construct a list including the new height.
            (setq changed (subst newalist alist (entget ename)));   subst new list for old
            (entmod changed)
            (setq index (1+ index))
         )
      )
   )
   (if (eq (cdr (assoc 0 (entget ename))) "TEXT")
      (progn
         (setq chg (strcase (getstring "\nChange: Style/Height/Oblq angle/Rotation/Width/Content/cAse/eXit <X>: ")))
         (while (= chg "S")(styl))
         (while (= chg "C")(content))
         (while (= chg "H")(hite))
         (while (= chg "O")(oblique))
         (while (= chg "R")(rot))
         (while (= chg "W")(wid))
         (while (= chg "A")(case))
         (if (= chg "X")(gterr msg))
      )
   )
   (if (eq (cdr (assoc 0 (entget ename))) "ARCALIGNEDTEXT")
      (progn
         (setq chg (strcase (getstring "\nChange: Height/Style/Dist from center/Ending angle/sTarting angle/Width/Content/cAse/eXit <X>: ")))
         (while (= chg "H")(athite))
         (while (= chg "S")(styl))
         (while (= chg "C")(content))
         (while (= chg "D")(hite))
         (while (= chg "E")(oblique))
         (while (= chg "S")(rot))
         (while (= chg "W")(wid))
         (while (= chg "A")(case))
         (if (= chg "X")(gterr msg))
      )
   )
);

(defun athite ();(/ sset sslen index newht ename elist oldstr newelist)
     (if (eq (cdr (assoc 0 (entget ename))) "ARCALIGNEDTEXT")
      (setq newht (getdist "\nHeight:  "))
   )
   (repeat sslen
      (setq ename (ssname sset index))
      (setq alist (assoc 42 (entget ename)))
      (if 
         (or
            (eq (cdr (assoc 0 (entget ename))) "TEXT")
            (eq (cdr (assoc 0 (entget ename))) "ARCALIGNEDTEXT")
         )
         (progn
            (setq oldht (cdr (assoc 42 (entget ename)))); return the old height
            (setq newalist (cons 42 newht)); then construct a list including the new height.
            (setq changed (subst newalist alist (entget ename)));   subst new list for old
            (entmod changed)
            (setq index (1+ index))
         )
      )
   )
   (if (eq (cdr (assoc 0 (entget ename))) "TEXT")
      (progn
         (setq chg (strcase (getstring "\nChange: Style/Height/Oblq angle/Rotation/Width/Content/cAse/eXit <X>: ")))
         (while (= chg "S")(styl))
         (while (= chg "C")(content))
         (while (= chg "H")(hite))
         (while (= chg "O")(oblique))
         (while (= chg "R")(rot))
         (while (= chg "W")(wid))
         (while (= chg "A")(case))
         (if (= chg "X")(gterr msg))
      )
   )
   (if (eq (cdr (assoc 0 (entget ename))) "ARCALIGNEDTEXT")
      (progn
         (setq chg (strcase (getstring "\nChange: Height/Style/Dist from center/Ending angle/sTarting angle/Width/Content/cAse/eXit <X>: ")))
         (while (= chg "H")(athite))
         (while (= chg "S")(styl))
         (while (= chg "C")(content))
         (while (= chg "D")(hite))
         (while (= chg "E")(oblique))
         (while (= chg "S")(rot))
         (while (= chg "W")(wid))
         (while (= chg "A")(case))
         (if (= chg "X")(gterr msg))
      )
   )
);

(defun oblique ();(/ sset sslen index newht ename elist oldstr newelist)
   (setq sslen (sslength sset))
   (setq index 0)
   (if (eq (cdr (assoc 0 (entget ename))) "TEXT")
      (setq newobl (dtr (getreal "\nOblique angle:  ")))
   )
   (if (eq (cdr (assoc 0 (entget ename))) "ARCALIGNEDTEXT")
      (setq newobl (dtr (getreal "\nEnding angle:  ")))
   )
   (repeat sslen
      (setq ename (ssname sset index))
      (setq alist (assoc 51 (entget ename)))
      (if 
         (or
            (eq (cdr (assoc 0 (entget ename))) "TEXT")
            (eq (cdr (assoc 0 (entget ename))) "ARCALIGNEDTEXT")
         )
         (progn
            (setq oldobl (cdr (assoc 51 (entget ename)))); return the old oblique angle
            (setq newalist (cons 51 newobl)); then construct a list including the new angle.
            (setq changed (subst newalist alist (entget ename)));   subst new list for old
            (entmod changed)
            (setq index (1+ index))
         )
      )
   )
   (if (eq (cdr (assoc 0 (entget ename))) "TEXT")
      (progn
         (setq chg (strcase (getstring "\nChange: Style/Height/Oblq angle/Rotation/Width/Content/cAse/eXit <X>: ")))
         (while (= chg "S")(styl))
         (while (= chg "C")(content))
         (while (= chg "H")(hite))
         (while (= chg "O")(oblique))
         (while (= chg "R")(rot))
         (while (= chg "W")(wid))
         (while (= chg "A")(case))
         (if (= chg "X")(gterr msg))
      )
   )
   (if (eq (cdr (assoc 0 (entget ename))) "ARCALIGNEDTEXT")
      (progn
         (setq chg (strcase (getstring "\nChange: Height/Style/Dist from center/Ending angle/sTarting angle/Width/Content/cAse/eXit <X>: ")))
         (while (= chg "H")(athite))
         (while (= chg "S")(styl))
         (while (= chg "C")(content))
         (while (= chg "D")(hite))
         (while (= chg "E")(oblique))
         (while (= chg "S")(rot))
         (while (= chg "W")(wid))
         (while (= chg "A")(case))
         (if (= chg "X")(gterr msg))
      )
   )
);

(defun rot ();(/ sset sslen index newht ename elist oldstr newelist)
   (setq sslen (sslength sset))
   (setq index 0)
   (if (eq (cdr (assoc 0 (entget ename))) "TEXT")
      (setq newrot (dtr (getreal "\nRotation:  ")))
   )
   (if (eq (cdr (assoc 0 (entget ename))) "ARCALIGNEDTEXT")
      (setq newrot (dtr (getreal "\nStarting angle:  ")))
   )
   (repeat sslen
      (setq ename (ssname sset index))
      (setq alist (assoc 50 (entget ename)))
      (if
         (or
            (eq (cdr (assoc 0 (entget ename))) "TEXT")
            (eq (cdr (assoc 0 (entget ename))) "ARCALIGNEDTEXT")
         )
         (progn
            (setq oldrot (cdr (assoc 50 (entget ename)))); return the old rotation
            (setq newalist (cons 50 newrot)); then construct a list including the new angle.
            (setq changed (subst newalist alist (entget ename)));   subst new list for old
            (entmod changed)
            (setq index (1+ index))
         )
      )
   )
   (if (eq (cdr (assoc 0 (entget ename))) "TEXT")
      (progn
         (setq chg (strcase (getstring "\nChange: Style/Height/Oblq angle/Rotation/Width/Content/cAse/eXit <X>: ")))
         (while (= chg "S")(styl))
         (while (= chg "C")(content))
         (while (= chg "H")(hite))
         (while (= chg "O")(oblique))
         (while (= chg "R")(rot))
         (while (= chg "W")(wid))
         (while (= chg "A")(case))
         (if (= chg "X")(gterr msg))
      )
   )
   (if (eq (cdr (assoc 0 (entget ename))) "ARCALIGNEDTEXT")
      (progn
         (setq chg (strcase (getstring "\nChange: Height/Style/Dist from center/Ending angle/sTarting angle/Width/Content/cAse/eXit <X>: ")))
         (while (= chg "H")(athite))
         (while (= chg "S")(styl))
         (while (= chg "C")(content))
         (while (= chg "D")(hite))
         (while (= chg "E")(oblique))
         (while (= chg "S")(rot))
         (while (= chg "W")(wid))
         (while (= chg "A")(case))
         (if (= chg "X")(gterr msg))
      )
   )
);

(defun wid ();(/ sset sslen index newht ename elist oldstr newelist)
   (setq sslen (sslength sset))
   (setq index 0)
   (setq newwid (getreal "\nWidth factor:  "))
   (repeat sslen
      (setq ename (ssname sset index))
      (setq alist (assoc 41 (entget ename)))
      (if
         (or
            (eq (cdr (assoc 0 (entget ename))) "TEXT")
            (eq (cdr (assoc 0 (entget ename))) "ARCALIGNEDTEXT")
         )
         (progn
            (setq oldwid (cdr (assoc 41 (entget ename)))); return the old width factor
            (setq newalist (cons 41 newwid)); then construct a list including the new angle.
            (setq changed (subst newalist alist (entget ename)));   subst new list for old
            (entmod changed)
            (setq index (1+ index))
         )
      )
   )
   (if (eq (cdr (assoc 0 (entget ename))) "TEXT")
      (progn
         (setq chg (strcase (getstring "\nChange: Style/Height/Oblq angle/Rotation/Width/Content/cAse/eXit <X>: ")))
         (while (= chg "S")(styl))
         (while (= chg "C")(content))
         (while (= chg "H")(hite))
         (while (= chg "O")(oblique))
         (while (= chg "R")(rot))
         (while (= chg "W")(wid))
         (while (= chg "A")(case))
         (if (= chg "X")(gterr msg))
      )
   )
   (if (eq (cdr (assoc 0 (entget ename))) "ARCALIGNEDTEXT")
      (progn
         (setq chg (strcase (getstring "\nChange: Height/Style/Dist from center/Ending angle/sTarting angle/Width/Content/cAse/eXit <X>: ")))
         (while (= chg "H")(athite))
         (while (= chg "S")(styl))
         (while (= chg "C")(content))
         (while (= chg "D")(hite))
         (while (= chg "E")(oblique))
         (while (= chg "S")(rot))
         (while (= chg "W")(wid))
         (while (= chg "A")(case))
         (if (= chg "X")(gterr msg))
      )
   )
);

(defun content ();(/ sset sslen index newht ename elist oldstr newelist)
   (setq sslen (sslength sset))
   (setq index 0)
   (setq newcon (getstring t "\nNew Content::  "))
   (repeat sslen
      (setq ename (ssname sset index))
      (setq alist (assoc 1 (entget ename)))
      (if
         (or
            (eq (cdr (assoc 0 (entget ename))) "TEXT")
            (eq (cdr (assoc 0 (entget ename))) "ARCALIGNEDTEXT")
         )
         (progn
            (setq oldcon (cdr (assoc 1 (entget ename)))); return the old content
            (setq newalist (cons 1 newcon)); then construct a list including the new content
            (setq changed (subst newalist alist (entget ename)));   subst new list for old
            (entmod changed)
            (setq index (1+ index))
         )
      )
   )
   (if (eq (cdr (assoc 0 (entget ename))) "TEXT")
      (progn
         (setq chg (strcase (getstring "\nChange: Style/Height/Oblq angle/Rotation/Width/Content/cAse/eXit <X>: ")))
         (while (= chg "S")(styl))
         (while (= chg "C")(content))
         (while (= chg "H")(hite))
         (while (= chg "O")(oblique))
         (while (= chg "R")(rot))
         (while (= chg "W")(wid))
         (while (= chg "A")(case))
         (if (= chg "X")(gterr msg))
      )
   )
   (if (eq (cdr (assoc 0 (entget ename))) "ARCALIGNEDTEXT")
      (progn
         (setq chg (strcase (getstring "\nChange: Height/Style/Dist from center/Ending angle/sTarting angle/Width/Content/cAse/eXit <X>: ")))
         (while (= chg "H")(athite))
         (while (= chg "S")(styl))
         (while (= chg "C")(content))
         (while (= chg "D")(hite))
         (while (= chg "E")(oblique))
         (while (= chg "S")(rot))
         (while (= chg "W")(wid))
         (while (= chg "A")(case))
         (if (= chg "X")(gterr msg))
      )
   )
);


(defun c:gt (/ chg)
   (setq olderr *error*)
   (setq *error* gterr)
   (command "_.undo" "g")
   (if  (null chg) (setq sset (ssget)))
   (setq index 0)
   (setq ename (ssname sset index))
   (if (eq (cdr (assoc 0 (entget ename))) "TEXT")
      (progn
         (setq enttype "TEXT")
         (setq chg (strcase (getstring "\nChange: Style/Height/Oblq angle/Rotation/Width/Content/cAse/eXit <X>: ")))
         (while (= chg "S")(styl))
         (while (= chg "C")(content))
         (while (= chg "H")(hite))
         (while (= chg "O")(oblique))
         (while (= chg "R")(rot))
         (while (= chg "W")(wid))
         (while (= chg "A")(case))
         (if (= chg "X")(gterr msg))
      )
   )
   (if (eq (cdr (assoc 0 (entget ename))) "ARCALIGNEDTEXT")
      (progn
         (setq enttype "ARCALIGNEDTEXT")   
         (setq chg (strcase (getstring "\nChange: Height/Style/Dist from center/Ending angle/sTarting angle/Width/Content/cAse/eXit <X>: ")))
         (while (= chg "H")(athite))
         (while (= chg "S")(styl))
         (while (= chg "C")(content))
         (while (= chg "D")(hite))
         (while (= chg "E")(oblique))
         (while (= chg "T")(rot))
         (while (= chg "W")(wid))
         (while (= chg "A")(case))
         (if (= chg "X")(gterr msg))
      )
   )
   (setq *error* olderr)
   (command "_.undo" "_end")
   (princ)
)

(Alert "      Select multiple objects of one object type
\n            Either TEXT or ARCALIGNEDTEXT.
\n      Because of an bug in ARCALIGNEDTEXT objects,\nplease name the text styles identical with the font names.

                 This is NOT an MTEXT editing tool.")
(prompt "Type GT to start.  ")(terpri)
(princ)