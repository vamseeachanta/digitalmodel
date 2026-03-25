(defun C:PANEL
  (/ clay osm cmde pt1 len wid pt1H pt2H pt2V pt3V pt3H pt4H pt4V pt1V)
  (setq
    clay (getvar 'clayer)
    osm (getvar 'osmode)
    cmde (getvar 'cmdecho)
  ); setq
  (setq
    pt1 (getpoint "\nLower left White-lines intersection: ")
    len (getdist pt1 "\nLength (horizontal dimension): ")
    wid (getdist pt1 "\nWidth (vertical dimension): ")
  ); setq
  (setvar 'osmode 0)
  (setvar 'cmdecho 0)
  (command
    "_.LAYER" "_T" "White" "_M" "White" "_Color" 7 "" ""
    "_.pline"
      (setq pt1H (polar pt1 pi 25))
      (setq pt2H (polar pt1 0 (+ len 25)))
      ""
    "_.pline"
      (setq pt2V (mapcar '- pt2H '(25 25 0)))
      (setq pt3V (polar pt2V (/ pi 2) (+ wid 50)))
      ""
    "_.pline"
      (setq pt3H (polar pt2H (/ pi 2) wid))
      (setq pt4H (polar pt1H (/ pi 2) wid))
      ""
    "_.pline"
      (setq pt4V (polar pt1 (/ pi 2) (+ wid 25)))
      (setq pt1V (polar pt1 (* pi 1.5) 25))
      ""
    "_.LAYER" "_T" "Green" "_M" "Green" "_Color" 3 "" ""
    "_.line" pt1V pt2V ""
    "_.line" pt2H pt3H ""
    "_.line" pt3V pt4V ""
    "_.line" pt4H pt1H ""
  ); command
  (setvar 'clayer clay)
  (setvar 'osmode osm)
  (setvar 'cmdecho cmde)
  (princ)
); defun