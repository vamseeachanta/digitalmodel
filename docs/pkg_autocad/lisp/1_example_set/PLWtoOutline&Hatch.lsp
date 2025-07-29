;;  PLWtoOutline&Hatch.lsp [command name: PLWOH]
;;  To convert lightweight PolyLines with global Width to closed outlining Polylines
;;    without width, tracing the edges of the selected Polylines and closing their ends,
;;    and fill them with Hatch patterns.
;;  For closed Polylines, results in two Polylines tracing edges of original (does not
;;    connect their ends, and, of course, cannot join them together).
;;  Ignores non-Polylines, those with zero or varying width, and those on locked layers.
;;  Works with Polylines in different Coordinate Systems.
;;  Kent Cooper, February 2012
;
(defun C:PLWOH (/ *error* osm plset pl pldata two ucschanged firstmid ang offs pljoin)
;
  (defun *error* (errmsg)
    (if (wcmatch errmsg "Function cancelled,quit / exit abort,console break") (princ (strcat "\nError: " errmsg)))
    (setvar 'osmode osm)
    (if ucschanged (command "_.ucs" "_prev")); don't go back unless routine changed UCS but didn't change it back yet
    (command "_.undo" "_end")
    (setvar 'cmdecho cmde)
  ); end defun - *error*
;
  (vl-load-com)
  (command "_.undo" "_begin")
  (setq osm (getvar 'osmode) cmde (getvar 'cmdecho))
  (setvar 'cmdecho 0)
  (setvar 'osmode 0)
  (prompt "\nTo change PLines with global Width to outline PLines tracing their edges,")
  (setq
    osm (getvar 'osmode)
    plset (ssget '((0 . "LWPOLYLINE"))); [(ssget) provides its own Select objects: prompt]
  ); end setq
  (while (> (sslength plset) 0); as long as there's still something in the set
    (setq
      pl (ssname plset 0); get the first item
      pldata (entget pl); and its entity data
      two nil; marker for closed initial Polyline
    ); end setq
    (if
      (and
        (> (cdr (assoc 43 pldata)) 0.0); it has a non-zero global width
        (= (cdr (assoc 70 (tblsearch "layer" (cdr (assoc 8 pldata))))) 0); unlocked layer
      ); end and
      (progn ; then - process this Polyline
        ;;;;; (if [..... check relation to current CS .....]
        ;;;;;   (progn
        (command "_.ucs" "_new" "_object" pl) ; set UCS to match object;;;;;;;;;;; only if NOT in current CS????
          ;;;;;;;;;;;;;;;;;; UCSXDIR, UCSYDIR, UCSORG, maybe also ELEVATION System Variables
          ;;;;;;;;;;;;;;;;;; (assoc 210)
        (setq ucschanged T) ; for use in *error* function if UCS does not get reset
        ;;;;;   ); progn
        ;;;;; ); if
        (setq
          firstmid (trans (vlax-curve-getPointAtParam pl 0.5) 0 1); halfway along first segment
;           [more reliable basis for offset than start point, in case Pline closes at perpendicular or acute angle]
          ang (angle '(0 0 0) (vlax-curve-getFirstDeriv pl 0.5)); direction it's going there
          offs (/ (cdr (assoc 43 pldata)) 2); half of Pline's global width
          pljoin (ssadd); start initially empty set of items to join together
        ); end setq
        (command
          "_.pedit" pl "_width" 0 ""
          "_.offset" offs pl (polar firstmid (+ ang (/ pi 2)) 0.1) ""
        ); end command
        (ssadd (entlast) pljoin)
        (command "_.offset" offs pl (polar firstmid (- ang (/ pi 2)) 0.1) "")
        (ssadd (entlast) pljoin)
        (command "_.erase" pl ""); eliminate original
        (if (not (vlax-curve-isClosed (entlast))); open-ended - close ends & join
          (progn; then
            (setvar 'clayer (cdr (assoc 8 pldata)))
            (command
              "_.line"
                (trans (vlax-curve-getStartPoint (ssname pljoin 0)) 0 1)
                (trans (vlax-curve-getStartPoint (ssname pljoin 1)) 0 1)
                ""
            ); end command
            (ssadd (entlast) pljoin)
            (command
              "_.line"
                (trans (vlax-curve-getEndPoint (ssname pljoin 0)) 0 1)
                (trans (vlax-curve-getEndPoint (ssname pljoin 1)) 0 1)
                ""
              "_.layerp"
            ); end command
            (ssadd (entlast) pljoin)
            (command
              "_.pedit" (ssname pljoin 0) "_join" pljoin "" ""
            ); end command
          ); end progn
          (setq two T); else - closed initial Polyline; select pljoin set instead of Last
        ); end if
;;;;;;;;;; Edit, and remove semicolons at beginnings of, one set of lines for desired Hatch result:
;;;;; For USER-DEFINED PATTERN [Non-Associative]:
;        (command
;           "_.hatch"
;           "_User"
;           YourAngleInCurrentUnits
;           YourSpacing ; [with double-quotes if feet/inches/fractions are involved]
;           "_yesOr_noForDouble"
;           (if two pljoin "_last"); picks both if initial Polyline was closed
;           ""
;        ); command
;;;;; For USER-DEFINED PATTERN [Associative]:
;        (command
;          "_.bhatch"
;          "_properties"
;          "_User"
;           YourAngleInCurrentUnits
;           YourSpacing ; [with double-quotes if feet/inches/fractions are involved]
;           "_yesOr_noForDouble"
;          "_select"
;          (if two pljoin "_last"); picks both if initial Polyline was closed
;          ""
;          ""
;        ); command
;;;;; For PRE-DEFINED PATTERN [Non-Associative]:
        (command
          "_.hatch"
          "SOLID"
;          YourScale ;;;;; for other-than-SOLID patterns
;          YourAngleInCurrentUnits ;;;;; for other-than-SOLID patterns
          (if two pljoin "_last"); picks both if initial Polyline was closed
          ""
        ); command
;;;;; For PRE-DEFINED PATTERN [Associative]:
;        (command
;          "_.bhatch"
;          "_properties"
;          "YourPatternName"
;          YourScale ;;;;; for other-than-SOLID patterns
;          YourAngleInCurrentUnits ;;;;; for other-than-SOLID patterns
;          "_select"
;          (if two pljoin "_last"); picks both if initial Polyline was closed
;          ""
;          ""
;        ); command
        (command "_.ucs" "_prev"); reset UCS
        (setq ucschanged nil); eliminate reset in *error* function
      ); end progn
    ); end if
    (ssdel pl plset); remove from set, and...
  ); end while - ...go back and do next one if appropriate
  (setvar 'osmode osm)
  (command "_.undo" "_end")
  (setvar 'cmdecho cmde)
  (princ)
); end defun
(prompt "\nType PLWOH to change PLines with global Width to Outlines tracing their edges, and Hatch them.")
