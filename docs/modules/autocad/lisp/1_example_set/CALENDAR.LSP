;CADENCE
;modified for international versions of AutoCAD - (^v^) CAD Studio sro
;www.cadstudio.cz

(defun CALENDAR (/ mn dy cd cel ar crx cry qu
                     d c y m ox oy xcl ycl am nd cc loc m0 y0)
;  Initial settings and definition of constants.
   (setvar "CMDECHO" 0)
   (setvar "BLIPMODE" 0)
   (setvar "OSMODE" 0)
   (setq mn '("JANUARY" "FEBRUARY" "MARCH" "APRIL" "MAY" "JUNE" "JULY"
              "AUGUST" "SEPTEMBER" "OCTOBER" "NOVEMBER" "DECEMBER")
         dy '(31 28 31 30 31 30 31 31 30 31 30 31)
         cd '(1 4 4 0 2 5 0 3 6 1 4 6))
   (setq cel 1.5                    ; Cell size
         ar  0.8333                 ; Aspect Ratio [to fit into A-size sheet]
         crx 0.0                    ; lower-left hand Corner Reference
         cry 0.0
         qu  1)                     ; QUadrant [0=centered, 1=lower right]
   (command "_.Style" "TXA" "romant" "" "0.75" "" "" "" "")  ; for characters
   (command "_.Style" "TXB" "romans" "" "0.75" "" "" "" "")  ; for numbers
   (setq d (rtos (getvar "CDATE") 2 0)                ; get system date,
         c (atoi (substr d 1 2))                      ; century,
         y (atoi (substr d 3 2))                      ; year and
         m (atoi (substr d 5 2))                      ; month
         xcl cel                                      ; Cell size - X
         ycl (* ar xcl)                               ; Cell size - Y
         am (strcat (nth (1- m) mn) "  " (substr d 1 4))  ; set month string,
         nd (nth (1- m) dy)                               ; number of days &
         cc (+ (nth (1- m) cd) (- 19 c)))                 ; month code.
   (princ "   Generating calendar for ") (princ am)
   (princ ", please wait . .")
;  CALDRAW returns the day-of-the-week and location of the last date.
   (setq loc (caldraw qu 0 crx cry xcl ycl m y am nd cc))
;  To generate mini calendars for the previous and next months.
   (if (or (zerop (car loc)) (> (car loc) 2))
      (if (< (cadr loc) (+ crx (* xcl 5.0)))
         (setq ox (+ crx (* xcl 5.0))
               oy (+ cry (* ycl 0.05)))
         (setq ox crx
               oy (+ cry (* ycl 4.05))))
      (if (< (caddr loc) (+ cry ycl))
         (setq ox (+ crx (* xcl 5.0))
               oy (+ cry (* ycl 0.05)))
         (setq ox (+ crx (* xcl (+ (car loc) 1)))
               oy (+ cry (* ycl 4.05)))))
   (setq xcl (/ xcl 7.0)                     ; Cell size - X
         ycl (* ar xcl))                     ; Cell size - Y
;  For the previous month:
   (if (= m 1)                               ; wrap around if January
      (setq m0 12 y0 (1- y))
      (setq m0 (1- m) y0 y))
   (setq am (nth (1- m0) mn)                 ; set month string,
         nd (nth (1- m0) dy)                 ; number of days and
         cc (+ (- 19 c) (nth (1- m0) cd)))   ; month code.
   (if (minusp y0) (setq y0 99 cc (1+ cc)))  ; change of century
   (CALDRAW 0 1 ox oy xcl ycl m0 y0 am nd cc)
;  For the next month:
   (setq ox (+ ox (* xcl 7.0)))
   (if (= m 12)                              ; wrap around if December
      (setq m0 1 y0 (1+ y))
      (setq m0 (1+ m) y0 y))
   (setq am (nth (1- m0) mn)                 ; set month string,
         nd (nth (1- m0) dy)                 ; number of days and
         cc (+ (nth (1- m0) cd) (- 19 c)))   ; month code.
   (if (> y0 99) (setq y0 0 cc (1- cc)))     ; change of century
   (CALDRAW 0 1 ox oy xcl ycl m0 y0 am nd cc)
   (command "_.Zoom" "_E")
   (princ " . complete.\n  Save the drawing and use PRPLOT or PLOT.\n")
   (princ "\nCALENDAR -- from A's Computing Expertise - (609) 772-1309\n")
   (princ)
)
;  This function actually generates the calendar.
(defun CALDRAW (qflg dflg xo yo xcl ycl mn yr am nd cc
                / WK dw cfx cfy ta re te x y ht i)
   (setq WK '("SUN" "MON" "TUE" "WED" "THU" "FRI" "SAT"))
   (if (and (= mn 2) (zerop (rem yr 4))) (setq nd 29))  ; leap year corrections
   (if (and (zerop (rem yr 4)) (or (= mn 1) (= mn 2)))
         (setq cc (1- cc)))
;  Compute the day of the week for the 1st; 1=Sun, 0,7=Sat.
   (setq dw (rem (+ yr (/ yr 4) 1 cc) 7))
   (if (zerop dw) (setq dw 7))
   (if (zerop qflg)
       (setq cfx 0.0 cfy 0.0 ta "_M")
       (setq cfx (* 0.45 xcl) cfy (* 0.45 ycl) ta "R"))
   (setq re (+ xo (* 7.0 xcl))               ; define right edge and
         te (+ yo (* 5.5 ycl))               ; top edge of frames
         x (+ xo (* 3.5 xcl)))               ; To write month,
   (command "_.Text" "_S" "TXA") (command)      ; reset Text .Style.
   (command "_.Text" "_C" (list x (* te 1.01)) (* xcl 0.3333) "0" am)
;  Draw the calender frames.
   (if (zerop dflg)(progn                    ; only for the main calendar
         (setq x xo y yo)
         (repeat 8                           ; draw verticals
            (command "_.Line" (list x yo) (list x te) "")
            (setq x (+ x xcl)))
         (repeat 6                           ; draw horizontals
            (command "_.Line" (list xo y) (list re y) "")
            (setq y (+ y ycl)))
         (command "_.Line" (list xo te) (list re te) "")   ; draw top edge
         (setq x (+ xo (* 0.5 xcl))          ; set values for writing
               y (- te (* 0.25 ycl))         ; the days of the week
               ht (* ycl 0.25)
               i 0)
         (repeat 7                           ; write days
            (command "_.Text" "_M" (list x y) ht "0" (nth i WK))
            (setq x (+ x xcl)
                  i (1+ i))))); IF ZEROP DFLG
   (command "_.Text" "_S" "TXB") (command)      ; set Text .Style and
   (setq x (+ xo (* (- dw 1.5) xcl) cfx)     ; starting point - X
         y (- (+ yo (* 4.5 ycl)) cfy)        ; starting point - Y
         ht (* ycl 0.5)                      ; text height and
         i 0)                                ; date
   (repeat nd                                ; To write the dates
      (setq x (+ x xcl)
            i (1+ i))
      (if (> x re)                           ; To go to next row
         (setq x (+ xo (* 0.5 xcl) cfx)
               y (- y ycl)))
      (if (< y yo)                           ; To go to top row
         (setq y (- (+ yo (* 4.5 ycl)) cfy)))
      (command "_.Text" ta (list x y) ht "0" (itoa i)))
;  Return the day-of-the-week and the location of the last date.
   (setq dw (rem (+ (1- dw) nd) 7))
   (list dw x y))
;  Execute the program automatically, upon loading.
(CALENDAR)
