; Next available MSG number is     8 
; MODULE_ID JULIAN_LSP_
;;;
;;;    JULIAN.LSP
;;;    
;;;    Copyright (C) 1988, 1989, 1990, 1991, 1992, 1993, 1994 by Autodesk, Inc.
;;;
;;;    Permission to use, copy, modify, and distribute this software
;;;    for any purpose and without fee is hereby granted, provided
;;;    that the above copyright notice appears in all copies and
;;;    that both that copyright notice and the limited warranty and
;;;    restricted rights notice below appear in all supporting
;;;    documentation.
;;;
;;;    AUTODESK PROVIDES THIS PROGRAM "AS IS" AND WITH ALL FAULTS.
;;;    AUTODESK SPECIFICALLY DISCLAIMS ANY IMPLIED WARRANTY OF
;;;    MERCHANTABILITY OR FITNESS FOR A PARTICULAR USE.  AUTODESK, INC.
;;;    DOES NOT WARRANT THAT THE OPERATION OF THE PROGRAM WILL BE
;;;    UNINTERRUPTED OR ERROR FREE.
;;;
;;;    Use, duplication, or disclosure by the U.S. Government is subject to
;;;    restrictions set forth in FAR 52.227-19 (Commercial Computer
;;;    Software - Restricted Rights) and DFAR 252.227-7013(c)(1)(ii) 
;;;    (Rights in Technical Data and Computer Software), as applicable.
;;;
;;;.
;;;
;;;----------------------------------------------------------------------------
;;;
;;; DESCRIPTION
;;;
;;;         AutoCAD Julian date / calendar date conversion routines
;;;
;;; CTOJ  --  Converts calendar date and time to Julian date
;;;
;;;     Call:     (ctoj <year> <month> <day> <hour> <minute> <second/fraction>)
;;;     Input:    Calendar date as argument list, for example:
;;;                     (ctoj 1957 10 4 19 26 24) ; Launch of Sputnik 1
;;;     Returns:  Julian date / fraction, as in DATE setvar
;;;
;;;
;;; DTOJ  --  Converts AutoCAD calendar date/time to Julian date
;;;
;;;     Call:     (dtoj <calendar date>)
;;;     Input:    Real number YYYYMMDD<.HHMMSSmsec>, like CDATE setvar.
;;;     Returns:  Julian date / fraction, as in DATE setvar
;;;
;;;
;;; JTOC  --  Converts Julian date to calendar date list
;;;
;;;     Call:     (jtoc <Julian date>)
;;;     Input:    Real number <Julian day>.<fraction>, like DATE setvar
;;;     Returns:  Calendar date/time list:
;;;                 (<year> <month> <day> <hour> <minute> <second/fraction>)
;;;
;;;
;;; JTOD  --  Converts Julian date to AutoCAD calendar date/time
;;;
;;;     Call:     (jtod <Julian date>)
;;;     Input:    Real number <Julian day>.<fraction>, like DATE setvar
;;;     Returns:  Calendar date/time, as in CDATE setvar
;;;
;;;
;;; JTOW  --  Determines day of the week for a given Julian day
;;;
;;;     Call:     (jtow <Julian date>)
;;;     Input:    Real number <Julian day>.<fraction>, like DATE setvar
;;;     Returns:  Integer day of the week, 0 = Sunday, 1 = Monday, ...
;;;               6 = Saturday
;;;
;;;
;;; C:DATE  --  Implements DATE command to display the current date/time
;;;             in the format: Day YYYY/M/D HH:MM:SS.msec
;;;
;;;     Call:     DATE  (at AutoCAD's Command: prompt)
;;;     Input:    None (obtains DATE setvar in Julian format)
;;;     Returns;  Nothing
;;;
;;;     Uses (JTOD) to convert to calendar format, and edits the date/time
;;;     from there.  Day of the week is calculated with (JTOW).
;;;
;;;
;;; Note  that  a  Julian  date  returned  by AutoCAD's DATE setvar or
;;; computed from the CDATE setvar is a true Julian date only  if  the
;;; system's clock is set to UTC/Zulu  (Greenwich Mean Time).   Julian
;;; dates  are easily compared for earlier/later, and durations can be
;;; computed via simple subtraction.  However,  such calculations  are
;;; accurate  only  for  readings obtained in the same time zone, with
;;; the same clock or synchronized clocks.
;;;
;;;
;;; DETAILS
;;;
;;; If  you're  interested  solely  in  converting  contemporary dates
;;; provided by AutoCAD between Julian and calendar  date  format  you
;;; can  ignore  the  following  discussion.  If you wish to use these
;;; functions for general work with Julian  dates  over  their  entire
;;; historical  span  of validity (any day beginning with the start of
;;; the year 4713 B.C. has a valid Julian day number), read on.
;;;
;;; 1.  B.C. versus negative year numbers
;;;
;;;     Historians refer to the first year of the Christian era as  "1
;;;     A.D." with the year that preceded it called "1 B.C.".  This is
;;;     consistent with usage at the time, since  zero  did  not  come
;;;     into  use  until much later, but creates a messy discontinuity
;;;     in the numbering of  years  which  complicates  any  algorithm
;;;     which attempts to calculate across that boundary.  Astronomers
;;;     have adopted the convention that the  year  which  preceded  1
;;;     A.D. is  denoted  "year 0", the year before that "year -1" and
;;;     so on.  Thus any year less than 1  can  be  converted  to  the
;;;     nomenclature  used  by  historians  by discarding the sign and
;;;     adding one to  get  the  B.C. year  number.   These  functions
;;;     follow  the  astronomical convention for years prior to 1 A.D.
;;;     and hence the year in  which  Julius  Caesar  established  the
;;;     Julian  calendar  in  the Roman Empire, 46 B.C. in the history
;;;     books, is specified as "-45" when using these functions.
;;;
;;; 2.  Julian versus Gregorian calendar
;;;
;;;     In  October  of  1582,  the  modern  Gregorian  calendar   was
;;;     proclaimed  by the Vatican, replacing the less-accurate Julian
;;;     calendar.  At the same time, 10 days were skipped  to  correct
;;;     the  inaccuracy  in  the  date  of the equinoxes and solstices
;;;     which had accumulated over the almost six centuries the Julian
;;;     calendar  had  been  used.   Thus  Thursday,  October  4, 1582
;;;     (Julian calendar) was followed by  Friday,  October  15,  1582
;;;     (Gregorian calendar).  These functions assume, therefore, that
;;;     dates on or before October 4, 1582 are in the Julian  calendar
;;;     and dates thereafter in the Gregorian.  If you're working with
;;;     dates from history, you must be extremely  careful  to  verify
;;;     which  calendar  they  are  specified in, as not all countries
;;;     adopted  the  Gregorian calendar immediately.  Britain and its
;;;     colonies, for example, remained on the Julian  calendar  until
;;;     Wednesday,  September 2, 1752, at which time 11 days had to be
;;;     dropped to align with  the  Gregorian  calendar  on  Thursday,
;;;     September  14,  1752.   Russia remained on the Julian calendar
;;;     until after the 1917 revolution, and Turkey did not adopt  the
;;;     Gregorian   calendar  until  1927.   The  later  the  date  of
;;;     adoption, naturally, the greater the number of days of  Julian
;;;     calendar error skipped.
;;;
;;; 3.  Round-off in "calendar date" format
;;;
;;;     AutoCAD's calendar date format as returned by (getvar "cdate")
;;;     is defined as a floating-point number interpreted as:
;;;
;;;         yyyymmdd.hhiissttt
;;;
;;;     where yyyy = year, mm = month number, dd = year number,  hh  =
;;;     hours,  ii = minutes, ss = seconds, and ttt = thousandths of a
;;;     second.  If you look carefully at  this  format,  you'll  note
;;;     that  the  full  specification  occupies 17 digits, while IEEE
;;;     floating-point numbers as used in AutoCAD have a precision  of
;;;     16 digits at best and less than that once you start to perform
;;;     calculations on them.  Thus,  extracting  millisecond  timings
;;;     from  calendar  dates is problematic, and using calendar dates
;;;     for any but the simplest calculations can lead to obscure  and
;;;     intermittent errors due to round-off.  The best approach is to
;;;     avoid   using   "calendar   dates"   entirely,   perform   all
;;;     calculations  with  Julian  dates, and  use  the JTOC and CTOJ
;;;     functions to convert calendar dates to and from Julian.  Since
;;;     these functions don't try to pack an entire calendar date into
;;;     one floating point number, they avoid the  round-off  problems
;;;     which plague AutoCAD "calendar dates".
;;; 
;;;
;;; REFERENCES
;;;
;;; The algorithms and test cases used herein may be found in Chapter 7
;;; (pages 59-66) of:
;;;
;;;     Meeus, Jean.  Astronomical Algorithms.  Richmond: Willman-Bell, 1991.
;;;
;;;----------------------------------------------------------------------------
;;;
;;; (CTOJ <year> <month> <day> <hour> <minute> <second/fraction>)
;;;
(defun ctoj (yr m d hh mm ss / y a b)
    (setq y yr)

    (if (<= m 2)
        (setq y (1- y)
              m (+ m 12)
        )
    )

    (if (or (< yr 1582)
            (and (= yr 1582) (or (< m 10) (and (= m 10) (< d 5)))))
        (setq b 0)                    ; Julian calendar
        (setq a (fix (/ y 100))       ; Gregorian calendar
              b (+ (- 2 a) (fix (/ a 4)))
        )
    )

    (+ (fix (+ (* 365.25 (+ y 4716)) (fix (* 30.6001 (+ m 1)))))
         d b -1524.0 (/ (+ (* (+ (* hh 60) mm) 60) ss) (* 24.0 60 60)))
)
;;;
;;;----------------------------------------------------------------------------
;;;
;;; (DTOJ <calendar date>)  --  convert calendar date/time to Julian
;;;
(defun dtoj (cdate / c f yr ys m d)
    (setq ys (if (< cdate 0) -1 1)    ; Sign on year
          c (fix (abs cdate))         ; Date in unsigned digits
          yr (* (/ c 10000) ys)       ; Get year
          m (rem (/ c 100) 100)       ; Get month
          d (rem c 100)               ; Get day
          f (rem (abs cdate) 1)       ; Fraction of day
    )
    (ctoj yr m d (fix (+ (* f 100) 0.1))
                  (rem (fix (+ (* f 10000) 0.1)) 100)
                  (+ (rem (fix (+ (* f 1000000) 0.1)) 100)
                     (/ (rem (fix (+ (* f 1000000000) 0.1)) 1000) 1000.0)))
)
;;;
;;;----------------------------------------------------------------------------
;;;
;;; (JTOC <Julian date>)  --  convert Julian date/time to calendar date list
;;;
(defun jtoc (td / time a b c d e alpha z m hh mm)
    (setq time (* 86400.0 (- td (setq z (fix td)))))
    (if (< z 2299161)
        (setq a z)                                         ; Julian calendar
        (setq alpha (fix (/ (- z 1867216.25) 36524.25))    ; Gregorian calendar
              a (- (+ z 1 alpha) (fix (/ alpha 4)))
        )
    )

    (setq b (+ a 1524)
          c (fix (/ (- b 122.1) 365.25))
          d (fix (* 365.25 c))
          e (fix (/ (- b d) 30.6001))
    )

    (setq m (fix (if (< e 14) (1- e) (- e 13))))

    ; Determine the clock time from the fraction of a day

    (setq hh (fix (/ time 3600.0))
          time (- time (* hh 3600.0))
          mm (fix (/ time 60.0))
    )

    ; Return calendar date as list

    (list (fix (- c (if (> m 2) 4716 4715))) m (fix (- b d (fix (* 30.6001 e))))
          hh mm 
          (- time (* mm 60))
    )
)
;;;
;;;
;;;----------------------------------------------------------------------------
;;;
;;; (JTOD <Julian date>)  --  convert Julian date/time to calendar
;;;
(defun jtod (td / j)
    (setq j (jtoc td))

    ; Return calendar date in form YYYYMMDD.HHMMSSmsec

    (* (+ (* (abs (car j)) 10000)     ; year
          (* (cadr j) 100)            ; month
          (caddr j)                   ; day
          (/ (cadddr j) 100.0)        ; hour
          (/ (nth 4 j) 10000.0)       ; minute
          (/ (nth 5 j) 1000000.0)     ; seconds, milliseconds
       )
       (if (< (car j) 0) -1 1)        ; apply sign to year
    )
)
;;;
;;;----------------------------------------------------------------------------
;;;
;;; (JTOW <Julian date>)  --  Convert a Julian date to day of week
;;;
(defun jtow (j)
    (fix (rem (1+ j) 7))
)
;;;
;;;----------------------------------------------------------------------------
;;;
;;;   (c:JTEST)  --  Internal test program for Julian date functions
;;;                  Displays several lines of numbers.  If none are
;;;                  flagged with "** Error **", all is okay.
;;;
;(defun c:JTEST (/ dl jl cjl cdl err eps)
;    (setq dl '(20000101.12    19870127.0   19870619.12  19880127.0
;               19880619.12    19000101.0   16000101.0   16001231.0
;                8370410.0712 -10000712.12 -10000229.0  -10010817.2136
;              -47120101.12   -47120101.0   19930309.12  15821004.0
;               15821015.0     19770426.0   19571004.0   19100420.0
;               19860209.0      3330127.0   -5840528.0)
;          jl '(2451545.5 2446823.0 2446966.5 2447188.0 2447332.5
;               2415021.0 2305448.0 2305813.0 2026872.3 1356001.5
;               1355867.0 1355671.9       0.5       0.0 2449056.5
;               2299160.0 2299161.0 2443260.0 2436116.0 2418782.0
;               2446471.0 1842713.0 1507900.0)
;          eps 0.00005
;    )
;
;    ; Test DTOJ
;
;    (setq cjl (mapcar 'dtoj dl)
;          err (mapcar '- cjl jl))
;    (mapcar '(lambda (x y z w) (princ (rtos x 2 4)) (princ " ")
;                               (princ (rtos y 2 12)) (princ " ")
;                               (princ (rtos z 2 12)) (princ " ")
;                               (princ w)
;                               (if (> (abs w) eps) (princ " ** Error **"))
;                               (terpri))
;        dl jl cjl err)
;    (terpri)
;
;    ; Test JTOD
;
;    (setq cdl (mapcar 'jtod jl)
;          err (mapcar '- cdl dl))
;    (mapcar '(lambda (x y z w) (princ (rtos x 2 4)) (princ " ")
;                               (princ (rtos y 2 12)) (princ " ")
;                               (princ (rtos z 2 12)) (princ " ")
;                               (princ w)
;                               (if (> (abs w) eps) (princ " ** Error **"))
;                               (terpri))
;        jl dl cdl err)
;
;    ; Test JTOW
;
;    (if (or (/= (jtow (dtoj 19540630)) 3)
;            (/= (jtow (dtoj 15821004)) 4)
;            (/= (jtow (dtoj 15821015)) 5)
;        )
;        (princ "\n** Error in jtow.\n")
;    )
;
;    (princ)
;)
;;;
;;;----------------------------------------------------------------------------
;;;
;;; (C:DATE)  --  Implement DATE command to display date/time
;;;
(defun c:date (/ j c cdate m d y hh mm ss msec)
   (setq cdate (jtod (setq j (getvar "date")))
         c (fix cdate)
         y (/ c 10000)              ; Get year
         m (rem (/ c 100) 100)      ; Ger month
         d (rem c 100)              ; Get day
         c (- cdate (fix cdate))    ; Strip date from date/time
         c (fix (* c 1000000000))   ; Scale time to get HHMMSSmmm integer
         hh (/ c 10000000)          ; Get hours
         mm (rem (/ c 100000) 100)  ; Get minutes
         ss (rem (/ c 1000) 100)    ; Get seconds
         msec (rem c 1000)          ; Get milliseconds
   )

   ; Print the day of the week

   (princ (nth (jtow j) '("Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat")))
   (princ " ")

   ; Print the date.  YYYY/M/D

   (princ (strcat (itoa y) "/" (itoa m) "/" (itoa d)))

   ; Print the time.  HH:MM:SS.msec

   (princ (strcat " " (if (> hh 9) "" "0") (itoa hh)))
   (princ (strcat ":" (if (> mm 9) "" "0") (itoa mm)))
   (princ (strcat ":" (if (> ss 9) "" "0") (itoa ss)))
   (princ (cond ((> msec 99) "."  )
                ((> msec 9)  ".0" )
                (T           ".00")
          )
   )
   (princ msec)
   (terpri)
   (princ)
)       
