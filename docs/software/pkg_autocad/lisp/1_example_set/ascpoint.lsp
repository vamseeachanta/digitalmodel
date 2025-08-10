
;; ASCPOINT.LSP Copyright 1990-97 Tony Tanzillo All Rights Reserved.

;;-----------------------------------------------------------------

;;    Author: Tony Tanzillo,

;;            Design Automation Consulting

;;            http://ourworld.compuserve.com/homepages/tonyt

;;    Permission to use, copy, modify, and distribute this software

;;    for any purpose and without fee is hereby granted, provided

;;    that the above copyright notice appears in all copies and

;;    that both that copyright notice and the limited warranty and

;;    restricted rights notice below appear in all copies and all

;;    supporting documentation, and that there is no charge or fee

;;    charged in return for distribution or duplication.

;;

;;    This SOFTWARE and documentation are provided with RESTRICTED

;;    RIGHTS.

;;

;;    Use, duplication, or disclosure by the Government is subject

;;    to restrictions as set forth in subparagraph (c)(1)(ii) of

;;    the Rights in Technical Data and Computer Software clause at

;;    DFARS 252.227-7013 or subparagraphs (c)(1) and (2) of the

;;    Commercial Computer Software Restricted Rights at 48 CFR

;;    52.227-19, as applicable. The manufacturer of this SOFTWARE

;;    is Tony Tanzillo, Design Automation Consulting.

;;

;;    NO WARRANTY

;;

;;    ANY USE OF THIS SOFTWARE IS AT YOUR OWN RISK. THE SOFTWARE

;;    IS PROVIDED FOR USE "AS IS" AND WITHOUT WARRANTY OF ANY KIND.

;;    TO THE MAXIMUM EXTENT PERMITTED BY APPLICABLE LAW, THE AUTHOR

;;    DISCLAIMS ALL WARRANTIES, EXPRESS OR IMPLIED, INCLUDING, BUT

;;    NOT LIMITED TO, IMPLIED WARRANTIES OF MERCHANTABILITY AND

;;    FITNESS FOR A PARTICULAR PURPOSE, WITH REGARD TO THE SOFTWARE.

;;

;;    NO LIABILITY FOR CONSEQUENTIAL DAMAGES. TO THE MAXIMUM

;;    EXTENT PERMITTED BY APPLICABLE LAW, IN NO EVENT SHALL

;;    THE AUTHOR OR ITS SUPPLIERS BE LIABLE FOR ANY SPECIAL,

;;    INCIDENTAL, INDIRECT, OR CONSEQUENTIAL DAMAGES WHATSOEVER

;;    (INCLUDING, WITHOUT LIMITATION, DAMAGES FOR LOSS OF

;;    BUSINESS PROFITS, BUSINESS INTERRUPTION, LOSS OF BUSINESS

;;    INFORMATION, OR ANY OTHER PECUNIARY LOSS) ARISING OUT OF

;;    THE USE OF OR INABILITY TO USE THE SOFTWARE PRODUCT, EVEN

;;    IF THE AUTHOR HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH

;;    DAMAGES.  BECAUSE SOME JURISDICTIONS DO NOT ALLOW EXCLUSION

;;    OR LIMITATION OF LIABILITY FOR CONSEQUENTIAL OR INCIDENTAL

;;    DAMAGES, THE ABOVE LIMITATION MAY NOT APPLY TO YOU.



;

; ASCPOINT.LSP is a utility for use with AutoCAD Release 10 or 11,

; which reads coordinate data from ASCII files in CDF or SDF format,

; and generates AutoCAD geometry using the incoming coordinate data.

;

; The ASCPOINT command will read coordinate data from an ASCII file,

; and generate either a continuous string of LINES, a POLYLINE, a

; 3DPOLYline, multiple copies of a selected group of objects, or

; AutoCAD POINT entities.

;

;  Format:

;

;    Command: ASCPOINT

;    File to read: MYFILE.TXT                           <- ASCII input file

;    Comma/Space delimited <Comma>: Comma               <- data format

;    Generate Copies/Lines/Nodes/3Dpoly/<Pline>: Nodes  <- entity to create

;    Reading coordinate data...

;

; If you selected "Copies", then ASCPOINT will prompt you to select the

; objects that are to be copied.  The basepoint for all copies is the

; current UCS origin (0,0,0).  One copy of the selected objects will be

; created for each incoming coordinate, and placed at each coordinate.

;

; A comma-delimited (CDF) ascii file contains one coordinate per line,

; with each component seperated by a comma, like this:

;

;    2.333,4.23,8.0

;    -4.33,0.0,6.3

;    0.322,5.32,0.0

;    etc....

;

; There should be no spaces or blank lines in a CDF coordinate data file.

;

; A space-delimited (SDF) ascii file contains one coordinate per line,

; with each component seperated by one or more spaces, like this:

;

;    2.333  4.23   8.0

;   -4.33   0.0    6.3

;    0.322  5.32   0.0

;    ...

;

; Coordinate data can be 2D or 3D.

;

; Note that all numeric values must have at least one digit to the left

; and the right of the decimal point (values less than one must have a

; leading 0), and a leading minus sign indicates negative values.  This

; applys to both CDF and SDF formats.

;

; ASCPOINT can generate a continuous chain of LINE entities from your

; coordinate data, where each pair of adjacent lines share a coordinate

; from the file.

;

; ASCPOINT can also generate a polyline or 3DPOLYline from the coordinate

; data, where each point in the file becomes a vertice of the polyline.

; If the input file contains 3D coordinates, and you specify a polyline,

; then the Z component is ignored and the default of 0.0 is used.

;

; ASCPOINT will also COPY a selected group of objects, creating one copy

; for each incoming coordinate, and using the coordinate as the absolute

; copy displacement from the CURRENT UCS origin (0,0,0).

;

; Finally, ASCPOINT will generate AutoCAD POINT entities from the data in

; the file.  Specify the point size and type prior to invoking ASCPOINT.

;

; Good luck,

;

; Tony Tanzillo



   (defun C:ASCPOINT ( / f bm hi format input read-point line plist ss)

      (cond (  (eq "" (setq f (getstring "\nFile to read: "))))

            (  (not (setq f (open f "r")))

               (princ "\nCan't open file for input."))

            (t (initget "Space Comma")

               (setq format

                 (cond ((getkword "\nComma/Space delimited <Comma>: "))

                       (t "Comma")))

               (initget "Copies Lines Nodes 3dpoly Pline")

               (setq input

                 (cdr (assoc

                   (cond

                      (  (getkword

                            "\nGenerate   Copies/Lines/Nodes/3Dpoly/<Pline>: "))

                      (t "Pline"))

                     '(("Lines" . ".line")

                       ("Copies"  .  ".copy")

                       ("Nodes" . ".point")

                       ("3Dpoly" . ".3dpoly")

                       ("Pline" . ".pline")))))

               (setq read-point (cond (  (eq format "Comma") cdf)

                                      (t sdf)))

               (setvar "cmdecho" 0)

               (command "_.undo" "g")

               (setq bm (getvar "blipmode"))

               (setq hi (getvar "highlight"))

               (setvar "blipmode" 0)

               (princ "\nReading coordinate data...")

               (while (setq line (read-line f))

                      (cond (  (setq line (strtrim line))

                               (setq line (read-point line))

                               (setq plist (append plist

                                  (cond (  (eq input ".pline")

                                           (list (noz line)))

                                        (t (list line))))))))

               (close f)

               (cond (  (eq input ".point")

                        (setvar "highlight" 0)

                        (command "_.point" "0,0,0"

                                 "_.copy" (setq ss (entlast)) "" "_m" "0,0,0")

                        (apply 'command plist)

                        (command)

                        (entdel ss))

                     (  (eq input ".copy")

                        (princ "\nSelect objects to copy,")

                        (while (not (setq ss (ssget)))

                               (princ "\nNo objects selected,")

                               (princ " select objects to copy,"))

                        (setvar "highlight" 0)

                        (command "_.copy" ss "" "_m" "0,0,0")

                        (apply 'command plist)

                        (command))

                     (t (command input)

                        (apply 'command plist)

                        (command)))

               (command "_.undo" "e")

               (setvar "highlight" hi)

               (setvar "blipmode" bm)))

      (princ)

   )



   (defun cdf (l)

     (command "_.setvar" "_lastpoint" l)

     (getvar "lastpoint")

   )



   (defun sdf (l)

      (read (strcat "(" l ")"))

   )



   (defun noz (p)

      (list (car p) (cadr p))

   )



   (defun strtrim (s)

     (while (eq " " (substr s 1 1))

            (setq s (substr s 2)))

     (while (eq " " (substr s (strlen s)))

            (setq s (substr s 1 (1- (strlen s)))))

     (cond (  (eq s "") nil)

           (t s))

   )



(princ "\nC:ASCPOINT loaded.  Start command with ASCPOINT. ")

(princ)

</PRE>

