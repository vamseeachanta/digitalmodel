;
;      AutoLISP utility function  : written by Paul Petersen
;                                   A B Consulting Co., Inc.
;                                   3939 N 48th
;                                   Lincoln NE, 68504
;
;                     copyright July 1986
;
;    This ACAD command creates the basis for a mathematical function graph.
;  It uses a AutoLISP function evaluated at a number of points in the
;  defined interval as the data points to plot the function.  After the
;  graph is drawn it is scaled to the screen and smoothed.
;
;  Example
;
;  GRAPH Function (QUIT to exit): SIN
;  Minimum value: 0.0
;  Maximum value: 10.0
;  Intervals: 20
;
;  (DEFUN DOUBLE (X)
;         (* X X))
;
;  GRAPH Function (QUIT to exit): DOUBLE
;  Minimum value: 0.0
;  Maximum value: 10.0
;  Intervals: 20
;

(DEFUN C:GRAPH (/ FNC XCOR XMAX STEPS DELTA YCOR CMDSAVE)
        (WHILE (NOT (OR (EQUAL FNC (QUOTE QUIT))
                                (EQUAL (QUOTE SUBR) (TYPE (EVAL FNC)))
                                (AND (BOUNDP FNC)
                                        (EQUAL 1 (LENGTH (CAR (EVAL FNC)))))))
                (SETQ FNC (READ (GETSTRING "Function (QUIT to exit): "))))
        (COND ((NOT (EQUAL FNC (QUOTE QUIT)))
                (SETQ XCOR (GETREAL "Minimum value: "))
                (SETQ XMAX (GETREAL "Maximum value: "))
                (SETQ STEPS (GETINT "Intervals: "))
                (SETQ CMDSAVE (GETVAR "CMDECHO"))
                (SETVAR "CMDECHO" 0)
                (SETQ DELTA (/ (- XMAX XCOR) (FLOAT STEPS)))
                (SETQ YCOR (APPLY FNC (LIST XCOR)))
                (COMMAND "PLINE" (LIST XCOR YCOR))
                (REPEAT STEPS
                        (SETQ XCOR (+ XCOR DELTA))
                        (SETQ YCOR (APPLY FNC (LIST XCOR)))
                        (COMMAND (LIST XCOR YCOR)))
                (COMMAND "" "ZOOM" "E")
                (COMMAND "PEDIT" (LIST XCOR YCOR) "F" "X")
                (SETVAR "CMDECHO" CMDSAVE)))
                nil)


