;;; 12345678901234567890123456789012345678901234567890
;;; VERTEXT.LSP  A program to extract the xyz
;;; coordinates from any polyline and export them
;;; to an ascii file.

(defun ERR (S)
   (if (= S "Function cancelled")
      (princ "\nVERTEXT - cancelled: ")
      (progn (princ "\nVERTEXT - Error: ") (princ S) (terpri)) ;_ progn
   ) ; if
   (RESETTING)
   (princ "SYSTEM VARIABLES have been reset\n")
   (princ)
) ; err
(defun SETV (SYSTVAR NEWVAL)
   (setq X (read (strcat SYSTVAR "1")))
   (set X (getvar SYSTVAR))
   (setvar SYSTVAR NEWVAL)
) ; setv 
(defun SETTING ()
   (setq OERR *ERROR*)
   (setq *ERROR* ERR)
   (SETV "CMDECHO" 0)
   (SETV "BLIPMODE" 0)
) ; end of setting 
(defun RSETV (SYSTVAR) (setq X (read (strcat SYSTVAR "1"))) (setvar SYSTVAR (eval X)))
 ; restv
(defun RESETTING () (RSETV "CMDECHO") (RSETV "BLIPMODE") (setq *ERROR* OERR))
 ; end of resetting 

(defun DXF (CODE ENAME) (cdr (assoc CODE (entget ENAME)))) ; dxf

(defun VERTEXT (/ EN VLIST)
   (setq EN (GET-EN))
   (if (= (DXF 0 EN) "LWPOLYLINE")
      (setq VLIST (GET-LWVLIST EN))
      (setq VLIST (GET-PLVLIST EN))
   ) ; if
   (WRITE-IT VLIST EN)
) ;_ vertext

(defun GET-EN (/ NO-ENT EN MSG1 MSG2)
   (setq NO-ENT 1
         EN     NIL
         MSG1   "\nSelect a polyline: "
         MSG2   "\nNo polyline selected, try again."
   ) ; setq
   (while NO-ENT
      (setq EN (car (entsel MSG1)))
      (if (and EN
               (or (= (DXF 0 EN) "LWPOLYLINE") (= (DXF 0 EN) "POLYLINE")) ; or
          ) ; and
         (progn (setq NO-ENT NIL)) ; progn
         (prompt MSG2)
      ) ; if
   ) ; while
   EN
) ; get-en

(defun GET-LWVLIST (EN / ELIST NUM-VERT VLIST)
   (setq ELIST    (entget EN)
         NUM-VERT (cdr (assoc 90 ELIST))
         ELIST    (member (assoc 10 ELIST) ELIST)
         VLIST    NIL
   ) ; setq
   (repeat NUM-VERT
      (setq VLIST (append VLIST (list (cdr (assoc 10 ELIST)))) ; append
      ) ; setq
      (setq ELIST (cdr ELIST)
            ELIST (member (assoc 10 ELIST) ELIST)
      ) ; setq
   ) ; repeat
   VLIST
) ; get-lwvlist

(defun GET-PLVLIST (EN / VLIST)
   (setq VLIST NIL
         EN    (entnext EN)
   ) ; setq
   (while (/= "SEQEND" (DXF 0 EN))
      (setq VLIST (append VLIST (list (DXF 10 EN))))
      (setq EN (entnext EN))
   ) ; while
   VLIST
) ; get-plvlist

(defun WRITE-IT (VLST EN / NEWVLIST MSG3 FNAME)
   (setq NEWVLIST (mapcar '(lambda (X) (trans X EN 0)) ;_ lambda
                          VLST
                  ) ;_ mapcar
         MSG3     "Polyline vertex file"
        ;FNAME    (getfiled MSG3 "" "txt" 1)
         F1       (open "FNAME" "w")
   ) ; setq
   (WRITE-HEADER)
   (WRITE-VERTICES NEWVLIST)
   (setq F1 (close F1))
) ;_ write-it

(defun WRITE-HEADER (/ STR)
  (setq STR "        POLYLINE VERTEX POINTS")
   (write-line STR F1)
  (setq STR (strcat "  X            " "  Y            " "  Z") ;_ strcat
   ) ;_ setq
   (write-line STR F1)
) ;_ write-header


(defun WRITE-VERTICES (NEWVLIST / XSTR YSTR ZSTR STR)
(setq httt "0.2") 
(setq gptx (getpoint "\nBasepoint for X axis: "))
(setq gpty (getpoint "\nBasepoint for Y axis: "))

   (foreach ITEM NEWVLIST
      (setq XSTR (rtos (nth 0 ITEM) 2 3)
            YSTR (rtos (nth 1 ITEM) 2 3)
            ZSTR (rtos (nth 2 ITEM) 2 3)
            STR  (strcat XSTR (SPACES XSTR) YSTR (SPACES YSTR) ZSTR) ;_ strcat
      ) ; setq
;      (write-line STR F1)
 


(command "text" (list (+(atof xstr)(/ (atof httt) 2.0)) (cadr gptx)) httt "90" (strcat xstr))
(command "text" (list (+(atof xstr)(/ (atof httt) 2.0)) (cadr gpty)) httt "90" (strcat ystr))

   ) ; foreach

) ; write-vertices


(defun SPACES (STR / FIELD NUM CHAR SPACE)
   (setq FIELD 15
         NUM   (- FIELD (strlen STR))
         CHAR  " "
         SPACE ""
   ) ;_ setq
   (repeat NUM (setq SPACE (strcat SPACE CHAR))) ;_ repeat
) ;_ spaces

(defun C:prl () (SETTING) (VERTEXT) (RESETTING) (princ)) ; c:nsl

(prompt "\nwritten by Tariq and company")
(prompt "\nEnter PRL to start")

