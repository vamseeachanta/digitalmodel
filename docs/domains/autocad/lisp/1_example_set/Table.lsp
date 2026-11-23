;; TABLE.LSP Copyright 1994-2000 Tony Tanzillo
;;
;; Functions to produce tabular text output to
;; display or a file.

;; The (table-format) function accepts a list
;; whose elements are uniform length lists of
;; two or more strings, along with a list of
;; integers that indicate column aligments, and
;; an integer indicating the margin between each
;; column, and returns a list of strings containing
;; the tabular formatted lines.

; (table-format <table> <alignments> <margin>)
;
;  <alignments> is a list of integers whose
;  values can range from -1 to 1, which define
;  the justification of the corrsponding column
;  in the table (Left = 1, Center = 0, Right = -1).

(defun table-format (table alignments margin / tabs)
   (setq tabs (mapcar '* (column-width table) alignments))
   (mapcar
     '(lambda (row)
         (row-format tabs row margin)
      )
      table
   )
)

;; LTABLE command (example)

;; This sample application demonstrates how 
;; to use (table-format) to format and write 
;; tabular output to the text console.

(defun C:LTABLE ( / layer layers line lines)

   ;; Build a table from layer settings,
   ;; excluding XREF layers:

   (while (setq layer (tblnext "layer" (not layer)))
      (if (not (wcmatch (cdr (assoc 2 layer)) "*|*"))
         (setq layers
            (cons
               (list
                  (cdr (assoc 2 layer))
                  (itoa (abs (cdr (assoc 62 layer))))
                  (cdr (assoc 6 layer))
                  (if (> (cdr (assoc 62 layer)) -1)
                     "Yes" "No"
                  )
                  (if (eq 1 (logand 1 (cdr (assoc 70 layer))))
                    "No" "Yes" 
                  )
               )
               layers
            )
         )
      )
   )
   
   ;; Add column headings, so they are compensated
   ;; for in the columnar formatting:

   (setq layers  
      (cons 
         '("Name" "Color" "Linetype" "On" "Thawed" )
          (reverse layers)
      )
   )
   
   ;; Format for tabular output. The second argument
   ;; (a list of integers) indicates the alignment of
   ;; each column (-1 = left, 0 = center, 1 = right).
   ;; The third argument is the gutter width between
   ;; each column:
   
   (setq lines (table-format layers '(1 -1 1 1 -1) 2))      
   
   (textpage)
   (terpri)
   
   ;; Output the column headings:

   (write-line (car lines))
   
   ;; Output a string of dash chars ("-"), equal in 
   ;; length to the total width of the table text:

   (write-line (char-replicate "-" (strlen (car lines))))
   
   ;; Output the table rows:
   
   (foreach line (cdr lines)
      (write-line line)
   )
   (princ)
)
      
;; *space255* is a global that's Used by the (space) 
;; function to effeciently generate a strings of
;; a specified number of spaces:


(defun char-replicate (char len / r)
   (setq r "")
   (repeat len (setq r (strcat r char)))
)

(setq *space255* (char-replicate " " 255))

(defun space (n)
  (substr *space255* 1 n)
)

(setq *overflow-picture* (char-replicate "*" 255))

(defun overflow (len) 
   (substr *overflow-picture* 1 len)
)

; Left/right-justify string

(defun justify (s width / l len)
   (setq len (abs width))
   (cond
      (  (> (setq l (strlen s)) len)
         (overflow len))
      (  (minusp width)
         (strcat (substr *space255* 1 (- len l)) s))
      (t (strcat s (substr *space255* 1 (- len l))))
   )
)


(defun column-width (table)
   (cond
      (  (not (caar table)) nil)
      (t (cons (apply 'max (mapcar 'strlen (mapcar 'car table)))
               (column-width (mapcar 'cdr table)))))
)

(defun row-format (tabs data margin / gutter s)
   (setq gutter (space (cond (margin) (t 0))))
   (substr (setq s (row-format-aux tabs data)) 1 (- (strlen s) margin))
)

(defun row-format-aux (tabs data)
   (cond
      (  (not data) "")
      (t (strcat (justify (car data) (car tabs))
                 gutter
                 (row-format-aux (cdr tabs) (cdr data)))))
)

(princ "\nTABLE.LSP loaded, use LTABLE command")
(princ "\nto see example output of (table-format)")
(princ)
