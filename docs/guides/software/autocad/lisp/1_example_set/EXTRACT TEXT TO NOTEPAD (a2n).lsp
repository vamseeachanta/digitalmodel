;Tip1730:   ETEXT.LSP      Extract text             (c)2001, Prasad Chodankar


;;;;
;;;;********** PROGRAM FOR EXTRACTING TEXT FROM AutoCAD TO *.TXT FILE********

(defun C:a2n  (/ FILE FUNC ENTITY ENTTYP TEXTMT)
  (if (= FNAME NIL)
    (setq FNAME
           (getfiled
             "Select the text file in which you want to extract data"
             "c:/"
             "txt"
             1))
    (setq FUNC
           (getstring
             (strcat
               "[data to be stored in "
               FNAME
               "]"
               "/Change file/<press Enter or Right-click to proceed>:")))
    )
  (if (= FUNC "c")
    (setq FUNC "C"))
  (if (= FUNC "C")
    (setq FNAME
           (getfiled
             "Select the text file in which you want to extract data"
             "c:/"
             "txt"
             1)))
  (while (/= FNAME NIL)
    (setq ENTITY
           (car
             (entsel
               "\nPress `Esc` to exit, or select the text string to be exported.")))
    (if (/= ENTITY NIL)
      (progn
        (setq ENTTYP (cdr (assoc 0 (entget ENTITY))))
        (if (wcmatch ENTTYP "*TEXT*")
          (progn
            (setq TEXTMT (cdr (assoc 1 (entget ENTITY))))
            (setq FILE (open FNAME "a"))
            (write-line TEXTMT FILE)
            (close FILE)
            (prompt (strcat "\nLine- `" TEXTMT "` added to " FNAME))
            )
          (prompt "\nSelected entity is not a text string")
          )
        )
      (prompt "\nNo entity selected")
      )
    )
  )
;;;;end of code