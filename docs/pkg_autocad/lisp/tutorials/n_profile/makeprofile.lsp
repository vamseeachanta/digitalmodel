(defun read-xy-coordinates-from-csv (filename / f line coords-list xy x y)
  (setq coords-list '())
  (setq f (open filename "r"))
  (if f
    (progn
      (while (setq line (read-line f nil))
        (setq xy (vl-string-split "," line))
        (if (= (length xy) 2)
          (progn
            (setq x (atof (car xy)))
            (setq y (atof (cadr xy)))
            (setq coords-list (append coords-list (list (list x y))))
          )
        )
      )
      (close f)
    )
    (prompt (strcat "\nUnable to open file: " filename))
  )
  coords-list
)

(defun c:MAKETERRAINPROFILE (/ coords filename)
  ;; Specify the full path to your CSV file here:
  (setq filename "P:\\61863 Talos Lakach FEED\\999 Work Space\\500 Subsea Facilities\\Alignments\\18inch Flowline\\18inch Main FL Terrain Profile.csv")

  (setq coords (read-xy-coordinates-from-csv filename))
  (if (and coords (> (length coords) 1))
    (progn
      (command "_.PLINE")
      (foreach pt coords
        ;; Convert the numeric values to strings and feed them to the PLINE command
        (command (strcat (rtos (car pt) 2 6) "," (rtos (cadr pt) 2 6)))
      )
      (command "") ;; End the PLINE command
      (prompt "\n2D Polyline created successfully.")
    )
    (prompt "\nNo valid coordinates found or only one point in the file.")
  )
  (princ)
)
