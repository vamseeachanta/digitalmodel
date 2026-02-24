(defun c:zpt ( / i p s )
    (if (setq s (ssget '((0 . "POINT"))))
        (repeat (setq i (sslength s))
            (setq p (assoc 10 (entget (ssname s (setq i (1- i))))))
            (entmake
                (list
                   '(000 . "MTEXT")
                   '(100 . "AcDbEntity")
                   '(100 . "AcDbMText")
                    (cons 1 (strcat "" (rtos (cadddr p))))
                    p
                 )
             )
        )
    )
    (princ)
)