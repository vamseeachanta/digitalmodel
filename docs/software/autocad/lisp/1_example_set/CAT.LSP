(defun c:cat ()
   (setq osm (getvar "osmode"))
   (setq thop (ssget))
   (setvar "osmode" 512)
   (setq p1 (getpoint "\nCho diem cat thu nhat cua duong thang cat"))
   (setq p2 (getpoint p1 "\nCho diem cat thu hai cua duong thang cat"))
   (setvar "osmode" 0)
   (setq i 0)
   (repeat (sslength thop)
     (setq dt (ssname thop i)) 
     (setq ds (entget dt))
     (setq loaidt (cdr (assoc 0 ds)))
     (cond
       ((= loaidt "LINE")
        (progn
        (setq p3 (cdr (assoc 10 ds)))
        (setq p4 (cdr (assoc 11 ds)))
        (setq p (inters p1 p2 p3 p4))
        (if (null p) (princ)
          (progn
          (command ".break" (list dt p3) "f" p p)
          );;; end progn
        );;; end if
       ))

       ((= loaidt "POLYLINE")
        (progn
        ;;; chu y voi polyline ma 10 o day la (10 0.0 0.0 0.0)
        (setq cat (cdr (assoc 70 ds)))
        (setq Elist (list '("start")))

        (setq vlap "C" dt1 dt)
        (while (= vlap "C")
          (setq dt1 (entnext dt1))
          (setq ds (entget dt1))
          (setq loaidt (cdr (assoc 0 ds)))
          (if (= loaidt "VERTEX")
           (setq Elist (cons (cdr (assoc 10 ds)) Elist))

           (setq vlap "K")
          );; end if 
        );; end while
        (setq Elist (cdr (reverse Elist)))
        (setq pluu (car elist))

        ;; ***
        (setq p nil)
        (while (and (null p) (> (length Elist) 1) )
          (setq p3 (car Elist))
          (setq Elist (cdr Elist))
          (setq p4 (car Elist))
          (setq p (inters p1 p2 p3 p4))
        );;; end while

        (if (null p) (princ)
          (if (or (< (distance p3 p) 0.1) (< (distance p4 p) 0.1) )
              (setq p nil)
          );;; end if
         );;; end if

        (if (null p) (princ "Khong cat duoc!")
          (progn
            (if (= cat 0) (setq pc p)
              (setq pc (polar p (angle p3 p4) 0.01))
            );;; end if
            (command ".break" (list dt pluu) "f" p pc)
          );;; end progn
        );;; end if
       ))


       ((= loaidt "LWPOLYLINE")
        (progn
        (setq i2 0 Elist '(Start))
        (setq pluu (assoc 10 ds))
        (setq cat (cdr (assoc 70 ds)))
        (while (nth i2 ds)
          (setq p (nth i2 ds))
          (if (null p)(princ)
             (if (/= (car p) 10) (princ)
              (progn 
              (setq Elist (cons (cdr p) Elist) )
              );; end progn
             );; end if

          );;end if
          (setq i2 (1+ i2))
        );;; end while
        (setq Elist (cdr (reverse Elist)))

        ;; ***
        (setq p nil)
        (while (and (null p) (> (length Elist) 1) )
          (setq p3 (car Elist))
          (setq Elist (cdr Elist))
          (setq p4 (car Elist))
          (setq p (inters p1 p2 p3 p4))
        );;; end while

        (if (null p) (princ)
          (if (or (< (distance p3 p) 0.1) (< (distance p4 p) 0.1) )
              (setq p nil)
          );;; end if
         );;; end if

        (if (null p) (princ "Khong cat duoc!")
          (progn
            (if (= cat 0) (setq pc p)
              (setq pc (polar p (angle p3 p4) 0.01))
            );;; end if
            (command ".break" (list dt pluu) "f" p pc)
          );;; end progn
        );;; end if
       ))

     (T (progn
       (princ "\nChuong trinh moi cat duoc doi tuong line & polyline.")
       ; Chu y ham (redraw dt 3)

     ))
     );; end cond
     (setq i (1+ i))
   );end repeat


   (princ "\nOK?")
   (setvar "osmode" osm)
   (princ)
)










