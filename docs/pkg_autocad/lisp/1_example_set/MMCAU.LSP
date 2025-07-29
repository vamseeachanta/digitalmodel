

                             MAI MANH CAU

    Mai duoc ghep boi cac manh cau.Ban kinh duong tron ngoai tiep 
 hinh chieu bang mai chon nho hon 1.4 lan ban kinh cac manh cau

-------------------------------------------------------------------- 


 (defun DLMC ()
    (setq rmc (getint "ban kinh manh cau : ")
          rnv (getint "ban kinh duong tron ngoai tiep h/c bang : ")
          sm (getint "so manh cau : ")
          dv (getint "do day mai : ")
          hm (getint "do cao dung mai : ")
    )
    (setq A2 (getpoint "hinh chieu bang dinh mai : "))
 )
 (defun SLMC ()
    (setq ngd (/ 180.0 sm))
    (setq ngdr (* pi (/ ngd 180.0)))
    (setq O2 (polar A2 0 (* rnv (cos ngdr))))
    (setq A1 (polar O2 (/ pi 2) (* rnv (sin ngdr))))
    (setq A3 (polar O2 (* 3 (/ pi 2)) (* rnv (sin ngdr))))
    (setq P1 (polar O2 (/ pi 2) rmc)
          P2 (polar O2 pi rmc)
          P3 (polar O2 pi (- rmc dv))
          P4 (polar O2 (/ pi 2) (- rmc dv))
    )
    (setq P33 (polar P3 (/ pi 2) 100))
 )
 (defun VMC ()
    (command "PLINE" P4 P1 "A" "CE" O2 P2 "L" P3 "A" "D" P33 P4 "")
        (setq ss1 (ssget "L"))
    (command "REVOLVE" ss1 "" P3 P2 "180")
        (setq ss2 (ssget "L"))
    (command "SLICE" ss2 "" A1 A2 (list (car A2) (cadr A2) 10) A3)
        (setq ss3 (ssget "L"))
    (command "SLICE" ss3 "" A2 A3 (list (car A2) (cadr A2) 10) A1)
        (setq ss4 (ssget "L"))
 )
 (defun VVOM ()
    (setq A0 (polar A2 (* 3 (/ pi 4)) 100))
    (setq A00 (polar A2 (* 7 (/ pi 4)) 100))
    (command "ARRAY" ss4 "" "P" A2 sm "360" "y")
        (setq ss5 (ssget "C" A00 A0))
    (command "MOVE" ss5 "" A2 (list (car A2) (cadr A2) (+ rmc hm)))
 )
 (defun C:MMCAU ()
    (SETVAR "OSMODE" 0)
    (DLMC) (SLMC) (VMC) (VVOM)
    (COMMAND "REDRAW")
 )
