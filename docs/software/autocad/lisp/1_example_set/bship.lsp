;;;--- BSHIP.lsp
;;;
;;;--- Battle Ship game
;;;
;;;--- Copyright 2003 by JefferyPSanders.com
;;;    All rights reserved
;;;
;;;--- Created on 11/8/03
;;;
;;; (globalization by Xanadu - www.xanadu.cz)
;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                   N O T E S
;;;
;;;
;;;--- Grids:
;;;
;;;    There are two grids.  The one on the left is where the computer's ships are
;;;    located.  The one on the right is where your ships are located.
;;;    Each grid has 10 rows and 10 columns. The bottom left cell is cell number 1.
;;;    The top right cell is cell number 100.  [Cell 100 is not used]
;;;
;;;
;;;
;;;
;;;
;;;--- Ship maps:
;;;
;;;        MAP1 = A list of cell numbers locating your ships on the grid.
;;;        MAP2 = A list of cell numbers location the computer's ships on the grid.
;;;
;;;        A ship map consist of: (2) minesweepers  [A mine sweeper has 2 cell numbers]
;;;                               (2) frigates      [A frigate has 3 cell numbers]
;;;                               (1) cruiser       [A cruiser has 4 cell numbers]
;;;                               (1) battleship    [A battleship has 5 cell numbers]
;;;
;;;
;;;        Example of a ship map:
;;;
;;;        ( 12 13    20 30    34 35 36   50 60 70  1 2 3 4   48 58 68 78 88 )
;;;
;;;         [12 13]          Represent the cell numbers to make mine sweeper #1  [ship1]
;;;         [20 30]          Represent the cell numbers to make mine sweeper #2  [ship2]
;;;         [34 35 36]       Represent the cell numbers to make frigate #1       [ship3]
;;;         [50 60 70]       Represent the cell numbers to make frigate #2       [ship4]
;;;         [1 2 3 4]        Represent the cell numbers to make the cruiser      [ship5]
;;;         [48 58 68 78 88] Represent the cell numbers to make the battleship   [ship6]
;;;
;;;
;;;
;;;--- Other maps:
;;;
;;;
;;;       MAP3 = A list of all cells available for the user to choose from.
;;;       MAP4 = A list of all cells available for the computer to choose from.
;;;
;;;       They are simply used to keep up with which cells have already been chosen.
;;;
;;;       
;;;       OLDMAP1 - Copy of original map MAP1 
;;;       OLDMAP2 - Copy of original map MAP2
;;;
;;;       These are used because MAP1 and MAP2 get modified when a ship is hit or sunk.
;;;       The cell numbers inside MAP1 and MAP2 get changed to zero or -1 when this occurs.
;;;       After a ship is sunk I need to draw it on the grid.  If I didn't have a copy
;;;       of the original map, I wouldn't know where the ship was originally located.
;;;
;;;
;;;
;;;
;;;
;;;
;;;--- Drawing a ship:
;;;
;;;    A horizontal or vertical ship can consist of three parts:
;;;
;;;       The front of the ship, the middle of the ship, and the back of the ship.
;;;
;;;       In order to do this I had to create 6 functions:
;;;
;;;       Three for horizontal: drawF drawM drawE
;;;       Three for vertical  : drawT drawC drawB
;;;
;;;       F = Front   M = Middle   E = End
;;;       T = Top     C = Center   B = Bottom  
;;;
;;;       To draw a horizontal minesweeper:  (drawF)(drawE)
;;;       To draw a vertical minesweeper  :  (drawT)(drawB)
;;;
;;;       To draw a horizontal cruiser:  (drawF)(drawM)(drawM)(drawE)
;;;       To draw a vertical cruiser  :  (drawT)(drawC)(drawC)(drawB)       
;;;
;;;
;;;
;;;
;;;
;;;
;;;--- Other important variable names:
;;;
;;;    LBL - Lower left corner of the left grid
;;;    RBL - Lower left corner of the right grid
;;;    RW  - Height of a row and width of a column
;;;    ANS - The users answer.  Usually a point selected on the grid.
;;;    CELLNUM - The number of a cell [1-99]
;;;    CELLPT  - The X,Y location of a cell's lower left hand corner.
;;;
;;;
;;;
;;;
;;;
;;;
;;;
;;;
;;;
;;;
;;;--- There is a lot of code unremarked below.  Most of this code is simply for drawing
;;;    the ships, grid, and text to the screen.  Don't let this concern you.
;;;



(defun C:BSHIP() ;/ vc vs rw lbl rbl ans ruleStr a b c cellPt cellNum
                 ;hitShip map1 map2 map3 map4 oldmap1 oldmap2 bombList num)





;;;--- Set up the grid and find all of the points used on the grid
(defun setUpGrid(bl / bmp tmp blpt brpt tlpt trpt tpt dpt mcpt ccpt rcpt lcpt xs ys pt1 pt2 pt3 pt4 cw gap nrw)

  ;;;--- Find the corners of the grid
  (setq bmp(polar bl 0 (* rw 5.0)))
  (setq tmp(polar (polar bl (* pi 0.5) (* rw 10.0)) 0 (* rw 5.0)))
  (setq blpt(polar bmp pi (* rw 5.0)))
  (setq brpt(polar bmp 0  (* rw 5.0)))
  (setq tlpt(polar tmp pi (* rw 5.0)))
  (setq trpt(polar tmp 0  (* rw 5.0)))

  ;;;--- Fill in the background area for the grid
  (setq tpt blpt)
  (while (<= (cadr tpt)(cadr tlpt))
    (grdraw tpt (polar tpt 0 (* rw 10.0)) 142)
    (setq tpt(polar tpt (* pi 0.5) (/ rw (* vs 2.0))))
  )

  ;;;--- Draw the cells
  (setq xs (car blpt) ys (cadr blpt))
  (while(<= ys (- (cadr trpt)rw))
    
    (while (<= xs (- (car trpt)rw))
      (setq pt1 (list xs ys))
      (setq pt2(polar pt1 (* pi 0.5) rw))
      (setq pt3(polar pt2 0 rw))
      (setq pt4(polar pt1 0 rw))
      (grdraw pt1 pt4 252)
      (grdraw pt4 pt3 252)
      (grdraw pt3 pt2 254)
      (grdraw pt2 pt1 254)
      (setq xs(+ xs rw))  
    )

    (setq xs(car blpt))
    (setq ys(+ ys rw))
  )  

  ;;;--- Draw JPS
  (drawJPS 
     (polar (polar vc (* pi 1.5) (* rw 7.0)) pi (* rw 10.0))
     (polar (polar vc (* pi 1.5) (* rw 7.0)) 0  (* rw 10.0))
  )


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;--- Draw the headings
  
  ;;;--- Draw YOUR SHIPS
  (setq dpt(polar rbl (* pi 0.5) (* 10.125 rw)))
  (setq cw(/ rw 4.0))
  (setq gap(/ rw 8.0))
  (setq nrw(/ rw 2.0))
  
  ;;;--- Y
  (setq tlpt(polar dpt(* pi 0.5) nrw))
  (setq trpt(polar tlpt 0 cw))
  (setq brpt(polar dpt 0 cw))
  (setq mcpt(polar dpt 0 (/ cw 2.0)))
  (setq ccpt(polar mcpt (* pi 0.5) (/ nrw 2.0)))
  (grdraw mcpt ccpt 2)
  (grdraw ccpt tlpt 2)
  (grdraw ccpt trpt 2)
  ;;;--- O
  (setq dpt(polar brpt 0 gap))
  (setq tlpt(polar dpt(* pi 0.5) nrw))
  (setq trpt(polar tlpt 0 cw))
  (setq brpt(polar dpt 0 cw))
  (grdraw dpt tlpt 2)
  (grdraw tlpt trpt 2)
  (grdraw trpt brpt 2)
  (grdraw brpt dpt 2)
  ;;;--- U
  (setq dpt(polar brpt 0 gap))
  (setq tlpt(polar dpt(* pi 0.5) nrw))
  (setq trpt(polar tlpt 0 cw))
  (setq brpt(polar dpt 0 cw))
  (grdraw tlpt dpt 2)
  (grdraw dpt brpt 2)
  (grdraw brpt trpt 2)
  ;;;--- R
  (setq dpt(polar brpt 0 gap))
  (setq tlpt(polar dpt(* pi 0.5) nrw))
  (setq trpt(polar tlpt 0 cw))
  (setq brpt(polar dpt 0 cw))
  (setq mcpt(polar dpt 0 (/ cw 2.0)))
  (setq ccpt(polar mcpt (* pi 0.5) (/ nrw 2.0)))
  (grdraw dpt tlpt 2)
  (grdraw tlpt trpt 2)
  (grdraw trpt (polar ccpt 0 (/ cw 2.0)) 2)
  (grdraw (polar dpt (* pi 0.5) (/ nrw 2.0)) (polar ccpt 0 (/ cw 2.0)) 2)
  (grdraw (polar ccpt 0 (* cw 0.35)) brpt 2)
  ;;;--- Space and S
  (setq dpt(polar brpt 0 cw))
  (setq tlpt(polar dpt(* pi 0.5) nrw))
  (setq trpt(polar tlpt 0 cw))
  (setq brpt(polar dpt 0 cw))
  (setq lcpt(polar dpt (* pi 0.5) (/ nrw 2.0)))
  (setq rcpt(polar lcpt 0 cw))
  (grdraw dpt brpt 2)
  (grdraw brpt rcpt 2)
  (grdraw rcpt lcpt 2)
  (grdraw lcpt tlpt 2)
  (grdraw tlpt trpt 2)
  ;;;--- H
  (setq dpt(polar brpt 0 gap))
  (setq tlpt(polar dpt(* pi 0.5) nrw))
  (setq trpt(polar tlpt 0 cw))
  (setq brpt(polar dpt 0 cw))
  (setq lcpt(polar dpt (* pi 0.5) (/ nrw 2.0)))
  (setq rcpt(polar lcpt 0 cw))
  (grdraw dpt tlpt 2)
  (grdraw brpt trpt 2)
  (grdraw lcpt rcpt 2)
  ;;;--- I
  (setq dpt(polar brpt 0 gap))
  (setq tlpt(polar dpt(* pi 0.5) nrw))
  (setq trpt(polar tlpt 0 cw))
  (setq brpt(polar dpt 0 cw))
  (setq mcpt(polar dpt 0 (/ cw 2.0)))
  (grdraw dpt brpt 2)
  (grdraw tlpt trpt 2)
  (grdraw mcpt (polar mcpt (* pi 0.5) nrw) 2)
  ;;;--- P
  (setq dpt(polar brpt 0 gap))
  (setq tlpt(polar dpt(* pi 0.5) nrw))
  (setq trpt(polar tlpt 0 cw))
  (setq brpt(polar dpt 0 cw))
  (setq lcpt(polar dpt (* pi 0.5) (/ nrw 2.0)))
  (setq rcpt(polar lcpt 0 cw))
  (grdraw dpt tlpt 2)
  (grdraw tlpt trpt 2)
  (grdraw trpt (polar trpt (* pi 1.5) cw) 2)
  (grdraw (polar dpt (* pi 0.5) cw)(polar trpt (* pi 1.5) cw) 2)
  ;;;--- S
  (setq dpt(polar brpt 0 gap))
  (setq tlpt(polar dpt(* pi 0.5) nrw))
  (setq trpt(polar tlpt 0 cw))
  (setq brpt(polar dpt 0 cw))
  (setq lcpt(polar dpt (* pi 0.5) (/ nrw 2.0)))
  (setq rcpt(polar lcpt 0 cw))
  (grdraw dpt brpt 2)
  (grdraw brpt rcpt 2)
  (grdraw rcpt lcpt 2)
  (grdraw lcpt tlpt 2)
  (grdraw tlpt trpt 2)

  ;;;--- Draw MY SHIPS
  (setq dpt(polar lbl (* pi 0.5) (* 10.125 rw)))
  (setq cw(/ nrw 2.0))
  (setq gap(/ nrw 4.0))
  
  ;;;--- M
  (setq tlpt(polar dpt(* pi 0.5) nrw))
  (setq trpt(polar tlpt 0 cw))
  (setq brpt(polar dpt 0 cw))
  (setq mcpt(polar dpt 0 (/ cw 2.0)))
  (setq ccpt(polar mcpt (* pi 0.5) (/ nrw 2.0)))
  (grdraw dpt tlpt 2)
  (grdraw tlpt ccpt 2)
  (grdraw ccpt trpt 2)
  (grdraw trpt brpt 2)
  ;;;--- Y
  (setq dpt(polar brpt 0 gap))
  (setq tlpt(polar dpt(* pi 0.5) nrw))
  (setq trpt(polar tlpt 0 cw))
  (setq brpt(polar dpt 0 cw))
  (setq mcpt(polar dpt 0 (/ cw 2.0)))
  (setq ccpt(polar mcpt (* pi 0.5) (/ nrw 2.0)))
  (grdraw mcpt ccpt 2)
  (grdraw ccpt tlpt 2)
  (grdraw ccpt trpt 2)
  ;;;--- Space and S
  (setq dpt(polar brpt 0 cw))
  (setq tlpt(polar dpt(* pi 0.5) nrw))
  (setq trpt(polar tlpt 0 cw))
  (setq brpt(polar dpt 0 cw))
  (setq lcpt(polar dpt (* pi 0.5) (/ nrw 2.0)))
  (setq rcpt(polar lcpt 0 cw))
  (grdraw dpt brpt 2)
  (grdraw brpt rcpt 2)
  (grdraw rcpt lcpt 2)
  (grdraw lcpt tlpt 2)
  (grdraw tlpt trpt 2)
  ;;;--- H
  (setq dpt(polar brpt 0 gap))
  (setq tlpt(polar dpt(* pi 0.5) nrw))
  (setq trpt(polar tlpt 0 cw))
  (setq brpt(polar dpt 0 cw))
  (setq lcpt(polar dpt (* pi 0.5) (/ nrw 2.0)))
  (setq rcpt(polar lcpt 0 cw))
  (grdraw dpt tlpt 2)
  (grdraw brpt trpt 2)
  (grdraw lcpt rcpt 2)
  ;;;--- I
  (setq dpt(polar brpt 0 gap))
  (setq tlpt(polar dpt(* pi 0.5) nrw))
  (setq trpt(polar tlpt 0 cw))
  (setq brpt(polar dpt 0 cw))
  (setq mcpt(polar dpt 0 (/ cw 2.0)))
  (grdraw dpt brpt 2)
  (grdraw tlpt trpt 2)
  (grdraw mcpt (polar mcpt (* pi 0.5) nrw) 2)
  ;;;--- P
  (setq dpt(polar brpt 0 gap))
  (setq tlpt(polar dpt(* pi 0.5) nrw))
  (setq trpt(polar tlpt 0 cw))
  (setq brpt(polar dpt 0 cw))
  (setq lcpt(polar dpt (* pi 0.5) (/ nrw 2.0)))
  (setq rcpt(polar lcpt 0 cw))
  (grdraw dpt tlpt 2)
  (grdraw tlpt trpt 2)
  (grdraw trpt (polar trpt (* pi 1.5) cw) 2)
  (grdraw (polar dpt (* pi 0.5) cw)(polar trpt (* pi 1.5) cw) 2)
  ;;;--- S
  (setq dpt(polar brpt 0 gap))
  (setq tlpt(polar dpt(* pi 0.5) nrw))
  (setq trpt(polar tlpt 0 cw))
  (setq brpt(polar dpt 0 cw))
  (setq lcpt(polar dpt (* pi 0.5) (/ nrw 2.0)))
  (setq rcpt(polar lcpt 0 cw))
  (grdraw dpt brpt 2)
  (grdraw brpt rcpt 2)
  (grdraw rcpt lcpt 2)
  (grdraw lcpt tlpt 2)
  (grdraw tlpt trpt 2)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
)


;;--- Draw JPS
(defun drawJPS(spt ept / tpt spt chwd chgp chht fpt blp trp)
  (setq tpt spt)
  (setq chwd(/ (distance spt ept) 26.0)) 
  (setq chgp(/ (* 7.0 chwd)20.0))        
  (setq chht(* chwd 2.0))                
  (setq fpt(polar spt 0 chgp))           
  ;j
  (setq blp(polar fpt (* pi 1.5) chht))
  (setq trp(polar fpt 0 chwd))         
  (grdraw (polar blp (* pi 0.5) (/ chht 4.0)) blp 5)
  (grdraw blp (polar blp 0 chwd) 5)
  (grdraw trp (polar blp 0 chwd) 5)
  ;e
  (setq blp(polar blp 0 (+ chwd chgp)))
  (setq trp(polar trp 0 (+ chwd chgp)))
  (grdraw blp (polar blp 0 chwd) 5)
  (grdraw blp (polar blp (* pi 0.5) chht) 5)
  (grdraw (polar blp (* pi 0.5) chht) trp 5)
  (grdraw (polar blp (* pi 0.5) (/ chht 2.0)) (polar(polar blp (* pi 0.5) (/ chht 2.0)) 0 chwd) 5) 
  ;f
  (setq blp(polar blp 0 (+ chwd chgp)))
  (setq trp(polar trp 0 (+ chwd chgp)))
  (grdraw blp (polar blp (* pi 0.5) chht) 5)
  (grdraw (polar blp (* pi 0.5) chht) trp 5)
  (grdraw (polar blp (* pi 0.5) (/ chht 2.0)) (polar(polar blp (* pi 0.5) (/ chht 2.0)) 0 chwd) 5) 
  ;f
  (setq blp(polar blp 0 (+ chwd chgp)))
  (setq trp(polar trp 0 (+ chwd chgp)))
  (grdraw blp (polar blp (* pi 0.5) chht) 5)
  (grdraw (polar blp (* pi 0.5) chht) trp 5)
  (grdraw (polar blp (* pi 0.5) (/ chht 2.0)) (polar(polar blp (* pi 0.5) (/ chht 2.0)) 0 chwd) 5) 
  ;e
  (setq blp(polar blp 0 (+ chwd chgp)))
  (setq trp(polar trp 0 (+ chwd chgp)))
  (grdraw blp (polar blp 0 chwd) 5)
  (grdraw blp (polar blp (* pi 0.5) chht) 5)
  (grdraw (polar blp (* pi 0.5) chht) trp 5)
  (grdraw (polar blp (* pi 0.5) (/ chht 2.0)) (polar(polar blp (* pi 0.5) (/ chht 2.0)) 0 chwd) 5) 
  ;r
  (setq blp(polar blp 0 (+ chwd chgp)))
  (setq trp(polar trp 0 (+ chwd chgp)))
  (grdraw blp (polar blp (* pi 0.5) chht) 5)
  (grdraw (polar blp (* pi 0.5) chht) trp 5)
  (grdraw (polar blp (* pi 0.5) (/ chht 2.0)) (polar(polar blp (* pi 0.5) (/ chht 2.0)) 0 chwd) 5) 
  (grdraw trp (polar trp (* pi 1.5) (/ chht 2.0)) 5)
  (grdraw (polar (polar trp (* pi 1.5) (/ chht 2.0)) pi (/ chwd 3.0)) (polar blp 0 chwd) 5)
  ;y
  (setq blp(polar blp 0 (+ chwd chgp)))
  (setq trp(polar trp 0 (+ chwd chgp)))
  (setq cpt(polar (polar blp 0 (/ chwd 2.0)) (* pi 0.5) (/ chht 2.0)))
  (grdraw cpt (polar cpt (* pi 1.5) (/ chht 2.0)) 5)
  (grdraw cpt (polar cpt 0 (/ chwd 2.0)) 5)
  (grdraw cpt (polar cpt pi(/ chwd 2.0)) 5)
  (grdraw trp (polar trp (* pi 1.5) (/ chht 2.0)) 5)
  (grdraw (polar blp (* pi 0.5) (/ chht 2.0))(polar trp pi chwd) 5)
  ;p
  (setq blp(polar blp 0 (+ chwd chgp)))
  (setq trp(polar trp 0 (+ chwd chgp)))
  (grdraw blp (polar blp (* pi 0.5) chht) 5)
  (grdraw (polar blp (* pi 0.5) chht) trp 5)
  (grdraw (polar blp (* pi 0.5) (/ chht 2.0)) (polar(polar blp (* pi 0.5) (/ chht 2.0)) 0 chwd) 5) 
  (grdraw trp (polar trp (* pi 1.5) (/ chht 2.0)) 5)
  ;s
  (setq blp(polar blp 0 (+ chwd chgp)))
  (setq trp(polar trp 0 (+ chwd chgp)))
  (grdraw blp (polar blp 0 chwd) 5)
  (grdraw (polar blp 0 chwd) (polar (polar blp 0 chwd) (* pi 0.5) (/ chht 2.0)) 5)
  (grdraw (polar blp (* pi 0.5) (/ chht 2.0)) (polar trp (* pi 1.5) (/ chht 2.0)) 5)
  (grdraw (polar blp (* pi 0.5) chht) trp 5)
  (grdraw (polar blp (* pi 0.5) (/ chht 2.0)) (polar(polar blp (* pi 0.5) (/ chht 2.0)) (* pi 0.5) (/ chht 2.0)) 5) 
  ;a
  (setq blp(polar blp 0 (+ chwd chgp)))
  (setq trp(polar trp 0 (+ chwd chgp)))
  (grdraw blp (polar blp (* pi 0.5) chht) 5)
  (grdraw (polar blp (* pi 0.5) chht) trp 5)
  (grdraw (polar blp (* pi 0.5) (/ chht 2.0)) (polar(polar blp (* pi 0.5) (/ chht 2.0)) 0 chwd) 5) 
  (grdraw trp (polar trp (* pi 1.5) chht) 5)
  ;n
  (setq blp(polar blp 0 (+ chwd chgp)))
  (setq trp(polar trp 0 (+ chwd chgp)))
  (grdraw blp (polar blp (* pi 0.5) chht) 5)
  (grdraw (polar blp (* pi 0.5) chht) (polar trp (* pi 1.5) chht) 5)
  (grdraw (polar trp (* pi 1.5) chht) trp 5)
  ;d
  (setq blp(polar blp 0 (+ chwd chgp)))
  (setq trp(polar trp 0 (+ chwd chgp)))
  (setq tlp(polar blp (* pi 0.5) chht))
  (setq brp(polar blp 0 chwd))
  (grdraw blp tlp 5)
  (grdraw blp (setq tpt(polar brp pi (/ chwd 4.0))) 5)
  (grdraw tpt (setq tpt(polar brp (* pi 0.5) (/ chht 4.0))) 5)
  (grdraw tpt (setq tpt(polar trp (* pi 1.5) (/ chht 4.0))) 5)
  (grdraw (setq npt(polar trp pi (/ chwd 4.0))) tlp 5) 
  (grdraw npt tpt 5)
  ;e
  (setq blp(polar blp 0 (+ chwd chgp)))
  (setq trp(polar trp 0 (+ chwd chgp)))
  (grdraw blp (polar blp 0 chwd) 5)
  (grdraw blp (polar blp (* pi 0.5) chht) 5)
  (grdraw (polar blp (* pi 0.5) chht) trp 5)
  (grdraw (polar blp (* pi 0.5) (/ chht 2.0)) (polar(polar blp (* pi 0.5) (/ chht 2.0)) 0 chwd) 5) 
  ;r
  (setq blp(polar blp 0 (+ chwd chgp)))
  (setq trp(polar trp 0 (+ chwd chgp)))
  (grdraw blp (polar blp (* pi 0.5) chht) 5)
  (grdraw (polar blp (* pi 0.5) chht) trp 5)
  (grdraw (polar blp (* pi 0.5) (/ chht 2.0)) (polar(polar blp (* pi 0.5) (/ chht 2.0)) 0 chwd) 5) 
  (grdraw trp (polar trp (* pi 1.5) (/ chht 2.0)) 5)
  (grdraw (polar (polar trp (* pi 1.5) (/ chht 2.0)) pi (/ chwd 3.0)) (polar blp 0 chwd) 5)
  ;s
  (setq blp(polar blp 0 (+ chwd chgp)))
  (setq trp(polar trp 0 (+ chwd chgp)))
  (grdraw blp (polar blp 0 chwd) 5)
  (grdraw (polar blp 0 chwd) (polar (polar blp 0 chwd) (* pi 0.5) (/ chht 2.0)) 5)
  (grdraw (polar blp (* pi 0.5) (/ chht 2.0)) (polar trp (* pi 1.5) (/ chht 2.0)) 5)
  (grdraw (polar blp (* pi 0.5) chht) trp 5)
  (grdraw (polar blp (* pi 0.5) (/ chht 2.0)) (polar(polar blp (* pi 0.5) (/ chht 2.0)) (* pi 0.5) (/ chht 2.0)) 5) 
  ;.
  (setq blp(polar blp 0 (+ chwd chgp)))
  (setq trp(polar trp 0 (+ chwd chgp)))
  (setq bmp(polar blp 0 (/ chwd 2.0)))
  (setq bds(/ chwd 2.0))
  (grdraw bmp (setq tpt(polar bmp 0 (/ bds 2.0))) 5)
  (grdraw tpt (setq tpt(polar tpt (* pi 0.5) bds)) 5)
  (grdraw tpt (setq tpt(polar tpt pi bds)) 5)
  (grdraw tpt (setq tpt(polar tpt (* pi 1.5) bds)) 5)
  (grdraw tpt bmp 5)
  ;c
  (setq blp(polar blp 0 (+ chwd chgp)))
  (setq trp(polar trp 0 (+ chwd chgp)))
  (grdraw blp (polar blp 0 chwd) 5)
  (grdraw blp (polar blp (* pi 0.5) chht) 5)
  (grdraw (polar blp (* pi 0.5) chht) trp 5)
  ;o
  (setq blp(polar blp 0 (+ chwd chgp)))
  (setq trp(polar trp 0 (+ chwd chgp)))
  (grdraw blp (polar blp 0 chwd) 5)
  (grdraw blp (polar blp (* pi 0.5) chht) 5)
  (grdraw (polar blp (* pi 0.5) chht) trp 5)
  (grdraw (polar blp 0 chwd) trp 5)
  ;m
  (setq blp(polar blp 0 (+ chwd chgp)))
  (setq chw(* chwd 1.5))
  (setq trp(polar trp 0 (+ chw chgp)))
  (setq bmp(polar blp 0 (/ chw 2.0)))
  (grdraw blp (setq tpt(polar blp (* pi 0.5) chht)) 5)
  (grdraw tpt bmp 5)
  (grdraw bmp trp 5)
  (grdraw trp (polar blp 0 chw) 5)
)


;;;--- Draw a bomb
;;;    pt = lower left corner of cell  r = row height
(defun drawBomb(pt r / tpt cpt a)

  ;;;--- Make a copy of the start point [lower left corner of cell]
  (setq tpt pt)

  ;;;--- Find the center point of the cell
  (setq cpt(list (+ (car pt)(/ r 2.0)) (+ (cadr pt) (/ r 2.0))))

  ;;;--- Start with angle zero
  (setq a 0)

  ;;;--- Draw lines from the center of the cell on every degree to create a circle
  (repeat 360 
     (grdraw cpt (polar cpt a (/ r 5.0)) 1)
     (setq a(+ a (/ (* pi 2.0) 360.0)))
  )  

  ;;;--- Start with angle zero
  (setq a 0)

  ;;;--- Draw lines from the center of the cell to create the mine's spikes
  (repeat 8
    (grdraw cpt (polar cpt a (/ r 3.0)) 1)
    (setq a(+ a (/ (* pi 2.0) 8.0)))
  )

  ;;;--- Start with angle 1.7 radians
  (setq a 1.7)

  ;;;--- Draw a white spot on the mine for a small shiny reflection
  (while (< a 2.35)
    (grdraw (polar cpt a (/ r 7.0))(polar cpt a (/ r 6.0)) 7)
    (setq a(+ a 0.1))
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;--- Draw a ship  
;;;    shipNum = ship number   hv     = horizontal or vertical ship orientation
;;;    map     = ship map      gridpt = bottom left point of grid
;;;      r     = row height        bc = background color  fc = foreground color

(defun drawShip(shipNum hv map gridpt r bc fc)

  ;;;--- Draw ship number 1
  (if(= shipNum 1)
    (progn

      ;;;--- Draw it horizontal
      (if (= hv "H")
        (progn
          (drawE (nth 0 map) gridpt r bc fc)
          (drawF (nth 1 map) gridpt r bc fc)
        )

        ;;;--- Or draw it vertical
        (progn
          (drawB (nth 0 map) gridpt r bc fc)
          (drawT (nth 1 map) gridpt r bc fc)
        )
      )
    )
  )

  ;;;--- Draw ship number 2
  (if(= shipNum 2)
    (progn

      ;;;--- Draw it horizontal
      (if (= hv "H")
        (progn
          (drawE (nth 2 map) gridpt r bc fc)
          (drawF (nth 3 map) gridpt r bc fc)
        )

        ;;;--- Or draw it vertical
        (progn
          (drawB (nth 2 map) gridpt r bc fc)
          (drawT (nth 3 map) gridpt r bc fc)
        )
      )
    )
  )

  ;;;--- Draw ship number 3
  (if(= shipNum 3)
    (progn

      ;;;--- Draw it horizontal
      (if (= hv "H")
        (progn
          (drawE (nth 4 map) gridpt r bc fc)
          (drawM (nth 5 map) gridpt r bc fc)
          (drawF (nth 6 map) gridpt r bc fc)
        )

        ;;;--- Or draw it vertical
        (progn
          (drawB (nth 4 map) gridpt r bc fc)
          (drawC (nth 5 map) gridpt r bc fc)
          (drawT (nth 6 map) gridpt r bc fc)
        )
      )
    )
  )

  ;;;--- Draw ship number 4
  (if(= shipNum 4)
    (progn

      ;;;--- Draw it horizontal
      (if (= hv "H")
        (progn
          (drawE (nth 7 map) gridpt r bc fc)
          (drawM (nth 8 map) gridpt r bc fc)
          (drawF (nth 9 map) gridpt r bc fc)
        )

        ;;;--- Or draw it vertical
        (progn
          (drawB (nth 7 map) gridpt r bc fc)
          (drawC (nth 8 map) gridpt r bc fc)
          (drawT (nth 9 map) gridpt r bc fc)
        )
      )
    )
  )

  ;;;--- Draw ship number 5
  (if(= shipNum 5)
    (progn

      ;;;--- Draw it horizontal
      (if (= hv "H")
        (progn
          (drawE (nth 10 map) gridpt r bc fc)
          (drawM (nth 11 map) gridpt r bc fc)
          (drawM (nth 12 map) gridpt r bc fc)
          (drawF (nth 13 map) gridpt r bc fc)
        )

        ;;;--- Or draw it vertical
        (progn
          (drawB (nth 10 map) gridpt r bc fc)
          (drawC (nth 11 map) gridpt r bc fc)
          (drawC (nth 12 map) gridpt r bc fc)
          (drawT (nth 13 map) gridpt r bc fc)
        )
      )
    )
  )

  ;;;--- Draw ship number 6
  (if(= shipNum 6)
    (progn

      ;;;--- Draw it horizontal
      (if (= hv "H")
        (progn
          (drawE (nth 14 map) gridpt r bc fc)
          (drawM (nth 15 map) gridpt r bc fc)
          (drawM (nth 16 map) gridpt r bc fc)
          (drawM (nth 17 map) gridpt r bc fc)
          (drawF (nth 18 map) gridpt r bc fc)
        )

        ;;;--- Or draw it vertical
        (progn
          (drawB (nth 14 map) gridpt r bc fc)
          (drawC (nth 15 map) gridpt r bc fc)
          (drawC (nth 16 map) gridpt r bc fc)
          (drawC (nth 17 map) gridpt r bc fc)
          (drawT (nth 18 map) gridpt r bc fc)
        )
      )
    )
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;--- Draw all ships in a map
;;;    shipMap = ship map
;;;     gridpt = lower left corner of grid
;;;          r = row height

(defun drawShips(shipMap gridpt r)

  ;;;--- Check to see if ship number 1 is horizontal
  (if(= (+ (nth 0 shipMap) 1)(nth 1 shipMap))

    ;;;--- If it is horizontal
    (drawShip 1 "H" shipMap gridpt r c1 c2)

    ;;;--- Else, it is vertical
    (drawShip 1 "V" shipMap gridpt r c1 c2)
  )

  ;;;--- Check to see if ship number 2 is horizontal
  (if(= (+ (nth 2 shipMap) 1)(nth 3 shipMap))

    ;;;--- If it is horizontal
    (drawShip 2 "H" shipMap gridpt r c1 c2)

    ;;;--- Else, it is vertical
    (drawShip 2 "V" shipMap gridpt r c1 c2)
  )

  ;;;--- Check to see if ship number 3 is horizontal
  (if(= (+ (nth 4 shipMap) 1)(nth 5 shipMap))

    ;;;--- If it is horizontal
    (drawShip 3 "H" shipMap gridpt r c1 c2)

    ;;;--- Else, it is vertical
    (drawShip 3 "V" shipMap gridpt r c1 c2)
  )

  ;;;--- Check to see if ship number 4 is horizontal
  (if(= (+ (nth 7 shipMap) 1)(nth 8 shipMap))

    ;;;--- If it is horizontal
    (drawShip 4 "H" shipMap gridpt r c1 c2)

    ;;;--- Else, it is vertical
    (drawShip 4 "V" shipMap gridpt r c1 c2)
  )

  ;;;--- Check to see if ship number 5 is horizontal
  (if(= (+ (nth 10 shipMap) 1)(nth 11 shipMap))

    ;;;--- If it is horizontal
    (drawShip 5 "H" shipMap gridpt r c1 c2)

    ;;;--- Else, it is vertical
    (drawShip 5 "V" shipMap gridpt r c1 c2)
  )

  ;;;--- Check to see if ship number 6 is horizontal
  (if(= (+ (nth 14 shipMap) 1)(nth 15 shipMap))

    ;;;--- If it is horizontal
    (drawShip 6 "H" shipMap gridpt r c1 c2)

    ;;;--- Else, it is vertical
    (drawShip 6 "V" shipMap gridpt r c1 c2)
  )
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;--- For all ship drawing functions below
;;;    a = cell number   
;;;    b = bottom left corner of grid
;;;    r = row height
;;;    bc = background color
;;;    fc = foreground color

;;;--- Draw the Front section of a horizontal ship
(defun drawF(a b r bc fc / c d e f g h)

  ;;;--- Convert the cell number to a cell's bottom left point
  (setq c(CN2CPT a b r))

  ;;;--- Get the center point of the cell
  (setq d(polar (polar c 0 (/ r 2.0)) (* pi 0.5) (/ r 2.0)))


  ;;;--- Fill the ship in
  (setq e 0)
  (setq f(polar c (* pi 0.5)(/ r 4.0)))
  (setq g(polar d (* pi 1.5)(/ r 4.0)))
  (while(< e (/ r 4.0))
    (grdraw f (polar g 0 e) bc)
    (setq f(polar f (* pi 0.5)(/ r 40.0)))
    (setq g(polar g (* pi 0.5)(/ r 40.0)))
    (setq e(+ e (/ r 40.0)))
  )
  (while(> e 0)
    (grdraw f (polar g 0 e) bc)
    (setq f(polar f (* pi 0.5)(/ r 40.0)))
    (setq g(polar g (* pi 0.5)(/ r 40.0)))
    (setq e(- e (/ r 40.0)))  
  )  
  ;;;--- Draw the gun
  (setq f(polar d pi (/ r 8.0)))
  (setq f(polar f (* pi 1.5) (/ r 8.0)))
  (while(< (cadr f) (+ (cadr d)(/ r 8.0)))
    (grdraw f (polar f 0 (/ r 8.0)) fc)
    (setq f(polar f (* pi 0.5) (/ r 40.0)))
  )
  (setq f(polar d (* pi 0.5)(/ r 14.0)))
  (grdraw f (polar f 0 (/ r 14.0)) fc)
  (setq f(polar d (* pi 1.5)(/ r 14.0)))
  (grdraw f (polar f 0 (/ r 14.0)) fc)

  (setq e(polar c (* pi 0.5) (* r 0.35)))
  (setq g(polar c (* pi 0.5) (* r 0.65)))
  (setq f(polar e 0 (* r 0.25)))
  (setq h(polar g 0 (* r 0.25)))
  (grdraw e f fc)
  (grdraw f h fc)
  (grdraw g h fc)


  ;;;--- Draw the outline of horizontal ship Front section
  (grdraw (polar c (* pi 0.5) (/ r 4.0)) (polar d (* pi 1.5) (/ r 4.0)) fc)  
  (grdraw (polar c (* pi 0.5) (* r 0.75))(polar d (* pi 0.5) (/ r 4.0)) fc)
  (grdraw (polar d (* pi 0.5) (/ r 4.0)) (polar d 0 (/ r 4.0)) fc)
  (grdraw (polar d (* pi 1.5) (/ r 4.0)) (polar d 0 (/ r 4.0)) fc)
)

;;;--- Draw the Middle section of a horizontal ship
(defun drawM(a b r bc fc / c d e f g h)

  ;;;--- Convert the cell number to a cell's bottom left point
  (setq c(CN2CPT a b r))

  ;;;--- Get the center point of the cell
  (setq d(polar (polar c 0 (/ r 2.0)) (* pi 0.5) (/ r 2.0)))  
  
  ;;;--- Fill in the ship
  (setq f (polar c (* pi 0.5) (/ r 4.0)))
  (setq g (polar c (* pi 0.5) (* r 0.75)))
  (while (< (cadr f)(cadr g))
    (grdraw f (polar f 0 r) bc)
    (setq f(polar f (* pi 0.5) (/ r 40.0)))
  )

  (setq e(polar c (* pi 0.5) (* r 0.35)))
  (setq f(polar c (* pi 0.5) (* r 0.65)))
  (setq g(polar e 0 (* r 0.25)))
  (setq h(polar f 0 (* r 0.25)))
  (grdraw e g fc)
  (grdraw g h fc)
  (grdraw f h fc)
  (setq e(polar e 0 (* r 0.75)))
  (setq f(polar f 0 (* r 0.75)))
  (setq g(polar e 0 (* r 0.25)))
  (setq h(polar f 0 (* r 0.25)))
  (grdraw f h fc)
  (grdraw f e fc)
  (grdraw e g fc)

  ;;;--- Start with 45 degrees
  (setq f (/ pi 8.0))

  ;;;--- Draw lines from the center of the cell to create a circle
  (setq pt1(polar d 0 (/ r 8.0)))
  (repeat 16 
     (setq f(+ f (/ pi 8.0)))
     (grdraw pt1 (setq pt1(polar d f (/ r 8.0))) fc)
  )  
  

  ;;;--- Get the right bottom corner of the cell
  (setq e(polar c 0 r))

  ;;;--- Draw the outline of a horzontal ship Middle section
  (grdraw (polar c (* pi 0.5) (/ r 4.0))  (polar e (* pi 0.5) (/ r 4.0))  fc)
  (grdraw (polar c (* pi 0.5) (* r 0.75)) (polar e (* pi 0.5) (* r 0.75)) fc)
)

;;;--- Draw the End section of a horizontal ship
(defun drawE(a b r bc fc / c e f g h)


  ;;;--- Convert the cell number to a cell's bottom left point
  (setq c(CN2CPT a b r))

  ;;;--- Get the center point of the cell
  (setq d(polar (polar c 0 (/ r 2.0)) (* pi 0.5) (/ r 2.0)))

  ;;;--- Fill the ship in
  (setq f(polar c (* pi 0.5)(* r 0.25)))
  (setq f(polar f 0 (/ r 4.0)))
  (setq g(polar c (* pi 0.5)(* r 0.75)))
  (setq g(polar g 0 (/ r 4.0)))
  (while(< (cadr f)(cadr g))
    (grdraw f (polar f 0 (* r 0.75)) bc)
    (setq f(polar f (* pi 0.5)(/ r 40.0)))
  )
  ;;;--- Draw the gun
  (setq f(polar d pi (/ r 8.0)))
  (setq f(polar f (* pi 1.5) (/ r 8.0)))
  (while(< (cadr f) (+ (cadr d)(/ r 8.0)))
    (grdraw f (polar f 0 (/ r 8.0)) fc)
    (setq f(polar f (* pi 0.5) (/ r 40.0)))
  )
  (setq f(polar d (* pi 0.5)(/ r 14.0)))
  (grdraw f (polar f pi (/ r 6.0)) fc)
  (setq f(polar d (* pi 1.5)(/ r 14.0)))
  (grdraw f (polar f pi (/ r 6.0)) fc)

  (setq e(polar d (* pi 0.5) (* r 0.15)))
  (setq e(polar e 0 (* r 0.15)))
  (setq g(polar d (* pi 1.5) (* r 0.15)))
  (setq g(polar g 0 (* r 0.15)))
  (setq f(polar e 0 (* r 0.35)))
  (setq h(polar g 0 (* r 0.35)))
  (grdraw e f fc)
  (grdraw g h fc)
  (grdraw e g fc)

  ;;;--- Get the right bottom corner of the cell
  (setq e(polar c 0 r))

  ;;;--- Get the point on the cell a 1/4 of the way horizontally
  (setq f(polar c 0 (/ r 4.0)))

  ;;;--- Draw the outline of horizontal ship End section
  (grdraw (polar e (* pi 0.5) (/ r 4.0)) (polar f (* pi 0.5) (/ r 4.0))  fc)  
  (grdraw (polar f (* pi 0.5) (/ r 4.0)) (polar f (* pi 0.5) (* r 0.75)) fc)
  (grdraw (polar f (* pi 0.5) (* r 0.75))(polar e (* pi 0.5) (* r 0.75)) fc)
)
;;;--- Draw the Top section of a vertical ship
(defun drawT(a b r bc fc / c d e f g h)

  ;;;--- Convert the cell number to a cell's bottom left point
  (setq c(CN2CPT a b r))

  ;;;--- Get the center point of the cell
  (setq d(polar (polar c 0 (/ r 2.0)) (* pi 0.5) (/ r 2.0)))

  ;;;--- Fill the ship in
  (setq e 0)
  (setq f(polar c 0 (/ r 4.0)))
  (setq g(polar f (* pi 0.5) (* r 0.5)))
  (while(< e (/ r 4.0))
    (grdraw f (polar g (* pi 0.5) e) bc)
    (setq f(polar f 0 (/ r 40.0)))
    (setq g(polar g 0 (/ r 40.0)))
    (setq e(+ e (/ r 40.0)))
  )
  (while(> e 0)
    (grdraw f (polar g (* pi 0.5) e) bc)
    (setq f(polar f 0 (/ r 40.0)))
    (setq g(polar g 0 (/ r 40.0)))
    (setq e(- e (/ r 40.0)))  
  )  

  ;;;--- Draw the gun
  (setq f(polar d pi (* r 0.15)))
  (while(> (cadr f)(- (cadr d) (/ r 8.0)))
    (grdraw f (polar f 0 (* r 0.3)) fc)
    (setq f(polar f (* pi 1.5) (/ r 40.0)))
  )
  (setq f(polar d pi(/ r 14.0)))
  (grdraw f (polar f (* pi 0.5) (/ r 14.0)) fc)
  (setq f(polar d 0(/ r 14.0)))
  (grdraw f (polar f (* pi 0.5)(/ r 14.0)) fc)

  (setq e(polar c 0 (* r 0.35)))
  (setq f(polar c 0 (* r 0.65)))
  (setq g(polar e (* pi 0.5) (* r 0.25)))
  (setq h(polar f (* pi 0.5) (* r 0.25)))
  (grdraw e g fc)
  (grdraw g h fc)
  (grdraw h f fc)


  ;;;--- Draw the outline of the vertical ship Top section
  (grdraw (polar c 0 (/ r 4.0)) (polar d pi (/ r 4.0)) fc)
  (grdraw (polar c 0 (* r 0.75))(polar d 0  (/ r 4.0)) fc)
  (grdraw (polar d 0 (/ r 4.0)) (polar d (* pi 0.5) (/ r 4.0)) fc)
  (grdraw (polar d pi(/ r 4.0)) (polar d (* pi 0.5) (/ r 4.0)) fc)
)
;;;--- Draw the Center section of a vertical ship
(defun drawC(a b r bc fc / c d e f g h)

  ;;;--- Convert the cell number to a cell's bottom left point
  (setq c(CN2CPT a b r))

;;;--- Fill in the ship
  (setq f (polar c 0 (/ r 4.0)))
  (setq g (polar c 0 (* r 0.75)))
  (while (< (car f)(car g))
    (grdraw f (polar f (* pi 0.5) r) bc)
    (setq f(polar f 0 (/ r 40.0)))
  )

  (setq e(polar c 0 (* r 0.35)))
  (setq f(polar c 0 (* r 0.65)))
  (setq g(polar e (* pi 0.5) (* r 0.25)))
  (setq h(polar f (* pi 0.5) (* r 0.25)))
  (grdraw e g fc)
  (grdraw g h fc)
  (grdraw f h fc)
  (setq e(polar e (* pi 0.5) (* r 0.75)))
  (setq f(polar f (* pi 0.5) (* r 0.75)))
  (setq g(polar e (* pi 0.5) (* r 0.25)))
  (setq h(polar f (* pi 0.5) (* r 0.25)))
  (grdraw f h fc)
  (grdraw f e fc)
  (grdraw e g fc)

  ;;;--- Get the center point of the cell
  (setq d(polar (polar c 0 (/ r 2.0)) (* pi 0.5) (/ r 2.0)))

  ;;;--- Start with 45 degrees
  (setq f (/ pi 8.0))

  ;;;--- Draw lines from the center of the cell to create a circle
  (setq pt1(polar d 0 (/ r 8.0)))
  (repeat 16 
     (setq f(+ f (/ pi 8.0)))
     (grdraw pt1 (setq pt1(polar d f (/ r 8.0))) fc)
  )  


  ;;;--- Get the top left corner of the cell
  (setq f(polar c (* pi 0.5) r))
  
  ;;;--- Draw the outline of a vertical ship Center section
  (grdraw (polar c 0 (/ r 4.0))  (polar f 0 (/ r 4.0))  fc)
  (grdraw (polar c 0 (* r 0.75)) (polar f 0 (* r 0.75)) fc)
)
;;;--- Draw the Bottom section of a vertical ship
(defun drawB(a b r bc fc / c d e f g h)

  ;;;--- Convert the cell number to a cell's bottom left point
  (setq c(CN2CPT a b r))

  ;;;--- Get the center point of the cell
  (setq d(polar (polar c 0 (/ r 2.0)) (* pi 0.5) (/ r 2.0)))

  ;;;--- Fill in the ship
  (setq e(polar c (* pi 0.5) r))
  (setq f(polar e 0 (/ r 4.0)))
  (setq g(polar e 0 (* r 0.75)))
  (while(< (car f)(car g))
    (grdraw f (polar f (* pi 1.5) (* r 0.75)) bc)
    (setq f(polar f 0 (/ r 40.0)))
  )

  ;;;--- Draw the guns
  (setq e (polar d (* pi 1.5) (/ r 8.0)))
  (setq e (polar e pi (* r 0.15)))
  (while (< (cadr e) (cadr d))
    (grdraw e (polar e 0 (* r 0.3)) fc)
    (setq e(polar e (* pi 0.5) (/ r 40.0)))
  )
  (setq e (polar d (* pi 1.5) (/ r 8.0)))
  (setq f (polar e pi (/ r 16.0)))
  (grdraw f (polar f (* pi 1.5) (/ r 16.0)) fc)  
  (setq f (polar e 0 (/ r 16.0)))
  (grdraw f (polar f (* pi 1.5) (/ r 16.0)) fc)      

  (setq e(polar c 0 (* r 0.35)))
  (setq f(polar c 0 (* r 0.65)))
  (setq g(polar e (* pi 0.5) (* r 0.25)))
  (setq h(polar f (* pi 0.5) (* r 0.25)))
  (setq e(polar e (* pi 0.5) (* r 0.75)))
  (setq f(polar f (* pi 0.5) (* r 0.75)))
  (setq g(polar e (* pi 0.5) (* r 0.25)))
  (setq h(polar f (* pi 0.5) (* r 0.25)))
  (grdraw f h fc)
  (grdraw f e fc)
  (grdraw e g fc)
  


  ;;;--- Get the point on the cell a 1/4 of the way vertically
  (setq d(polar c (* pi 0.5) (/ r 4.0)))

  ;;;--- Get the top left corner of the cell
  (setq f(polar c (* pi 0.5) r))

  ;;;--- Draw the outline of the vertical ship Bottom section
  (grdraw (polar f 0 (/ r 4.0)) (polar d 0 (/ r 4.0))  fc)  
  (grdraw (polar d 0 (/ r 4.0)) (polar d 0 (* r 0.75)) fc)
  (grdraw (polar d 0 (* r 0.75))(polar f 0 (* r 0.75)) fc)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;--- Draw a miss [non-hit]
;;;
;;;    a = cell number
;;;    b = lower left corner of grid
;;;    h = hit map
;;;    r = row height

(defun drawMiss(a b h r / c d di ang pt1 pt2)

  ;;;--- If the cell hasn't been chosen before
  (if(or (member a h) (= a 100))
    (progn

      ;;;--- Convert the cell number to a cell point
      (setq c (CN2CPT a b r))

      ;;;--- Find the center point of the cell
      (setq d(polar (polar c 0 (/ r 2.0)) (* pi 0.5) (/ r 2.0)))

      ;;;--- Set a starting diameter to draw circles
      (setq di(/ r 4.0))

      ;;;--- Draw three circles
      (repeat 3

        ;;;--- Start at angle zero
        (setq ang 0)
        
        ;;;--- Repeat one time for every degree
        (repeat 360

          ;;;--- Find the start point of a line segment to create an arc
          (setq pt1(polar d ang (/ di 2.0)))

          ;;;--- Find the end of the line segment
          (setq pt2(polar d (+ ang (/ pi 180.0)) (/ di 2.0)))

          ;;;--- Draw the line
          (grdraw pt1 pt2 146)

          ;;;--- Add a degree to the angle
          (setq ang(+ ang (/ pi 180.0)))
        )

        ;;;--- Increase the diameter for the next circle
        (setq di(+ di (/ r 4.0)))
      )

      ;;;--- Start with angle zero
      (setq ang 0.0)

      ;;;--- Draw eight lines from the center at 45 degree increments
      (repeat 8

        ;;;--- Draw the line
        (grdraw d (polar d ang (/ r 8.0)) 7)

        ;;;--- Add 45 degrees to the angle
        (setq ang(+ ang (/ pi 4.0)))
      )
    )
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;--- Check for a sunken ship
;;;
;;;    s = ship map
;;;   os = original ship map [oldmap]
;;;    b = botton left corner of grid
;;;    r = row height
;;;    f = Computer or User ["C" = computer "U" = user]

(defun chkSunken(s os b r f / horVer sunk)

  ;;;
  ;;;
  ;;;    Notes:
  ;;;
  ;;;    When a ship is hit, the cell number in that ship's map is replaced
  ;;;    with -1.  When all cell numbers for that ship equal -1 that ship
  ;;;    is sunk.  Remember to replace the -1's with zeros afterwards so you
  ;;;    will not sink that ship again.
  ;;;
  ;;;

  ;;;--- Set up a return variable
  (setq sunk 0)

  ;;;--- If the first ship in the ship map adds up to -2, it is sunk...
  (if (= -2 (+ (nth 0 s) (nth 1 s)))
    (progn

      ;;;--- See if the ship was horizontal or vertical
      (if(= (+ 1 (nth 0 os))(nth 1 os))(setq horVer "H")(setq horVer "V"))

      ;;;--- Draw sunken ship number 1
      (drawShip 1 horVer os b r c3 c4)

      ;;;--- Set the return variable to ship 1
      (setq sunk 1)

      (setq rnum(atoi(substr (rtos (getvar "cdate") 2 18)17)))

      ;;;--- Alert the user
      (if (= f "U")
        (alert "Computer says: You sank my MineSweeper!")
        (alert "Computer says: I sank your MineSweeper!")
      )

    )
  )

  ;;;--- If the second ship in the ship map adds up to -2, it is sunk...
  (if (= -2 (+ (nth 2 s) (nth 3 s)))
    (progn

      ;;;--- See if the ship was horizontal or vertical
      (if(= (+ 1 (nth 2 os))(nth 3 os))(setq horVer "H")(setq horVer "V"))

      ;;;--- Draw sunken ship number 2
      (drawShip 2 horVer os b r c3 c4)

      ;;;--- Set the return variable to ship 1
      (setq sunk 2)

      ;;;--- Alert the user
      (if (= f "U")
        (alert "Computer says: You sank my MineSweeper!")
        (alert "Computer says: I sank your MineSweeper!")
      )
    )
  )

  ;;;--- If the third ship in the ship map adds up to -3, it is sunk...
  (if (= -3 (+ (nth 4 s) (nth 5 s)(nth 6 s)))
    (progn

      ;;;--- See if the ship was horizontal or vertical
      (if(= (+ 1 (nth 4 os))(nth 5 os))(setq horVer "H")(setq horVer "V"))

      ;;;--- Draw sunken ship number 3
      (drawShip 3 horVer os b r c3 c4)

      ;;;--- Set the return variable to ship 1
      (setq sunk 3)

      ;;;--- Alert the user
      (if(= f "U")
        (alert "Computer says: You sank my Frigate!")
        (alert "Computer says: I sank your Frigate!")
      )
    )
  )

  ;;;--- If the fourth ship in the ship map adds up to -3, it is sunk...
  (if (= -3 (+ (nth 7 s) (nth 8 s)(nth 9 s)))
    (progn

      ;;;--- See if the ship was horizontal or vertical
      (if(= (+ 1 (nth 7 os))(nth 8 os))(setq horVer "H")(setq horVer "V"))

      ;;;--- Draw the computer's sunken ship number 4
      (drawShip 4 horVer os b r c3 c4)

      ;;;--- Set the return variable to ship 1
      (setq sunk 4)

      ;;;--- Alert the user
      (if(= f "U")
        (alert "Computer says: You sank my Frigate!")
        (alert "Computer says: I sank your Frigate!")
      )
    )
  )

  ;;;--- If the fifth ship in the ship map adds up to -4, it is sunk...
  (if (= -4 (+ (nth 10 s) (nth 11 s)(nth 12 s)(nth 13 s)))
    (progn

      ;;;--- See if the ship was horizontal or vertical
      (if(= (+ 1 (nth 10 os))(nth 11 os))(setq horVer "H")(setq horVer "V"))

      ;;;--- Draw sunken ship number 5
      (drawShip 5 horVer os b r c3 c4)

      ;;;--- Set the return variable to ship 1
      (setq sunk 5)

      ;;;--- Alert the user
      (if(= f "U")
        (alert "Computer says: You sank my Cruiser!")
        (alert "Computer says: I sank your Cruiser!")
      )
    )
  )

  ;;;--- If the sixth ship in the ship map adds up to -5, it is sunk...
  (if (= -5 (+ (nth 14 s)(nth 15 s)(nth 16 s)(nth 17 s)(nth 18 s)))
    (progn

      ;;;--- See if the ship was horizontal or vertical
      (if(= (+ 1 (nth 14 os))(nth 15 os))(setq horVer "H")(setq horVer "V"))

      ;;;--- Draw sunken ship number 6
      (drawShip 6 horVer os b r c3 c4)

      ;;;--- Set the return variable to ship 1
      (setq sunk 6)

      ;;;--- Alert the user
      (if(= f "U")
        (alert "Computer says: You sank my Battle Ship!")
        (alert "Computer says: I sank your Battle Ship!")
      )
    )
  )
  
  ;;;--- Return the ship number that was sunk
  sunk
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;--- Functions


;;;--- Convert the cell number to a cell's bottom left point
;;;    a = cellnumber   b = Bottom left point of grid   r = row height
(defun CN2CPT(a b r / x y c)
  (setq y(* (/ (- a 1) 10) r))
  (setq x(* r(-(- a(* (/ (- a 1)10)10))1)))
  (setq c(polar b 0 x))
  (setq c(polar c (* pi 0.5) y))
)

;;;--- Convert the bottom left cell point to a cell number
;;;    a = bottom left cell point  b = bottom left point of grid  r = row height
(defun CPT2CN(a b r / x y)
  (setq x(- (car a)(car b)))
  (setq y(- (cadr a)(cadr b)))
  (setq x(+(fix(/ x r))1))
  (setq y(fix(/ y r)))
  (setq y(* y 10))
  (+ x y)
)

;;;--- Convert the selected point to a cell number
;;;    a = selected point  b = bottom left point of grid  r=row height
(defun SPT2CN(a b r / x y)
  (setq x(- (car a)(car b)))
  (setq y(- (cadr a)(cadr b)))
  (setq x(+(fix(/ x r))1))
  (setq y(fix(/ y r)))
  (setq y(* y 10))
  (+ x y)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun setUpMaps(/ rnum)
  (setq rnum(atoi(substr (rtos (getvar "cdate") 2 18)17)))
  (setq map1
    (cond
      ((= rnum 0)(list 13 23 53 54 45 46 47 84 85 86 26 27 28 29 48 58 68 78 88))
      ((= rnum 1)(list 14 24 16 17 25 26 27 62 72 82 18 28 38 48  4  5  6  7  8)) 
      ((= rnum 2)(list 38 48 76 77  4  5  6 97 98 99 10 20 30 40 32 42 52 62 72))
      ((= rnum 3)(list 70 80 87 97 38 39 40 42 43 44 47 57 67 77 58 68 78 88 98))
      ((= rnum 4)(list 13 23 52 53 18 19 20 44 45 46 48 58 68 78 71 72 73 74 75))
      ((= rnum 5)(list  1  2 60 70  7  8  9 30 40 50 24 34 44 54 94 95 96 97 98))
      ((= rnum 6)(list 23 33 42 43 58 59 60 75 85 95 61 71 81 91 12 13 14 15 16))
      ((= rnum 7)(list 51 52 88 89 46 56 66 82 83 84  4 14 24 34  9 19 29 39 49))
      ((= rnum 8)(list  6  7 44 45 28 38 48 65 75 85 21 22 23 24 52 62 72 82 92))
      ((= rnum 9)(list 53 63 71 81 11 12 13 76 77 78 27 37 47 57 50 60 70 80 90))
    )
  )
  (setq map2
    (cond
      ((= rnum 0)(list 70 80 87 97 38 39 40 42 43 44 47 57 67 77 58 68 78 88 98))
      ((= rnum 1)(list 23 33 42 43 58 59 60 75 85 95 61 71 81 91 12 13 14 15 16))
      ((= rnum 2)(list 13 23 53 54 45 46 47 84 85 86 26 27 28 29 48 58 68 78 88))
      ((= rnum 3)(list 38 48 76 77  4  5  6 97 98 99 10 20 30 40 32 42 52 62 72))
      ((= rnum 4)(list 14 24 16 17 25 26 27 62 72 82 18 28 38 48  4  5  6  7  8)) 
      ((= rnum 5)(list 53 63 71 81 11 12 13 76 77 78 27 37 47 57 50 60 70 80 90))
      ((= rnum 6)(list  1  2 60 70  7  8  9 30 40 50 24 34 44 54 94 95 96 97 98))
      ((= rnum 7)(list  6  7 44 45 28 38 48 65 75 85 21 22 23 24 52 62 72 82 92))
      ((= rnum 8)(list 13 23 52 53 18 19 20 44 45 46 48 58 68 78 71 72 73 74 75))
      ((= rnum 9)(list 51 52 88 89 46 56 66 82 83 84  4 14 24 34  9 19 29 39 49))
    )
  )
  (setq map3
    (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 
          26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50
          51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75
          76 77 78 79 80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95 96 97 98 99
    )
  )
  (setq map4 map3)
  (setq oldmap1 map1)
  (setq oldmap2 map2)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;; Artificial Intelligence ;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;--- Return the horizontal unused cells next to this cell number [hn]
(defun addHorVer(hn / tList nnum)

  ;;;--- Build an empty list to hold the cell numbers
  (setq tList(list))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;
  ;;;  Note:  Taking the following map....                                        
  ;;;
  ;;;        -------------------------------------
  ;;;        |   |   |   | F |   |   |   |   |   |
  ;;;        -------------------------------------
  ;;;        |   |   |   | C |   |   |   |   |   |
  ;;;        -------------------------------------
  ;;;        |   | H | A | X | B | G |   |   |   |
  ;;;        -------------------------------------
  ;;;        |   |   |   | D |   |   |   |   |   |
  ;;;        -------------------------------------
  ;;;        |   |   |   | E |   |   |   |   |   |
  ;;;        -------------------------------------
  ;;;
  ;;;  If X marks the spot where the computer dropped a bomb and hit something,
  ;;;  we want to check the cells located around it to find the ship.  We will
  ;;;  tell the computer to check cell A first, B second, C third, D fourth,
  ;;;  E fifth, F sixth, G seventh, and H last.  These cell numbers will be
  ;;;  stored in a list called TLIST.  This list will be returned to a variable
  ;;;  named BOMBLIST.  BOMBLIST will be erased when a ship sinks or another hit
  ;;;  occurs.  If another hit occurs, a new BOMBLIST will be generated.  The 
  ;;;  BOMBLIST should never contain a cell number that has already been chosen.  
  ;;;  
  ;;;  Ideally, I would like to keep up with the cell numbers that contain bombs,
  ;;;  then revise the BOMBLIST to remove vertical or horizontal cells if I find
  ;;;  two locations on a ship.  For example, if I find a ship at Cells X and A,
  ;;;  then I would remove cells C,D,E & F from the list.  No reason to check them
  ;;;  since I know the ship is horizontal.  This would make the computer much
  ;;;  more efficient on destroying ships.
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  


  ;;;--- If X is greater than 10, there is a cell D
  (if(> hn 10)
    (progn

      ;;;--- Add cell D to the beginning of the list if it has not been selected before
      (if(member (- hn 10) map4)(setq tList(append (list (- hn 10))tList)))

      ;;;--- If X is greater than 20 then there is a cell E
      (if(> hn 20)

        ;;;--- Add cell E to the end of the list if it has not been selected before
        (if(member (- hn 20) map4)(setq tList(append tList(list (- hn 20)))))     
      )
    )
  )

  ;;;--- If X is greater than 91 then cell C exist
  (if(< hn 91)
    (progn

      ;;;--- Add cell C to the beginning of the list if it has not been selected before
      (if(member (+ hn 10) map4)(setq tList(append (list (+ hn 10))tList)))

      ;;;--- If X is less than 81 then cell F exist
      (if(< hn 81)

        ;;;--- Add cell F to the end of the list if it has not been selected before         
        (if(member (+ hn 20) map4)(setq tList(append tList(list (+ hn 20)))))
      )
    )
  )

  ;;;--- If X is not one of these numbers then cell B exist
  (if(and(/= hn 10)(/= hn 20)(/= hn 30)(/= hn 40)(/= hn 50)(/= hn 60)(/= hn 70)(/= hn 80)(/= hn 90))
    (progn

      ;;;--- Add cell B to the beginning of the list if it has not been selected before
      (if(member (+ hn 1) map4)(setq tList(append (list (+ hn 1))tList)))

      ;;;--- If X is not one of these numbers then cell G exist
      (if(and(/= hn 9)(/= hn 19)(/= hn 29)(/= hn 39)(/= hn 49)(/= hn 59)(/= hn 69)(/= hn 79)(/= hn 89)(/= hn 99))

        ;;;--- Add cell G to the end of the list if it has not been selected before
        (if(member (+ hn 2) map4)(setq tList(append tList(list (+ hn 2)))))
      )
    )
  )

  ;;;--- If X is not one of these cell numbers then there is a cell A
  (if(and(/= hn 1)(/= hn 11)(/= hn 21)(/= hn 31)(/= hn 41)(/= hn 51)(/= hn 61)(/= hn 71)(/= hn 81)(/= hn 91))
    (progn

      ;;;--- Add cell A to the beginning of the list if it hasn't been selected before
      (if(member (- hn 1) map4)(setq tList(append (list (- hn 1))tList)))

      ;;;--- If X is not one of these numbers then cell H exist
      (if(and (/= hn 2)(/= hn 12)(/= hn 22)(/= hn 32)(/= hn 42)(/= hn 52)(/= hn 62)(/= hn 72)(/= hn 82)(/= hn 92))

        ;;;--- Add cell H to the end of the list if it hasn't been selected 
        (if(member (- hn 2) map4)(setq tList(append tList(list (- hn 2)))))
      )
    )
  )

  ;;;--- Return the list of cell numbers to bomb next
  tList
)




;;;--- Set up a new BOMBLIST
;;;    cn = cell number of last bombed cell

(defun setupBombNextList(cn / x y num)
  
  ;;;--- Get the horizontal and vertical cells next to this cell
  (if (setq nnum(addHorVer cn))

    ;;;--- Save the returned list [tList] to BOMBLIST 
    (setq bombList(append bombList nnum))
  )
)

;;;--- Function to replace a member in the bomblist
(defun replBomb(b chk / a c d f g h i j k l m)
  (setq d chk)
  (setq f(- d 1) g(+ d 1) h(- d 2) i(+ d 2) j(+ d 10) k(- d 10) l(+ d 20) m(- d 20))
  (if(member d b)(setq b(subst 0 d b)))
  (if(member f b)(setq b(subst 0 f b)))
  (if(member g b)(setq b(subst 0 g b)))
  (if(member h b)(setq b(subst 0 h b)))
  (if(member i b)(setq b(subst 0 i b)))
  (if(member j b)(setq b(subst 0 j b)))
  (if(member k b)(setq b(subst 0 k b)))
  (if(member l b)(setq b(subst 0 l b)))
  (if(member m b)(setq b(subst 0 m b)))
  (setq c b)
  (setq b(list))
  (foreach a c
     (if(/= a 0)
       (setq b(append b (list a)))
     )
  )
  b
)

;;;--- Function to remove Bomb locations after a ship sinks
;;;    b = bomblist   s = ship number
(defun fixBombList(b s / a c d)

  ;;;--- Check ship 1
  (if(= s 1)
    (progn
      (setq b(replBomb b (nth 0 oldmap2)))
      (setq b(replBomb b (nth 1 oldmap2)))
    )
  )
  ;;;--- Check ship 2
  (if(= s 2)
    (progn
      (setq b(replBomb b (nth 2 oldmap2)))
      (setq b(replBomb b (nth 3 oldmap2)))
    )
  )
  ;;;--- Check ship 3
  (if(= s 3)
    (progn
      (setq b(replBomb b (nth 4 oldmap2)))
      (setq b(replBomb b (nth 5 oldmap2)))
      (setq b(replBomb b (nth 6 oldmap2)))
    )
  )
  ;;;--- Check ship 4
  (if(= s 4)
    (progn
      (setq b(replBomb b (nth 7 oldmap2)))
      (setq b(replBomb b (nth 8 oldmap2)))
      (setq b(replBomb b (nth 9 oldmap2)))
    )
  )
  ;;;--- Check ship 5
  (if(= s 5)
    (progn
      (setq b(replBomb b (nth 10 oldmap2)))
      (setq b(replBomb b (nth 11 oldmap2)))
      (setq b(replBomb b (nth 12 oldmap2)))
      (setq b(replBomb b (nth 13 oldmap2)))
    )
  )
  ;;;--- Check ship 6
  (if(= s 6)
    (progn
      (setq b(replBomb b (nth 14 oldmap2)))
      (setq b(replBomb b (nth 15 oldmap2)))
      (setq b(replBomb b (nth 16 oldmap2)))
      (setq b(replBomb b (nth 17 oldmap2)))
      (setq b(replBomb b (nth 18 oldmap2)))
    )
  )
  b
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;; End of Artificial Intelligence ;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;--- Function to let the user select a cell
;;;    r = row height

(defun getAnswer( r / a p c)

  ;;;--- Ask the user to pick a cell
  (princ (strcat "\n.\n.\n Pick a Cell : "))

  ;;;--- Use grread to get a point
  (setq a(grread nil 4 2))

  ;;;--- If it was a mouse click...
  (if(= (type (cadr a)) 'LIST)
    (progn

      ;;;--- Get the selected point
      (setq p(cadr a))

      ;;;--- Check to make sure the user selected a cell on the grid
      (setq xdis(- (car p)(car lbl)))
      (setq ydis(- (cadr p)(cadr lbl)))
      (if(and (< xdis (* r 10.0)) (> xdis 0) (< ydis (* r 10.0)) (> ydis 0))

        ;;;--- Convert the selected point to a cell number
        (setq c(SPT2CN p lbl r))

        (progn         

          ;;;--- Check to see if the user selected a cell on the wrong grid
          (setq xdis(- (car p)(car rbl)))
          (setq ydis(- (cadr p)(cadr rbl)))
          (if(and (< xdis (* r 10.0)) (> xdis 0) (< ydis (* r 10.0)) (> ydis 0))
            (progn

              ;;;--- Alert the user
              (alert "You are not allowed to bomb your own ships! \nSelect a cell in the other grid.")

              (setq c 101)
  
            )
            (setq c 0)
          )
        )
      )
    )
    (setq c 0)
  )

  ;;;--- Return the cell number selected
  c

)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;--- Replace the -1's indicating a ship sunk with zeros
;;;    map = ship map      s = ship number  
(defun fixMap(map s)

  (cond
    ( (= s 1) (setq map(append (list 0 0) (cddr map))) )
    ( (= s 2) (setq map(append(list (car map) (cadr map) 0 0) (cddddr map))) )
    ( (= s 3) 
      (setq map
        (append (list (nth 0 map)(nth 1 map)(nth 2 map)(nth 3 map) 0 0 0) (cdddr(cddddr map)))
      )        
    )
    ( (= s 4) 
      (setq map
        (list (nth 0 map)(nth 1 map)(nth 2 map)(nth 3 map)(nth 4 map)(nth 5 map)(nth 6 map)
          0 0 0 
          (nth 10 map)(nth 11 map)(nth 12 map)(nth 13 map)
          (nth 14 map)(nth 15 map)(nth 16 map)(nth 17 map)(nth 18 map)
        )
      )
    )
    ( (= s 5) 
      (setq map
        (list (nth 0 map)(nth 1 map)(nth 2 map)(nth 3 map)(nth 4 map)(nth 5 map)(nth 6 map)
           (nth 7 map)(nth 8 map)(nth 9 map)
           0 0 0 0
           (nth 14 map)(nth 15 map)(nth 16 map)(nth 17 map)(nth 18 map)
        )
      )
    )
    ( (= s 6) 
      (setq map
        (list (nth 0 map)(nth 1 map)(nth 2 map)(nth 3 map)(nth 4 map)(nth 5 map)(nth 6 map)
           (nth 7 map)(nth 8 map)(nth 9 map)
           (nth 10 map)(nth 11 map)(nth 12 map)(nth 13 map)
           0 0 0 0 0
        )
      )
    )
  )
  map
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;888;;;;;;;888;;;;;;;;;;888;;;;;;;;;;;8888888;;;;;;8888;;;888;;;;;;;;;;;;
;;;;;;;;8888;;;;;8888;;;;;;;;;88888;;;;;;;;;;;;888;;;;;;;;88888;;888;;;;;;;;;;;;
;;;;;;;;88888;;;88888;;;;;;;;888;888;;;;;;;;;;;888;;;;;;;;888888;888;;;;;;;;;;;;
;;;;;;;;888888;888888;;;;;;;888;;;888;;;;;;;;;;888;;;;;;;;888;888888;;;;;;;;;;;;
;;;;;;;;888;88888;888;;;;;;88888888888;;;;;;;;;888;;;;;;;;888;;88888;;;;;;;;;;;;
;;;;;;;;888;;888;;888;;;;;888;;;;;;;888;;;;;;8888888;;;;;;888;;;8888;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;888;;;;;;;;;;;;888888888;;;;;;;;888888888;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;88888;;;;;;;;;;;888;;;888;;;;;;;;888;;;888;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;888;888;;;;;;;;;;888;;;888;;;;;;;;888;;;888;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;888;;;888;;;;;;;;;888888888;;;;;;;;888888888;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;88888888888;;;;;;;;888;;;;;;;;;;;;;;888;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;888;;;;;;;888;;;;;;;888;;;;;;;;;;;;;;888;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  ;;;--- Zoom to a small area
  (command "_zoom" "_window" (list 0 0) (list 35 24))

  ;;;--- Clear the command Line.
  (princ "\n.\n.\n Starting BSHIP.lsp [ Battleship by JefferyPSanders.com  11/8/03 ]")

  ;;;--- Set the colors for the ships
  (setq c1 252 c2 250 c3 30 c4 1)

  ;;;--- Get the center of the screen
  (setq vc(getvar "viewctr"))

  ;;;--- Get the screen size [y coordinate]
  (setq vs(getvar "viewsize"))

  ;;;--- Find the cell width and height
  (setq rw(/ vs 20.0))

  ;;;--- Find the bottom left point on the left grid
  (setq lbl(polar (polar vc pi (* rw 11.0)) (* pi 1.5) (* rw 5.0)) )

  ;;;--- Find the bottom left point on the right grid
  (setq rbl(polar (polar vc 0 rw) (* pi 1.5) (* rw 5.0)) )

  ;;;--- Set up the grid
  (setUpGrid lbl)

  ;;;--- Set up grid
  (setUpGrid rbl)

  ;;;--- Set up the map
  (setUpMaps)

  ;;;--- Print your ships on the computers bombing area
  (drawShips map2 rbl rw)
  
  ;;;--- Preset an answer variable
  (setq ans "Y")

  ;;;--- Alert the user of the rules...
  (setq ruleStr
    (strcat
      "                     BattleShip Rules"
      "\n------------------------------------------------------------"
      "\n1. Your ships are on the right grid."
      "\n2. The computer's ships are hidden on the left grid."
      "\n3. Select a cell on the left grid to drop a bomb."
      "\n4. Select anywhere off of the grid to end."
      "\n5. You have 2-MineSweepers 2-Frigates 1-Cruiser 1-Battleship"
      "\n6. The computer has the same number of ships."
      "\n7. Sink all ships to win."
      "\n8. You go first then the computer immediately takes a turn."
      "\n "
      "\n   Good luck!"
    )
  )
  (alert ruleStr)

  ;;;--- Set up a list for Artificial Intelligence
  (setq bombList(list))

  ;;;--- Set up a check
  (setq b 0 c 0 lastBomb nil)
  
  ;;;--- While the user selects a point and doesn't hit a key
  (while(and(>(foreach a map1(setq b(+ b a)))0)(>(foreach a map2(setq c(+ c a)))0)(= ans "Y"))

    ;;;--- Let the user select a point
    (setq cellAns(getAnswer rw))

    (if(and(< cellAns 101)(> cellAns 0))
      (progn

        ;;;--- Convert the cell number to a cell point
        (setq cellPt(CN2CPT cellAns lbl rw))

        ;;;--- See if that cell number lands on a ship 
        (if(setq hitShip(member cellAns map1))
          (progn
               
            ;;;--- Draw a bomb
            (drawBomb cellPt rw)
                
            ;;;--- Replace the cell number with -1
            (setq map1(subst -1 cellAns map1))
                   
            ;;;--- See if the ship needs to be sunk
            (setq sunk(chkSunken map1 oldmap1 lbl rw "U"))
                
            ;;;--- Remove the -1's where the ship sunk
            (setq map1(fixMap map1 sunk))
            
          )              

          ;;;--- Else it is a miss, draw a splash
          (drawMiss cellAns lbl map3 rw)
        )

        ;;;--- Remove the selected cell number from the available cell list
        (setq map3(subst -1 cellAns map3))

      )

      (if(= cellAns 101)(setq ans "I")(setq ans "N"))

    )

    ;;;--- Set p some temporary variables for a check
    (setq b 0 c 0)

    ;;;;;;;;;;;;;;;;;;;;;;;; COMPUTER'S TURN ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ;;;--- Computers turn if not exiting and there are ships left to bomb
    (if(and (> (foreach a map1(setq b(+ b a))) 0)(> (foreach a map2(setq c(+ c a))) 0)(= ans "Y"))
      (progn
     
        ;;;--- Let the user know it is the computer's turn
        (princ "\n.\n.\n Computer's Turn!  Please wait...")

        ;;;--- If Artificial Intelligence does not have a bomb-next list then
        (if (not bombList)
          (progn

            ;;;--- Get a random number between 1 and 99 that is in the map4 list [not chosen yet]      
            (while(not(member (setq cCellNum(atoi(substr(rtos (getvar "cdate") 2 18)16 2))) map4))
              (princ ".")
            )
          )
          (progn
            
            ;;;--- Get the next cell to bomb from the Artificial Intelligence bomb list
            (setq cCellNum(car bombList))

            ;;;--- Remove the cell number from the BOMBLIST so we don't use it again
            (setq bombList(cdr bombList))

          )
        )

        ;;;--- Convert the cell number to a cell's bottom left point
        (setq cCellPt(CN2CPT cCellNum rbl rw))

        ;;;--- Check to see if it is a hit or a miss
        (if(setq hitShip(member cCellNum map2))
          (progn

            ;;;--- It is a hit, so draw a bomb
            (drawBomb cCellPt rw)

            ;;;--- We have a hit, use Artificial Intelligence to find the rest of the ship
            (setupBombNextList cCellNum)

            ;;;--- Mark it as a hit!
            (setq map2(subst -1 (car hitShip) map2))
         
            ;;;--- See if the ship needs to be sunk
            (setq sunk2(chkSunken map2 oldmap2 rbl rw "C"))
                
            ;;;--- Remove the -1's where the ship sunk
            (setq map2(fixMap map2 sunk2))

            ;;;--- Remove possible bomb locations from the BOMBLIST
            (setq bombList(fixBombList bombList sunk2))

          )
         
          ;;;--- Else, it is a miss
          (drawMiss cCellNum rbl map4 rw)
        )

        ;;;--- Replace the number from the map4 list
        (setq map4(subst -1 cCellNum map4))

      )
    )

    ;;;--- If the user clicked the wrong grid, don't exit
    (if(= cellAns 101)(setq ans "Y"))
 
    ;;;--- Set some checks to zero before entering the while statement again
    (setq b 0 c 0)
  )

  ;;;--- Print the computers ships on the computers bombing area
  (drawShips oldmap1 lbl rw)  

  ;;;--- Set up a temp variable to check
  (setq b 0)

  ;;;--- If all cell numbers in map1 add up to be less than one, You won.
  (if(< (foreach a map1(setq b(+ b a))) 1)
    (progn
      (alert "You Won!")
      (princ "\n.\n.\n. Press any key to exit...")
      (grread nil)
    )
  )

  ;;;--- Set up a temp variable to check
  (setq b 0)

  ;;;--- If all cell numbers in map2 add up to be less than one, the Computer won.
  (if(< (foreach a map2(setq b(+ b a))) 1)
    (progn
      (alert "You Lost!")
      (princ "\n.\n.\n Press any key to exit...")
      (grread nil)
    )
  )
 
  ;;;--- Erase the grid
  (command "_redraw")

  ;;;--- Clear the command line
  (princ "\n.\n.\n.")

  ;;;--- Suppress the last echo for a clean exit.
  (princ)
)