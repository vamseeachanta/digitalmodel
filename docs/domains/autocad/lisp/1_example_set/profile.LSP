					;Programme for drawing the long&cross Profiles
					;By kumar
					;AL OWAIDAH GEN.CONT.CO
					;P.O BOX 11701
					;NAKHEEL,RAK
					;###############################################

(defun c:profile (/	     BASE1	BASE2	   CH
		  CHAIN	     CHAIN1	CHAIN1TXTPOI
		  CHAINTXTPOI		CHAIPOI	   CHAITXTPOI
		  DATPOI     DATTXTPOI	DISSC	   EXEPOI
		  EXETXTPOI  LASTC	LASTD	   LASTE
		  LASTP	     LEVL	MIDPNT	   LEVL1
		  LEVLTXTPOI PLSC	PRLEVL	   PRLEVL1
		  PRLEVL1TXTPOI		PRLEVLTXTPOI
		  PROPOI     PROTXTPOI	PRPT1	   PRPT2
		  PT1	     PT2	RONA	   STDIS
		  TXTSIZ     XDATUM	YDATUM
		 )
  (alert "Kumar Welcomes You in Profile if any Problem contact 050-5402083")
  (setvar "cmdecho" 0)
  (setvar "osmode" 0)
  (setq rona (getstring "\nenter the road name:"))
  (setq	ch (getreal
	     "\nenter the chainage:"
	   ) ;_ end of getreal
  ) ;_ end of setq
  (setq plsc (getreal "\nenter the plotting scale:"))
  (initget 1)
  (setq ydatum (getreal "\nenter the datum:"))
  (setq txtsiz (* 2 plsc))
  (setq dissc (* 1 plsc))
  (setq stdis (* txtsiz 6))
  (setq chain (getreal "\nenter the least offset or -999 to quit:"))
  (if (/= chain -999)
    (progn
      (initget 1)
      (setq levl (getreal "\nenter the existing level:"))
      (setq prlevl (getreal "\nenter the prprosed level:"))
      (setq xdatum (- chain (* 30 plsc)))
      (setq pt1 (list chain levl))
      (setq datpoi (list xdatum ydatum))
      (setq dattxtpoi (list (+ xdatum dissc) (+ ydatum dissc)))
      (setq propoi (list xdatum (- ydatum stdis)))
      (setq
	protxtpoi (list (+ xdatum dissc) (+ dissc (- ydatum stdis)))
      ) ;_ end of setq
      (setq exepoi (list xdatum (- ydatum (* 2 stdis))))
      (setq exetxtpoi (list (+ xdatum dissc)
			    (+ dissc (- ydatum (* 2 stdis)))
		      ) ;_ end of list
      ) ;_ end of setq
      (setq chaipoi (list xdatum (- ydatum (* 3 stdis))))
      (setq chaitxtpoi
	     (list (+ xdatum dissc)
		   (+ dissc (- ydatum (* 3 stdis)))
	     ) ;_ end of list
      ) ;_ end of setq
      (setq chaintxtpoi
	     (list (- chain dissc)
		   (+ dissc (- ydatum (* 3 stdis)))
	     ) ;_ end of list
      ) ;_ end of setq
      (setq levltxtpoi
	     (list (- chain dissc)
		   (+ dissc (- ydatum (* 2 stdis)))
	     ) ;_ end of list
      ) ;_ end of setq
      (setq base1 (list chain (- ydatum (* 3 stdis))))
      (command "style" "roms" "romans" txtsiz 0.8 "" "" "" "")
      (command "layer" "m" "exe" "c" 5 "" "")
      (command "layer" "m" "pro" "c" 6 "" "")
      (command "layer" "m" "txt" "c" 3 "" "")
      (command "layer" "m" "oth" "c" 1 "" "")
      (command "layer" "s" "txt" "")
      (if prlevl
	(progn
	  (setq prpt1 (list chain prlevl))
	  (setq	prlevltxtpoi
		 (list (- chain dissc) (+ dissc (- ydatum (* 1 stdis))))
	  ) ;_ end of setq
	  (command "text" prlevltxtpoi 90 (rtos prlevl 2 3))
	  (command "layer" "s" "oth" "")
	  (command "line" pt1 prpt1 ^c)
	) ;_ end of progn
      ) ;_ end of if
      (command "layer" "s" "txt" "")
      (command "text"
	       dattxtpoi
	       0
	       (strcat "Datum=" (rtos ydatum 2 3))
      ) ;_ end of command
      (command "text" protxtpoi 0 "Proposed level")
      (command "text" exetxtpoi 0 "Existing level")
      (command "text" chaitxtpoi 0 "Offset")
      (command "text" chaintxtpoi 90 (rtos chain 2 3))
      (command "text" levltxtpoi 90 (rtos levl 2 3))
      (command "layer" "s" "oth" "")
      (command "line" pt1 base1 ^c)
      (command "line" datpoi chaipoi ^c)
      (setq chain1 (getreal "\nenter next offset or -999 to quit:"))
      (while (/= chain1 -999)
	(initget 1)
	(setq levl1 (getreal "\nenter the Existing level:"))
	(setq pt2 (list chain1 levl1))
	(setq prlevl1 (getreal "\nenter the prprosed level:"))
	(if prlevl1
	  (progn
	    (setq prlevl1txtpoi
		   (list (- chain1 dissc) (+ dissc (- ydatum (* 1 stdis))))
	    ) ;_ end of setq
	    (setq prpt2 (list chain1 prlevl1))
	    (command "layer" "s" "pro" "")
	    (if	prpt1
	      (command "line" prpt1 prpt2 ^c)
	      (setq prpt1 prpt2)
	    ) ;_ end of if
	    (command "layer" "s" "oth" "")
	    (command "line" pt2 prpt2 ^c)
	    (command "layer" "s" "txt" "")
	    (command "text" prlevl1txtpoi 90 (rtos prlevl1 2 3))
	  ) ;_ end of progn
	) ;_ end of if
	(setq base2 (list chain1 (- ydatum (* 3 stdis))))
	(setq chain1txtpoi
	       (list (- chain1 dissc) (+ dissc (- ydatum (* 3 stdis))))
	) ;_ end of setq
	(setq levltxtpoi
	       (list (- chain1 dissc)
		     (+ dissc (- ydatum (* 2 stdis)))
	       ) ;_ end of list
	) ;_ end of setq
	(command "layer" "s" "txt" "")
	(command "text" chain1txtpoi 90 (rtos chain1 2 3))
	(command "text" levltxtpoi 90 (rtos levl1 2 3))
	(command "layer" "s" "exe" "")
	(command "line" pt1 pt2 ^c)
	(command "layer" "s" "oth" "")
	(command "line" pt2 base2 ^c)
	(setq chain chain1)
	(setq levl levl1)
	(setq pt1 pt2)
	(setq prpt1 prpt2)
	(setq
	  chain1 (getreal "\nenter the next offset or -999 quit:")
	) ;_ end of setq
      ) ;_ end of while
      (if (= chain1 -999)
	(progn
	  (setq lastd (list chain ydatum))
	  (setq lastp (list chain (- ydatum stdis)))
	  (setq laste (list chain (- ydatum (* 2 stdis))))
	  (setq lastc (list chain (- ydatum (* 3 stdis))))
	  (command "layer" "s" "oth" "")
	  (command "line" datpoi lastd ^c)
	  (command "line" propoi lastp ^c)
	  (command "line" exepoi laste ^c)
	  (command "line" chaipoi lastc ^c)
	  (setq	midpnt (list (/ (+ (nth 0 chaipoi) (nth 0 lastc)) 2)
			     (- ydatum (* stdis 4))
		       ) ;_ end of list
	  ) ;_ end of setq
	  (command "layer" "s" "txt" "")
	  (command "text"
		   "j"
		   "m"
		   midpnt
		   0
		   (strcat "%%U" rona (rtos ch 2 3))
	  ) ;_ end of command
	) ;_ end of progn
      ) ;_ end of if

    ) ;_ end of progn
  ) ;_ end of if
) ;_ end of defun


