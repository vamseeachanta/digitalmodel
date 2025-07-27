;This File contains two functions C:TRIMM and C:XTEND which are designed to 
;emulate the AutoCAD Trim and Extend Functions except that it is possible to 
;select entities as cutting edges or boundary edges which are nested within 
;Blocks or Xrefs, even Blocks or XREF's with unequal XYZ values, although 
;Arcs, Circles and Text nested within unequal blocks are currently error 
;trapped until I resolve how to recreate their geometry. Also you may find
;problems with entities disappearing after a transparent zoom. These entities
;will reappear after a redraw. If you select a text entity as the cutting
;edge a box will be drawn around the text and used as a cutting edge. For those
;don't want to trim in this situation the code could easily be adapted to
;insert a 3DFace instead of a Pline and then do a hidden line plot.
;
;Acknowledgements to Tony Tanzillo for help in converting coords from WCS to
;ECS and for the streamlined MCS to WCS conversion. The method of measuring 
;the length of text entities is as described in Cadalyst Aug 91.
;
;               ***IMPORTANT WARNING TO UK USERS***
;Any UK Users using the Compiled Lisp Overlay
;should first check that they have a recent copy of the ACADL.EXP File. 
;First save your drawing then type (nentsel) and hit return without picking
;a point. If you get a "bad argument type" or more likely a system hang then
;you should upgrade your ACADL.EXP before running this programme. Versions
;of the ACADL.EXP prior to June 91 can cause system hangs if null input
;is fed to (nentsel)
;
;Both C:TRIMM and C:XTEND call a dual action function (action) which will 
;either trim or extend depending on value of arguments. (action) calls 
;(getss) which loops until a selection set of cutting/boundary edges has
;been collected. If the entity selected is nested then (getss) calls (makent) 
;which will make a new temporary entity in the drawing as the cutting/boundary
;edge, (converting the coords from MCS to ECS). The original block remains
;unaffected. If the entity selected is nested and is a polyline then (getss) 
;calls (ploop) which loops through the entities and prepares a list of the 
;header, vertices and seqend; before calling (makent) to make the new 
;polyline. Temporary entities created by (makent) are collected in a list 
;"dellst" and deleted by (restore) which is called on completion or by 
;the error handler. Entities affected by deletions are added to "drwlst",
;and these entities are redrawn on completion or error.
;
;GLOBAL VARIABLES
;
;	ndata	results of (nentsel)
;	mx	matrix from (nentsel) used by (mcs2wcs)
;	sstrim	selection set of cutting/boundary edges
;	dellst	list of temporary entities for deletion on completion
;	drwlst	list of entities to be redrawn on completion
;	lst	list of elements in an entity or a list of vertices if
;		the element is a polyline
;	exvx	extrusion vector (210) of parent block if entity is nested
;
;Ian Thomson, Reigate, Surrey, UK.  (100014,106)
;
;-------------------------- INTERNAL ERROR HANDLER ------------------
(defun newerr (s)	          	; If an error (such as CTRL-C) occurs
                                      	; while this command is active...
  (if (/= s "Function cancelled")
    (princ (strcat "\nError: " s))
  );end if
(restore)    ;delete created entities and redraw entities affected by delete
  (setq *error* olderr)			; restore old *error* handler
  (princ)
);end defun newerr
;--------------------------------------------------------------------
(defun restore ( / n)			;Clean up on completion or error
(setvar "highlight" 1)
(setvar "cmdecho" 1)
(foreach n dellst (entdel n))		;delete any entities created
(foreach n drwlst (redraw n))		;redraw any entities affected
(setq 	sstrim 	nil
	dellst	nil
	drwlst	nil
	ndata	nil
	mx	nil
	exvx	nil
);end setq
);end defun
;-------------------------------------
(defun mcs2wcs (point)
   (  (lambda (p)
         (mapcar
            '(lambda (place)
                (apply '+ (mapcar '* (mapcar place mx) p)))
            '(car cadr caddr)))
      (append point (list 1)))
);end defun
;-------------------------------------
(defun nstdxf (fnc code)	;Returns cumulative dxf group of nested ent
(apply fnc (mapcar '(lambda (s) (val code (entget s))) (cadddr ndata)))
);end defun
;---------------------------------------------------
(defun makent (item / new)		;create a new entity as a cutting edge
(setq 	new item)		;step through list and translate (xlate) 
(foreach n item (xlate n))	;each dxf group code in list
(entmake new)			;make a new temporary entity for use
);end defun makent		;as a temporary cutting/boundary edge
;--------------------------------------
(defun subdxf (code new item)
(subst (cons code new) (assoc code item) item)
);end defun
;-------------------------------------
(defun laydxf (lay / indx newlay)
(setq indx 2)
(while	(/= "|" (substr lay indx 1))
	(setq indx (1+ indx))
);end while
(setq newlay (substr lay (1+ indx)))
);end defun
;------------------------------------------------
(defun xlate (a / newpt nstblk)
(if 	(and	(listp (cdr a))		;if item is a point
		(/= (car a) 210)	;and not group code 210
	);and
	(progn
	(if (= typ "LINE")		;Dont change ECS of Lines
		(setq newpt (mcs2wcs (cdr a)))
		(setq newpt (trans (mcs2wcs (cdr a)) 0 exvx))
	);end if
	(setq new (subdxf (car a) newpt new))
	);end progn			;translate point to new coords
);end if
;-----
	(if 	(and			;Change Start and end angles of arc
			(= typ "ARC")
			(or
			(= (car a) 50)
			(= (car a) 51)
			);end or
		);end and
	(setq new (subdxf (car a) (+ (cdr a) (nstdxf '+ 50)) new))
	);end if
;-----					;Adjust for nested scale
	(if	(and
			(or
			(= typ "CIRCLE")
			(= typ "ARC")
			);end or
			(= (car a) 40)
		);end and
	(setq new (subdxf (car a) (* (cdr a) (nstdxf '* 41)) new))
	);end if
;-----
(if 	(and
	(= (car a) 8)
	(= (cdr a) "0")
	);end and
	(progn
	(setq 	nstblk (entget (car (cadddr ndata)))
		new (subdxf 8 (val 8 nstblk) new)
	);end setq
	(if (val 62 nstblk)
		(setq	new (subdxf 62 (val 62 nstblk) new))
	);end if
	);end progn

);end if
;-----
(if 	(and				;Change layers if entity is an XREF
		(= (car a) 8)
		(or
			(wcmatch (cdr a) "*|*")
			(wcmatch (val 8 new) "*|*")
		);end or
	);end and
	(if (wcmatch (cdr a) "*|*")
		(setq 	new 	(subdxf 8 (laydxf (cdr a)) new))
		(setq 	new 	(subdxf 8 (laydxf (val 8 new)) new))
	);end if
);end if
;-----
(if 	(and				;Change Colour to Bylayer
	(= (car a) 62)
	(= (cdr a) 0)			;if colour is Byblock
	);end and
	(if (= (cdr a) (val 62 new))	;check that colour hasn't already
	(setq new (subdxf 62 256 new))	;been changed
	);end if
);end if
;-----
(if (= (car a) 210)			;translate extrusion vertex
	(setq new (subdxf (car a) exvx new))
);end if
);end defun xlate
;---------------------------------------
(defun val ( n lst / val )
(setq val (cdr (assoc n lst)))
);end defun
;--------------------------------------
(defun beep (n)
(if (null n) (setq n 10))
	(repeat n 
	(write-char 7 )
	(write-char 8 )
	);end repeat
(princ)
);end defun beep
;---------------------------------------------------
(defun boxtxt (e1 / ang ang1 ang2 e5 ent1 ent2 leng1 offst 
		p1 p2 p3 p4 p5 p6 p7 sc)
(setq	ent1	(entget e1)
	offst	(/ (val 40 ent1) 2)
	ang	(val 50 ent1)
);end setq
(if mx
(progn
	(setq	ang	(+ (val 50 ent1) (nstdxf '+ 50))
		p1	(mcs2wcs (val 10 ent1))
		p7	(trans p1 0 1)
		p1	(trans p1 0 exvx)
		ent1	(subdxf 10 p1 ent1)
		ent1	(subdxf 50 ang ent1)
		ent1	(subdxf 210 exvx ent1)
		sc	(nstdxf '* 41)
		ent1	(subdxf 40 (* sc (val 40 ent1)) ent1)
		offst	(* offst sc)
	);end setq
	(entmake ent1)
	(setq 	e5 (entlast)
		ent1 (entget e5)
	);end setq
);end progn
);end if
;-----
(setq	ent2	ent1
	ent1	(subdxf 72 2 ent1)
);end setq
(entmod ent1)
(setq 	ent1 	(if mx
			(entget e5)
			(entget e1)
		);end if
	p5	(val 10 ent1)
	p6	(val 11 ent1)
	length	(distance p5 p6)
	ent1	ent2
);end setq
(entmod ent1)
(if	(= 0 (getvar "worlducs"))
	(setq 	ang1	(- ang pi)
	);end setq
);end if
(if (null mx )
	(setq	ang1	(if (= 0 (getvar "worlducs"))
				(- ang pi)
				ang
			);end if
		p1	(trans (val 10 ent1) (val 210 ent1) 1)
	);end setq
	(setq 	p1 	p7)
);end if
(setq 	
	ang2	(+ ang1 (/ pi 2))
	p1 	(polar p1 (+ ang1 pi) offst)
	p1	(polar p1 (+ ang2 pi) offst)
	p2	(polar p1 ang1 (+ length (* 2 offst)))
	leng1	(* 4 offst)
	p3	(polar p2 ang2 leng1)
	p4	(polar p1 ang2 leng1)
);end setq
(command	".pline" p1 p2 p3 p4 "c"
);end command
(if e5 (entdel e5)
);end if
);end defun
;---------------------------------------------------
(defun ploop (ent)			
(while (/= "SEQEND" (val 0 (entget ent)))	;step through entities
	(setq ent (entnext ent))		;till POLYLINE header
);end while
(setq 	ent (val -2 (entget ent))
	lst (list (entget ent))		
);end setq				;create a list of POLYLINE vertices
(while (/= "SEQEND" (val 0 (entget ent)))
(setq 	ent (entnext ent)
	lst (cons (entget ent) lst)
);end setq
);end while
(setq lst (reverse lst))
(foreach n lst (makent n)		;step through each entity of the
);end foreach				;polyline and convert model to WCS
);end defun
;--------------------------------------
(defun ploop2 (ent2)			
(while (/= "SEQEND" (val 0 (entget ent2)))	;step through entities
	(setq ent2 (entnext ent2))		;till POLYLINE header
);end while
(setq 	ent2 (val -2 (entget ent2))
);end setq				;create a list of POLYLINE vertices
);end defun
;--------------------------------------
(defun getss (msg1 / e1 e4 lst typ fail)			
(setq 	ndata (nentsel (strcat "\nSelect " msg1 " Edge \n"))
	e1 (car ndata)
	mx (caddr ndata)	;used by (mcs2wcs)
	lst nil			;reset list 
	fail nil
);end setq
(if ndata			;create selection set of cutting edges
(progn
(setq	typ (val 0 (entget e1))
);end setq
(if mx
(progn
	(setq	exvx (val 210 (entget (last (cadddr ndata))))
	);end setq
	(cond 	(	(and
				(/= (nstdxf '* 41) (nstdxf '* 42))
				(or 
					(= typ "ARC")
					(= typ "CIRCLE")
					(= typ "TEXT")
				);end or
			);end and
			(beep 20)
			(princ 	(strcat
				"\nCan't select "
				typ
				" in an unequal block as "
				msg1
				" edge."
				);end strcat
			);end princ
			(setq fail T)
		)
		((= typ "VERTEX")
			(ploop e1)
		)
		((= typ "TEXT")
			(boxtxt e1)
		)
		(t
			(setq lst (entget e1))
			(makent lst)
		)
	);end cond
	(if (null fail)
	(progn
	(setq 	e4 (entlast)
		sstrim (ssadd e4 sstrim)	;cutting edges
		dellst (cons e4 dellst)		;new ents to delete
		drwlst (cons (last (cadddr ndata)) drwlst);ents to redraw
	);end setq
	(redraw e4 3)
	);end progn
	);end if
);end progn
(progn
	(cond
	((= typ "TEXT")
		(boxtxt e1)
		(setq 	e4 (entlast)
			dellst (cons e4 dellst)
		);end setq
	)
	((= typ "VERTEX")
		(setq e4 (ploop2 e1))
	)
	(t
		(setq	e4 e1)
	)
	);end cond
	(setq	sstrim (ssadd e4 sstrim)
		drwlst (cons e4 drwlst)
	);end setq
	(redraw e4 3)
);end progn
);end if
);end progn
);end if
(princ)		
);end defun getss
;--------------------------------------
(defun action (msg msg1 / p)	;dual action function which will either
(setvar "highlight" 0)		;TRIM or EXTEND depending on argument msg
(setvar "cmdecho" 0)
(command ".undo" "m")
(setq	olderr *error*
	*error* newerr
	sstrim (ssadd)
	dellst nil
	drwlst nil
	ndata T
);end setq
(while ndata			;loop until sel set of cutting/boundary
	(getss msg1)		;edges has been collected
);end while
(if (> (sslength sstrim) 0)	;abort if no cutting/boundary edges selected
(progn
(command 	(strcat "." msg)	;TRIM or EXTEND
		sstrim			;sel set prepared by (getss)
		""
);end command
(prompt "\nSelect Object to ")(princ msg)(princ ": ")	;"trim or extend"
(while (setq p (entsel "")) 
	(cond
		(  (member (car p) dellst)	;check it is not one of the
		(beep 20)			;temporary entities
		(princ (strcat "Can't " msg " entity nested in Block, "
				"\nSelect object to " msg ": "))
		)
		(t (command p)
		)
	);end cond
);end while
(command "")
(restore)				;clean up on completion
);end progn
);end if
(princ)
);end defun
;--------------------------------------
(defun c:trimm ()
(action "trim" "cutting")
(princ)
);end defun
;--------------------------------------
(defun c:xtend ()
(action "extend" "boundary")
(princ)
);end defun