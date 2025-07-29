;;;   CHTEXT.lsp
;;;--------------------------------------------------------------------------
;;; DESCRIPTION
;;;   This is a "text processor" which operates in a global manner 
;;;   on all of the text entities that the user selects; i.e., the
;;;   Height, Justification, Location, Rotation, Style, Text, and
;;;   Width can all be changed globally or individually, and the
;;;   range of values for a given parameter can be listed.
;;;   
;;;   The command is called with CHT from the command line at which
;;;   time the user is asked to select the objects to change.
;;;   
;;;     Select text to change. 
;;;     Select objects:

(defun chtxt (/ sset opt ssl nsset temp unctr ct_ver cht_er cht_oe
                sslen style hgt rot txt ent cht_oc cht_ot cht_oh
                loc loc1 justp justq orthom )

  (setq ct_ver "1.02")                ; Reset this local if you make a change.
  ;;
  ;; Internal error handler defined locally
  ;;
  (defun cht_er (s)                   ; If an error (such as CTRL-C) occurs
                                      ; while this command is active...
    (if (/= s "Function cancelled")
      (if (= s "quit / exit abort")
        (princ)
        (princ (strcat "\nError: " s))
      )
    )
    (eval(read U:E))
    (if cht_oe                        ; If an old error routine exists
      (setq *error* cht_oe)           ; then, reset it 
    )
    (if temp (redraw temp 1))
    (if cht_oc (setvar "cmdecho" cht_oc)) ; Reset command echoing
    (if cht_ot (setvar "texteval" cht_ot)) 
    (if cht_oh (setvar "highlight" cht_oh)) 
    (princ)
  )
  ;;
  ;; Body of function
  ;;
  (if *error*                         ; Set our new error handler
    (setq cht_oe *error* *error* cht_er) 
    (setq *error* cht_er) 
  )

  ;; Set undo groups and ends with (eval(read U:G)) or (eval(read U:E))
  (setq U:G "(command \"undo\" \"group\")"
        U:E "(command \"undo\" \"en\")"
  )
  
  (setq cht_oc (getvar "cmdecho"))
  (setq cht_oh (getvar "highlight")) 
  (setvar "cmdecho" 0)
  
  (eval(read U:G))

  (princ (strcat "\nChange text, Version " ct_ver 
                 ", (c) 1990-1991 by Autodesk, Inc. "))
  (prompt "\nSelect text to change. ")
  (setq sset (ssget))
  (if (null sset) 
    (progn
      (princ "\nERROR: Nothing selected.")
      (exit)
    )
  )
  ;; Verify the entity set.
  (cht_ve)
  ;; This is the main option loop.
  (cht_ol)

  (if cht_oe (setq *error* cht_oe))   ; Reset old error function if error
  (eval(read U:E))
  (if cht_ot (setvar "texteval" cht_ot)) 
  (if cht_oh (setvar "highlight" cht_oh)) 
  (if cht_oc (setvar "cmdecho" cht_oc)) ; Reset command echoing
  (princ)
)
;;;
;;; Verify and sort out non-text entities from the selection set.
;;;
(defun cht_ve ()
  (setq ssl   (sslength sset)
        nsset (ssadd))
  (if (> ssl 25)
    (princ "\nVerifying the selected entities -- please wait. ")
  )
  (while (> ssl 0)
    (setq temp (ssname sset (setq ssl (1- ssl))))
    (if (= (cdr(assoc 0 (entget temp))) "TEXT")
      (ssadd temp nsset)
    )
  )
  (setq ssl (sslength nsset)
        sset nsset
        unctr 0
  )
  (print ssl)
  (princ "text entities found. ")
)
;;;
;;; The option loop.
;;;
(defun cht_ol ()
  (setq opt T)
  (while (and opt (> ssl 0))
    (setq unctr (1+ unctr))
    (command "_.UNDO" "_GROUP")
    (initget "Location Justification Style Height Rotation Width Text Undo")
    (setq opt (getkword 
      "\nHeight/Justification/Location/Rotation/Style/Text/Undo/Width: "))
    (if opt
      (cond
        ((= opt "Undo")
          (cht_ue)                    ; Undo the previous command.
        )
        ((= opt "Location")
          (cht_le)                    ; Change the location.
        )
        ((= opt "Justification")
          (cht_je)                    ; Change the justification.
        )
        ((= opt "Style")    (cht_pe "Style"    "style name"      7) )
        ((= opt "Height")   (cht_pe "Height"   "height"         40) )
        ((= opt "Rotation") (cht_pe "Rotation" "rotation angle" 50) )
        ((= opt "Width")    (cht_pe "Width"    "width factor"   41) )
        ((= opt "Text")
          (cht_te)                    ; Change the text.
        )
      )
      (setq opt nil)
    )
    (command "_.UNDO" "_END")
  )
)
;;;
;;; Undo an entry.
;;;
(defun cht_ue ()
  (if (> unctr 1)
    (progn
      (command "_.UNDO" "_END")
      (command "_.UNDO" "2")
      (setq unctr (- unctr 2))
    )
    (progn
      (princ "\nNothing to undo. ")
      (setq unctr (- unctr 1))
    )
  )
)
;;;
;;; Change the location of an entry.
;;;
(defun cht_le ()
  (setq sslen (sslength sset)
        style ""
        hgt   ""
        rot   ""
        txt   ""
  )
  (command "_.CHANGE" sset "" "")
  (while (> sslen 0)
    (setq ent (entget(ssname sset (setq sslen (1- sslen))))
          opt (list (cadr (assoc 11 ent))
                    (caddr (assoc 11 ent))
                    (cadddr (assoc 11 ent)))
    )
    (prompt "\nNew text location: ")
    (command pause)
    (if (null loc)
      (setq loc opt)
    )
    (command style hgt rot txt)
  )
  (command)
)
;;;
;;; Change the justification of an entry.
;;;
(defun cht_je ()
  (if (getvar "DIMCLRD")
    (initget (strcat "TLeft TCenter TRight "
                     "MLeft MCenter MRight "
                     "BLeft BCenter BRight "
                     "Aligned Center Fit Left Middle Right ?"))
    (initget "Aligned Center Fit Left Middle Right ?")
  )
  (setq sslen (sslength sset))
  (setq justp (getkword (strcat "\nJustification point(s) - "
              "Aligned/Center/Fit/Left/Middle/Right/<?>: ")))
  (cond
    ((= justp "Left")    (setq justp 0 justq 0) )
    ((= justp "Center")  (setq justp 1 justq 0) )
    ((= justp "Right")   (setq justp 2 justq 0) )
    ((= justp "Aligned") (setq justp 3 justq 0) )
    ((= justp "Fit")     (setq justp 5 justq 0) )
    ((= justp "TLeft")   (setq justp 0 justq 3) )
    ((= justp "TCenter") (setq justp 1 justq 3) )
    ((= justp "TRight")  (setq justp 2 justq 3) )
    ((= justp "MLeft")   (setq justp 0 justq 2) )
    ((= justp "Middle")  (setq justp 4 justq 0) )
    ((= justp "MCenter") (setq justp 1 justq 2) )
    ((= justp "MRight")  (setq justp 2 justq 2) )
    ((= justp "BLeft")   (setq justp 0 justq 1) )
    ((= justp "BCenter") (setq justp 1 justq 1) )
    ((= justp "BRight")  (setq justp 2 justq 1) )
    ((= justp "?")       (setq justp nil)       )
    (T                   (setq justp nil)       )
  )   
  (if justp
    (justpt) ; Process them...
    (justpn) ; List options...
  )
  (command)
)
;;;
;;; Get alignment points for "aligned" or "fit" text.
;;;
(defun justpt ()
  (while (> sslen 0)
    (setq ent (entget(ssname sset (setq sslen (1- sslen))))
          ent (subst (cons 72 justp) (assoc 72 ent) ent)
          opt (trans (list (cadr (assoc 11 ent))
                           (caddr (assoc 11 ent))
                           (cadddr (assoc 11 ent))) 
                     (cdr(assoc -1 ent)) ; from ECS
                     1)               ; to current UCS
    )
    (if (getvar "DIMCLRD")
      (setq ent (subst (cons 73 justq) (assoc 73 ent) ent))
    )
    (cond
      ((or (= justp 3) (= justp 5))
        (prompt "\nNew text alignment points: ")
        (if (= (setq orthom (getvar "orthomode")) 1)
          (setvar "orthomode" 0)
        )
        (redraw (cdr(assoc -1 ent)) 3)
        (initget 1)
        (setq loc (getpoint))
        (initget 1)
        (setq loc1 (getpoint loc))
        (redraw (cdr(assoc -1 ent)) 1)
        (setvar "orthomode" orthom)
        (setq ent (subst (cons 10 loc) (assoc 10 ent) ent))
        (setq ent (subst (cons 11 loc1) (assoc 11 ent) ent))
      )
      ((or (/= justp 0) (/= justq 0))
        (redraw (cdr(assoc -1 ent)) 3)
        (prompt "\nNew text location: ")
        (if (= (setq orthom (getvar "orthomode")) 1)
          (setvar "orthomode" 0)
        )
        (setq loc (getpoint opt))
        (setvar "orthomode" orthom)
        (redraw (cdr(assoc -1 ent)) 1)
        (if (null loc)
          (setq loc opt)
          (setq loc (trans loc 1 (cdr(assoc -1 ent))))
        )
        (setq ent (subst (cons 11 loc) (assoc 11 ent) ent))
      )
    )
    (entmod ent)
  )
)
;;;
;;; List the options.
;;;
(defun justpn ()
  (if (getvar "DIMCLRD") (textpage))
  (princ "\nAlignment options: ")
  (princ "\n\t TLeft   TCenter   TRight ")
  (princ "\n\t MLeft   MCenter   MRight ")
  (princ "\n\t BLeft   BCenter   BRight ")
  (princ "\n\t  Left    Center    Right")
  (princ "\n\tAligned   Middle    Fit")
  (if (not (getvar "DIMCLRD")) (textscr))
  (princ "\n\nPress any key to return to your drawing. ")
  (grread)
  (princ "\r                                           ")
  (graphscr)
)
;;;
;;; Change the text of an entity.
;;;
(defun cht_te ()
  (setq sslen (sslength sset))
  (initget "Globally Individually Retype")
  (setq ans (getkword 
    "\nSearch and replace text.  Individually/Retype/<Globally>:"))
  (setq cht_ot (getvar "texteval"))
  (setvar "texteval" 1)
  (cond 
    ((= ans "Individually")
      (if (= (getvar "popups") 1)
        (progn
          (initget "Yes No")
          (setq ans (getkword "\nEdit text in dialogue? <Yes>:"))
        )
        (setq ans "No")
      )
  
      (while (> sslen 0)
        (redraw (setq sn (ssname sset (setq sslen (1- sslen)))) 3)
        (setq ss (ssadd))
        (ssadd (ssname sset sslen) ss)
        (if (= ans "No") 
          (chgtext ss)
          (command "_.DDEDIT" sn "")
        )
        (redraw sn 1)
      )
    )
    ((= ans "Retype")
      (while (> sslen 0)
        (setq ent (entget(ssname sset (setq sslen (1- sslen)))))
        (redraw (cdr(assoc -1 ent)) 3)
        (prompt (strcat "\nOld text: " (cdr(assoc 1 ent))))
        (setq nt (getstring  T "\nNew text: "))
        (redraw (cdr(assoc -1 ent)) 1)
        (if (> (strlen nt) 0)
          (entmod (subst (cons 1 nt) (assoc 1 ent) ent))
        )
      )
    )
    (T
      (chgtext sset)                  ; Change 'em all
    )
  )
  (setvar "texteval" cht_ot)
)
;;;
;;; The old CHGTEXT command - rudimentary text editor
;;;
;;;
(defun C:CHGTEXT () (chgtext nil))

(defun chgtext (objs / last_o tot_o ent o_str n_str st s_temp 
                       n_slen o_slen si chf chm cont ans)
  (if (null objs)
    (setq objs (ssget))               ; Select objects if running standalone
  )
  (setq chm 0)
  (if objs 
    (progn                   ; If any objects selected
      (if (= (type objs) 'ENAME) 
        (progn
          (setq ent (entget objs))
          (princ (strcat "\nExisting string: " (cdr (assoc 1 ent))))
        )
        (if (= (sslength objs) 1)
          (progn
            (setq ent (entget (ssname objs 0)))
            (princ (strcat "\nExisting string: " (cdr (assoc 1 ent))))
          )
        )
      )
      (setq o_str (getstring "\nMatch string   : " t))
      (setq o_slen (strlen o_str))
      (if (/= o_slen 0)
        (progn
          (setq n_str (getstring "\nNew string     : " t))
          (setq n_slen (strlen n_str))
          (setq last_o 0 
                tot_o  (if (= (type objs) 'ENAME)
                         1
                         (sslength objs)
                       )
          )
          (while (< last_o tot_o)     ; For each selected object...
            (if (= "TEXT"             ; Look for TEXT entity type (group 0)
                   (cdr (assoc 0 (setq ent (entget (ssname objs last_o))))))
              (progn
                (setq chf nil si 1)
                (setq s_temp (cdr (assoc 1 ent)))
                (while (= o_slen (strlen (setq st (substr s_temp si o_slen))))
                  (if (= st o_str)
                    (progn
                      (setq s_temp (strcat 
                                     (if (> si 1)
                                       (substr s_temp 1 (1- si)) 
                                       ""
                                     )
                                     n_str
                                     (substr s_temp (+ si o_slen))
                                   )
                      )
                      (setq chf t)    ; Found old string
                      (setq si (+ si n_slen))
                    )
                    (setq si (1+ si))
                  )
                )
                (if chf 
                  (progn              ; Substitute new string for old
                    ; Modify the TEXT entity
                    (entmod (subst (cons 1 s_temp) (assoc 1 ent) ent))
                    (setq chm (1+ chm))
                  )
                )
              )
            )
            (setq last_o (1+ last_o))
          )
        )
        ;; else go on to the next line...
      )
    )
  )
  (if (/= (type objs) 'ENAME)
    (if (/= (sslength objs) 1)        ; Print total lines changed
      (princ (strcat "Changed "
                     (rtos chm 2 0)
                     " text lines."
             )
      )
    )
  )
  (terpri)
)
;;;
;;; Main procedure for manipulating text entities
;;; ARGUMENTS:
;;;   typ   -- Type of operation to perform
;;;   prmpt -- Partial prompt string to insert in standard prompt line
;;;   fld   -- Assoc field to be changed
;;; GLOBALS:
;;;   sset  -- The selection set of text entities
;;;
(defun cht_pe (typ prmpt fld / temp ow nw ent tw sty w hw lw 
                              sslen n sn ssl)
  (if (= (sslength sset) 1)           ; Special case if there is only
                                      ; one entity selected
    ;; Process one entity.
    (cht_p1)
    ;; Else
    (progn
      ;; Set prompt string.
      (cht_sp)
      (if (= nw "List")
        ;; Process List request.
        (cht_pl)
        (if (= nw "Individual")
          ;; Process Individual request.
          (cht_pi)
          (if (= nw "Select")
            ;; Process Select request.
            (cht_ps)
            ;; Else
            (progn
              (if (= typ "Rotation")
                (setq nw (* (/ nw 180.0) pi))
              )
              (if (= (type nw) 'STR)
                (if (not (tblsearch "style" nw))
                  (progn
                    (princ (strcat "\nStyle " nw " not found. "))
                  )
                  (cht_pa)
                )
                (cht_pa)
              )
            )
          )
        )
      )
    )
  )
)
;;;
;;; Change all of the entities in the selection set.
;;;
(defun cht_pa (/ cht_oh temp)
  (setq sslen (sslength sset))
  (setq cht_oh (getvar "highlight"))
  (setvar "highlight" 0)
  (while (> sslen 0)
    (setq temp (ssname sset (setq sslen (1- sslen))))
    (entmod (subst (cons fld nw)
                   (assoc fld (setq ent (entget temp)))
                   ent
            )
    )
    
  )
  (setvar "highlight" cht_oh)
)
;;;
;;; Change one text entity.
;;;
(defun cht_p1 ()
  (setq temp (ssname sset 0))
  (setq ow (cdr(assoc fld (entget temp))))
  (if (= opt "Rotation")
    (setq ow (/ (* ow 180.0) pi))
  )
  (redraw (cdr(assoc -1 (entget temp))) 3)
  (initget 0)
  (if (= opt "Style")
    (setq nw (getstring (strcat "\nNew " prmpt ". <"
                              ow ">: ")))
    (setq nw (getreal (strcat "\nNew " prmpt ". <"
                              (rtos ow 2) ">: ")))
  )
  (if (or (= nw "") (= nw nil))
    (setq nw ow)
  )
  (redraw (cdr(assoc -1 (entget temp))) 1)
  (if (= opt "Rotation")
    (setq nw (* (/ nw 180.0) pi))
  )
  (if (= opt "Style")
    (if (null (tblsearch "style" nw))
      (princ (strcat "\nStyle " nw " not found. "))
      
      (entmod (subst (cons fld nw)
                     (assoc fld (setq ent (entget temp)))
                     ent
              )
      )
    )
    (entmod (subst (cons fld nw)
                   (assoc fld (setq ent (entget temp)))
                   ent
            )
    )
  )
)
;;;
;;; Set the prompt string.
;;;
(defun cht_sp ()
  (if (= typ "Style")
    (progn
      (initget "Individual List New Select ")
      (setq nw (getkword (strcat "\nIndividual/List/Select style/<New "
                                 prmpt
                                 " for all text entities>: ")))
      (if (or (= nw "") (= nw nil) (= nw "Enter"))
        (setq nw (getstring (strcat "\nNew "
                                    prmpt
                                    " for all text entities: ")))
      )
    )
    (progn
      (initget "List Individual" 1)
      (setq nw (getreal (strcat "\nIndividual/List/<New "
                                 prmpt
                                 " for all text entities>: ")))
    )
  )
)
;;;
;;; Process List request.
;;;
(defun cht_pl ()
  (setq unctr (1- unctr))
  (setq sslen (sslength sset))
  (setq tw 0)
  (while (> sslen 0)
    (setq temp (ssname sset (setq sslen (1- sslen))))
    (if (= typ "Style")
      (progn
        (if (= tw 0)
          (setq tw (list (cdr(assoc fld (entget temp)))))
          (progn
            (setq sty (cdr(assoc fld (entget temp))))
            (if (not (member sty tw))
              (setq tw (append tw (list sty)))
            )
          )
        )
      )
      (progn
        (setq tw (+ tw (setq w (cdr(assoc fld (entget temp))))))
        (if (= (sslength sset) (1+ sslen)) (setq lw w hw w))
        (if (< hw w) (setq hw w))
        (if (> lw w) (setq lw w))
      )
    )
  )
  (if (= typ "Rotation")
    (setq tw (* (/ tw pi) 180.0)
          lw (* (/ lw pi) 180.0)
          hw (* (/ hw pi) 180.0))
  )
  (if (= typ "Style")
    (progn
      (princ (strcat "\n"
                     typ
                     "(s) -- "))
      (princ tw)
    )
    (princ (strcat "\n"
                     typ
                     " -- Min: "
                     (rtos lw 2)
                     "\t Max: "
                     (rtos hw 2)
                     "\t Avg: "
                     (rtos (/ tw (sslength sset)) 2) ))
  )
)
;;;
;;; Process Individual request.
;;;
(defun cht_pi ()
  (setq sslen (sslength sset))
  (while (> sslen 0)
    (setq temp (ssname sset (setq sslen (1- sslen))))
    (setq ow (cdr(assoc fld (entget temp))))
    (if (= typ "Rotation")
      (setq ow (/ (* ow 180.0) pi))
    )
    (initget 0)
    (redraw (cdr(assoc -1 (entget temp))) 3)
    (if (= typ "Style")
      (progn
        (setq nw (getstring (strcat "\nNew "
                                   prmpt
                                   ". <"
                                   ow ">: ")))
      )
      (progn
        (setq nw (getreal (strcat "\nNew "
                                   prmpt
                                   ". <"
                                (rtos ow 2) ">: ")))
      )
    )
    (if (or (= nw "") (= nw nil))
      (setq nw ow)
    )
    (if (= typ "Rotation")
      (setq nw (* (/ nw 180.0) pi))
    )
    (entmod (subst (cons fld nw)
                   (assoc fld (setq ent (entget temp)))
                   ent
            )
    )
    (redraw (cdr(assoc -1 (entget temp))) 1)
  )
)
;;;
;;; Process the Select option.
;;;
(defun cht_ps ()
  (princ "\nSearch for which Style name?  <*>: ")
  (setq sn  (strcase (getstring))
        n   -1
        nsset (ssadd)
        ssl (1- (sslength sset))
        )
  (if (or (= sn "*") (null sn) (= sn ""))
    (setq nsset sset sn "*")
    (while (and sn (< n ssl))
      (setq temp (ssname sset (setq n (1+ n))))
      (if (= (cdr(assoc 7 (entget temp))) sn)
        (ssadd temp nsset)
      )
    )
  )
  (setq ssl (sslength nsset))  
  (princ "\nFound ")
  (princ ssl)
  (princ " text entities with STYLE of ")
  (princ sn)
  (princ ". ")
)
;;;
;;; The C: function definition.
;;;
(defun c:cht    () (chtxt))
(princ "\n\tc:CHText loaded.  Start command with CHT.")
(princ)
