;;;load VLISP extensions
(vl-load-com);initialize VLISP extensions

;;;load the reactor manager
(if (member nil (atoms-family 1 '("vlr-manager" "vlr-family" "vlr-object" "vlr-object-p" "vlr-syntax")))
  (load "VLR-Manager")
)

;toggles autolay on and off
(defun c:autolay (/ funcs file)
  (setq funcs '(c:autolay al:autolay al:cmdbegin al:cmdcancelled al:color al:cmdend al:disable al:exist al:layerstate al:linetype al:loadtype al:lweight al:mklayer al:putinlist al:reactorload))
  (if
    (and
      (not (vl-registry-read "HKEY_CURRENT_USER\\SOFTWARE\\AutoLay" "AutoLay"));is AutoLay turned on or off in the registry?
      (member nil (atoms-family 1 (mapcar 'vl-princ-to-string funcs)));are any AutoLay subfunctions not loaded
      (setq file (findfile "AutoLay[3.0].lsp"))
    )
    (progn
      (vl-registry-write "HKEY_CURRENT_USER\\SOFTWARE\\AutoLay" "AutoLay" "T");set registry for AutoLay enabled
      (load file);load "AutoLay[3.0].lsp"
    )
    (progn
      (vl-registry-delete "HKEY_CURRENT_USER\\SOFTWARE\\AutoLay" "AutoLay");set registry for AutoLay disabled
      (mapcar
       '(lambda (x)(set x nil));set all AutoLay subroutines to nil
        (vl-remove 'c:autolay (vl-remove 'al:putinlist funcs));except for the ones needed to re-enable AutoLay
      );set AutoLay subroutines to nil
      (vlr-manager '(VLR-DWG-reactor nil '((:VLR-beginClose . al:disable))) 1);remove reactor
      (vlr-manager '(VLR-Command-reactor nil '((:VLR-commandWillStart . al:autolay)(:VLR-commandEnded . al:autolay)(:VLR-commandCancelled . al:autolay))) 1);remove reactor
      (princ "\nUnloaded AutoLay[3.0].lsp")
    )
  )
  (princ)
)

(defun al:putinlist (a d l)
  (cond
    ( (not l)(list (cons a d)));if no list, create association list in a list
    ( (not (assoc a l))(cons (cons a d) l));if no association in list, create one
    ( (subst (cons a d)(assoc a l) l));if association in list, replace it
  )
)

(if (vl-registry-read "HKEY_CURRENT_USER\\SOFTWARE\\AutoLay" "AutoLay");if AutoLay is enabled in the registry, continue loading
  (progn

(defun al:autolay (reactor info / cmd crn dat)
  (setq cmd (car info);get command
        crn (vlr-current-reaction-name);get the name of the reactor calling this function
        dat (vlr-data reactor);get data list from reactor object
  )
  (cond
    ( (and
        (= crn ':VLR-commandWillStart);is command starting?
        (not (assoc 'COMMAND dat));has a association to 'COMMAND been assiged to the reactor data to eliminate problems with transparent commands?
      )
      (vlr-data-set reactor (al:putinlist 'COMMAND cmd dat));assign an association to 'COMMAND to the reactor data list
      (al:cmdbegin reactor info);call the layer manipulating routine
    )
    ( (and
        (= crn ':VLR-commandEnded);is command ending?
        (equal cmd (cdr (assoc 'COMMAND dat)));is the association to 'COMMAND assiged to the reactor data equal to the command that filed the reactor?
      )
      (vlr-data-set reactor (vl-remove (assoc 'COMMAND dat) dat));Remove association to 'COMMAND in reactor data list
      (al:cmdend reactor info);restore previous layer status
    )
    ( (and
        (= crn ':VLR-commandCancelled);was command cancelled?
        (equal cmd (cdr (assoc 'COMMAND dat)));is the association to 'COMMAND assiged to the reactor data equal to the command that filed the reactor?
      )
      (vlr-data-set reactor (vl-remove (assoc 'COMMAND dat) dat));Remove association to 'COMMAND in reactor data list
      (al:cmdcancelled reactor info);restore previous layer status and remove new layer if one was created
    )
  )
)

;;;tests the command name, textsize, dimtext size etc.
;;;to determine if action needs to be taken. This is where the layer name
;;;comes from and where the conditions under which to act under are set up.
(defun al:cmdbegin (reactor info / ado cmd dat dsc dst dtx tst tsz)
  (setq cmd (car info);get the command
        dat (vlr-data reactor);get reactor data
        ado (cdr (assoc 'ado dat));get pointer to activeDocument
        dsc (vlax-variant-value (vla-getvariable ado "dimscale"));get scale
        dst (strcase (vla-get-name (vla-get-activedimstyle ado)));get dimstyle
        dtx (vlax-variant-value (vla-getvariable ado "dimtxt"));get dimtxt size
        tst (strcase (vla-get-name (vla-get-activetextstyle ado)));get textstyle
        tsz (vlax-variant-value (vla-getvariable ado "textsize"));get text size
  );end setq
  (cond
    ( (wcmatch cmd "HATCH,BHATCH");is the command "hatch"?
      (al:layerstate reactor info "Hatch");make, thaw, turn on and make current "Hatch" layer as needed
    );end cond 1
    ( (wcmatch cmd "VPORTS");is the command "vports"?
      (al:layerstate reactor info "Viewport");make, thaw, turn on and make current "Viewport" layer as needed
    );end cond 2
    ( (wcmatch cmd "DIM,BREAKLINE,DIMLINEAR,DIMALIGNED,DIMORDINATE,DIMRADIUS,DIMDIAMETER,DIMANGULAR,DIMBASELINE,DIMCONTINUE,QDIM,LEADER,QLEADER");are you creating a dimension?
      (al:layerstate reactor info "Dim2");make, thaw, turn on and make current "Dim" layer as needed
    );end cond 3
    ( (wcmatch cmd "DIMCENTER");are you creating a center mark?
      (al:layerstate reactor info "Center");make, thaw, turn on and make current "Dim" layer as needed
    );end cond 4
    ( (wcmatch cmd "DTEXT,MTEXT,TEXT");are you creating text?
      (al:layerstate reactor info "Text");make, thaw, turn on and make current "Text" layer as needed
    );end cond 5
  );end cond
);end defun

;;;figures out what action needs to be taken by checking
;;;the state of the layer "name" and the command being used
(defun al:layerstate (reactor info name / clo cmd lso lyo lon frz lck lof lyr)
  (setq cmd (car info);get command
        dat (vlr-data reactor);get reactor data
        ado (cdr (assoc 'ado dat));get pointer to activeDocument object
        clo (vla-get-ActiveLayer ado);get the clayer object
        lso (cdr (assoc 'lso dat));get pointer to layers object
        lyo (al:exist lso name);get the layer object for "name"
  );end setq
  (if lyo;does layer not exist?
    (progn
      (if (= (vla-get-LayerOn lyo) :vlax-false);is layer off?
        (progn
          (vla-put-LayerOn lyo :vlax-true);turn it on
          (setq lon (list 'vla-put-layeron lyo :vlax-false));save an expression to turn off later via al:cmdend
        );end progn
      );end if
      (if (= (vla-get-Freeze lyo) :vlax-true);is layer frozen?
        (progn
          (vla-put-Freeze lyo :vlax-false);thaw it
          (setq frz (list 'vla-put-freeze lyo :vlax-true));save an expression to refreeze later via al:cmdend
        );end progn
      );end if
      (if (= (vla-get-Lock lyo) :vlax-true);is layer locked?
        (progn
          (vla-put-Lock lyo :vlax-false);unlock it
          (setq lok (list 'vla-put-lock lyo :vlax-true));save an expression to lock it again later via al:cmdend
        );end progn
      );end if
    );end progn
    (setq lyo (al:mklayer reactor info name);make it
          dat (al:putinlist 'NLR lyo dat);add to data list to be stored in reactor object incase of commandCancelled event
    )
  );end if
  (if
    (and
      (wcmatch cmd "BHATCH,HATCH,BREAKLINE,DIMLINEAR,DIMALIGNED,DIMORDINATE,DIMRADIUS,DIMDIAMETER,DIMANGULAR,DIMBASELINE,DIMCONTINUE,QDIM,LEADER,QLEADER");is the command is "*hatch"*
      (setq lof (if (wcmatch cmd "BHATCH,HATCH")(al:exist lso "Hidden")(al:exist lso "Hatch")));sets the value of LOF to the hidden layer object or nil depending on the results of al:exist
      (= (vla-get-LayerOn lof) :vlax-true);on
      (= (vla-get-Freeze lof) :vlax-false);thawed
    );end and
    (progn
      (vla-put-LayerOn lof :vlax-false);turn "hidden" layer off
      (setq lof (list 'vla-put-layeron lof :vlax-true));add to data list to be stored in reactor object for commandEnded or commandCancelled event
    );end progn
  );end if
  (if (not (equal clo lyo));if layer is not current
    (progn
      (vla-put-ActiveLayer ado lyo);sets layer current
      (setq lyr (list 'vla-put-activelayer ado clo));add to data list to be stored in reactor object for commandEnded or commandCancelled event
    )
  );end if
  (setq dat (al:putinlist 'LAYDAT (vl-remove nil (list lyr lon lof frz lok)) dat));compile data
  (vlr-data-set reactor dat);commit data to reactor object
);end defun

;;;======================================================================
;;;creates layer, sets properties and returns layer object
(defun al:mkLayer (reactor info name / ado dat lso lyo color lineType lineWeight val)
  (setq dat       (vlr-data reactor)
        ado       (cdr (assoc 'ado dat))
        lso       (cdr (assoc 'lso dat))
        lyo       (vla-add lso name);create the layer
        name      (strcase name);set "name" to all capitols
        color     (al:color name);get color
        lineType  (vla-get-name
                    (al:loadLinetype reactor info
                      (al:ltype name);get linetype
                      (if (= (vlax-variant-value (vla-getvariable ado "measureinit")) 0);English or metric
                        "acad.lin"; load linetype from English linetype file
                        "acadiso.lin"; load linetype from metric linetype file
                      );end if
                    );end al:loadlinetype
                  );end vla-get-name
       lineWeight (al:lweight name)
  );end setq
  (if lyo; if the layer was made, (lyo not nil)
    (foreach prop '(color lineType lineWeight);loop therugh properties
      (if (setq val (eval prop))
        (vlax-put-property lyo prop val);set the properties for layer "name"
      );end if
    );end foreach
  );end if
  lyo; return layer object to calling function
);end defun

;;;attempts to load linetype if not loaded
;;;returns linetype object name if loaded else nil
(defun al:loadlinetype (reactor info name fname / dat linetypes)
  (setq dat (vlr-data reactor)
        linetypes (cdr (assoc 'linetypes dat))
  )
  (if (not (al:exist linetypes name));linetype is not loaded
    (vl-catch-all-apply 
       'vla-load (list linetypes name fname);load it, unfortunately vla-load returns nil on success or error on fail instead of linetype object
    );trap error if load fails
  );end if
  (al:exist linetypes name);return linetype object if exists, else nil
);end defun

;;;returns ActiveX object if exist, else nil
(defun al:exist (collection item / rslt)
  (if
    (not
      (vl-catch-all-error-p 
        (setq rslt
          (vl-catch-all-apply 'vla-item
            (list collection item)
          );trap error
        );end setq
      );return T if successful, else nil
    );end not
    rslt; return object or nil
  );end if
);end defun

;;;======================================================================
;;;returns lineTypeName according to layer standard for "name"
;;;edit linetypes according to your layer standard
(defun al:ltype (name)
  (cond
    ((wcmatch name "BREAK") "phantom");break layer, phantom linetype
    ((wcmatch name "CENTER") "center2");center layer, center2 linetype
    ((wcmatch name "HIDDEN") "hidden");hidden layer, hidden linetype
    (T "continuous");if "name" not found return "continuous" linetype
  );end cond
);end defun

;;;returns lineweight (enumerated, metric equivelant) according to layer "name"
;;;edit lineweights according to your layer standard
;;;valid values are:
;;;acLnWtByBlock, acLnWtByLwDefault,
;;;acLnWt000, acLnWt005, acLnWt009, acLnWt013
;;;acLnWt015, acLnWt018, acLnWt020, acLnWt025
;;;acLnWt030, acLnWt035, acLnWt040, acLnWt050
;;;acLnWt053, acLnWt060, acLnWt070, acLnWt080
;;;acLnWt090, acLnWt100, acLnWt106, acLnWt120
;;;acLnWt140, acLnWt158, acLnWt200, acLnWt211
(defun al:lweight (name / color)
  (cond
    ((wcmatch name "CENTER,DIM") acLnWt020);red, 0.008"
    ((wcmatch name "TEXT") acLnWt015);blue, 0.006"
    ((wcmatch name "HATCH,HIDDEN") acLnWt009);magenta, 0.004"
    ((wcmatch name "VIEWPORT") acLnWt053);white, 0.021"
    (T acLnWtByLwDefault);if "color" not found return default lineweight
  );end cond
);end defun

;;;returns color (intiger) for layer "name"
;;;edit colors according to your layer standard
(defun al:color (name)
  (cond
    ((wcmatch name "CENTER,DIM") 1)
    ((wcmatch name "DIM2,OBJECT_MEDIUM,PIPE,TEXT,TITLE") 5)
    ((wcmatch name "HATCH,HIDDEN") 6)
    ((wcmatch name "Viewport") 7)
    (T (acad_colordlg 1 nil));if "name" not found bring up color dialog
  );end cond
);end defun

;;;======================================================================
;;;upon completion of command restores lso to previous state
(defun al:cmdend (reactor info / dat ldt)
  (setq dat (vlr-data reactor);get reactor data
        ldt (assoc 'LAYDAT dat);get saved expressions to restore layer
        dat (vl-remove ldt dat);remove LDT from data list
  )
  (mapcar 'eval (cdr ldt));evaluate elements of LDT
  (vlr-data-set reactor dat);commit data to reactor object
)

;;;======================================================================
(defun al:cmdcancelled (reactor info / dat nlr)
  (setq dat (vlr-data reactor);get reactor data
        nlr (assoc 'NLR dat);get pointer to newly created layer
        dat (vl-remove nlr dat);remove NLR from data list
  )
  (al:cmdend reactor info);restore previous layer state
  (if nlr (vla-delete (cdr nlr)));delete newly created layer if any
  (vlr-data-set reactor dat);commit data to reactor object
)

;;;======================================================================
;;;disables commandEnded reactor to avoid errors when using "new" and "open"
;;;in SDI mode. The error is merely annoying and only appears at the command
;;;line as "error: no function definition: al:restore" when opening or creating
;;;a new drawing. The cause of the error is commandEnded reactor present form
;;;last dwg but LISP has not yet loaded the called function in a new or opened
;;;dwg. Furthermore, the reactor cannot be removed because it has already been
;;;activated and is waiting for the command to end. Therefore, the reactor must
;;;be rendered non-functional by changing its call to the LISP command "LIST".
(defun al:disable (reactor info)
  (if (= (vlax-variant-value (vla-getvariable (vlr-data reactor) "sdi")) 1);in SDI mode?
    (vlr-reaction-set
      (car (vlr-object '(VLR-Command-reactor nil '((:VLR-commandWillStart . al:autolay)(:VLR-commandEnded . al:autolay)(:VLR-commandCancelled . al:autolay)))))
      :VLR-commandEnded
      'list;the only function I could find that would work without errors
    )
  );end if
);end defun

;;;======================================================================
;;;Here's where we construct the reactors to do all this cool stuff
(defun al:reactorload (/ ado dat)
  (setq ado (vla-get-ActiveDocument (vlax-get-acad-object));get pointer to activeDocument
        dat     (list;create a list of associations to pointers for use during the entire drawing session
		  (cons 'ado ado);store pointer to activeDocument
                  (cons 'lso (vla-get-layers ado));get and store pointer to layers collection
                  (cons 'linetypes (vla-get-linetypes ado));get and store pointer to linetypes collection
                );end list
  );end setq
  (vlr-manager '(VLR-DWG-reactor ado '((:VLR-beginClose . al:disable))) 3);construct reactor to call 'al:disable' on beginClose event
  (vlr-manager '(VLR-Command-reactor dat '((:VLR-commandWillStart . al:autolay)(:VLR-commandEnded . al:autolay)(:VLR-commandCancelled . al:autolay))) 3);construct reactor to call 'al:cmdbegin' on commandWillStart commandEnded and commandCancelled events
)
(al:reactorload);excecute reactor construction

;;;======================================================================
;;;get rid of old reactor if present. The reactor will be present, because in
;;;SDI mode, it's associated namespace is not destroyed, but has the new drawing
;;;loaded into it. At the time this file is loaded, this reactor is either not
;;;present or has been rendered useless (in SDI mode) at the closing of the last
;;;dwg and is excess loaded code bulk and should be removed. The VLR-MANAGER
;;;provides an easy means of doing this.
(vlr-manager '(VLR-Command-reactor nil '((:VLR-commandWillStart . al:autolay)(:VLR-commandEnded . list)(:VLR-commandCancelled . al:autolay))) 1)

;;;======================================================================
(princ "\nLoaded AutoLay[3.0].lsp\n")

  );end progn
  (princ "\nLoaded AutoLay[3.0].lsp. Type \"autolay\" to enable.")
);end if
(princ)
