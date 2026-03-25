;;; profiles.lsp
;;; miscellaneous profile commands
;;; By Jimmy Bergmark
;;; Copyright (C) 1997-2008 JTB World, All Rights Reserved
;;; Website: www.jtbworld.com
;;; E-mail: info@jtbworld.com
;;;  2000-01-25
;;;  2003-01-23 Added functions
;;;  2008-08-08 Added forceImport

(vl-load-com)

; Brute force a profile to be imported
; (forceImport "profilename" "C:\\temp\\testprof.arg" 1)
; inclpathinfo=1   The path information in the registry file will be preserved.
; inclpathinfo=0   The path information in the registry file will not be preserved.
; return T if profile is imported
(defun forceImport (profilename argname inclpathinfo / tmp1 tmp2)
  (setq tmp1 "<<most-temporary1>>")
  (deleteProfile tmp1)
  (if (importProfile tmp1 argname inclpathinfo)
    (progn
      (putActiveProfile tmp1)
      (deleteProfile profilename)
      (renameProfile tmp1 profilename)
      (putActiveProfile profilename)
      T
    )
    nil
  )
)

(defun getActiveProfile ()
  (vla-get-activeprofile
    (vla-get-profiles (vla-get-preferences (vlax-get-acad-object)))
  )
)

(defun putActiveProfile (profilename)
  (vla-put-activeprofile
    (vla-get-profiles (vla-get-preferences (vlax-get-acad-object)))
    profilename
  )
)

(defun getAllProfileNames(/ allprofiles)
  (vla-getallprofilenames
    (vla-get-profiles (vla-get-preferences (vlax-get-acad-object))) 
    'allprofiles
  )
  (vlax-safearray->list allprofiles)
)

(defun existProfile (profilename)
  (not (not (member
    (strcase profilename)
    (mapcar '(lambda (x) (strcase x)) (getallprofilenames))
  )))
)

(defun c:listProfileNames(/ nr profnames)
  (setq nr 0)
  (setq profnames (getAllProfileNames))
  (repeat (length profnames)
    (princ (nth nr profnames))
    (print)
    (setq nr (1+ nr))
  )
  (princ)
)

; return T if profile is deleted and nil if not
(defun deleteProfile (profilename)
  (if (and
        (/= (strcase profilename) (strcase (getActiveProfile)))
        (existProfile profilename))
    (not (vla-deleteprofile
      (vla-get-profiles (vla-get-preferences (vlax-get-acad-object)))
      profilename
    ))
  )
)

; Delete all profiles except the current profile
(defun deleteAllProfilesExceptCurrent (/ item)
  (foreach item	(getAllProfileNames)
    (vl-catch-all-apply
      'vla-deleteprofile
      (list (vla-get-profiles
	      (vla-get-preferences (vlax-get-acad-object))
	    )
	    item
      )
    )
  )
)

; return T if profile is reseted and nil if not
(defun resetProfile (profilename)
  (if (existProfile profilename)
    (not (vla-resetprofile
      (vla-get-profiles (vla-get-preferences (vlax-get-acad-object)))
      profilename
    ))
  )
)

; return T if profile is renamed and nil if not
; if profilenameNew exist it's substituded with profilenameOld
(defun renameProfile (profilenameOld profilenameNew)
  (if (existProfile profilenameOld)
    (not (vla-renameprofile
      (vla-get-profiles (vla-get-preferences (vlax-get-acad-object)))
      profilenameOld
      profilenameNew
    ))
  )
)

; return T if profile is copied and nil if not
; if profilename2 exists it's copied
(defun copyProfile (profilename1 profilename2)
  (if (existProfile profilename1)
    (not (vla-copyprofile
      (vla-get-profiles (vla-get-preferences (vlax-get-acad-object)))
      profilename1
      profilename2
    ))
  )
)

; (exportProfile "profilename" "C:\\TEMP\\profilename.arg")
; if path is omitted profile is saved in active directory
; overwrites argname if it exists
; return T if profile is exported
(defun exportProfile (profilename argname)
  (if (existProfile profilename)
    (not (vla-exportprofile
      (vla-get-profiles (vla-get-preferences (vlax-get-acad-object)))
      profilename
      argname
    ))
  )
)

; (importProfile "profilename" "C:\\TEMP\\profilename.arg" 1)
; overwrites profilename if it exists
; if path is omitted profile is imported from active directory
; inclpathinfo=1   The path information in the registry file will be preserved.
; inclpathinfo=0   The path information in the registry file will not be preserved.
; return T if profile is imported
(defun importProfile (profilename argname inclpathinfo)
  (not (vl-catch-all-apply 'vla-importprofile (list
    (vla-get-profiles (vla-get-preferences (vlax-get-acad-object)))
    profilename
    argname
    inclpathinfo
  ))
  )
)

; Brute force a profile to be imported and all the resting to be deleted
; (forceImportAndDeleteTheRest "profilename" "C:\\temp\\testprof.arg" 1)
; inclpathinfo=1   The path information in the registry file will be preserved.
; inclpathinfo=0   The path information in the registry file will not be preserved.
; return T if profile is imported
(defun forceImportAndDeleteTheRest (profilename argname inclpathinfo / tmp1 tmp2)
  (setq tmp1 "<<most-temporary1>>")
  (deleteProfile tmp1)
  (if (importProfile tmp1 argname inclpathinfo)
    (progn
      (putActiveProfile tmp1)
      (deleteProfile profilename)
      (renameProfile tmp1 profilename)
      (putActiveProfile profilename)
      (deleteAllProfilesExceptCurrent)
      T
    )
    nil
  )
)




