;;;
;;;    Copyright (C) 2002 by Autodesk, Inc.
;;;
;;;    Permission to use, copy, modify, and distribute this software
;;;    for any purpose and without fee is hereby granted, provided
;;;    that the above copyright notice appears in all copies and
;;;    that both that copyright notice and the limited warranty and
;;;    restricted rights notice below appear in all supporting
;;;    documentation.
;;;
;;;    AUTODESK PROVIDES THIS PROGRAM "AS IS" AND WITH ALL FAULTS.
;;;    AUTODESK SPECIFICALLY DISCLAIMS ANY IMPLIED WARRANTY OF
;;;    MERCHANTABILITY OR FITNESS FOR A PARTICULAR USE.  AUTODESK, INC.
;;;    DOES NOT WARRANT THAT THE OPERATION OF THE PROGRAM WILL BE
;;;    UNINTERRUPTED OR ERROR FREE.
;;;
;;;    Use, duplication, or disclosure by the U.S. Government is subject to
;;;    restrictions set forth in FAR 52.227-19 (Commercial Computer
;;;    Software - Restricted Rights) and DFAR 252.227-7013(c)(1)(ii) 
;;;    (Rights in Technical Data and Computer Software), as applicable.
;;;
;;;
;;; DESCRIPTION: 
;;;  Sample profile manipulation utilities. All functions return T on success and nil 
;;  on failure. See comments above each function for additional details.
;;;
;;; EXAMPLES:
;;;   
;;; - Set active profile: 
;;;     (sample-profile-set-active "MyProfile")
;;;
;;; - Import a profile:
;;;     (sample-profile-import "c:\\myExportedProfile.arg" "MyFavoriteProfile" T)
;;;
;;; - Delete a profile:
;;;     (sample-profile-delete "unwanted")
;;;
;;;
;;; - Import a profile, even if it already exists, and set it active.
;;;
;;;    (sample-profile-import "c:\\CompanyProfile.arg" "MyProfile" T)
;;;    (sample-profile-set-active "MyProfile")
;;;
;;;
;;; - Import a profile, if not already present, and set it active
;;;
;;;    (if (not (sample-profile-exists "myProfile"))
;;;        (progn
;;;         (sample-profile-import "c:\\CompanyProfile.arg" "MyProfile" T)
;;;         (sample-profile-set-active "MyProfile")
;;;        )
;;;    )
;;;
;;;
;;; - Import a profile and set it active when AutoCAD is first started.
;;;  Place the following code in acaddoc.lsp with the desired ".arg" filename 
;;;  and profile name...
;;;
;;;    (defun s::startup ()
;;;      (if (not (vl-bb-ref ':sample-imported-profile)) ;; have we imported the profile yet?
;;;          (progn
;;;  
;;;            ;; Set a variable on the bulletin-board to indicate that we've been here before.
;;;            (vl-bb-set ':sample-imported-profile T) 
;;;          
;;;            ;; Import the profile and set it active
;;;            (sample-profile-import "c:\\CompanyProfile.arg" "MyProfile" T)
;;;            (sample-profile-set-active "MyProfile")
;;;   
;;;          );progn then
;;;      );if
;;;    );defun s::startup
;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This helper function gets the profiles object.
;;
(defun sample-get-profiles-object ( / app pref profs )
 (vl-load-com)
 (and
  (setq   app (vlax-get-acad-object))
  (setq  pref (vla-get-preferences app))
  (setq profs (vla-get-profiles pref))
 )
 profs
);defun sample-get-profiles-object

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Determine if a profile exists. Returns T if the specified profile name exists, and nil if not.
;;
(defun sample-profile-exists ( name / profs )
 (and name
      (setq names (sample-profile-names))
      (member (strcase name) (mapcar 'strcase names))
 )
);defun sample-profile-exists

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set the active profile. 
;; NOTES: 
;;  - If the specified profile name is already active then the function returns T and makes no additional 
;;    changes.
;;
;;  - The specified profile must exist. (You can import a profile using the  'sample-profile-import' 
;;    function.) If the specified profile does not exist, the function returns nil.
;;
(defun sample-profile-set-Active ( name / profs )
 (and
  name
  (setq profs (sample-get-profiles-object))
  (or (equal (strcase name) (strcase (getvar "cprofile")))
      (not (vl-catch-all-error-p (vl-catch-all-apply 'vla-put-activeProfile (list profs name))))
  )
 );and
);defun sample-profile-set-Active

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Delete the specified profile. Fails if the specified profile is current.
;; 
(defun sample-profile-delete ( name / profs )
 (and
  name
  (setq profs (sample-get-profiles-object))
  (not (vl-catch-all-error-p (vl-catch-all-apply 'vla-deleteprofile (list profs name))))
 )
);defun sample-profile-delete
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Copy profile.
;;
(defun acad-pref-profile-copy ( source target / profs )
 (and
  source
  target
  (setq profs (sample-get-profiles-object))
  (not (vl-catch-all-error-p (vl-catch-all-apply 'vla-CopyProfile (list profs source target))))
 )
);defun sample-profile-copy

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Get a list of profile names
;;
(defun sample-profile-names ( / profs result )
 (and
  (setq profs (sample-get-profiles-object))
  (not (vl-catch-all-error-p (vl-catch-all-apply 'vla-GetAllProfileNames (list profs 'result))))
  result
  (setq result (vlax-safearray->list result))
 )
 result
);defun sample-profile-names

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rename
;;
(defun sample-profile-rename ( oldName newName / profs )
 (and
  oldName
  newName
  (setq profs (sample-get-profiles-object))
  (not (vl-catch-all-error-p (vl-catch-all-apply 'vla-RenameProfile (list profs oldName newName))))
 )
);defun sample-profile-rename

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Get a unique profile name. This function returns a unique profile name that is guaranteed 
;; to not be present in the current list of profiles.
;;
(defun sample-get-unique-profile-name ( / names n name )
 (setq names (sample-profile-names)
       names (mapcar 'strcase names)
        name "TempProfileName"
           n 1
 )
 (while (member (strcase (setq name (strcat name (itoa n)))) names)
  (setq n (+ n 1))
 )
 name
);defun sample-get-unique-profile-name

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Import
;; This function imports the specified .arg file and creates a new profile with the provided profile name.
;; If the specified profile already exists, it will be overwritten.
;; If the 'bUsePathInfo' parameter is non-nil then path information will be imported from the specified 
;; file. Otherwise, path information will be ignored.
;;
;; NOTES: 
;;  This function does not set the active profile. If you import a new profile 
;;  it will not become active unless it matches the name of the existing active profile. 
;;
;;  You can set the active profile by calling: 
;;    (sample-profile-set-active "ProfileName")
;;
(defun sample-profile-import ( filename profileName bUsePathInfo / sample-oldError profs isCProfile tempProfile result )

 ;; Set up an error handler so, if something goes wrong, we can put things back the way we found them
 (setq sample-oldError *error*)
 (defun *error* ( msg / )
  (if (and profileName
           tempProfile
           (equal tempProfile (getvar "cprofile"))
      )
      (progn
       ;; Something went wrong so put things back the way they were.
       (sample-profile-rename tempProfile profileName)
       (sample-profile-set-active profileName)
       (sample-profile-delete tempProfile)
      );progn then
  );if
  (setq *error* sample-oldError)
  (if msg
      (*error* msg)
      (princ)
  )
 );defun *error*

 (if (and bUsePathInfo
          (not (equal :vlax-false bUsePathInfo))
     )
     (setq bUsePathInfo :vlax-true)
     (setq bUsePathInfo :vlax-false)
 )
 (if (and filename
          (setq filename (findfile filename))
          profileName
          (setq profs (sample-get-profiles-object))
     );and
     (progn
      ;; We can't import directly to the current profile, so if the provided profile name matches 
      ;; the current profile, we'll need to:
      ;;  - rename the current profile to a unique name
      ;;  - import
      ;;  - set the new one current
      ;;  - delete the old one with the temp name
      (setq isCProfile (equal (strcase (getvar "cprofile")) (strcase profileName)))
      (if isCProfile
          (progn
           (setq tempProfile (sample-get-unique-profile-name))
           (sample-profile-rename (getvar "cprofile") tempProfile)
          );progn then
      );if

      ;; Import          
      (setq result (not (vl-catch-all-error-p (vl-catch-all-apply 'vla-ImportProfile (list profs profileName filename bUsePathInfo)))))

      (if isCProfile
          (progn
           ;;  Handle current profile case...
           ;;  If the import was successful, then set the new profile active and delete the original
           ;;  else if something went wrong, then put the old profile back
           (if (and result
                    (setq result (sample-profile-set-Active profileName)) ;; set the newly imported profile active
               );and
               (sample-profile-delete tempProfile)            ;; then delete the old profile
               (sample-profile-rename tempProfile profileName);; else rename the original profile back to its old name
           );if
          );progn then
      );if
     );progn then
 );if

 (*error* nil) ;; quietly restore the original error handler
 result
);defun sample-profile-import

(princ)
