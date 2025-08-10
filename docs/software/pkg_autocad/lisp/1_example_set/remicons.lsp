;;; By Jimmy Bergmark
;;; Copyright (C) 1997-2006 JTB World, All Rights Reserved
;;; Website: www.jtbworld.com
;;; E-mail: info@jtbworld.com
;;; This program is created for AutoCAD 2002, AutoCAD 2004 and vertical products
;;; Removes the icons Buzzsaw, RedSpark, Point A and FTP in Open dialog box
;;;
;;; To remove all of the Icons above for all profiles use (remicons T T T T T)
;;; To remove only Buzzsaw in active profile use (remicons T nil nil nil nil)
;;; To restore all of the Icons to all profiles use (remicons nil nil nil nil T)
;;; (remicons <Buzzsaw> <RedSpark> <Point A> <FTP> <All profiles=T, current profile=nil>)

(defun remicons (ibuzz ired ipoint iftp allprof / prof profiles regkey)
  (vl-load-com)
  (defun getallprofilenames (/ allprofiles)
    (vla-getallprofilenames
      (vla-get-profiles
        (vla-get-preferences (vlax-get-acad-object))
      )
      'allprofiles
    )
    (vlax-safearray->list allprofiles)
  )
  (setq profiles (getallprofilenames))
  (if (not allprof) (setq profiles (list (getvar "CPROFILE"))))
  (foreach prof profiles
    (setq regkey (strcat "HKEY_CURRENT_USER\\"
                (vlax-product-key)
                "\\Profiles\\"
                prof
                "\\Dialogs\\AllAnavDialogs\\DeletedExtensions"
        )
    )
    (if ibuzz
      (vl-registry-write
        regkey
        "Buzzsaw"
        ""
      )
      (vl-registry-delete regkey "Buzzsaw")
    )
    (if ibuzz
      (vl-registry-write
        regkey
        "ACPROJECT"
        ""
      )
      (vl-registry-delete regkey "ACPROJECT")
    )
    (if ired
      (vl-registry-write
        regkey
        "RedSpark"
        ""
      )
      (vl-registry-delete regkey "RedSpark")
    )
    (if ipoint
      (vl-registry-write
        regkey
        "SimpleStorageSites"
        ""
      )
      (vl-registry-delete regkey "SimpleStorageSites")
    )
    (if iftp
      (vl-registry-write
        regkey
        "FTPSites"
        ""
      )
      (vl-registry-delete regkey "FTPSites")
    )
  )
  (princ)
)


(princ)


