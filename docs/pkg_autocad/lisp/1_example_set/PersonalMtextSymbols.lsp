;;; PersonalMtextSymbols.LSP ver 1.1
;;; Add personal mtext symbols in the right click menu in the mtext editor
;;; By Jimmy Bergmark
;;; Copyright (C) 1997-2008 JTB World, All Rights Reserved
;;; Website: www.jtbworld.com
;;; E-mail: info@jtbworld.com
;;; Tested on AutoCAD 2002, 2004, 2005, 2008, 2009. Not sure about 2006. 2007 does not support this.
;;;
;;; Remember that you can change the contents to whatever you would like
;;; The syntax is:
;;; (vl-registry-write key "Name <1,2,3...n>" "<Description>")
;;; (vl-registry-write key "Contents <1,2,3...n>" "<Value>")

(defun PersonalMtextSymbols ()
  (vl-load-com)
  (setq key (strcat "HKEY_CURRENT_USER\\" (vlax-product-key) "\\MTEXT\\Symbols"))
  (vl-registry-write key "Name 1" "Name")
  (vl-registry-write key "Name 2" "Company")
  (vl-registry-write key "Contents 1" "Jimmy Bergmark")
  (vl-registry-write key "Contents 2" "JTB World")
)

(defun c:PM ()
  (PersonalMtextSymbols)
  (princ)
)