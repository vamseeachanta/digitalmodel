
;;; PurgeReconciledLayers.LSP
;;;
;;; By Jimmy Bergmark
;;; Copyright (C) 2007 JTB World, All Rights Reserved
;;; Website: www.jtbworld.com
;;; E-mail: info@jtbworld.com
;;; 2007-04-05 - First release
;;; Written for AutoCAD 2008


;;; Purge all information about reconciled layers in the drawing

(defun PurgeReconciledLayers ()
  (vl-load-com)
  (vlax-for layer (vla-get-Layers
		    (vla-get-ActiveDocument
		      (vlax-get-acad-object)
		    )
		  )
    (vl-Catch-All-Apply
      '(lambda ()
	 (vla-Remove
	   (vla-GetExtensionDictionary
	     layer
	   )
	   "ADSK_XREC_LAYER_RECONCILED"
	 )
       )
    )
    (vl-Catch-All-Apply
      '(lambda ()
	 (vla-delete
	   (vla-GetExtensionDictionary
	     layer
	   )
	 )
       )
    )
  )
  (setvar "LAYEREVAL" 0)
  (setvar "LAYERNOTIFY" 0)
  (princ)
)

; Remove the row below if you don't want to run the code automatically when the AutoLISP file is loaded.
(PurgeReconciledLayers)

