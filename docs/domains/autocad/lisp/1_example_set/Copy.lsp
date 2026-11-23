Change your background color to grey.

 

The lisp function below (handy for clients with differing layer requirements) creates 4 commands

BGG turns the background grey
BGW turns the background white
BGB turns the background black
BGT toggles the background between black and white
 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; BKGTOGGLE.lsp                                                          ;;;
;;; Copyright (C) 2000 - 2017 by Falcon Design Services, Inc.              ;;;
;;; Permission to reuse, share, post, copy, borrow, steal, or              ;;; 
;;; generally do with it whatever you wish is hereby granted.              ;;;
;;; Like that would make a lot of difference anyway.                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; THIS CODE IS PROVIDED "AS IS" WITHOUT EXPRESS OR IMPLIED               ;;;
;;; WARRANTY. ALL IMPLIED WARRANTIES OF FITNESS FOR ANY PARTICULAR         ;;;
;;; PURPOSE AND OF MERCHANTABILITY ARE HEREBY DISCLAIMED.                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; BGG turns the background grey
;;; BGW turns the background white
;;; BGB turns the background black
;;; BGT toggles the background between black and white
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (vl-load-com)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Set the background in model and paper space to grey

(defun c:BGG ()
 (setq disp (vla-get-display (vla-get-preferences (vlax-get-acad-object))))
 (setq drafting (vla-get-drafting (vla-get-preferences (vlax-get-acad-object))))
 (vla-put-GraphicsWinModelBackgrndColor disp 5987163)
 (vla-put-GraphicsWinLayoutBackgrndColor disp 5987163)
 (vla-put-LayoutCrosshairColor disp 16777215)
 (vla-put-ModelCrosshairColor disp 16777215)
 (vla-put-AutoTrackingVecColor disp 16777215)
 (vla-put-AutoSnapMarkerColor drafting 2)
 (princ)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Set the background in model and paper space to white
(defun c:BGW ()
 (setq disp (vla-get-display (vla-get-preferences (vlax-get-acad-object))))
 (setq drafting (vla-get-drafting (vla-get-preferences (vlax-get-acad-object))))
 (vla-put-GraphicsWinModelBackgrndColor disp 16777215)
 (vla-put-GraphicsWinLayoutBackgrndColor disp 16777215)
 (vla-put-LayoutCrosshairColor disp 0)
 (vla-put-ModelCrosshairColor disp 0)
 (vla-put-AutoTrackingVecColor disp 0)
 (vla-put-AutoSnapMarkerColor drafting 6)
 (princ)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Set the background in model and paper space to black
(defun c:BGB ()
 (setq disp (vla-get-display (vla-get-preferences (vlax-get-acad-object))))
 (setq drafting (vla-get-drafting (vla-get-preferences (vlax-get-acad-object))))
 (vla-put-GraphicsWinModelBackgrndColor disp 0)
 (vla-put-GraphicsWinLayoutBackgrndColor disp 0)
 (vla-put-LayoutCrosshairColor disp 16777215)
 (vla-put-ModelCrosshairColor disp 16777215)
 (vla-put-AutoTrackingVecColor disp 16777215)
 (vla-put-AutoSnapMarkerColor drafting 2)
 (princ)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Background toggle between black and white
(defun c:bgt ()
 (setq disp (vla-get-display (vla-get-preferences (vlax-get-acad-object))))
 (setq drafting (vla-get-drafting (vla-get-preferences (vlax-get-acad-object))))
 (if (= (vlax-variant-value
 (vlax-variant-change-type
 (vla-get-graphicswinmodelbackgrndcolor disp)
 vlax-vblong
 )
 )
 0
 )
 (c:bgw)
 (c:bgb)
 )
 (princ)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(princ (strcat "\n*** Enter \"BGT\" or \"BGG\" or \"BGW\" or \"BGB\" to Invoke ***"))(princ)
(princ)

;;----------------------------------------------------------------------;;
;; End of File ;;
;;----------------------------------------------------------------------;;