;;; BackgroundChanger.LSP ver 1.0
;;; Lets you change the background in a simple way
;;; Works for both model space and paper space but could simply be customized for your need
;;;
;;; By Jimmy Bergmark
;;; Copyright (C) 1997-2004 JTB World, All Rights Reserved
;;; Website: www.jtbworld.com
;;; E-mail: info@jtbworld.com
;;; 2003-07-01 - First release
;;; Tested on AutoCAD 2002, 2004, 2005

; Set the background in model and paper space to grey
(defun c:BGGrey ()
  (vl-load-com)
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

; Set the background in model and paper space to white
(defun c:BGWhite ()
  (vl-load-com)
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

; Set the background in model and paper space to black
(defun c:BGBlack ()
  (vl-load-com)
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

; Background toggle between black and white
(defun c:bgt ()
  (vl-load-com)
  (setq	disp (vla-get-display (vla-get-preferences (vlax-get-acad-object))))
  (setq	drafting (vla-get-drafting (vla-get-preferences (vlax-get-acad-object))))
  (if (= (vlax-variant-value
	   (vlax-variant-change-type
	     (vla-get-graphicswinmodelbackgrndcolor disp)
	     vlax-vblong
	   )
	 )
	 0
      )
    (c:bgwhite)
    (c:bgblack)
  )
  (princ)
)

(princ)