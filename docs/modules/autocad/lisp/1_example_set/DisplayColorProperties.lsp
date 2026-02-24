;;; DisplayColorProperties.LSP
;;; Miscellaneous commands related to Colors on the Display tab on the Options dialog
;;; By Jimmy Bergmark
;;; Copyright (C) 1997-2006 JTB World, All Rights Reserved
;;; Website: www.jtbworld.com
;;; E-mail: info@jtbworld.com
;;; 2000-03-29 - First release
;;; 2003-03-07 - Now for AutoCAD 2004
;;; Tested on AutoCAD 2000, 2000i, 2002 and 2004

(vl-load-com)

(setq pref (vla-get-preferences (vlax-get-acad-object)))

(setq display (vla-get-display pref))

;;; Set the ModelColor using the color dialog box
(defun c:SetModelColor(/ col oldcol)
  (setq oldcol (getGraphicsWinModelBackgrndColor))
  (if (= oldcol nil) (setq oldcol 0))
  (if (= oldcol 0) (setq oldcol 1))
  (setq col (acad_colordlg oldcol nil))
  (if (and (= oldcol 7) (= col 7)) (setq col 0))
  (if (/= col nil) (putGraphicsWinModelBackgrndColor col))
  (princ)
)
(defun c:SetModelColor2004(/ col oldcol)
  (setq oldcol (getGraphicsWinModelBackgrndColor2004))
  (if (= oldcol nil) (setq oldcol 0))
  (if (= oldcol 0) (setq oldcol 1))
  (setq col (acad_colordlg oldcol nil))
  (if (and (= oldcol 7) (= col 7)) (setq col 0))
  (if (/= col nil) (putGraphicsWinModelBackgrndColor col))
  (princ)
)

;;; Set the LayoutColor using the color dialog box
(defun c:SetLayoutColor(/ col oldcol)
  (setq oldcol (getGraphicsWinLayoutBackgrndColor))
  (if (= oldcol nil) (setq oldcol 0))
  (if (= oldcol 0) (setq oldcol 1))
  (setq col (acad_colordlg oldcol nil))
  (if (and (= oldcol 7) (= col 7)) (setq col 0))
  (if (/= col nil) (putGraphicsWinLayoutBackgrndColor col))
  (princ)
)

(defun getGraphicsWinModelBackgrndColor()
  (OLE_color->ACI_color 
    (vla-get-GraphicsWinModelBackgrndColor
      (vla-get-display (vla-get-preferences (vlax-get-acad-object)))))
)

(defun getGraphicsWinModelBackgrndColor2004 ()
  (OLE_colorToACI
    (vlax-variant-value
      (vlax-variant-change-type
	(vla-get-GraphicsWinModelBackgrndColor
	  (vla-get-display
	    (vla-get-preferences (vlax-get-acad-object))
	  )
	)
	vlax-vbLong
      )
    )
  )
)

(defun putGraphicsWinModelBackgrndColor(col)
  (vla-put-GraphicsWinModelBackgrndColor
    (vla-get-display (vla-get-preferences (vlax-get-acad-object)))
    (ACI_color->OLE_color col)
  )
)

(defun getGraphicsWinLayoutBackgrndColor()
  (OLE_color->ACI_color 
    (vla-get-GraphicsWinLayoutBackgrndColor
      (vla-get-display (vla-get-preferences (vlax-get-acad-object)))))
)

(defun putGraphicsWinLayoutBackgrndColor(col)
  (vla-put-GraphicsWinLayoutBackgrndColor
    (vla-get-display (vla-get-preferences (vlax-get-acad-object)))
    (ACI_color->OLE_color col)
  )
)

(defun getModelCrosshairColor()
  (OLE_color->ACI_color 
    (vla-get-ModelCrosshairColor
      (vla-get-display (vla-get-preferences (vlax-get-acad-object)))))
)

(defun putModelCrosshairColor(col)
  (vla-put-ModelCrosshairColor
    (vla-get-display (vla-get-preferences (vlax-get-acad-object)))
    (ACI_color->OLE_color col)
  )
)

(defun getLayoutCrosshairColor()
  (OLE_color->ACI_color 
    (vla-get-LayoutCrosshairColor
      (vla-get-display (vla-get-preferences (vlax-get-acad-object)))))
)

(defun putLayoutCrosshairColor(col)
  (vla-put-LayoutCrosshairColor
    (vla-get-display (vla-get-preferences (vlax-get-acad-object)))
    (ACI_color->OLE_color col)
  )
)

(defun getTextWinBackgrndColor()
  (OLE_color->ACI_color 
    (vla-get-TextWinBackgrndColor
      (vla-get-display (vla-get-preferences (vlax-get-acad-object)))))
)

(defun putTextWinBackgrndColor(col)
  (vla-put-TextWinBackgrndColor
    (vla-get-display (vla-get-preferences (vlax-get-acad-object)))
    (ACI_color->OLE_color col)
  )
)

(defun getTextWinTextColor()
  (OLE_color->ACI_color 
    (vla-get-TextWinTextColor
      (vla-get-display (vla-get-preferences (vlax-get-acad-object)))))
)

(defun putTextWinTextColor(col)
  (vla-put-TextWinTextColor
    (vla-get-display (vla-get-preferences (vlax-get-acad-object)))
    (ACI_color->OLE_color col)
  )
)

(defun getAutoTrackingVecColor()
  (OLE_color->ACI_color 
    (vla-get-AutoTrackingVecColor
      display))
)

(defun putAutoTrackingVecColor(col)
  (vla-put-AutoTrackingVecColor
    display
    (ACI_color->OLE_color col)
  )
)


(defun OLE_color->ACI_color (olec)
  (vl-position
    (boole 1
           (vlax-variant-value
             (vlax-variant-change-type olec vlax-vbLong)
           )
           16777215
    )
    OLE_COLOR_LIST
  )
)


(defun ACI_color->OLE_color (aci)
  ; black is 0
  ; white is 7
  (if (and (>= aci 0) (<= 255))
    (setq aci (nth aci OLE_COLOR_LIST)
    )
  )
)

(setq OLE_COLOR_LIST
       '(0          255        65535      65280      16776960
         16711680   16711935   16777215   8421504    12632256
         255        8421631    166        5460902    128
         4210816    76         2500172    38         1250086
         16639      8429567    10662      5466278    8320
         4214912    4940       2502732    2598       1251366
         33023      8437759    21414      5471398    16512
         4219008    9804       2505036    4902       1252646
         49151      8445951    31910      5476774    24704
         4223104    14668      2507596    7462       1253670
         65535      8454143    42662      5482150    32896
         4227200    19532      2509900    9766       1254950
         65471      8454111    42620      5482129    32864
         4227184    19513      2509891    9757       1254945
         65408      8454079    42579      5482108    32832
         4227168    19494      2509881    9747       1254941
         65344      8454047    42537      5482088    32800
         4227152    19475      2509872    9738       1254936
         65280      8454016    42496      5482067    32768
         4227136    19456      2509862    9728       1254931
         4259584    10485632   2729472    6858323    2129920
         5275712    1264640    3165222    665088     1582611
         8453888    12582784   5481984    8169043    4227072
         6324288    2509824    3755046    1254912    1910291
         12582656   14679936   8168960    9545299    6324224
         7372864    3755008    4410406    1910272    2172435
         16776960   16777088   10921472   10921555   8421376
         8421440    5000192    5000230    2500096    2500115
         16760576   16768896   10910720   10916179   8413184
         8417344    4995328    4997926    2497792    2498835
         16744448   16760704   10900224   10910803   8404992
         8413248    4990464    4995366    2495232    2497811
         16728064   16752512   10889472   10905683   8396800
         8409152    4985600    4993062    2492928    2496531
         16711680   16744576   10878976   10900307   8388608
         8405056    4980736    4990502    2490368    2495251
         16711744   16744607   10879017   10900328   8388640
         8405072    4980755    4990512    2490378    2495256
         16711808   16744639   10879059   10900348   8388672
         8405088    4980774    4990521    2490387    2495261
         16711871   16744671   10879100   10900369   8388704
         8405104    4980793    4990531    2490397    2495265
         16711935   16744703   10879142   10900390   8388736
         8405120    4980812    4990540    2490406    2495270
         12517631   14647551   8126630    9524134    6291584
         7356544    3735628    4400716    1900582    2167590
         8388863    12550399   5439654    8147878    4194432
         6307968    2490444    3745356    1245222    1905446
         4194559    10453247   2687142    6837158    2097280
         5259392    1245260    3155532    655398     1577766
         5526612    5987163    10000536   12303291   14540253
         16777215
        )
)

 ;;; Only for AutoCAD 2004
 (defun RGBtoACI (RGB-codes)
   (vl-load-com)
   (setq ColorObj (vla-GetInterfaceObject
      (vlax-get-acad-object)
      "AutoCAD.AcCmColor.16"
    )
   )
   (vla-setRGB ColorObj (car RGB-codes) (cadr RGB-codes) (caddr RGB-codes))
   ; alternatively done as below
   ; (vlax-invoke-method  ColorObj 'setRGB (car RGB-codes) (cadr RGB-codes) (caddr RGB-codes))
   (vla-get-ColorIndex ColorObj)
 )

 (defun RGBtoOLE_color2 (RGB-codes)
   (+ (* (car RGB-codes) 65536)
      (* (cadr RGB-codes) 256)
      (caddr RGB-codes)
   )
 )

(defun OLEtoRGB_color2 (OLE_color / a b c)
   (setq a (fix (/ OLE_color 65536.0)))
   (setq b (fix (/ (- OLE_color (* a 65536)) 256.0)))
   (setq c (- OLE_color (* a 65536) (* b 256)))
   (list a b c)
 )

;; Convert TrueColor into a list of RGB
(defun OLEtoRGB_color (OLE_color / r g b)
  (setq r (lsh OLE_color -16))
  (setq g (lsh (lsh OLE_color 16) -24))
  (setq b (lsh (lsh OLE_color 24) -24))
  (list r g b)
)

;; Convert a list of RGB to TrueColor
;;; (RGBtoOLE_color '(118 118 118))
(defun RGBtoOLE_color (RGB-codes)
  (setq r (lsh (car RGB-codes) 16))
  (setq g (lsh (cadr RGB-codes) 8))
  (setq b (caddr RGB-codes))
  (+ (+ r g) b)
)

; For AutoCAD 2004
; (OLE_colorToACI 5987163) returns 251
(defun OLE_colorToACI (OLE_color)
  (RGBtoACI (OLEtoRGB_color OLE_color))
)

(defun C:getColor(/)
  (setq ename (car (entsel "\nPick an object with true color:")))
  (setq edata (entget ename))
  ;; we have a true color.
  (setq tcol (cdr (assoc 420 edata)))
  (princ "\n true color = ")(princ tcol) 
  ;; convert it to a list of RGB.
  (setq rgb (OLEtoRGB_color tcol))
  (princ "\n rgb = ")(princ rgb)
  (princ)
)
 
(defun C:setColor(/)
  (setq ename (car (entsel "\nPick an object to set a true color:")))
  (setq edata (entget ename))
  ;; set a true color from a list of rgb values.(R=10 G=100 B=200)
  (setq rgb (list 10 100 200))
  (setq tcol (RGBtoOLE_color rgb))
  ;; and set it.  
  (setq edata (subst (cons 420 tcol) (assoc 420 edata) edata))
  (entmod edata)
  (princ "\n rgb = ")(princ rgb) 
  (princ "\n true color = ")(princ tcol) 
  (princ)
)


;;; (ACItoRGB 123)
(defun ACItoRGB (aci)
  (if (and (>= aci 0) (<= 255))
    (nth aci RGB_list)
  )
)

(setq RGB_list '(
		 (0 0 0)
		 (255 0 0)
		 (255 255 0)
		 (0 255 0)
		 (0 255 255)
		 (0 0 255)
		 (255 0 255)
		 (255 255 255)
		 (128 128 128)
		 (192 192 192)
		 (255 0 0)
		 (255 127 127)
		 (165 0 0)
		 (165 82 82)
		 (127 0 0)
		 (127 63 63)
		 (76 0 0)
		 (76 38 38)
		 (38 0 0)
		 (38 19 19)
		 (255 63 0)
		 (255 159 127)
		 (165 41 0)
		 (165 103 82)
		 (127 31 0)
		 (127 79 63)
		 (76 19 0)
		 (76 47 38)
		 (38 9 0)
		 (38 23 19)
		 (255 127 0)
		 (255 191 127)
		 (165 82 0)
		 (165 124 82)
		 (127 63 0)
		 (127 95 63)
		 (76 38 0)
		 (76 57 38)
		 (38 19 0)
		 (38 28 19)
		 (255 191 0)
		 (255 223 127)
		 (165 124 0)
		 (165 145 82)
		 (127 95 0)
		 (127 111 63)
		 (76 57 0)
		 (76 66 38)
		 (38 28 0)
		 (38 33 19)
		 (255 255 0)
		 (255 255 127)
		 (165 165 0)
		 (165 165 82)
		 (127 127 0)
		 (127 127 63)
		 (76 76 0)
		 (76 76 38)
		 (38 38 0)
		 (38 38 19)
		 (191 255 0)
		 (223 255 127)
		 (124 165 0)
		 (145 165 82)
		 (95 127 0)
		 (111 127 63)
		 (57 76 0)
		 (66 76 38)
		 (28 38 0)
		 (33 38 19)
		 (127 255 0)
		 (191 255 127)
		 (82 165 0)
		 (124 165 82)
		 (63 127 0)
		 (95 127 63)
		 (38 76 0)
		 (57 76 38)
		 (19 38 0)
		 (28 38 19)
		 (63 255 0)
		 (159 255 127)
		 (41 165 0)
		 (103 165 82)
		 (31 127 0)
		 (79 127 63)
		 (19 76 0)
		 (47 76 38)
		 (9 38 0)
		 (23 38 19)
		 (0 255 0)
		 (127 255 127)
		 (0 165 0)
		 (82 165 82)
		 (0 127 0)
		 (63 127 63)
		 (0 76 0)
		 (38 76 38)
		 (0 38 0)
		 (19 38 19)
		 (0 255 63)
		 (127 255 159)
		 (0 165 41)
		 (82 165 103)
		 (0 127 31)
		 (63 127 79)
		 (0 76 19)
		 (38 76 47)
		 (0 38 9)
		 (19 38 23)
		 (0 255 127)
		 (127 255 191)
		 (0 165 82)
		 (82 165 124)
		 (0 127 63)
		 (63 127 95)
		 (0 76 38)
		 (38 76 57)
		 (0 38 19)
		 (19 38 28)
		 (0 255 191)
		 (127 255 223)
		 (0 165 124)
		 (82 165 145)
		 (0 127 95)
		 (63 127 111)
		 (0 76 57)
		 (38 76 66)
		 (0 38 28)
		 (19 38 33)
		 (0 255 255)
		 (127 255 255)
		 (0 165 165)
		 (82 165 165)
		 (0 127 127)
		 (63 127 127)
		 (0 76 76)
		 (38 76 76)
		 (0 38 38)
		 (19 38 38)
		 (0 191 255)
		 (127 223 255)
		 (0 124 165)
		 (82 145 165)
		 (0 95 127)
		 (63 111 127)
		 (0 57 76)
		 (38 66 76)
		 (0 28 38)
		 (19 33 38)
		 (0 127 255)
		 (127 191 255)
		 (0 82 165)
		 (82 124 165)
		 (0 63 127)
		 (63 95 127)
		 (0 38 76)
		 (38 57 76)
		 (0 19 38)
		 (19 28 38)
		 (0 63 255)
		 (127 159 255)
		 (0 41 165)
		 (82 103 165)
		 (0 31 127)
		 (63 79 127)
		 (0 19 76)
		 (38 47 76)
		 (0 9 38)
		 (19 23 38)
		 (0 0 255)
		 (127 127 255)
		 (0 0 165)
		 (82 82 165)
		 (0 0 127)
		 (63 63 127)
		 (0 0 76)
		 (38 38 76)
		 (0 0 38)
		 (19 19 38)
		 (63 0 255)
		 (159 127 255)
		 (41 0 165)
		 (103 82 165)
		 (31 0 127)
		 (79 63 127)
		 (19 0 76)
		 (47 38 76)
		 (9 0 38)
		 (23 19 38)
		 (127 0 255)
		 (191 127 255)
		 (82 0 165)
		 (124 82 165)
		 (63 0 127)
		 (95 63 127)
		 (38 0 76)
		 (57 38 76)
		 (19 0 38)
		 (28 19 38)
		 (191 0 255)
		 (223 127 255)
		 (124 0 165)
		 (145 82 165)
		 (95 0 127)
		 (111 63 127)
		 (57 0 76)
		 (66 38 76)
		 (28 0 38)
		 (33 19 38)
		 (255 0 255)
		 (255 127 255)
		 (165 0 165)
		 (165 82 165)
		 (127 0 127)
		 (127 63 127)
		 (76 0 76)
		 (76 38 76)
		 (38 0 38)
		 (38 19 38)
		 (255 0 191)
		 (255 127 223)
		 (165 0 124)
		 (165 82 145)
		 (127 0 95)
		 (127 63 111)
		 (76 0 57)
		 (76 38 66)
		 (38 0 28)
		 (38 19 33)
		 (255 0 127)
		 (255 127 191)
		 (165 0 82)
		 (165 82 124)
		 (127 0 63)
		 (127 63 95)
		 (76 0 38)
		 (76 38 57)
		 (38 0 19)
		 (38 19 28)
		 (255 0 63)
		 (255 127 159)
		 (165 0 41)
		 (165 82 103)
		 (127 0 31)
		 (127 63 79)
		 (76 0 19)
		 (76 38 47)
		 (38 0 9)
		 (38 19 23)
		 (0 0 0)
		 (51 51 51)
		 (102 102 102)
		 (153 153 153)
		 (204 204 204)
		 (255 255 255)
		)
)

;;; Examples
;;; 
;;; (RGBtoACI '(91 91 91)) returns 251
;;; (RGBtoACI '(118 118 118)) returns 8
;;; (RGBtoOLE_color '(118 118 118)) gives OLE_color=7763574
;;; (OLEtoRGB_color 7763574) gives (118 118 118)
;;; (RGBtoOLE_color '(91 91 91)) gives OLE_color=5987163
;;; (OLEtoRGB_color 5987163) gives (91 91 91)
;;; (RGBtoACI '(101 101 101)) returns 8
;;; (OLEtoRGB_color 6645093) returns (101 101 101)
;;; (ACItoRGB 123)

;;; Missing is conversion from ACI to RGB or ACI to OLE_color



