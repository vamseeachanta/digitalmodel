;; ==================================================================== ;;
;;                                                                      ;;
;;  XTUBE.LSP - Fast 3D-pipe extrude.                                   ;;
;;                                                                      ;;
;; ==================================================================== ;;
;;                                                                      ;;
;;  Command(s) to call: XTUBE                                           ;;
;;                                                                      ;;
;;  Select Lines, Polylines or Arcs, specify external and               ;;
;;  internal pipe diameter and press Enter. The program will            ;;
;;  extrude pipes. You can erase path lines after it.                   ;;
;;                                                                      ;;
;; ==================================================================== ;;
;;                                                                      ;;
;;  THIS PROGRAM AND PARTS OF IT MAY REPRODUCED BY ANY METHOD ON ANY    ;;
;;  MEDIUM FOR ANY REASON. YOU CAN USE OR MODIFY THIS PROGRAM OR        ;;
;;  PARTS OF IT ABSOLUTELY FREE.                                        ;;
;;                                                                      ;;
;;  THIS PROGRAM PROVIDES 'AS IS' WITH ALL FAULTS AND SPECIFICALLY      ;;
;;  DISCLAIMS ANY IMPLIED WARRANTY OF MERCHANTABILITY OR FITNESS        ;;
;;  FOR A PARTICULAR USE.                                               ;;
;;                                                                      ;;
;; ==================================================================== ;;
;;                                                                      ;;
;;  V1.2, 17 June, 2005, Riga, Latvia                                   ;;
;;  © Aleksandr Smirnov (ASMI)                                          ;;
;;  For AutoCAD 2000 - 2008 (isn't tested in a next versions)           ;;
;;                                                                      ;;
;;                             http://www.asmitools.com                 ;;
;;                                                                      ;;
;; ==================================================================== ;;

(defun c:xtube(/ 3DPOS ACTDOC ACTLAY ACTSP BASELINE 
                 BASESET DICOUNT DIVDID EXCIR EXENT 
                 EXTUBE INCIR INENT INTUBE LAYST 
                 OBJTYPE OLDECHO oldWidth oldHeight 
                 STARTPT XORD YORD ZORD DELFLAG *ERROR*)
  
  (vl-load-com) 

  (defun *error* (msg) 
    (vla-put-Lock actLay laySt) 
    (setvar "CMDECHO" oldEcho) 
    (vla-EndUndoMark actDoc) 
    (princ) 
    ); end of *error* 

  (if(not tube:Width)(setq tube:Width 40.0)) 
  (if(not tube:Height)(setq tube:Height 37.0)) 
  (setq actDoc 
    (vla-get-ActiveDocument 
      (vlax-get-Acad-object)) 
   actLay(vla-get-ActiveLayer actDoc) 
        oldWidth tube:Width 
   oldHeight tube:Height 
        oldEcho(getvar "CMDECHO") 
   ); end setq 
  (vla-StartUndoMark actDoc) 
  (setvar "CMDECHO" 0) 
  (if(= 0(vla-get-ActiveSpace actDoc)) 
    (setq actSp(vla-get-PaperSpace actDoc)) 
    (setq actSp(vla-get-ModelSpace actDoc)) 
    ); end if 
  (setq laySt(vla-get-Lock actLay)) 
  (vla-put-Lock actLay :vlax-false) 
  (setq tube:Width 
    (getreal 
      (strcat 
        "\nSpecify external diameter <"(rtos tube:Width)">: ")) 
       tube:Height 
    (getreal 
      (strcat 
        "\nSpecify internal diameter <"(rtos tube:Height)">: ")) 
   ); end setq 
  (if(null tube:Height)(setq tube:Height oldHeight)) 
  (if(null tube:Width)(setq tube:Width oldWidth)) 
  (if(< tube:Height tube:Width) 
    (progn 
  (princ "\n<<< Select objects to extrude and press Enter >>>") 
  (if 
    (setq baseSet 
      (ssget '((-4 . "<OR")(0 . "*LINE")(0 . "CIRCLE") 
           (0 . "ARC")(0 . "ELLIPSE")(-4 . "OR>") 
           (-4 . "<NOT")(-4 . "<OR")(0 . "SPLINE") 
                (0 . "MLINE")(-4 . "OR>")(-4 . "NOT>")))) 
    (progn 
      (setq baseSet(vl-remove-if 'listp 
                              (mapcar 
            'cadr 
            (ssnamex baseSet)))) 
      (foreach pathEnt baseSet 
      (setq baseLine 
        (vlax-ename->vla-object pathEnt) 
            objType(vla-get-ObjectName baseLine) 
            startPt(vlax-curve-getStartPoint baseLine) 
            3dPos 
        (vlax-curve-getFirstDeriv baseLine 
          (vlax-curve-getParamAtPoint baseLine startPt)) 
           diCount(strlen 
            (itoa 
         (apply 'max 
          (mapcar 'abs 
           (mapcar 'fix startPt))))) 
           divDid "1" 
       ); end setq
	(repeat diCount 
          (setq divDid(strcat divDid "0")) 
        ); end repeat 
         (setq divDid(atoi divDid)) 
         (if(/= 0.0(car 3dPos)) 
           (setq XOrd(/(car 3dPos)divDid)) 
           (setq XOrd (car 3dPos)) 
         ); end if 
         (if(/= 0.0(cadr 3dPos)) 
           (setq YOrd(/(cadr 3dPos)divDid)) 
           (setq YOrd (cadr 3dPos)) 
         ); end if 
        (if(/= 0.0(nth 2 3dPos)) 
           (setq ZOrd(/(nth 2 3dPos)divDid)) 
           (setq ZOrd (nth 2 3dPos)) 
         ); end if 
       (setq 3dPos(list XOrd YOrd ZOrd)
             exCir(vla-addCircle actSp 
                     (vlax-3d-Point startPt) 
                       (/ tube:Width 2)) 
             inCir(vla-addCircle actSp 
                     (vlax-3d-Point startPt) 
                        (/ tube:Height 2)) 
             ); end setq 
      (vla-put-Normal exCir(vlax-3D-point 3dPos)) 
      (vla-put-Normal inCir(vlax-3D-point 3dPos)) 
      (setq exEnt(vlax-vla-object->ename exCir) 
            inEnt(vlax-vla-object->ename inCir) 
            ); end setq 
  (command "_.extrude" exEnt "" "_p" pathEnt) 
  (setq exTube(entlast)) 
  (command "_.extrude" inEnt "" "_p" pathEnt) 
  (setq inTube(entlast)) 
  (command "_subtract" exTube "" inTube "") 
  (command "_.erase" exEnt "") 
  (command "_.erase" inEnt "") 
   ); end foreach
   (initget "Yes No") 
      (setq delFlag 
          (getkword "\nDelete extrude path(s)? [Yes/No] <No>: ")) 
      (if(null delFlag)(setq delFlag "No")) 
      (if(= "Yes" delFlag)
	(foreach pathEnt baseSet
           (vla-delete(vlax-ename->vla-object pathEnt))
	 ); end foreach
       ); end if 
      (vla-put-Lock actLay laySt) 
      (setvar "CMDECHO" oldEcho) 
       ); end progn 
      ); end if 
     ); end progn 
    (princ "\nInternal diameter more or equal external diameter! ") 
    ); end if 
  (vla-EndUndoMark actDoc) 
    (princ) 
); end  of c:xtube

(princ "\n[Info] http:\\\\www.AsmiTools.com [Info]")
(princ "\n[Info] Type XPIPE for extrude 3D-pipes. [Info]")