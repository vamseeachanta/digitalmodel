(defun c:blk2pt (/ doc spc ss)
  (vl-load-com)
  (setq doc (vla-get-ActiveDocument (vlax-get-Acad-Object))
        spc (if (zerop (vla-get-activespace doc))
              (if (= (vla-get-mspace doc) :vlax-true)
                (vla-get-modelspace doc)
                (vla-get-paperspace doc))
              (vla-get-modelspace doc)))  
  (if (setq ss (ssget '((0 . "INSERT"))))
    (progn
      (mapcar
        (function
          (lambda (x)
            (vla-addPoint spc
              (vlax-3D-point x))))
        (mapcar
          (function
            (lambda (x)
              (cdr (assoc 10 (entget x)))))
          (vl-remove-if 'listp
            (mapcar 'cadr (ssnamex ss)))))
      (command "_.erase" ss "")))
  (princ))