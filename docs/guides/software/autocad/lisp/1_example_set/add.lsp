;�� ���� ������� ���� ����� ������ ��� ������ �����
;���� �� ������ �� ���� ������ ����� ���� ����� ���� ��������� ������ �������
;������ ���  �� ���� ��� ����� �� �� ������� ���� ����� �� ��� ������ ���� ��� ����� ������ ��
;�� ������� ��� �������  ����� ��� ����� �� �������
; add ������ �������� ���� �� ���� 
(defun c:add ()
(setq sum (list + ))
  (while
    (setq pt1 (ssget))
    (Setvar "osmode" 0)
    (setq oldelem (entget (ssname pt1 0)))      
  (setq txtstr (assoc 1 oldelem))
  (setq num (atof(cdr txtstr)))                                       
  (setq sum (append sum (list num)))
  (terpri)
  (princ num)
  (terpri)
  (princ "total=")
  (princ (rtos(eval sum)2 3))
   )
  )
