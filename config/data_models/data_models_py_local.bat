CALL ..\SET_Directories

REM ================
REM Data Models
SET program=data_models
REM CALL ACTIVATE %program%

CD %git_root%
REM CALL PYTHON %program%.py

SET file_name=bsee_2020_data_refresh_eapd_sqlexpress.yml
CALL PYTHON %program%.py "%working_directory%\%file_name%"

REM SET file_name=bsee_2020_data_refresh_eapd.yml
REM CALL PYTHON %program%.py "%working_directory%\%file_name%"

CD %working_directory%
SET working_directory%=
