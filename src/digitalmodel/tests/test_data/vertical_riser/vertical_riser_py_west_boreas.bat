CALL SET_Directories

REM ================
REM FE Model
SET program=vertical_riser
CALL ACTIVATE %program%

SET file_name=vertical_riser_py_west_boreas.yml
CD %git_root%
CALL PYTHON %program%.py "%working_directory%\%file_name%"
CD %working_directory%
