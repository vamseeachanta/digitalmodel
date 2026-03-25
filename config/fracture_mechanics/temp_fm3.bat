CALL SET_Directories

REM ================
REM FE Model
SET program=fracture_mechanics
CALL ACTIVATE %program%

CD %git_root%
REM CALL PYTHON %program%.py

SET file_name=fracture_mechanics_py_sens_axial.yml
CALL PYTHON %program%.py "%working_directory%\%file_name%"

SET file_name=fracture_mechanics_py_sens_circumferential.yml
CALL PYTHON %program%.py "%working_directory%\%file_name%"

CD %working_directory%

