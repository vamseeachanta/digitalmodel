CALL SET_Directories

REM ================
REM FE Model
SET program=fracture_mechanics
CALL ACTIVATE %program%

CD %git_root%

REM SET file_name=fracture_mechanics_py_sens_stress_intensity_table_solution.yml
REM CALL PYTHON %program%.py "%working_directory%\%file_name%"

CD %working_directory%

