CALL SET_Directories

REM ================
REM FE Model
SET program=fracture_mechanics
REM CALL ACTIVATE %program%

CD %git_root%
REM CALL PYTHON %program%.py

REM SET file_name=fracture_mechanics_py_2500ft_WT0750_064pcf.yml
REM CALL PYTHON %program%.py "%working_directory%\%file_name%"

SET file_name=fracture_mechanics_py_sens_flaw_location_circumferential_internal_vs_external.yml
CALL PYTHON %program%.py "%working_directory%\%file_name%"
SET file_name=fracture_mechanics_py_sens_flaw_location_axial_internal_vs_external.yml
CALL PYTHON %program%.py "%working_directory%\%file_name%"
SET file_name=fracture_mechanics_py_sens_flaw_location_circumferential_external_vs_embedded.yml
CALL PYTHON %program%.py "%working_directory%\%file_name%"
SET file_name=fracture_mechanics_py_sens_flaw_location_axial_external_vs_embedded.yml
CALL PYTHON %program%.py "%working_directory%\%file_name%"

REM SET file_name=fracture_mechanics_py_sens_stress_intensity_table_solution.yml
REM CALL PYTHON %program%.py "%working_directory%\%file_name%"
REM SET file_name=fracture_mechanics_py_sens_stress_intensity_constant_solution.yml
REM CALL PYTHON %program%.py "%working_directory%\%file_name%"

CD %working_directory%

