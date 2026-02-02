CALL SET_Directories

REM ================
REM FE Model
SET program=fracture_mechanics
CALL ACTIVATE %program%

CD %git_root%
CALL PYTHON %program%.py

SET file_name=fracture_mechanics_py_2500ft_WT0750_064pcf.yml
CALL PYTHON %program%.py "%working_directory%\%file_name%"

SET file_name=fracture_mechanics_py_2500ft_WT0750_120pcf.yml
CALL PYTHON %program%.py "%working_directory%\%file_name%"

REM SET file_name=fracture_mechanics_py_2500ft_WT0750_120pcf_CK_line.yml
REM CALL PYTHON %program%.py "%working_directory%\%file_name%"

REM SET file_name=fracture_mechanics_py_sens_flaw_location_circumferential_internal_vs_external.yml
REM CALL PYTHON %program%.py "%working_directory%\%file_name%"

REM SET file_name=fracture_mechanics_py_sens_flaw_location_axial_internal_vs_external.yml
REM CALL PYTHON %program%.py "%working_directory%\%file_name%"

REM SET file_name=fracture_mechanics_py_sens_flaw_location_circumferential_internal_vs_embedded.yml
REM CALL PYTHON %program%.py "%working_directory%\%file_name%"

REM SET file_name=fracture_mechanics_py_sens_flaw_location_axial_internal_vs_embedded.yml
REM CALL PYTHON %program%.py "%working_directory%\%file_name%"


CD %working_directory%

