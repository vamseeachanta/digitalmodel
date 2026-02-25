CALL SET_Directories

REM ================
REM FE Model
SET program=fracture_mechanics
CALL ACTIVATE %program%

CD %git_root%

SET file_name=fracture_mechanics_py_ecs_2500ft_upper_riser.yml
CALL PYTHON %program%.py "%working_directory%\%file_name%"

SET file_name=fracture_mechanics_py_ecs_2500ft_buoy_jt.yml
CALL PYTHON %program%.py "%working_directory%\%file_name%"

SET file_name=fracture_mechanics_py_ecs_2500ft_lower_riser.yml
CALL PYTHON %program%.py "%working_directory%\%file_name%"

CD %working_directory%

