CALL SET_Directories

REM ================
REM FE Model
SET program=ETL
CALL ACTIVATE %program%

CD %git_root%
REM CALL PYTHON %program%.py

REM SET file_name=ETL_py_current_profile_transformation.yaml
REM CALL PYTHON %program%.py "%working_directory%\%file_name%"

REM SET file_name=ETL_py_shear7_lid002_2500ft_WT0750_064pcf.yaml
REM CALL PYTHON %program%.py "%working_directory%\%file_name%"

REM SET file_name=ETL_py_shear7_hm_extreme.yaml
REM CALL PYTHON %program%.py "%working_directory%\%file_name%"


REM SET file_name=ETL_py_shear7_hm_extreme.yaml
REM CALL PYTHON %program%.py "%working_directory%\%file_name%"

REM SET file_name=ETL_py_shear7_hm_extreme_stem_225.yaml
REM CALL PYTHON %program%.py "%working_directory%\%file_name%"

REM SET file_name=ETL_py_shear7_hm_extreme_stem_510.yaml
REM CALL PYTHON %program%.py "%working_directory%\%file_name%"

REM SET file_name=ETL_py_shear7_hm_extreme_stem_no_shield.yaml
REM CALL PYTHON %program%.py "%working_directory%\%file_name%"

SET file_name=ETL_py_shear7_hm_long_term.yaml
CALL PYTHON %program%.py "%working_directory%\%file_name%"


CD %working_directory%
