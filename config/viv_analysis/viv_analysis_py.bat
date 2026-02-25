CALL SET_Directories

REM ================
REM FE Model
SET program=viv_analysis
CALL ACTIVATE %program%

CD %git_root%
REM CALL PYTHON %program%.py

REM SET file_name=viv_analysis_py_modes_comparison.yml
REM CALL PYTHON %program%.py "%working_directory%\%file_name%"

REM SET file_name=viv_analysis_py_modes_comparison_A10.yml
REM CALL PYTHON %program%.py "%working_directory%\%file_name%"

REM SET file_name=viv_analysis_py_modes_comparison_A6.yml
REM CALL PYTHON %program%.py "%working_directory%\%file_name%"

REM SET file_name=viv_analysis_py_hm_extreme_all_currents.yml
REM CALL PYTHON %program%.py "%working_directory%\%file_name%"

REM SET file_name=viv_analysis_py_hm_extreme_all_ttrs.yml
REM CALL PYTHON %program%.py "%working_directory%\%file_name%"
REM SET file_name=viv_analysis_py_hm_extreme_A6_ttr.yml
REM CALL PYTHON %program%.py "%working_directory%\%file_name%"
REM SET file_name=viv_analysis_py_hm_extreme_A10_ttr.yml
REM CALL PYTHON %program%.py "%working_directory%\%file_name%"
REM SET file_name=viv_analysis_py_hm_extreme_A5_ttr.yml
REM CALL PYTHON %program%.py "%working_directory%\%file_name%"

REM SET file_name=viv_analysis_py_hm_extreme_current_Shielding.yml
REM CALL PYTHON %program%.py "%working_directory%\%file_name%"


REM SET file_name=viv_analysis_py_hm_long_term_A5_Low.yml
REM CALL PYTHON %program%.py "%working_directory%\%file_name%"
REM SET file_name=viv_analysis_py_hm_long_term_A6_Low.yml
REM CALL PYTHON %program%.py "%working_directory%\%file_name%"
REM SET file_name=viv_analysis_py_hm_long_term_A10_Low.yml
REM CALL PYTHON %program%.py "%working_directory%\%file_name%"

REM SET file_name=viv_analysis_py_ecs_dr_ffs_currents.yml
REM CALL PYTHON %program%.py "%working_directory%\%file_name%"


REM External files
SET file_name=viv_analysis_py_ecs_mds_water_depth.yml
CALL PYTHON %program%.py "%working_directory%\%file_name%"
SET file_name=viv_analysis_py_ecs_mds_internal_fluid.yml
CALL PYTHON %program%.py "%working_directory%\%file_name%"
SET file_name=viv_analysis_py_ecs_mds_WT.yml
CALL PYTHON %program%.py "%working_directory%\%file_name%"


CD %working_directory%
