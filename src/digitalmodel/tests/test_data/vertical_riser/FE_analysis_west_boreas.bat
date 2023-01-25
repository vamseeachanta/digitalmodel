CALL SET_Directories

REM ================
REM Run FE Simulation for Effective Tension QA


SET program=orcaflex_analysis
CALL ACTIVATE %program%

SET file_name=FE_analysis_west_boreas.yml
CD %git_root%
CALL PYTHON %program%.py "%working_directory%\%file_name%"
CD %working_directory%

