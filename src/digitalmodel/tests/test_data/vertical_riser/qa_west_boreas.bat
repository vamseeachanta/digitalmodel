CALL SET_Directories

REM ================
REM Run Effective Tension QA
SET program=compare_tool
CALL ACTIVATE %program%

SET file_name=qa_west_boreas.yml
CD %git_root%
CALL PYTHON %program%.py "%working_directory%\%file_name%"
CD %working_directory%

