CALL SET_Directories

REM ================
REM FE Model
SET program=fatigue_analysis
CALL ACTIVATE %program%

CD %git_root%
CALL PYTHON %program%.py

CD %working_directory%

