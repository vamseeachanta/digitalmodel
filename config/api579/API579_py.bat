CALL SET_Directories

REM ================
REM FE Model
SET program=API579
CALL ACTIVATE %program%

CD %git_root%

CALL PYTHON %program%.py

SET file_name=API579_py_6in_field-k_Code_B314_2016.yml
CALL PYTHON %program%.py "%working_directory%\%file_name%"
SET file_name=API579_py_6in_field-k_Code_B318_2016.yml
CALL PYTHON %program%.py "%working_directory%\%file_name%"
SET file_name=API579_py_6in_field-k_Code_API1111_2009.yml
CALL PYTHON %program%.py "%working_directory%\%file_name%"

SET file_name=API579_py_8in_platform-q_Pipeline-02.yml
CALL PYTHON %program%.py "%working_directory%\%file_name%"

CD %working_directory%

