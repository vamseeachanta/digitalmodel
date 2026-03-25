CALL SET_Directories

REM ================
REM FE Model
SET program=compare_tool
CALL ACTIVATE %program%

CD %git_root%
CALL PYTHON %program%.py

SET file_name=compare_tool_py_all_ascii_files.yml
CALL PYTHON %program%.py "%working_directory%\%file_name%"

SET file_name=compare_tool_py_all_csv_files.yml
CALL PYTHON %program%.py "%working_directory%\%file_name%"

SET file_name=compare_tool_py_all_yaml_files.yml
CALL PYTHON %program%.py "%working_directory%\%file_name%"

SET file_name=compare_tool_py_all_xlsx_files.yml
CALL PYTHON %program%.py "%working_directory%\%file_name%"

SET file_name=compare_tool_py_log_scale_axis.yml
CALL PYTHON %program%.py "%working_directory%\%file_name%"

CD %working_directory%

