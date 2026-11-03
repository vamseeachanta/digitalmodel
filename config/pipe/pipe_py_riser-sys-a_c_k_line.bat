CALL SET_Directories

REM ================
REM FE Model
SET program=pipe
TITLE %program%
CALL ACTIVATE %program%

CD %git_root%
REM CALL PYTHON %program%.py

SET file_name=pipe_py_06OD_1000in_2500ft_upper_riser.yml
CALL PYTHON %program%.py "%working_directory%\%file_name%"

SET file_name=pipe_py_06OD_1000in_5000ft_upper_riser.yml
CALL PYTHON %program%.py "%working_directory%\%file_name%"

SET file_name=pipe_py_06OD_1000in_8000ft_upper_riser.yml
CALL PYTHON %program%.py "%working_directory%\%file_name%"

SET file_name=pipe_py_06OD_1000in_10000ft_upper_riser.yml
CALL PYTHON %program%.py "%working_directory%\%file_name%"

SET file_name=pipe_py_06OD_1000in_2500ft_lower_riser.yml
CALL PYTHON %program%.py "%working_directory%\%file_name%"

SET file_name=pipe_py_06OD_1000in_5000ft_lower_riser.yml
CALL PYTHON %program%.py "%working_directory%\%file_name%"

SET file_name=pipe_py_06OD_1000in_8000ft_lower_riser.yml
CALL PYTHON %program%.py "%working_directory%\%file_name%"

SET file_name=pipe_py_06OD_1000in_10000ft_lower_riser.yml
CALL PYTHON %program%.py "%working_directory%\%file_name%"

CD %working_directory%

