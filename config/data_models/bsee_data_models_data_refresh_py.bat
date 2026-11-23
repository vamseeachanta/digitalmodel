CALL ..\SET_Directories

REM ================
REM Data Models
SET program=data_models
REM CALL ACTIVATE %program%

CD %git_root%
REM CALL PYTHON %program%.py

SET file_name=bsee_API_and_BHPT.yml
CALL PYTHON %program%.py "%working_directory%\%file_name%"

SET file_name=bsee_Borehole.yml
CALL PYTHON %program%.py "%working_directory%\%file_name%"

SET file_name=bsee_eWellRawData.yml
CALL PYTHON %program%.py "%working_directory%\%file_name%"

SET file_name=bsee_reference_tables.yml
CALL PYTHON %program%.py "%working_directory%\%file_name%"

SET file_name=bsee_yearly_production_data.yml
CALL PYTHON %program%.py "%working_directory%\%file_name%"

SET file_name=bsee_directional_surveys.yml
CALL PYTHON %program%.py "%working_directory%\%file_name%"

SET file_name=bsee_Decomm.yml
CALL PYTHON %program%.py "%working_directory%\%file_name%"

CD %working_directory%
SET working_directory%=
