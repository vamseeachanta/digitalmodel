CALL ..\SET_Directories

REM ================
REM FE Model
SET program=data_models
REM CALL ACTIVATE %program%

CD %git_root%
REM CALL PYTHON %program%.py

REM SET file_name=bsee_2018_g_and_g_data_models.yml
REM CALL PYTHON %program%.py "%working_directory%\%file_name%"

REM SET file_name=bsee_2020_apd.yml
REM CALL PYTHON %program%.py "%working_directory%\%file_name%"

REM SET file_name=bsee_2020_yearly_production_data.yml
REM CALL PYTHON %program%.py "%working_directory%\%file_name%"

REM SET file_name=bsee_2017_public_tables.yml
REM CALL PYTHON %program%.py "%working_directory%\%file_name%"

REM SET file_name=bsee_2020_eWellRawData.yml
REM CALL PYTHON %program%.py "%working_directory%\%file_name%"

REM SET file_name=bsee_2020_eWellRawData_manual.yml
REM CALL PYTHON %program%.py "%working_directory%\%file_name%"

REM SET file_name=standard_pipe_data.yml
REM CALL PYTHON %program%.py "%working_directory%\%file_name%"

REM SET file_name=bsee_2020_reference_tables.yml
REM CALL PYTHON %program%.py "%working_directory%\%file_name%"

SET file_name=app_master_tables.yml
CALL PYTHON %program%.py "%working_directory%\%file_name%"

REM SET file_name=bsee_2020_directional_surveys.yml
REM CALL PYTHON %program%.py "%working_directory%\%file_name%"

REM SET file_name=bsee_2018_public_tables.yml
REM CALL PYTHON %program%.py "%working_directory%\%file_name%"

CD %working_directory%
SET working_directory%=
