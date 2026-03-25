CALL SET_Directories

REM ================
REM FE Model
SET program=pipe
CALL ACTIVATE %program%

CD %git_root%
CALL PYTHON %program%.py

SET file_name=pipe_py_vm_stress_validation.yml
CALL PYTHON %program%.py "%working_directory%\%file_name%"

SET file_name=pipe_py_21OD_1in_Drilling_Riser.yml
CALL PYTHON %program%.py "%working_directory%\%file_name%"

SET file_name=pipe_py_21OD_0875in_Drilling_Riser.yml
CALL PYTHON %program%.py "%working_directory%\%file_name%"

SET file_name=pipe_py_6OD_1000in_CK_line.yml
CALL PYTHON %program%.py "%working_directory%\%file_name%"

SET file_name=API579_py_8in_k2_rev2.yml
CALL PYTHON %program%.py "%working_directory%\%file_name%"

SET file_name=API579_py_8in_Lucius_PR-02.yml
CALL PYTHON %program%.py "%working_directory%\%file_name%"

SET file_name=pipe_py_10in_gas_matterhorn_p3_Code_B318_2016.yml
CALL PYTHON %program%.py "%working_directory%\%file_name%"

SET file_name=pipe_py_10in_gas_matterhorn_p3_Code_30CFR250.yml
CALL PYTHON %program%.py "%working_directory%\%file_name%"

SET file_name=pipe_py_8in_oil_matterhorn_p4_Code_B314_2016.yml
CALL PYTHON %program%.py "%working_directory%\%file_name%"

SET file_name=pipe_py_8in_oil_matterhorn_p4_Code_30CFR250.yml
CALL PYTHON %program%.py "%working_directory%\%file_name%"

SET file_name=pipe_py_8in_oil_matterhorn_p4_Code_B314_2016_PR_Sens.yml
CALL PYTHON %program%.py "%working_directory%\%file_name%"

SET file_name=pipe_py_8in_oil_matterhorn_p4_Code_30CFR250_PR_Sens.yml
CALL PYTHON %program%.py "%working_directory%\%file_name%"


CD %working_directory%

