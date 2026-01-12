SET py_environment=pandasDataReader_env

REM Runs in Anaconda windows prompt
ECHO Creating Anaconda Python environment
(echo y) | CALL conda create -n %py_environment% python=3.5
CALL activate %py_environment%

ECHO Location of virtual environment is "C:\Data\Continuum\Anaconda3\envs\"
REM 
