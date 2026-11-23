SET py_environment=PipeCapacity_env


REM Runs in Anaconda windows prompt
ECHO Creating Anaconda Python environment for DataLoader

(echo y) | CALL conda create -n %py_environment% python=3.5

CALL activate %py_environment%

