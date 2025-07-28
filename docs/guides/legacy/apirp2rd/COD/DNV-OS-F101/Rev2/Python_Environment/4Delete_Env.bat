SET py_environment=PipeCapacity_env

CALL c:\data\Continuum\Anaconda3\Scripts\activate.bat
CALL deactivate %py_environment%
(echo y) | CALL conda-env remove -n %py_environment%