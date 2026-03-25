SET py_environment=MySQL_env
SET py_program=dynaCard.py --filename 30015201550000MibA1_lf_rpc1_Hprd6_Card_Items_Lastshutdown_SurfaceCardinput.json
REM SET py_program=dynaCard.py --databaseID 150

CALL c:\data\Continuum\Anaconda3\Scripts\activate.bat  %py_environment%
CALL python %py_program%