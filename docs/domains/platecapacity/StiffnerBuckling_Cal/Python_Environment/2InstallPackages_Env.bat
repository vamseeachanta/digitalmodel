SET py_environment=DataLoader_env

CALL activate %py_environment%

CALL (echo y) | conda install pymssql
CALL (echo y) | conda install cx_Oracle
