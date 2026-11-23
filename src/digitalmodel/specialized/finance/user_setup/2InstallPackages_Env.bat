SET py_environment=pandasDataReader_env
CALL activate %py_environment%

CALL (echo y) | conda install pandas_datareader
CALL (echo y) | pip install pandas_datareader
CALL (echo y) | conda install pandas
