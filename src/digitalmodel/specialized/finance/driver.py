'''
Author: Vamsee Achanta
Date Updated: 2018-04-12
Objective: get finance data (using python)
Outputs: 
Running Program:
    python driver.py --dataSource="morningstar"
    python driver.py --dataSource="google"
'''
from data_manager.argumentParseFunction import *
from pandas_datareader import data

configData = argumentParseFunction()
print ("Getting Data from : " + configData.dataSource[0])
# Define which online source one should use
data_source = configData.dataSource[0]

import datetime

import pandas_datareader.data as web

start = datetime.datetime(2010, 1, 1)
end = datetime.datetime(2013, 1, 27)
f = web.DataReader('OXY', data_source, start, end)

print(web.DataReader('OXY', data_source, start, end))