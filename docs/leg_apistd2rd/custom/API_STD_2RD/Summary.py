# -*- coding: utf-8 -*-
"""
Created on September 20 2018
"""
'''
Author: Vamsee Achanta
Date Updated: 2018-09-20
Objective: To summarize Simple Catenary and Lazy Wave Catenary results
Run instructions with:
 Default yml file : python APISTD2RD.py

 UPDATES: 
 Input YML: Relocate Spacing to PlotSettings
 Rename Summary "Hangoff" to "HangOffToSag"

'''
import sys
import logging

from math import *
# from catenarycalculation import *
from DataManager.API_STD_2RD.ymlInput import ymlInput
from CommonFiles.API_STD_2RD.setLogging import setLogging
from fileList import fileList
from extractData import extractData
from CommonFiles.API_STD_2RD.saveData import saveDataFrame

# Data preparation
defaultYml = "DataManager\\API_STD_2RD\\summary.yml"
# Get updateYML file
try:
    if (sys.argv[1] != None):
        updateYml = "DataManager\\API_STD_2RD\\" + sys.argv[1]
        logging.critical("Updating default values with contents in file {0}" .format(updateYml))
except:
    updateYml = None
    logging.critical("No update values file is provided. Running program default values")

# Get updated configuration file for Analysis
cfg = ymlInput(defaultYml, updateYml)

# Set logging
setLogging(cfg['default']['logLevel'])

# Get file List
data = {"Folder": cfg['ymlFiles']['Folder'], "FileNameFilter": cfg['ymlFiles']['FileNameFilter']}
fileList = fileList(data)

# Get data
dataDF = extractData(fileList, cfg)

# Save Data
fileName = cfg['dataFrame']['label']
saveDataFrame(dataDF, fileName)