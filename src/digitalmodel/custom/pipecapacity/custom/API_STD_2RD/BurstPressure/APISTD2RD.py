# -*- coding: utf-8 -*-
"""
Created on September 16 2018
"""
'''
Author: Vamsee Achanta
Date Updated: 2018-09-16
Objective: To assess API STD 2RD methods
Run instructions with:
 Default yml file : python APISTD2RD.py
 Update default yml with parameters in 12.yml: python APISTD2RD.py 12.yml
 Outputs: DataFrame with outputs
'''
import logging
import sys

import numpy as np
from APISTD2RDMethods import variableWTBurst
from dataManager.customUpdate import customUpdate
from dataManager.ymlInput import ymlInput
from logs.setLogging import setLogging
from results.plotDefault import plotDefault
from results.saveData import saveData

# Data preparation
defaultYml = "dataManager\\APISTD2RD.yml"
# Get updateYML file
try:
    if (sys.argv[1] != None):
        updateYml = "dataManager\\" + sys.argv[1]
        logging.critical("Updating default values with contents in file {0}" .format(updateYml) )
except:
    updateYml = None
    logging.critical("No update values file is provided. Running program default values")

# Get updated configuration file for Analysis
cfg = ymlInput(defaultYml, updateYml)
cfg = customUpdate(cfg)
    
# Set logging
setLogging(cfg['default']['logLevel'])

if cfg['default']['Analysis']['variableWTBurst'] == True:
    (cfg, dataDF) = variableWTBurst(cfg)
    plotDefault(cfg, dataDF)
    saveData(cfg, dataDF)


