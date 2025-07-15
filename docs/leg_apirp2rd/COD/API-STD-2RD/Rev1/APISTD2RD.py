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
import sys
import logging

from APISTD2RDMethods import variableWTBurst, nominalWTAPISTD2RDMethod1
from dataManager.ymlInput import ymlInput
from dataManager.customUpdate import customUpdate
from logs.setLogging import setLogging
from results.plotDefault import plotDefault
from results.plotDefaultM1 import plotDefaultM1
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

if cfg['default']['Analysis']['variableWTCollapse'] == True:
    pass
 #   variableWTCollapse(cfg)

cfg = ymlInput(defaultYml, updateYml)
cfg = customUpdate(cfg)
if cfg['default']['Analysis']['nominalWTAPISTD2RDMethod1'] == True:
    resultData, resultDF_SLS = nominalWTAPISTD2RDMethod1(cfg, "SLS")

    resultData, resultDF_ALS = nominalWTAPISTD2RDMethod1(cfg, "ALS")
    #print(resultData)
    #print(resultDF)
#  Save required data into cfg

#  Prepare plot
    plotDefaultM1(cfg, resultDF_SLS, resultDF_ALS)
    #print(resultDF_SLS)
    #print(resultDF_ALS)
    #saveData(cfg, resultDF_SLS, resultDF_ALS)
   

    

