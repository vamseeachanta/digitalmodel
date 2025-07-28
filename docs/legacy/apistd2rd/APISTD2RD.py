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
from DataManager.API_STD_2RD.ymlInput import ymlInput
from DataManager.API_STD_2RD.customUpdate import customUpdate
from CommonFiles.API_STD_2RD.setLogging import setLogging
from Results.API_STD_2RD.plotDefault import plotDefault
from Results.API_STD_2RD.plotDefaultM1 import plotDefaultM1
from Results.API_STD_2RD.saveData import saveData
# Data preparation
defaultYml = "DataManager\\API_STD_2RD\\APISTD2RD.yml"
# Get updateYML file
try:
    if (sys.argv[1] != None):
        updateYml = "DataManager\\API_STD_2RD\\" + sys.argv[1]
        logging.critical("Updating default values with contents in file {0}" .format(updateYml) )
except:
    updateYml = None
    logging.critical("No update values file is provided. Running program default values")

# Get updated configuration file for Analysis
cfg = ymlInput(defaultYml, updateYml)
try:
    cfg['FileName'] = updateYml.split('\\')[1].split('.')[0]
except:
    cfg['FileName'] = defaultYml.split('\\')[1].split('.')[0]

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

if cfg['default']['Analysis']['nominalWTAPISTD2RDMethod1'] == True:
    UtilizationDF = []
    for Method1Index in range(0, len(cfg['nominalWTAPISTD2RDMethod1']['data'])):
        # Prepare input data
        if cfg['geometry']['NominalOD'] !=None:
            cfg['geometry']['NominalID'] = cfg['geometry']['NominalOD'] - 2*cfg['geometry']['DesignWT']
        elif cfg['geometry']['NominalID'] !=None:
            cfg['geometry']['NominalOD'] = cfg['geometry']['NominalID'] + 2*cfg['geometry']['DesignWT']

        inputData = ({"OD": cfg['geometry']['NominalOD'],"ID": cfg['geometry']['NominalID'],
            "t": cfg['geometry']['DesignWT'], "E": cfg['material']['E'],
            "Poissionsratio": cfg['material']['Poissionsratio'], "alphafab": cfg['material']['alphafab'],
            "k": cfg['material']['k'], "S": cfg['material']['SMYS'],
            "U": cfg['material']['SMUS'],
            "CorrosionAllowance": cfg['nominalWTAPISTD2RDMethod1']['data'][Method1Index]['CorrosionAllowance'], 
            "designFactors": cfg['designFactors'],
            # "AllowableStressFac": cfg['geometry']['AllowableStressFac'],
            "externalPressure": cfg['nominalWTAPISTD2RDMethod1']['data'][Method1Index]['ExternalPressure'],
            "internalPressure": cfg['nominalWTAPISTD2RDMethod1']['data'][Method1Index]['InternalPressure'],
            "LimitState": cfg['nominalWTAPISTD2RDMethod1']['data'][Method1Index]['LimitState']
            })

        resultData, DF = nominalWTAPISTD2RDMethod1(cfg, inputData)
        UtilizationDF.append(DF)
        cfg['Method1SLS'] = resultData

#  Prepare plot
    plotDefaultM1(cfg, UtilizationDF)
    # cfg = APISTD2RDMethod1Utilization(cfg)
    # saveData(cfg, dataDF)
