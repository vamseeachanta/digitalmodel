import logging
import math
import sys
from math import *

import pandas as pd
# from catenarycalculation import *
from dataManager.ymlInput import ymlInput
from Fatigue_plotRAODirection import Fatigue_plotRAODirection
from logs.setLogging import setLogging

# Data preparation
defaultYml = "dataManager\\RAOs_Fatigue.yml"
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
try:
    cfg['FileName'] = updateYml.split('\\')[1].split('.')[0]
except:
    cfg['FileName'] = defaultYml.split('\\')[1].split('.')[0]

# cfg = customUpdate(cfg)

# Set logging
setLogging(cfg['default']['logLevel'])

RAOFile = "dataManager\\" + cfg['RAO']['FileName']
RAOData = ymlInput(RAOFile, None)

print("Read RAO File is Successful")

DataTag = 'RAOPeriodOrFreq, RAOSurgeAmp, RAOSurgePhase, RAOSwayAmp, RAOSwayPhase, RAOHeaveAmp, RAOHeavePhase, RAORollAmp, RAORollPhase, RAOPitchAmp, RAOPitchPhase, RAOYawAmp, RAOYawPhase'
dataDF = pd.DataFrame(columns = cfg['RAO']['DataFrameColumns'])

#  Extract RAO Data
for VesselType in range(0, len(RAOData['VesselTypes'])):
    for Draught in range(0,len(RAOData['VesselTypes'][VesselType]['Draughts'])):
        filteredRAOData = RAOData['VesselTypes'][VesselType]['Draughts'][Draught]['DisplacementRAOs']['RAOs']
        for RAODirection in range(0, len(filteredRAOData)):
            for Period in range(0, len(filteredRAOData[RAODirection][DataTag])):
                newDFRow = []
                newDFRow.append(RAOData['VesselTypes'][VesselType]['Name'])
                newDFRow.append(RAOData['VesselTypes'][VesselType]['Draughts'][Draught]['Name'])
                newDFRow.append(filteredRAOData[RAODirection]['RAODirection'])
                for QuantityIndex in range(0,len(cfg['RAO']['Quantities'])):
                    newDFRow.append(filteredRAOData[RAODirection][DataTag][Period][QuantityIndex])
                dataDF.loc[len(dataDF)] = newDFRow

for index in range(0, len(cfg['RAO']['RAODirection'])):
    Fatigue_plotRAODirection(dataDF, cfg, cfg['RAO']['RAODirection'][index], cfg['RAO']['VesselType'])
