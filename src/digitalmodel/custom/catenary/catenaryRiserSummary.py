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
import logging
import math
import sys
from math import *

import matplotlib.pyplot as plt
# from catenarycalculation import *
from dataManager.ymlInput import ymlInput
from extractData import extractData, extractPlotData
from fileList import fileList
from logs.setLogging import setLogging
from results.saveData import saveDataFrame

# Data preparation
defaultYml = "dataManager\\catenarySummary.yml"
# Get updateYML file
try:
    if (sys.argv[1] != None):
        updateYml = "dataManager\\" + sys.argv[1]
        logging.critical("Updating default values with contents in file {0}" .format(updateYml))
except:
    updateYml = None
    logging.critical("No update values file is provided. Running program default values")

# Get updated configuration file for Analysis
cfg = ymlInput(defaultYml, updateYml)
try:
    cfg['FileName'] = updateYml.split('\\')[1].split('.')[0]
except:
    cfg['FileName'] = defaultYml.split('\\')[1].split('.')[0]

# Set logging
setLogging(cfg['default']['logLevel'])

# Get file List

fileList = []
for fileIndex in range(0, len(cfg['ymlFiles'])):
    fileList.append(cfg['ymlFiles'][fileIndex]['io'])

# Get data
dataDF = extractData(fileList, cfg)
# Save Data
fileName = cfg['dataFrame']['label']
saveDataFrame(dataDF, fileName)

# Get plot data
dataPlotDF = extractPlotData(fileList, cfg)

colors = ['blue', 'green', 'cyan', 'magenta', 'yellow', 'black']

for fileIndex in range(0, len(dataPlotDF)):
    X = dataPlotDF.iloc[fileIndex, 1]["X"]
    Y = dataPlotDF.iloc[fileIndex, 1]["Y"]
    if cfg['plot']['settings'] == None:
        plt.plot(X, Y, color = colors[fileIndex], label = cfg['ymlFiles'][fileIndex]['Label'])
    else:
        plt.plot(X, Y, color = colors[fileIndex], label = cfg['ymlFiles'][fileIndex]['Label'], linewidth=cfg['plot']['settings']['linewidth'][fileIndex])
    X = dataPlotDF.iloc[fileIndex, 2]["X"]
    Y = dataPlotDF.iloc[fileIndex, 2]["Y"]
    if cfg['plot']['settings'] == None:
        plt.plot(X, Y, color = 'red')
    else:
        plt.plot(X, Y, color = 'red', linewidth=cfg['plot']['settings']['linewidth'][fileIndex]*2)
    X = dataPlotDF.iloc[fileIndex, 3]["X"]
    Y = dataPlotDF.iloc[fileIndex, 3]["Y"]
    if cfg['plot']['settings'] == None:
        plt.plot(X, Y, color = colors[fileIndex])
    else:
        plt.plot(X, Y, color = colors[fileIndex], linewidth=cfg['plot']['settings']['linewidth'][fileIndex])
    
plt.xlabel('Horizontal distance[m]', fontsize=12, fontweight='bold', color='black')
plt.ylabel('Distance Below Hang-off [m]', fontsize=12, fontweight='bold', color='black')
plt.title('Lazy Wave Riser Shape',fontsize=14, fontweight='bold', color='black')
plt.grid()
plt.legend()
plt.savefig('results\\catenary\\' + cfg['FileName'].replace(".", ""), dpi=800)
