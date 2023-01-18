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
import os

import matplotlib.pyplot as plt

from common.application_configuration import application_configuration
from common.compare_tool_components import CompareTools
from common.set_logging import set_logging

# Data preparation
basename = os.path.basename(__file__).split('.')[0]
cfg = application_configuration(basename)

# Set logging
set_logging(cfg)
logging.info(cfg)

# Overwrite flag
if cfg['default']['config']['overwrite']['output'] == True:
    cfg['Analysis']['file_name'] = cfg['Analysis']['file_name_for_overwrite']

# Get file List
fileList = []
for fileIndex in range(0, len(cfg['ymlFiles'])):
    fileList.append(cfg['ymlFiles'][fileIndex]['io'])

compare_tools = CompareTools(cfg)
# Get data
dataDF = compare_tools.extractData(fileList, cfg)
# Save Data
dataDF.to_csv(cfg['Analysis']['result_folder'] + cfg['Analysis']['file_name'] + '.csv')

# Get plot data
dataDF_SLWR, dataDF_SCR = compare_tools.extractPlotData(fileList, cfg)

colors = ['blue', 'green', 'cyan', 'magenta', 'yellow', 'black']

for fileIndex in range(0, len(dataDF_SLWR)):
    X = dataDF_SLWR.iloc[fileIndex, 3]["X"]
    Y = dataDF_SLWR.iloc[fileIndex, 3]["Y"]
    if cfg['plot']['settings'] == None:
        plt.plot(X, Y, color = colors[fileIndex], label = dataDF_SLWR.iloc[fileIndex, 2])
    else:
        plt.plot(X, Y, color = colors[fileIndex], label = dataDF_SLWR.iloc[fileIndex, 2], linewidth=cfg['plot']['settings']['linewidth'][fileIndex], linestyle=cfg['plot']['settings']['linestyle'][fileIndex])
    X = dataDF_SLWR.iloc[fileIndex, 4]["X"]
    Y = dataDF_SLWR.iloc[fileIndex, 4]["Y"]
    if cfg['plot']['settings'] == None:
        plt.plot(X, Y, color = 'red')
    else:
        plt.plot(X, Y, color = 'red', linewidth=cfg['plot']['settings']['linewidth'][fileIndex]*2)
    X = dataDF_SLWR.iloc[fileIndex, 5]["X"]
    Y = dataDF_SLWR.iloc[fileIndex, 5]["Y"]
    if cfg['plot']['settings'] == None:
        plt.plot(X, Y, color = colors[fileIndex])
    else:
        plt.plot(X, Y, color = colors[fileIndex], linewidth=cfg['plot']['settings']['linewidth'][fileIndex], linestyle=cfg['plot']['settings']['linestyle'][fileIndex])

for fileIndex in range(0, len(dataDF_SCR)):
    X = dataDF_SCR.iloc[fileIndex, 3]["X"]
    Y = dataDF_SCR.iloc[fileIndex, 3]["Y"]
    if cfg['plot']['settings'] == None:
        plt.plot(X, Y, color = colors[fileIndex+len(dataDF_SLWR)], label = dataDF_SCR.iloc[fileIndex, 2])
    else:
        plt.plot(X, Y, color = colors[fileIndex+len(dataDF_SLWR)], label = dataDF_SCR.iloc[fileIndex, 2], linewidth=cfg['plot']['settings']['linewidth'][fileIndex])


plt.xlabel('Horizontal distance[m]', fontsize=12, fontweight='bold', color='black')
plt.ylabel('Distance Below Hang-off [m]', fontsize=12, fontweight='bold', color='black')
plt.title('Riser Configurations',fontsize=14, fontweight='bold', color='black')
plt.grid()
plt.legend()
plt.savefig(cfg['Analysis']['result_folder'] + cfg['Analysis']['file_name'], dpi=800)
