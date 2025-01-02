import logging
import os
import sys

import matplotlib.pyplot as pyplot
import matplotlib.pyplot as plt
import OrcFxAPI
from matplotlib import rc

rc('mathtext', default='regular')
import csv
import json

import numpy as np
import pandas as pd

from common.DataFrame_To_Image import DataFrame_To_Image
from common.DataFrame_To_xlsx import DataFrameArray_To_xlsx_openpyxl
from common.set_logging import setLogging
from custom.OrcaFlex_Post.postProcess import postProcess
from custom.OrcaFlex_Post.postProcessPlotting import postProcessPlotting
from dataManager.OrcaFlex_Post.ymlInput import ymlInput

try:
    user_paths = os.environ['PYTHONPATH']
except KeyError:
    user_paths = []

# Data preparation
defaultYml = "dataManager\\OrcaFlex_Post\\OrcaFlex.yml"
# Get updateYML file
try:
    if (sys.argv[1] != None):
        updateYml = "dataManager\\OrcaFlex_Post\\" + sys.argv[1]
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
# print(cfg)

# Set logging
setLogging(cfg['default']['logLevel'])

# Analysis (code under development)
# for fileIndex in range(0, len(cfg['Analyze'])):
#     filename = cfg['Analyze'][fileIndex].split('.')
#     # model = OrcFxAPI.Model(cfg['Analyze'][fileIndex])
#     model = OrcFxAPI.Model()
#     model.LoadData(cfg['Analyze'][fileIndex])
#     if filename[1] == 'yml':
#         model.saveData(filename[0] + '.dat')
#     elif filename[1] == 'dat':
#         model.saveData(filename[0] + '.yml')

#     model.CalculateStatics()
#     model.RunSimulation()
#     model.SaveSimulation(filename[0] + '.sim')

# print("Analysis done for {0} .yml files" .format(len(cfg['Analyze'])))


#  Postprocessing
RangeAllFiles, SummaryDF = postProcess(cfg)
print("Successful process {0} sim files" .format(len(RangeAllFiles)))
# Plot Range Data
postProcessPlotting(RangeAllFiles, cfg)

SummaryFileNameArray = []
for SummaryIndex in range(0, len(cfg['Summary'])):
    SummaryFileName = cfg['Summary'][SummaryIndex]['SummaryFileName']
    SummaryFileNameArray.append(SummaryFileName)
    if cfg['default']['Analysis']['AddMinimumToSummary']:
        SummaryDF[SummaryIndex].loc[len(SummaryDF[SummaryIndex])] = SummaryDF[SummaryIndex].min(axis=0).tolist()
        SummaryDF[SummaryIndex].loc[len(SummaryDF[SummaryIndex])-1, 'Description'] = 'Minimum'
    if cfg['default']['Analysis']['AddMaximumToSummary']:
        SummaryDF[SummaryIndex].loc[len(SummaryDF[SummaryIndex])] = SummaryDF[SummaryIndex].max(axis=0).tolist()
        SummaryDF[SummaryIndex].loc[len(SummaryDF[SummaryIndex])-1, 'Description'] = 'Maximum'
    if cfg['default']['Analysis']['AddMeanToSummary']:
        SummaryDF[SummaryIndex].loc[len(SummaryDF[SummaryIndex])] = ['Mean'] + SummaryDF[SummaryIndex].mean(axis=0).tolist()

    try:
        decimalArray = pd.Series([0, 0, 0, 2, 0], index=SummaryDF[SummaryIndex].columns.values)
        SummaryDF[SummaryIndex] = SummaryDF[SummaryIndex].round(decimalArray)
    except:
        SummaryDF[SummaryIndex] = SummaryDF[SummaryIndex].round(2)
    # SummaryDF[SummaryIndex].to_csv('results//' + str(SummaryIndex)+ SummaryFileName + '.csv', sep=",")
    # DataFrame_To_Image(SummaryDF[SummaryIndex], SummaryFileName + '.png')

#  Send LML Data to Excel Sheet
customdata = {"FileName" : 'results\\OrcaFlex_Post\\data\\' + cfg['FileName'] + '.xlsx',
        "SheetNames": SummaryFileNameArray,
        "thin_border" : True}
DataFrameArray_To_xlsx_openpyxl(SummaryDF, customdata)

print("Processed {0} summary files" .format(len(cfg['Summary'])))
