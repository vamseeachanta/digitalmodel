import logging
import os
import sys

import matplotlib.pyplot as pyplot
import matplotlib.pyplot as plt
from matplotlib import rc

rc('mathtext', default='regular')
import csv
import json

import numpy as np
import pandas as pd
from dataManager.customInputs import ExcelRead
# from DataFrame_To_Image import DataFrame_To_Image
from results.plotCustomFatigue import (plotCustomFatigue,
                                       plotCustomFatigueComparison)
from xlsx_To_DataFrame import xlsx_To_DataFrame

from common.saveData import saveDataFrame
from common.set_logging import setLogging
from common.ymlInput import ymlInput

# Data preparation
defaultYml = "dataManager\\OrcaFlex_Fatigue.yml"
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

# Set logging
setLogging(cfg['Default']['logLevel'])

dfArray = []
dfSummary = pd.DataFrame(columns=['FileName', 'SN1', 'SN2', 'SN3'])
for fileIndex in range(0, len(cfg['FatigueLifeReadingSets'])):
    customdata = {"FileName": cfg['FatigueLifeReadingSets'][fileIndex]['io'],
                    "SheetName": cfg['FatigueLifeReadingSets'][fileIndex]['sheet_name'],
                    "KeyWords": cfg['FatigueLifeReadingSets'][fileIndex]['KeyWords'],
                    "RowsToSkip": cfg['FatigueLifeReadingSets'][fileIndex]['RowsToSkip'],
                    "RowsToRead": cfg['FatigueLifeReadingSets'][fileIndex]['RowsToRead'],
                    "Columns":  cfg['FatigueLifeReadingSets'][fileIndex]['Columns'],
                    "FactorOfSafety": cfg['FatigueLifeReadingSets'][fileIndex]['FactorOfSafety']
    }

    df = xlsx_To_DataFrame(customdata)
    df['Life (years)'] = df['Life (years)']/customdata['FactorOfSafety']

#  For plot
    customdata = {"PltSupTitle": cfg['FatigueLifePlotSettings']['PltSupTitle'],
            "PltTitle": cfg['FatigueLifePlotSettings']['PltTitle'],
            "PltXLabel": cfg['FatigueLifePlotSettings']['PltXLabel'],
            "PltYLabel" : cfg['FatigueLifePlotSettings']['PltYLabel'],
            'Label' : cfg['FatigueLifePlotSettings']['PltLabel'],
            'Axhline' : [cfg['FatigueLifePlotSettings']['LifeLimit']],
            'PltColumns' : ['Arc Length', 'Life (years)'],
            # 'Text': 'MAWP Reduced, Flaw dimension o: s={0} and c={1},  Min. Measured WT ={2}' .format(LMLSummaryDF.iloc[0]['s'], LMLSummaryDF.iloc[0]['c'], LMLSummaryDF.iloc[0]['tmm']),
            'TextFields': [{"x": 2000, "y": 40, "Text": "Minimum Fatigue Life"}],
            'FileName': 'results\\Plots\\' + 'Life'+ os.path.basename(cfg['FatigueLifeReadingSets'][fileIndex]['io']).replace(".xlsx", ""),
            'XLimRanges': cfg['FatigueLifePlotSettings']['XLimRanges'],
            'YLimRanges': cfg['FatigueLifePlotSettings']['YLimRanges']
    }
    plotCustomFatigue(df, customdata)

    dfArray.append(df)

    rowDF = []
    rowDF.append(customdata['FileName'])
    UniqueSNCurves = df['S-N Curve'].unique()
    for SNCurveIndex in range(0, len(UniqueSNCurves)):
        rowDF.append(df[df['S-N Curve']==UniqueSNCurves[SNCurveIndex]][customdata['PltColumns'][1]].min())

    dfSummary.loc[len(dfSummary)] = rowDF


#  For plot
customdata = {"PltSupTitle": cfg['FatigueLifePlotSettings']['PltSupTitle'],
        "PltTitle": cfg['FatigueLifePlotSettings']['PltTitle'],
        "PltXLabel": cfg['FatigueLifePlotSettings']['PltXLabel'],
        "PltYLabel" : cfg['FatigueLifePlotSettings']['PltYLabel'],
        'Label' : cfg['FatigueLifePlotSettings']['PltLabel'],
        'Axhline' : [cfg['FatigueLifePlotSettings']['LifeLimit']],
        'PltColumns' : ['Arc Length', 'Life (years)'],
        # 'Text': 'MAWP Reduced, Flaw dimension o: s={0} and c={1},  Min. Measured WT ={2}' .format(LMLSummaryDF.iloc[0]['s'], LMLSummaryDF.iloc[0]['c'], LMLSummaryDF.iloc[0]['tmm']),
        'TextFields': [{"x": 2000, "y": 40, "Text": "Minimum Fatigue Life"}],
        'FileName': 'results\\Plots\\' + 'Life'+ cfg['FileName'],
        'XLimRanges': cfg['FatigueLifePlotSettings']['XLimRanges'],
        'YLimRanges': cfg['FatigueLifePlotSettings']['YLimRanges'],
        'LineStyles': cfg['FatigueLifePlotSettings']['LineStyles'],
        'Colors': cfg['FatigueLifePlotSettings']['Colors']
}

plotCustomFatigueComparison(dfArray, customdata, cfg)

saveDataFrame(dfSummary, 'Data\\'+cfg['FileName'])
