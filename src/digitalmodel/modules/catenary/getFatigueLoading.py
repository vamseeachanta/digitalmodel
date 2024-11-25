# -*- coding: utf-8 -*-
"""
Created on September 21 2018
"""
'''
Author: Vamsee Achanta
Date Updated: 2018-09-21
Objective: To generate pipe properties
'''
import copy
import logging
import math
import sys
from collections import OrderedDict

import numpy as np
import pandas as pd
from dataManager.ymlInput import ymlInput
from results.saveData import saveDataYaml


def getFatigueData(data):
    SeasDF = pd.read_excel(data['WorkBook'])
    return SeasDF

def prepareFatigueLoading(SeasDF, cfg):
    ColumnsList = ['Hs', 'Tp', 'WaveDirection', 'FileName', 'Probability']
    FatigueLoadingDF = pd.DataFrame(columns=ColumnsList)
    TotalBins = 0
    FatigueLoads = []
    for columni in range(0, len(SeasDF.columns)):
        for indexi in range(0, len(SeasDF.index)):
            if SeasDF.iloc[indexi,columni] > 0:
                Hs = SeasDF.index[indexi]
                Tp = np.round(SeasDF.columns[columni], decimals=1)
                WaveDirection = cfg['EnvironmentLoadTemplate']['Fatigue'][0]['Wave']['WaveTrains'][0]['WaveDirection']
                WaveSeed = cfg['RANDOM250'][TotalBins]
                FileName = "WH{2} Hs={0} m, Tp={1} s" .format(Hs, Tp, WaveDirection)
                cfg['EnvironmentLoadTemplate']['Fatigue'][0]['Wave']['WaveTrains'][0]['Name'] =  FileName
                cfg['EnvironmentLoadTemplate']['Fatigue'][0]['Wave']['WaveTrains'][0]['WaveHs'] = float(Hs)
                cfg['EnvironmentLoadTemplate']['Fatigue'][0]['Wave']['WaveTrains'][0]['WaveTp'] = float(Tp)
                cfg['EnvironmentLoadTemplate']['Fatigue'][0]['Wave']['WaveTrains'][0]['WaveSeed'] = int(WaveSeed)
                FatigueLoads.append(copy.deepcopy(cfg['EnvironmentLoadTemplate']['Fatigue'][0]))

                FatigueLoadingDFRow = [Hs, Tp, WaveDirection, cfg['SimFilePrefix'] +FileName, SeasDF.iloc[indexi,columni]]
                FatigueLoadingDF.loc[len(FatigueLoadingDF)] = FatigueLoadingDFRow
                TotalBins = TotalBins + 1
    print("Total number of bins is {}" .format(TotalBins))
    return FatigueLoads, FatigueLoadingDF

def prepareFatiguePostProcessing(FatigueLoadingDF, cfg):
    model = {}
    model.update(FatiguePostProcessing())
    model['SNCurves'] = cfg['SNCurves']
    model['LoadCases'] = []
    for i in range(0,len(FatigueLoadingDF)):
        customData = { "LoadCaseFileName": FatigueLoadingDF.loc[i]['FileName']+ '.sim',
                    "LoadCaseLineName": cfg['FEASettings']['LoadCaseLineName'],
                "PeriodFrom": cfg['FEASettings']['SimulationPeriod']['From'],
	            "PeriodTo": cfg['FEASettings']['SimulationPeriod']['To'], 
	            "LoadCaseExposureTime": FatigueLoadingDF.loc[i]['Probability'].item()*87.66
	                }

        model['LoadCases'].append(LoadCase(customData))

    model['AnalysisData'] = cfg['AnalysisData']
    model.update(cfg['Outputs'])

    return model

def FatiguePostProcessing():
    model={}
    model.update({"Title": "SLWR Fatigue Analysis"})
    model.update({"DamageCalculation": "Homogeneous pipe stress"})
    model.update({"AnalysisType": "Rainflow"})
    model.update({"UnitsSystem": "SI"})
    model.update({"CriticalDamageFactor": 0.1})
    model.update({"ThetaCount": 16})

    return model    

def SNCurves():
    pass

def LoadCase(data):
    model={}
    model.update({"LoadCaseFileName": data['LoadCaseFileName']})
    model.update({"LoadCaseLineName": data['LoadCaseLineName']})
    model.update({"PeriodFrom": data['PeriodFrom']})
    model.update({"PeriodTo": data['PeriodTo']})
    model.update({"LoadCaseExposureTime": data['LoadCaseExposureTime']})
    
    return model    

def AnalysisData():
    pass

def Outputs():
    model={}
    model.update({"OutputLoadCaseTables": "Yes"})
    model.update({"OutputDetailedLoadCaseTables": "Yes"})
    model.update({"LoadCaseDamageUnits": "Damage per year"})

    return model    

if __name__ == "__main__":
    # Data preparation
    defaultYml = "dataManager\\fatigueLoadingTemplate.yml"
    # Get updateYML file
    try:
        if (sys.argv[1] != None):
            updateYml = "dataManager\\catenary\\" + sys.argv[1]
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

    data = {'WorkBook' : "dataManager\\catenary\\" + cfg['WorkBook'],
    'WorkSheet': cfg['WorkSheet']
    }

#  Save wave data for inputs to Catenary program
    SeasDF = getFatigueData(data)
    FatigueLoads, FatigueLoadingDF = prepareFatigueLoading(SeasDF, cfg)
    cfg['FatigueLoads'] = FatigueLoads
    saveDataYaml(cfg, 'results\\' + 'FatigueLoading_' + cfg['FileName'], None)
    FatigueLoadingDF.to_csv('results\\catenary\\' + 'FatigueLoading_' + cfg['FileName'])

#  Save data for inputs to ftg file
    FatigueProcessor = prepareFatiguePostProcessing(FatigueLoadingDF, cfg)
    saveDataYaml(FatigueProcessor, 'results\\' + 'FatigueProcess_' + cfg['FileName'], False)
