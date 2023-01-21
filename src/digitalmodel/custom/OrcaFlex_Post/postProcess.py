import math
import sys

import OrcFxAPI
import pandas as pd

''' 
To load and postprocess model files
'''
def postProcess(cfg):
    # Intialize output arrays
    RangeAllFiles = []
    SummaryDFAllFiles = []

    # Load Simulation file(s)
    for fileIndex in range(0, len(cfg['Files'])):
        FileName = cfg['Files'][fileIndex]['Name']
        FileDescription = cfg['Files'][fileIndex]['Label']
        FileObjectName = cfg['Files'][fileIndex]['ObjectName']
        model = loadSimulation(FileName)

        # Perform (All) Range Postprocessing for each simulation file
        RangeAllFiles.append(postProcessRange(model, cfg, FileObjectName))

        for SummaryIndex in range(0, len(cfg['Summary'])):
            try:
                SummaryDFAllFiles[SummaryIndex]
            except:
                SummaryDFAllFiles.append(pd.DataFrame())

            SummaryDFAllFiles[SummaryIndex] = postProcessSummary(model, cfg, SummaryDFAllFiles[SummaryIndex], SummaryIndex, FileDescription, FileObjectName)
            # TO DO: Parametrize this.
        # decimals = pd.Series([1, 1], index=columns)
    return RangeAllFiles, SummaryDFAllFiles

# Load Simulation File
def loadSimulation(FileName):
    model = OrcFxAPI.Model(FileName)
    return model

def postProcessRange(model, cfg, FileObjectName):
    # Range Graphs for a simulation
    RangeFile = []

    for RangeGraphIndex in range(0, len(cfg['RangeGraph'])):
        RangeDF = pd.DataFrame()
        #Read Object
        try:
            objectName = cfg['RangeGraph'][RangeGraphIndex]['ObjectName']
            OrcFXAPIObject = model[objectName]
        except:
            OrcFXAPIObject = model[FileObjectName]
        
        # Arc Length Definition
        #Time Period Definition
        TimePeriod = exec("OrcFxAPI.{0}" .format(cfg['RangeGraph'][RangeGraphIndex]['SimulationPeriod']))
        VariableName = cfg['RangeGraph'][RangeGraphIndex]['Variable']

        try:
            if len(cfg['RangeGraph'][RangeGraphIndex]['ArcLength']) > 1:
                StartArcLength = cfg['RangeGraph'][RangeGraphIndex]['ArcLength'][0]
                EndArcLength = cfg['RangeGraph'][RangeGraphIndex]['ArcLength'][1]
                output = OrcFXAPIObject.RangeGraph(VariableName, TimePeriod, arclengthRange = OrcFxAPI.arSpecifiedArclengths(StartArcLength, EndArcLength))
            else:
                StartArcLength = cfg['RangeGraph'][RangeGraphIndex]['ArcLength'][0]
                output = OrcFXAPIObject.RangeGraph(VariableName, TimePeriod, arclengthRange = OrcFxAPI.arSpecifiedArclengths(StartArcLength))
                # arclengthRange = OrcFxAPI.arSpecifiedArclengths(0, 100)
        except:
            arclengthRange = None
            output = OrcFXAPIObject.RangeGraph(VariableName, TimePeriod, arclengthRange = arclengthRange)

        # output = OrcFXAPIObject.RangeGraph(VariableName, TimePeriod, arclengthRange = OrcFxAPI.arSpecifiedArclengths(0, 100))
        # Assign Arc Length
        AdditionalDataName = 'X'
        RangeDF[AdditionalDataName] = output.X

        for AdditionalDataIndex in range(0, len(cfg['RangeGraph'][RangeGraphIndex]['AdditionalData'])):
            AdditionalDataName = cfg['RangeGraph'][RangeGraphIndex]['AdditionalData'][AdditionalDataIndex]
            if AdditionalDataName == "Max":
                RangeDF[VariableName] = output.Max
            elif AdditionalDataName == "Min":
                RangeDF[VariableName] = output.Min
            elif AdditionalDataName == "Mean":
                RangeDF[VariableName] = output.Mean
            if VariableName == "API STD 2RD Method 1":
                APISTD2RDM1 = [math.sqrt(x) for x in RangeDF[VariableName]]
                RangeDF[VariableName]= APISTD2RDM1
        RangeFile.append(RangeDF)

    return RangeFile

def postProcessSummary(model, cfg, SummaryDF, SummaryIndex, FileDescription, FileObjectName):
    if SummaryDF.empty:
        columns = []
        columns.append('Description')
        for SummaryColumnIndex in range(0, len(cfg['Summary'][SummaryIndex]['Columns'])):
            columns.append(cfg['Summary'][SummaryIndex]['Columns'][SummaryColumnIndex]['Label'])
        
        SummaryDF = pd.DataFrame(columns = columns)
    
    SummaryFile = []
    SummaryFile.append(FileDescription)
    for SummaryColumnIndex in range(0, len(cfg['Summary'][SummaryIndex]['Columns'])):
        RangeDF = pd.DataFrame()
        #Read Object

        try:
            objectName = cfg['Summary'][SummaryIndex]['Columns'][SummaryColumnIndex]['ObjectName']
            OrcFXAPIObject = model[objectName]
        except:
            OrcFXAPIObject = model[FileObjectName]

        # Arc Length Definition
        #Time Period Definition
        TimePeriod = exec("OrcFxAPI.{0}" .format(cfg['Summary'][SummaryIndex]['Columns'][SummaryColumnIndex]['SimulationPeriod']))
        VariableName = cfg['Summary'][SummaryIndex]['Columns'][SummaryColumnIndex]['Variable']

        try:
            if len(cfg['Summary'][SummaryIndex]['Columns'][SummaryColumnIndex]['ArcLength']) > 1:
                StartArcLength = cfg['Summary'][SummaryIndex]['Columns'][SummaryColumnIndex]['ArcLength'][0]
                EndArcLength = cfg['Summary'][SummaryIndex]['Columns'][SummaryColumnIndex]['ArcLength'][1]
                output = OrcFXAPIObject.RangeGraph(VariableName, TimePeriod, arclengthRange = OrcFxAPI.arSpecifiedArclengths(StartArcLength, EndArcLength))
            else:
                StartArcLength = cfg['Summary'][SummaryIndex]['Columns'][SummaryColumnIndex]['ArcLength'][0]
                output = OrcFXAPIObject.RangeGraph(VariableName, TimePeriod, arclengthRange = OrcFxAPI.arSpecifiedArclengths(StartArcLength))
                # arclengthRange = OrcFxAPI.arSpecifiedArclengths(0, 100)
        except:
            arclengthRange = None
            output = OrcFXAPIObject.RangeGraph(VariableName, TimePeriod, arclengthRange = arclengthRange)

        # output = OrcFXAPIObject.RangeGraph(VariableName, TimePeriod, arclengthRange = OrcFxAPI.arSpecifiedArclengths(0, 100))
        # Assign Arc Length
        AdditionalDataName = 'X'
        RangeDF[AdditionalDataName] = output.X

        for AdditionalDataIndex in range(0, len(cfg['Summary'][SummaryIndex]['Columns'][SummaryColumnIndex]['AdditionalData'])):
            AdditionalDataName = cfg['Summary'][SummaryIndex]['Columns'][SummaryColumnIndex]['AdditionalData'][AdditionalDataIndex]
            if AdditionalDataName == "Max":
                RangeDF[VariableName] = output.Max
                if VariableName == "API STD 2RD Method 1":
                    APISTD2RDM1 = [math.sqrt(x) for x in RangeDF[VariableName]]
                    RangeDF[VariableName]= APISTD2RDM1
                SummaryFile.append(max(RangeDF[VariableName]))
            elif AdditionalDataName == "Min":
                RangeDF[VariableName] = output.Min
                if VariableName == "API STD 2RD Method 1":
                    APISTD2RDM1 = [math.sqrt(x) for x in RangeDF[VariableName]]
                    RangeDF[VariableName]= APISTD2RDM1
                SummaryFile.append(min(RangeDF[VariableName]))


    SummaryDF.loc[len(SummaryDF)] = SummaryFile

    return SummaryDF

def LinkedStatistics():
# LinkedStatistics using OrcaFlex. Code currently not working.
    VariableName = 'Effective Tension', 'Bend Moment'
    arclengthRange = OrcFxAPI.oeArcLength(25.0)
    TimePeriod = exec("OrcFxAPI.{0}" .format(cfg["PostProcess"]['Summary'][SummaryIndex]['SimulationPeriod']))
    stats = OrcFXAPIObject.LinkedStatistics(VariableName, TimePeriod, arclengthRange)
    query = stats.Query('Effective Tension', 'Bend Moment')
    print(query.ValueAtMax)
    print(query.ValueAtMin)
# Alternatively, use Range graphs for required quantities and obtain them using DFs. (easiest to customize)
