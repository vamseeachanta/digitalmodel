import pandas as pd
import yaml


def extractData(fileList, cfg):
    dataDF = pd.DataFrame(columns = cfg['dataFrame']['columns'])
    for file in fileList:
        with open(file, 'r') as ymlfile:
            FileData = yaml.load(ymlfile)
        columns = cfg['dataFrame']['data']
        newDFRow = []
        newDFRow.append(file)
        for column in range(0, len(columns)):
            L1 = cfg['dataFrame']['data'][column]['L1']
            L2 = cfg['dataFrame']['data'][column]['L2']
            L3 = cfg['dataFrame']['data'][column]['L3']
            L4 = cfg['dataFrame']['data'][column]['L4']
            L5 = cfg['dataFrame']['data'][column]['L5']
            if L5 != None:
                newDFRow.append(FileData[L1][L2][L3][L4][L5])
            elif L4 != None:
                newDFRow.append(FileData[L1][L2][L3][L4])
            elif L3 != None:
                newDFRow.append(FileData[L1][L2][L3])
            elif L2 != None:
                newDFRow.append(FileData[L1][L2])
            else:
                newDFRow.append(FileData[L1])

        dataDF.loc[len(dataDF)] = newDFRow

    return dataDF


def extractPlotData(fileList, cfg):
    dataDF = pd.DataFrame(columns = cfg['plot']['columns'])
    for file in fileList:
        with open(file, 'r') as ymlfile:
            FileData = yaml.load(ymlfile)
        columns = cfg['plot']['data']
        newDFRow = []
        newDFRow.append(file)
        for columnIndex in range(0, len(columns)):
            L1 = cfg['plot']['data'][columnIndex]['L1']
            L2 = cfg['plot']['data'][columnIndex]['L2']
            L3 = cfg['plot']['data'][columnIndex]['L3']
            L4 = cfg['plot']['data'][columnIndex]['L4']
            L5 = cfg['plot']['data'][columnIndex]['L5']
            if L5 != None:
                newDFRow.append(FileData[L1][L2][L3][L4][L5])
            elif L4 != None:
                newDFRow.append(FileData[L1][L2][L3][L4])
            elif L3 != None:
                newDFRow.append(FileData[L1][L2][L3])
            elif L2 != None:
                newDFRow.append(FileData[L1][L2])
            else:
                newDFRow.append(FileData[L1])

        dataDF.loc[len(dataDF)] = newDFRow

    return dataDF
