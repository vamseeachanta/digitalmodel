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
    dataDF_SLWR = pd.DataFrame(columns = cfg['plot']['columns']['SLWR'])
    dataDF_SCR = pd.DataFrame(columns = cfg['plot']['columns']['SCR'])
    for file_index in range(0, len(fileList)):
        with open(fileList[file_index], 'r') as ymlfile:
            FileData = yaml.load(ymlfile)

        columns = cfg['plot']['data'][cfg['ymlFiles'][file_index]['riser_type']]
        newDFRow = []
        newDFRow.append(fileList[file_index])
        newDFRow.append(cfg['ymlFiles'][file_index]['riser_type'])
        newDFRow.append(cfg['ymlFiles'][file_index]['label'])
        for columnIndex in range(0, len(columns)):
            L1 = cfg['plot']['data'][cfg['ymlFiles'][file_index]['riser_type']][columnIndex]['L1']
            L2 = cfg['plot']['data'][cfg['ymlFiles'][file_index]['riser_type']][columnIndex]['L2']
            L3 = cfg['plot']['data'][cfg['ymlFiles'][file_index]['riser_type']][columnIndex]['L3']
            L4 = cfg['plot']['data'][cfg['ymlFiles'][file_index]['riser_type']][columnIndex]['L4']
            L5 = cfg['plot']['data'][cfg['ymlFiles'][file_index]['riser_type']][columnIndex]['L5']
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

        if cfg['ymlFiles'][file_index]['riser_type'] == 'SLWR':
            dataDF_SLWR.loc[len(dataDF_SLWR)] = newDFRow
        elif cfg['ymlFiles'][file_index]['riser_type'] == 'SCR':
            dataDF_SCR.loc[len(dataDF_SCR)] = newDFRow

    return dataDF_SLWR, dataDF_SCR
