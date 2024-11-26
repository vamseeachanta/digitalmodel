import json


def saveData(cfg, dataDF):
    fileName = 'results\\'+cfg['plotSettings']['plotFileName']
    with open(fileName+'.json', 'w') as f:
        json.dump(cfg, f)
    dataDF.to_csv(fileName+'.csv')