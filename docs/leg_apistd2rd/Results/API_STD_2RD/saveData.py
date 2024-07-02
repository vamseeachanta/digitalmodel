import yaml
import json

def saveData(cfg, dataDF):
    fileName = 'Results\\API_STD_2RD\\'+'Summary_' + cfg['plotSettings']['variableWTBurst']['plotFileName']
    #fileName = 'results\\'+cfg['plotSettings']['nominalWTAPISTD2RDMethod1']['plotFileName']
    with open(fileName+'.json', 'w') as f:
        json.dump(cfg, f)

    with open(fileName+'.yml', 'w') as f:
        yaml.dump(cfg, f, default_flow_style=False)

    dataDF.to_csv(fileName+'.csv')

def saveDataFrame(df, fileName):
    df.to_csv('Results//API_STD_2RD//'+fileName+'.csv')