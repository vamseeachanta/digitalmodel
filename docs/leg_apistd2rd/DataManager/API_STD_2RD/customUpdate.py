import yaml
#from temperatureDerating import temperatureDerating 

def customUpdate(cfg):
    #data = temperatureDerating({"S": cfg['material']['SMYS'],                          \
    #        "U": cfg['material']['SMUS'], "temperature": 5/9*(cfg['design']['temperature']-32)})
    #cfg['material']['SMYS'] = data['S']
    #cfg['material']['SMUS'] = data['U']

    if cfg['geometry']['NominalOD'] !=None:
        cfg['plotSettings']['variableWTBurst']['plotFileName'] = cfg['plotSettings']['variableWTBurst']['plotFileName'] .format('OD', str(cfg['geometry']['NominalOD']), str(cfg['geometry']['CorrosionAllowance']))
        cfg['plotSettings']['variableWTBurst']['plotFileName'] = cfg['plotSettings']['variableWTBurst']['plotFileName'].replace('.', 'p')
        cfg['plotSettings']['variableWTBurst']['pltTitle'] = cfg['plotSettings']['variableWTBurst']['pltTitle'] .format(str(cfg['geometry']['NominalOD']),str(cfg['geometry']['CorrosionAllowance']),'OD')

        
    elif cfg['geometry']['NominalID'] !=None:
        cfg['plotSettings']['variableWTBurst']['plotFileName'] = cfg['plotSettings']['variableWTBurst']['plotFileName'] .format('ID', str(cfg['geometry']['NominalID']), str(cfg['geometry']['CorrosionAllowance']))
        cfg['plotSettings']['variableWTBurst']['plotFileName'] = cfg['plotSettings']['variableWTBurst']['plotFileName'].replace('.', 'p')
        cfg['plotSettings']['variableWTBurst']['pltTitle'] = cfg['plotSettings']['variableWTBurst']['pltTitle'] .format(str(cfg['geometry']['NominalID']),str(cfg['geometry']['CorrosionAllowance']),'ID')

    return cfg

