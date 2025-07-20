import os.path

def Execute(info):
    line = info.model['Riser']
    checks = info.model['Code Checks']
    
    condition_a = {'GammaF': 1.2,
                   'GammaE': 0.7}
    condition_b = {'GammaF': 1.1,
                  'GammaE': 1.3}
   
    outputFileName = os.path.splitext(info.modelFileName)[0] + '.txt'
    with open(outputFileName, 'w') as f:
        f.write(os.path.basename(info.modelFileName) + '\n')
        for case in condition_a, condition_b:
            checks.DNVOSF101GammaF = case['GammaF']
            checks.DNVOSF101GammaE = case['GammaE'] 
    
            F101_LC_Result = line.RangeGraph('DNV OS F101 load controlled', OrcFxAPI.pnWholeSimulation)
            f.write(str(max(F101_LC_Result.Max)) + '\n')