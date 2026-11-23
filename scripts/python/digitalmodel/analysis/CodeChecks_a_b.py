import OrcFxAPI

model = OrcFxAPI.Model('Code Checks.sim')
line = model['Riser']
checks = model['Code Checks']
    
condition_a = {'Name': 'a',
               'GammaF': 1.2,
               'GammaE': 0.7}
condition_b = {'Name': 'b',
               'GammaF': 1.1,
               'GammaE': 1.3}

for case in condition_a, condition_b:
    checks.DNVOSF101GammaF = case['GammaF']
    checks.DNVOSF101GammaE = case['GammaE']
    
    F101_LC_Result = line.RangeGraph('DNV OS F101 Load Controlled', OrcFxAPI.pnWholeSimulation)
    
    print('\nCondition:', case['Name'])
    print('Max F101 Load Controlled Result:', max(F101_LC_Result.Max))