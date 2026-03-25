import OrcFxAPI

waveDir = [0.0, 45.0, 90.0, 135.0, 180.0, 225.0, 270.0, 315.0]
wavePeriod = [8.0, 10.0]
vesselOffset = [10.0, 50.0]

casenumber = 0
for direction in waveDir:
    for period in wavePeriod:
        for offset in vesselOffset:
            casenumber += 1
            filename = 'Case{:03d}.yml'.format(casenumber)
            
            with open(filename, 'w') as f:
                print('BaseFile: Basecase.dat', file = f)
                print('General: ', file = f)
                print('  StageDuration[1]: ', period, file = f)
                print('  StageDuration[2]: ', period * 5, file = f)
                print('Environment: ', file = f)
                print('  WaveDirection: ', direction, file = f)
                print('  WavePeriod: ', period, file = f)
            
            #identify the object that we want to edit:
            model = OrcFxAPI.Model(filename)
            vessel = model['FPSO']
            #set the data on the 'Move selected objects...' data form
            rotation = OrcFxAPI.MoveObjectRotationSpecification(direction - 180.0, (0.0, 0.0))
            translation = OrcFxAPI.MoveObjectPolarDisplacementSpecification(direction, offset)
            points = OrcFxAPI.MoveObjectPoint(vessel, 1)
            #press the 'Move' button:
            OrcFxAPI.MoveObjects(rotation, [points])
            OrcFxAPI.MoveObjects(translation, [points])
            
#           model.SaveData(filename)
            
            with open(filename, 'a') as f:
               print('FPSO: ', file = f)
               print('  InitialX: ', vessel.InitialX, file = f)
               print('  InitialY: ', vessel.InitialY, file = f)
               print('  InitialHeading: ', vessel.InitialHeading, file = f)
               print('Turret: ', file = f)
               print('  InitialAzimuth: ', 180.0 - direction, file = f)