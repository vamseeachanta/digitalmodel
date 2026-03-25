waveDir = [0.0, 45.0, 90.0, 135.0, 180.0, 225.0, 270.0, 315.0]
wavePeriod = [8.0, 10.0]


casenumber = 0
for direction in waveDir:
    for period in wavePeriod:
        casenumber += 1
        filename = 'Case{:03d}.yml'.format(casenumber)
        with open(filename, 'w') as f:
            print('BaseFile: Basecase.dat', file = f)
            print('General:', file = f)
            print('  StageDuration[1]:', period, file = f)
            print('  StageDuration[2]:', period * 5, file = f)
            print('Environment:', file = f)
            print('  WaveDirection:', direction, file = f)
            print('  WavePeriod:', period, file = f)