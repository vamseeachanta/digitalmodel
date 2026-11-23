import math

# DEFINE VARIABLES
WeatherDir = (0, 45, 90, 135, 180, 225, 270, 315) #8x weather directions
WavePeriod = (8, 10) #Min & Max wave periods
Condition = ('SOL', 'EOL') #Start of life and end of life conditions
VesselOffset = (5, 10) #vessel offset with intact moorings and 1 line broken

# LOOP THROUGH EACH VARIATION:
casenumber = 0 #used in the filename
for Direction in WeatherDir:

    for Period in WavePeriod:

        for Lifestage in Condition:
            if Lifestage == 'SOL':
               UnbuoyedLinetype = 'RiserLine'
               BuoyedLinetype = 'Riser with Buoyancy'
            if Lifestage == 'EOL':
               UnbuoyedLinetype = 'RiserLine EOL'
               BuoyedLinetype = 'Riser with Buoyancy EOL'

            for Offset in VesselOffset:

                # CREATE INDIVIDUAL YML (TEXT) FILES
                casenumber = casenumber + 1
                filename = 'Case%.3d.yml' % casenumber
                f = open(filename, 'w')
                # The content of the file:
                print >>f, 'BaseFile: Basecase.dat'
                print >>f, 'General: '
                print >>f, '  StageDuration[1]: ', Period
                print >>f, '  StageDuration[2]: ', Period * 5
                print >>f, 'Environment: '
                print >>f, '  RefCurrentDirection: ', Direction
                print >>f, '  WaveDirection: ', Direction
                print >>f, '  WavePeriod: ', Period
                print >>f, 'FPSO: '
                print >>f, '  InitialX: ', Offset * math.cos(math.radians(Direction))
                print >>f, '  InitialY: ', Offset * math.sin(math.radians(Direction))
                print >>f, '  InitialHeading: ', Direction - 180
                print >>f, 'Riser: '
                print >>f, '  LineType[1]: ', UnbuoyedLinetype
                print >>f, '  LineType[2]: ', UnbuoyedLinetype
                print >>f, '  LineType[3]: ', UnbuoyedLinetype
                print >>f, '  LineType[4]: ', BuoyedLinetype
                print >>f, '  LineType[5]: ', UnbuoyedLinetype
                print >>f, '  LineType[6]: ', UnbuoyedLinetype
                print >>f, '  LineType[7]: ', UnbuoyedLinetype
                print >>f, '  EndAX: ', -2*math.cos(math.radians(Direction))
                print >>f, '  EndAY: ', 2*math.sin(math.radians(Direction))
                f.close()