import matplotlib.pyplot as plt

ColorList = ['blue', 'green', 'red', 'orange', 'black', 'darkorchid']
def plotRAODirection(dataDF, cfg, RAODirection, VesselType):
#  Plot RAO Data
    fig = plt.figure()
    plt.subplot(2, 1, 1)

    SixDOFPlot(dataDF, VesselType, cfg['RAO']['Draught'][0], RAODirection, '-')
    SixDOFPlot(dataDF, VesselType, cfg['RAO']['Draught'][1], RAODirection, '-.')
    SixDOFPlot(dataDF, VesselType, cfg['RAO']['Draught'][2], RAODirection, ':')

    plt.suptitle('RAOs for Various Drafts, RAO Direction = {0}' .format(RAODirection), fontsize=14, color='black')
    #it creates plot X-axis name, fontsize and colour
    plt.xlabel('Time period (s)', fontsize=9,  color='black')
    #it creates plot Y-axis name, fontsize and colour
    plt.ylabel('Amplitude (m/m or deg/m)', fontsize=9, color='black')
    #It creates limit
    plt.xlim(0, 25)
    # plt.ylim(0, 4)

    plt.legend(loc="upper left", bbox_to_anchor=[0, 1],ncol=3,fontsize=8)
    plt.grid()

    plt.subplot(2, 1, 2)
    for i in range(0, len(cfg['WaveData'])):
        x = cfg['WaveData'][i]['PeakPeriod']
        y = cfg['WaveData'][i]['SignificantHeight']
        plt.bar(x, y, color=ColorList[i], label = cfg['WaveData'][i]['Name'])

    #it creates plot X-axis name, fontsize and colour
    plt.xlabel('Peak Period (s)', fontsize=9,  color='black')
    #it creates plot Y-axis name, fontsize and colour
    plt.ylabel('Significant Wave (m)', fontsize=9, color='black')
    plt.xlim(0, 25)

    plt.legend(loc="upper left",ncol=3, fontsize=8)
    plt.grid()

    plt.savefig("{0} {1}_Degree.png" .format(VesselType, RAODirection), dpi=800)
    plt.savefig("{0} {1}_Degree.svg" .format(VesselType, RAODirection), format='svg', dpi=1200)

def SixDOFPlot(DF, VesselType, Draught, RAODirection, LineStyle='-'):
    FilteredDF = DF[(DF.VesselType==VesselType) & (DF.Draught==Draught) & (DF.RAODirection==RAODirection)]
    x = FilteredDF.RAOPeriodOrFreq
    y = FilteredDF.RAOHeaveAmp
    plt.plot(x, y, color=ColorList[0], label = 'Heave ({0})' .format(Draught), linestyle=LineStyle)
    x = FilteredDF.RAOPeriodOrFreq
    y = FilteredDF.RAOSurgeAmp
    plt.plot(x, y, color=ColorList[1], label = 'Surge ({0})' .format(Draught), linestyle=LineStyle)
    x = FilteredDF.RAOPeriodOrFreq
    y = FilteredDF.RAOSwayAmp
    plt.plot(x, y, color=ColorList[2], label = 'Sway ({0})' .format(Draught), linestyle=LineStyle)
    x = FilteredDF.RAOPeriodOrFreq
    y = FilteredDF.RAOYawAmp
    plt.plot(x, y, color=ColorList[3], label = 'Yaw ({0})' .format(Draught), linestyle=LineStyle)
    x = FilteredDF.RAOPeriodOrFreq
    y = FilteredDF.RAORollAmp
    plt.plot(x, y, color=ColorList[4], label = 'Roll ({0})' .format(Draught), linestyle=LineStyle)
    x = FilteredDF.RAOPeriodOrFreq
    y = FilteredDF.RAOPitchAmp
    plt.plot(x, y, color=ColorList[5], label = 'Pitch ({0})' .format(Draught), linestyle=LineStyle)

