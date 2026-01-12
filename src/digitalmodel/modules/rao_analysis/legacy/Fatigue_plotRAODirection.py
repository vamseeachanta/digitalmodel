import matplotlib.pyplot as plt


def Fatigue_plotRAODirection(dataDF, cfg, RAODirection, VesselType):
#  Plot RAO Data
    fig = plt.figure()
    plt.subplot(2, 1, 1)

    SixDOFPlot(dataDF, VesselType, cfg['RAO']['Draught'][0], RAODirection, '-')
    SixDOFPlot(dataDF, VesselType, cfg['RAO']['Draught'][1], RAODirection, '-.')
    SixDOFPlot(dataDF, VesselType, cfg['RAO']['Draught'][2], RAODirection, ':')
   
    plt.suptitle('RAOs for Various Drafts, RAO Direction = {0}' .format(RAODirection), fontsize=14, color='black')
    #it creates plot X-axis name, fontsize and colour
    plt.xlabel('Time period (s)', fontsize=10,  color='black')
    #it creates plot Y-axis name, fontsize and colour
    plt.ylabel('Amplitude (m/m or deg/m)', fontsize=10, color='black')
    #It creates limit
    plt.xlim(0, 25)
    # plt.ylim(0, 4)

    plt.legend(loc="upper left", bbox_to_anchor=[0, 1],ncol=3,fontsize=8)
    
    plt.grid()
               
    

    plt.subplot(2, 1, 2)
    for i in range(0, len(cfg['WaveData'])):
        x = cfg['WaveData'][i]['AvgerageTP']
        y = cfg['WaveData'][i]['SumPer']
        plt.bar(x,y, label = cfg['WaveData'][i]['Name'])

    #it creates plot X-axis name, fontsize and colour
    plt.xlabel('AvgerageTP', fontsize=10,  color='black')
    #it creates plot Y-axis name, fontsize and colour
    plt.ylabel('SumPer', fontsize=10, color='black')    
    plt.xlim(0, 25)

    plt.legend(loc="upper right",ncol=3, fontsize=8)
    plt.grid()

    plt.savefig("{0} {1}_Degree.png" .format(VesselType, RAODirection), dpi=800)
    plt.savefig("{0} {1}_Degree.svg" .format(VesselType, RAODirection), format='svg', dpi=1200)
    


def SixDOFPlot(DF, VesselType, Draught, RAODirection, LineStyle='-'):
    FilteredDF = DF[(DF.VesselType==VesselType) & (DF.Draught==Draught) & (DF.RAODirection==RAODirection)]
    x = FilteredDF.RAOPeriodOrFreq
    y = FilteredDF.RAOHeaveAmp
    plt.plot(x, y, color='blue', label = 'Heave ({0})' .format(Draught), linestyle=LineStyle)
    x = FilteredDF.RAOPeriodOrFreq
    y = FilteredDF.RAOSurgeAmp
    plt.plot(x, y, color='green', label = 'Surge ({0})' .format(Draught), linestyle=LineStyle)
    x = FilteredDF.RAOPeriodOrFreq
    y = FilteredDF.RAOSwayAmp
    plt.plot(x, y, color='red', label = 'Sway ({0})' .format(Draught), linestyle=LineStyle)
    x = FilteredDF.RAOPeriodOrFreq
    y = FilteredDF.RAOYawAmp
    plt.plot(x, y, color='orange', label = 'Yaw ({0})' .format(Draught), linestyle=LineStyle)
    x = FilteredDF.RAOPeriodOrFreq
    y = FilteredDF.RAORollAmp
    plt.plot(x, y, color='black', label = 'Roll ({0})' .format(Draught), linestyle=LineStyle)
    x = FilteredDF.RAOPeriodOrFreq
    y = FilteredDF.RAOPitchAmp
    plt.plot(x, y, color='darkorchid', label = 'Pitch ({0})' .format(Draught), linestyle=LineStyle)

