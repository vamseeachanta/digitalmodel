
import matplotlib.pyplot as plt
import matplotlib.patches as mpatches

# Tableau 20 Colors
tableau20Colors = [(31, 119, 180), (174, 199, 232), (255, 127, 14), (255, 187, 120),  
             (44, 160, 44), (152, 223, 138), (214, 39, 40), (255, 152, 150),  
             (148, 103, 189), (197, 176, 213), (140, 86, 75), (196, 156, 148),  
             (227, 119, 194), (247, 182, 210), (127, 127, 127), (199, 199, 199),  
             (188, 189, 34), (219, 219, 141), (23, 190, 207), (158, 218, 229)]

def plotDefaultM1(cfg, DFArray):

    # Define plot settings
    plt.suptitle(cfg['nominalWTAPISTD2RDMethod1']['pltSupTitle'], fontsize=14, color='black')
    #it creates plot Sub-tittle, color and size
    plt.title(cfg['nominalWTAPISTD2RDMethod1']['pltTitle'] .format(cfg['geometry']['NominalOD'], cfg['geometry']['DesignWT']), fontsize=12, color='black')
    #it creates plot X-axis name, fontsize and colour
    plt.xlabel(cfg['nominalWTAPISTD2RDMethod1']['pltXLabel'], fontsize=12,  color='black')
    #it creates plot Y-axis name, fontsize and colour
    plt.ylabel(cfg['nominalWTAPISTD2RDMethod1']['pltYLabel'], fontsize=12, color='black')
    
    #Plotting co-ordinates For Method1
    for Method1DFIndex in range(0, len(DFArray)):
        Label = cfg['nominalWTAPISTD2RDMethod1']['data'][Method1DFIndex]['Label'] .format(cfg['nominalWTAPISTD2RDMethod1']['data'][Method1DFIndex]['LimitState'], cfg['nominalWTAPISTD2RDMethod1']['data'][Method1DFIndex]['InternalPressure'])
        Color = cfg['nominalWTAPISTD2RDMethod1']['data'][Method1DFIndex]['PlotSettings']['Color']
        plt.plot(DFArray[Method1DFIndex]["TArray"], DFArray[Method1DFIndex]["MarrayPositive1"], color = Color, label=Label)
        plt.plot(DFArray[Method1DFIndex]["TArray"], DFArray[Method1DFIndex]["MarrayNegative1"], color = Color,label="")

    if cfg['default']['Analysis']['nominalWTAPISTD2RDMethod2']:
        #Plotting co-ordinates For Method2
        plt.plot(DF_SLS["TArray"], DF_SLS["MarrayPositive2"], '-b',dashes=[4, 2], label=cfg['plotSettings']['nominalWTAPISTD2RDMethod1']['label11'])
        plt.plot(DF_SLS["TArray"], DF_SLS["MarrayNegative2"], '-b',dashes=[4, 2],label="")

        plt.plot(DF_ALS["TArray"], DF_ALS["MarrayPositive2"], '-g',dashes=[4, 2], label=cfg['plotSettings']['nominalWTAPISTD2RDMethod1']['label22'])
        plt.plot(DF_ALS["TArray"], DF_ALS["MarrayNegative2"], '-g',dashes=[4, 2],label="")

    if cfg['default']['Analysis']['nominalWTAPISTD2RDMethod4']:
        TensionLimitCheck = mpatches.Patch(color='Blue', label=cfg['plotSettings']['nominalWTAPISTD2RDMethod1']['label3'])
        plt.legend(handles=[TensionLimitCheck])

    for ResultsIndex in range(0, len(cfg['LoadingConditionResults'])):
        plt.scatter(cfg['LoadingConditionResults'][ResultsIndex]['Tension'] 
            , cfg['LoadingConditionResults'][ResultsIndex]['BendingMoment'] 
            , color=cfg['LoadingConditionResults'][ResultsIndex]['PlotSettings']['Color']
            , label=cfg['LoadingConditionResults'][ResultsIndex]['Label'] 
            , marker = cfg['LoadingConditionResults'][ResultsIndex]['PlotSettings']['Marker']
            , facecolors = cfg['LoadingConditionResults'][ResultsIndex]['PlotSettings']['Markerfillstyle'] )

    axes = plt.gca()
    axes.set_ylim([0,axes.get_ylim()[1]*1.0])
    axes.set_xlim([0,axes.get_xlim()[1]*1.0])

    plt.legend(fontsize = 8)
    plt.grid()
    fileName = ('Results\\API_STD_2RD\\' + cfg['FileName']).replace(".", "P")
    plt.savefig(fileName, dpi=800)

