import matplotlib.pyplot as plt

def plotDefaultM1(cfg, DF_SLS, DF_ALS):
    # Define plot settings
    plt.suptitle(cfg['plotSettings']['nominalWTAPISTD2RDMethod1']['pltSupTitle'], fontsize=14, color='black')
    #it creates plot Sub-tittle, color and size
    plt.title(cfg['plotSettings']['nominalWTAPISTD2RDMethod1']['pltTitle'], fontsize=12, color='black')
    #it creates plot X-axis name, fontsize and colour
    plt.xlabel(cfg['plotSettings']['nominalWTAPISTD2RDMethod1']['pltXLabel'], fontsize=12,  color='black')
    #it creates plot Y-axis name, fontsize and colour
    plt.ylabel(cfg['plotSettings']['nominalWTAPISTD2RDMethod1']['pltYLabel'], fontsize=12, color='black')
    #plotting x and y values
    #plt.plot(x_axis, y_axis1, '-b', label='BurstPressure')
    plt.plot(DF_SLS["TArray"], DF_SLS["MarrayPositive"], '-b', label=cfg['plotSettings']['nominalWTAPISTD2RDMethod1']['label1'])
#     plt.plot(DF_SLS["TArray"], DF_SLS["MarrayNegative"], '-b', label=cfg['plotSettings']['nominalWTAPISTD2RDMethod1']['label1'])
    plt.plot(DF_SLS["TArray"], DF_SLS["MarrayNegative"], '-b',label="")
    plt.plot(DF_ALS["TArray"], DF_ALS["MarrayPositive"], '-g', label=cfg['plotSettings']['nominalWTAPISTD2RDMethod1']['label2'])
    plt.plot(DF_ALS["TArray"], DF_ALS["MarrayNegative"], '-g', label="")
    # plt.ylim(cfg['plotSettings']['yLim'])
#     plt.legend(loc='upper center', bbox_to_anchor=(0.5, -0.05),
        #     fancybox=True, shadow=True, ncol=5)
    plt.legend(loc='lower left',bbox_to_anchor=(0.55, -0.04))
    # plt.axhline(y=cfg['design']['pressure'], color='r',dashes=[4, 2])
    # plt.text(1.2, cfg['design']['pressure'],'Target Design Pressure of {0} psi' .format(cfg['design']['pressure']), color='r')
    # plt.axvline(x=cfg['design']['wallThickness'], color='m',dashes=[4, 2])
    # plt.text(cfg['design']['wallThickness'], 10000,'Thickness Fabrication Limit : {0} inch' .format(cfg['design']['wallThickness']), color='m', rotation=90)
    plt.grid()
    plt.savefig('results\\'+cfg['plotSettings']['nominalWTAPISTD2RDMethod1']['plotFileName']+"M1", dpi=800)
