import matplotlib.pyplot as plt


def plotDefault(cfg, dataDF):
    # Define plot settings
    plt.suptitle(cfg['plotSettings']['pltSupTitle'], fontsize=14, color='black')
    #it creates plot Sub-tittle, color and size
    plt.title(cfg['plotSettings']['pltTitle'], fontsize=12, color='black')
    #it creates plot X-axis name, fontsize and colour
    plt.xlabel(cfg['plotSettings']['pltXLabel'], fontsize=12,  color='black')
    #it creates plot Y-axis name, fontsize and colour
    plt.ylabel(cfg['plotSettings']['pltYLabel'], fontsize=12, color='black')
    #plotting x and y values
    #plt.plot(x_axis, y_axis1, '-b', label='BurstPressure')
    plt.plot(dataDF["t"], dataDF["Pd"], '-b', label=cfg['plotSettings']['label'])
    plt.ylim(cfg['plotSettings']['yLim'])
    plt.legend(loc='upper center', bbox_to_anchor=(0.5, -0.05),
            fancybox=True, shadow=True, ncol=5)
    plt.axhline(y=cfg['design']['pressure'], color='r',dashes=[4, 2])
    plt.text(1.2, cfg['design']['pressure'],'Target Design Pressure of {0} psi' .format(cfg['design']['pressure']), color='r')
    plt.axvline(x=cfg['design']['wallThickness'], color='m',dashes=[4, 2])
    plt.text(cfg['design']['wallThickness'], 10000,'Thickness Fabrication Limit : {0} inch' .format(cfg['design']['wallThickness']), color='m', rotation=90)
    plt.grid()
    plt.savefig('results\\catenary\\'+cfg['plotSettings']['plotFileName'], dpi=800)
