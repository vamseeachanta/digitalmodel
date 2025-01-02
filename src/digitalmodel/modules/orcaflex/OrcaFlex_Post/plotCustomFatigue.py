
def plotCustomFatigue(DF, data):
    import matplotlib.pyplot as plt

    # Define plot settings
    plt.suptitle(data['PltSupTitle'], fontsize=14, color='black')
    #it creates plot Sub-tittle, color and size
    plt.title(data['PltTitle'], fontsize=12, color='black')
    #it creates plot X-axis name, fontsize and colour
    plt.xlabel(data['PltXLabel'], fontsize=12,  color='black')
    #it creates plot Y-axis name, fontsize and colour
    plt.ylabel(data['PltYLabel'], fontsize=12, color='black')
    plt.yscale('log')

    UniqueSNCurves = DF['S-N Curve'].unique()
    for SNCurveIndex in range(0, len(UniqueSNCurves)):
        plt.plot(DF[DF['S-N Curve']==UniqueSNCurves[SNCurveIndex]][data['PltColumns'][0]], \
         DF[DF['S-N Curve']==UniqueSNCurves[SNCurveIndex]][data['PltColumns'][1]], label=data['Label'][SNCurveIndex])
    # plt.ylim(data['YLim'])
    plt.legend()
#     plt.legend(loc='best', bbox_to_anchor=(0.5, -0.05),
#             fancybox=True, shadow=True, ncol=5)
    plt.axhline(y=data['Axhline'], color='r',dashes=[4, 2])
    for TextFieldsIndex in range(0, len(data['TextFields'])):
        plt.text(data['TextFields'][TextFieldsIndex]['x'], data['TextFields'][TextFieldsIndex]['y'], data['TextFields'][TextFieldsIndex]['Text'], color='b')
    # plt.axvline(x=cfg['design']['wallThickness'], color='m',dashes=[4, 2])
    # plt.text(10000,'Thickness Fabrication Limit : {0} inch' .format(cfg['design']['wallThickness']), color='m', rotation=90)
    plt.grid()
    for plotIndex in range(0, len(data['XLimRanges'])):
        plt.xlim(data['XLimRanges'][plotIndex])
        plt.ylim(data['YLimRanges'][plotIndex])
        plt.savefig(data['FileName'] + '_'+ str(plotIndex) + '.png', dpi=800)

    plt.close()


def plotCustomFatigueComparison(DFArray, data, cfg):
    import matplotlib.pyplot as plt

    # Define plot settings
    plt.suptitle(data['PltSupTitle'], fontsize=14, color='black')
    #it creates plot Sub-tittle, color and size
    plt.title(data['PltTitle'], fontsize=12, color='black')
    #it creates plot X-axis name, fontsize and colour
    plt.xlabel(data['PltXLabel'], fontsize=12,  color='black')
    #it creates plot Y-axis name, fontsize and colour
    plt.ylabel(data['PltYLabel'], fontsize=12, color='black')
    plt.yscale('log')

    for fileIndex in range(0, len(DFArray)):
        DF = DFArray[fileIndex]
        UniqueSNCurves = DF['S-N Curve'].unique()
        for SNCurveIndex in range(0, len(UniqueSNCurves)):
            plt.plot(DF[DF['S-N Curve']==UniqueSNCurves[SNCurveIndex]][data['PltColumns'][0]], \
            DF[DF['S-N Curve']==UniqueSNCurves[SNCurveIndex]][data['PltColumns'][1]], \
            label=cfg['FatigueLifeReadingSets'][fileIndex]['Label'][SNCurveIndex], linestyle = data['LineStyles'][SNCurveIndex], color = data['Colors'][fileIndex])

    plt.legend(fontsize=8)
#     plt.legend(loc='best', bbox_to_anchor=(0.5, -0.05),
#             fancybox=True, shadow=True, ncol=5)
    plt.axhline(y=data['Axhline'], color='r',dashes=[4, 2])
    for TextFieldsIndex in range(0, len(data['TextFields'])):
        plt.text(data['TextFields'][TextFieldsIndex]['x'], data['TextFields'][TextFieldsIndex]['y'], data['TextFields'][TextFieldsIndex]['Text'], color='b')
    # plt.axvline(x=cfg['design']['wallThickness'], color='m',dashes=[4, 2])
    # plt.text(10000,'Thickness Fabrication Limit : {0} inch' .format(cfg['design']['wallThickness']), color='m', rotation=90)
    plt.grid()
    for plotIndex in range(0, len(data['XLimRanges'])):
        plt.xlim(data['XLimRanges'][plotIndex])
        plt.ylim(data['YLimRanges'][plotIndex])
        plt.savefig(data['FileName'] + '_'+ str(plotIndex) + '.png', dpi=800)

    plt.close()
