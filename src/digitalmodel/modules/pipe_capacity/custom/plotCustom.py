import matplotlib.pyplot as plt


def plotCustom(DF, data):
    # Define plot settings
    plt.suptitle(data['PltSupTitle'], fontsize=14, color='black')
    #it creates plot Sub-tittle, color and size
    plt.title(data['PltTitle'], fontsize=12, color='black')
    #it creates plot X-axis name, fontsize and colour
    plt.xlabel(data['PltXLabel'], fontsize=12,  color='black')
    #it creates plot Y-axis name, fontsize and colour
    plt.ylabel(data['PltYLabel'], fontsize=12, color='black')
    #plotting x and y values
    #plt.plot(x_axis, y_axis1, '-b', label='BurstPressure')
    for i in range(0, len(data['PltColumns'])-1):
        plt.plot(DF[data['PltColumns'][0]], DF[data['PltColumns'][i+1]], label=data['Label'][i])
    if data['YLim'] is not None:
        plt.ylim(data['YLim'])
    plt.legend()
#     plt.legend(loc='best', bbox_to_anchor=(0.5, -0.05),
#             fancybox=True, shadow=True, ncol=5)
    plt.axhline(y=data['Axhline'], color='r',dashes=[4, 2])
    for TextFieldsIndex in range(0, len(data['TextFields'])):
        plt.text(data['TextFields'][TextFieldsIndex]['x'], data['TextFields'][TextFieldsIndex]['y'], data['TextFields'][TextFieldsIndex]['Text'], color='b')
    # plt.axvline(x=cfg['design']['wallThickness'], color='m',dashes=[4, 2])
    # plt.text(10000,'Thickness Fabrication Limit : {0} inch' .format(cfg['design']['wallThickness']), color='m', rotation=90)
    plt.grid()
    plt.savefig(data['FileName'], dpi=800)
    plt.close()
