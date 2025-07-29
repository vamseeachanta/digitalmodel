import matplotlib.pyplot as plt

# plt.style.use('seaborn-white')
import numpy as np


def plotContourf(TableDF, data):
    # Data Transformation for the plot
    dataDF = TableDF.copy()
    X = list(dataDF.columns)
    Y = list(dataDF.index)
    Z = dataDF.values
    # Plot settings
    plt.suptitle(data['PltSupTitle'], fontsize=14, color='black')
    plt.title(data['PltTitle'], fontsize=12, color='black')
    plt.xlabel(data['PltXLabel'], fontsize=12, color='black')
    plt.ylabel(data['PltYLabel'], fontsize=12, color='black')

    if data['PltShowContourLines']:
        contours = plt.contour(X, Y, Z, 16, colors='black')
        plt.clabel(contours, inline=True, fontsize=8)
        # https://matplotlib.org/tutorials/colors/colormaps.html
        plt.imshow(Z, origin=data['PltOrigin'], cmap='RdYlGn', alpha=0.5)
        Xi = [i for i in range(0, len(X))]
        Yi = [i for i in range(0, len(Y))]
        plt.xticks(Xi, X, fontsize=8)
        plt.yticks(Yi, Y, fontsize=8)
    else:
        if data["PltLims"]['zlim'] != None:
            plt.contourf(X,
                         Y,
                         Z,
                         cmap='RdYlGn',
                         vmin=data["PltLims"]['zlim'][0],
                         vmax=data["PltLims"]['zlim'][1])
        else:
            contours = plt.contourf(X, Y, Z, 20, cmap='RdYlGn')

    if data["PltLims"]['xlim'] != None:
        plt.xlim(data["PltLims"]['xlim'][0], data["PltLims"]['xlim'][1])
    if data['DataFrameRowReversed']:
        plt.ylim(max(Y), min(Y))
    elif data["PltLims"]['ylim'] != None:
        plt.ylim(data["PltLims"]['ylim'][0], data["PltLims"]['ylim'][1])

    plt.colorbar()
    # TODO https://bytes.com/topic/python/answers/891999-matplotlib-colorbar-scale-problem

    plt.savefig(data["FileName"] + "_Contour.png", dpi=800)
    plt.close()


'''
For main function to execute with file for example plot
'''


def f(x, y):
    return np.sin(x)**10 + np.cos(10 + y * x) * np.cos(x)


# REF: https://jakevdp.github.io/PythonDataScienceHandbook/04.04-density-and-contour-plots.html
if __name__ == '__main__':
    x = np.linspace(0, 5, 50)
    y = np.linspace(0, 5, 40)

    X, Y = np.meshgrid(x, y)
    Z = f(X, Y)

    # For simple lines
    # plt.contour(X, Y, Z, colors='black');
    # plt.contour(X, Y, Z, 20, cmap='RdGy');

    # For simple lines
    # plt.contourf(X, Y, Z, 20, cmap='RdGy')
    # plt.colorbar();

    contours = plt.contour(X, Y, Z, 6, colors='black')
    plt.clabel(contours, inline=True, fontsize=8)

    plt.imshow(Z, extent=[0, 5, 0, 5], origin='lower', cmap='RdGy', alpha=0.5)
    plt.colorbar()
    plt.show()
