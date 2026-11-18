# -*- coding: utf-8 -*-
"""
Created on September 20 2018
"""
"""
Author: Vamsee Achanta
Date Updated: 2018-09-20
Objective: To summarize Simple Catenary and Lazy Wave Catenary results
Run instructions with:
 Default yml file : python APISTD2RD.py

 UPDATES: 
 Input YML: Relocate Spacing to PlotSettings
 Rename Summary "Hangoff" to "HangOffToSag"

"""
import sys

from .input_config import configuration_inputs as _configuration_inputs


def configuration_inputs(arguments):
    """Select summary input; no argument uses defaults, malformed input raises."""
    return _configuration_inputs(arguments, "dataManager/catenarySummary.yml", "dataManager")


def main(arguments=None):
    """Run the archived summary explicitly; importing never reads or writes data."""
    updateYml, file_name = configuration_inputs(sys.argv[1:] if arguments is None else arguments)
    import matplotlib.pyplot as plt
    from dataManager.ymlInput import ymlInput
    from .extractData import extractData, extractPlotData
    from logs.setLogging import setLogging
    from results.saveData import saveDataFrame

    cfg = ymlInput("dataManager/catenarySummary.yml", updateYml)
    cfg["FileName"] = file_name

    # Set logging
    setLogging(cfg["default"]["logLevel"])

    # Get file List

    fileList = []
    for fileIndex in range(0, len(cfg["ymlFiles"])):
        fileList.append(cfg["ymlFiles"][fileIndex]["io"])

    # Get data
    dataDF = extractData(fileList, cfg)
    # Save Data
    fileName = cfg["dataFrame"]["label"]
    saveDataFrame(dataDF, fileName)

    # Get plot data
    dataPlotDF = extractPlotData(fileList, cfg)

    colors = ["blue", "green", "cyan", "magenta", "yellow", "black"]

    for fileIndex in range(0, len(dataPlotDF)):
        X = dataPlotDF.iloc[fileIndex, 1]["X"]
        Y = dataPlotDF.iloc[fileIndex, 1]["Y"]
        if cfg["plot"]["settings"] == None:
            plt.plot(
                X, Y, color=colors[fileIndex], label=cfg["ymlFiles"][fileIndex]["Label"]
            )
        else:
            plt.plot(
                X,
                Y,
                color=colors[fileIndex],
                label=cfg["ymlFiles"][fileIndex]["Label"],
                linewidth=cfg["plot"]["settings"]["linewidth"][fileIndex],
            )
        X = dataPlotDF.iloc[fileIndex, 2]["X"]
        Y = dataPlotDF.iloc[fileIndex, 2]["Y"]
        if cfg["plot"]["settings"] == None:
            plt.plot(X, Y, color="red")
        else:
            plt.plot(
                X,
                Y,
                color="red",
                linewidth=cfg["plot"]["settings"]["linewidth"][fileIndex] * 2,
            )
        X = dataPlotDF.iloc[fileIndex, 3]["X"]
        Y = dataPlotDF.iloc[fileIndex, 3]["Y"]
        if cfg["plot"]["settings"] == None:
            plt.plot(X, Y, color=colors[fileIndex])
        else:
            plt.plot(
                X,
                Y,
                color=colors[fileIndex],
                linewidth=cfg["plot"]["settings"]["linewidth"][fileIndex],
            )

    plt.xlabel("Horizontal distance[m]", fontsize=12, fontweight="bold", color="black")
    plt.ylabel("Distance Below Hang-off [m]", fontsize=12, fontweight="bold", color="black")
    plt.title("Lazy Wave Riser Shape", fontsize=14, fontweight="bold", color="black")
    plt.grid()
    plt.legend()
    plt.savefig("results\\catenary\\" + cfg["FileName"].replace(".", ""), dpi=800)


if __name__ == "__main__":
    main()
