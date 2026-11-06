# -*- coding: utf-8 -*-
"""
Created on September 21 2018
"""
"""
Author: Vamsee Achanta
Date Updated: 2018-09-21
Objective: To generate pipe properties
"""
import copy
import logging
import sys
from numbers import Integral

import numpy as np
import pandas as pd
from ..models import _range
from .input_config import configuration_inputs as _configuration_inputs


def configuration_inputs(arguments):
    """Select the archived fatigue YAML input; explicit errors never use defaults."""
    return _configuration_inputs(
        arguments, "dataManager/fatigueLoadingTemplate.yml", "dataManager/catenary"
    )


def getFatigueData(data):
    SeasDF = pd.read_excel(data["WorkBook"])
    return SeasDF


def prepareFatigueLoading(SeasDF, cfg):
    """Build independent load dictionaries from numeric Hs [m], Tp [s] bins.

    Probabilities are finite, nonnegative caller weights, preserved without
    normalization. Only true zero bins are omitted. Numeric axes are required;
    locale strings are rejected. Tp is rounded to one decimal for compatibility,
    and the rounded value must stay positive. Seeds must be nonnegative integer
    values, one per positive bin. Validation completes before any output is built.
    This preprocessing contract does not qualify fatigue damage or sea spectra.
    """
    for height in SeasDF.index:
        _range("height", height, positive=True)
    for period in SeasDF.columns:
        _range("period", period, positive=True)
        _range("rounded period", np.round(period, decimals=1), positive=True)
    positive_bins = 0
    for probability in SeasDF.to_numpy().flat:
        _range("probability", probability)
        positive_bins += int(probability > 0)
    seeds = cfg["RANDOM250"]
    if len(seeds) < positive_bins:
        raise ValueError("seed count is less than positive bin count")
    for seed in seeds[:positive_bins]:
        if isinstance(seed, (bool, np.bool_)) or not isinstance(seed, Integral) or seed < 0:
            raise ValueError("seed must be a nonnegative integer")
    cfg = copy.deepcopy(cfg)
    ColumnsList = ["Hs", "Tp", "WaveDirection", "FileName", "Probability"]
    FatigueLoadingDF = pd.DataFrame(columns=ColumnsList)
    TotalBins = 0
    FatigueLoads = []
    for columni in range(0, len(SeasDF.columns)):
        for indexi in range(0, len(SeasDF.index)):
            if SeasDF.iloc[indexi, columni] > 0:
                Hs = SeasDF.index[indexi]
                Tp = np.round(SeasDF.columns[columni], decimals=1)
                WaveDirection = cfg["EnvironmentLoadTemplate"]["Fatigue"][0]["Wave"][
                    "WaveTrains"
                ][0]["WaveDirection"]
                WaveSeed = cfg["RANDOM250"][TotalBins]
                FileName = "WH{2} Hs={0} m, Tp={1} s".format(Hs, Tp, WaveDirection)
                cfg["EnvironmentLoadTemplate"]["Fatigue"][0]["Wave"]["WaveTrains"][0][
                    "Name"
                ] = FileName
                cfg["EnvironmentLoadTemplate"]["Fatigue"][0]["Wave"]["WaveTrains"][0][
                    "WaveHs"
                ] = float(Hs)
                cfg["EnvironmentLoadTemplate"]["Fatigue"][0]["Wave"]["WaveTrains"][0][
                    "WaveTp"
                ] = float(Tp)
                cfg["EnvironmentLoadTemplate"]["Fatigue"][0]["Wave"]["WaveTrains"][0][
                    "WaveSeed"
                ] = int(WaveSeed)
                FatigueLoads.append(
                    copy.deepcopy(cfg["EnvironmentLoadTemplate"]["Fatigue"][0])
                )

                FatigueLoadingDFRow = [
                    Hs,
                    Tp,
                    WaveDirection,
                    cfg["SimFilePrefix"] + FileName,
                    SeasDF.iloc[indexi, columni],
                ]
                FatigueLoadingDF.loc[len(FatigueLoadingDF)] = FatigueLoadingDFRow
                TotalBins = TotalBins + 1
    print("Total number of bins is {}".format(TotalBins))
    return FatigueLoads, FatigueLoadingDF


def prepareFatiguePostProcessing(FatigueLoadingDF, cfg):
    model = {}
    model.update(FatiguePostProcessing())
    model["SNCurves"] = cfg["SNCurves"]
    model["LoadCases"] = []
    for i in range(0, len(FatigueLoadingDF)):
        customData = {
            "LoadCaseFileName": FatigueLoadingDF.loc[i]["FileName"] + ".sim",
            "LoadCaseLineName": cfg["FEASettings"]["LoadCaseLineName"],
            "PeriodFrom": cfg["FEASettings"]["SimulationPeriod"]["From"],
            "PeriodTo": cfg["FEASettings"]["SimulationPeriod"]["To"],
            "LoadCaseExposureTime": FatigueLoadingDF.loc[i]["Probability"].item()
            * 87.66,
        }

        model["LoadCases"].append(LoadCase(customData))

    model["AnalysisData"] = cfg["AnalysisData"]
    model.update(cfg["Outputs"])

    return model


def FatiguePostProcessing():
    model = {}
    model.update({"Title": "SLWR Fatigue Analysis"})
    model.update({"DamageCalculation": "Homogeneous pipe stress"})
    model.update({"AnalysisType": "Rainflow"})
    model.update({"UnitsSystem": "SI"})
    model.update({"CriticalDamageFactor": 0.1})
    model.update({"ThetaCount": 16})

    return model


def SNCurves():
    pass


def LoadCase(data):
    model = {}
    model.update({"LoadCaseFileName": data["LoadCaseFileName"]})
    model.update({"LoadCaseLineName": data["LoadCaseLineName"]})
    model.update({"PeriodFrom": data["PeriodFrom"]})
    model.update({"PeriodTo": data["PeriodTo"]})
    model.update({"LoadCaseExposureTime": data["LoadCaseExposureTime"]})

    return model


def AnalysisData():
    pass


def Outputs():
    model = {}
    model.update({"OutputLoadCaseTables": "Yes"})
    model.update({"OutputDetailedLoadCaseTables": "Yes"})
    model.update({"LoadCaseDamageUnits": "Damage per year"})

    return model


def main(arguments=None):
    """Run archived exports explicitly; load/parse failures propagate unchanged."""
    updateYml, file_name = configuration_inputs(sys.argv[1:] if arguments is None else arguments)
    from dataManager.ymlInput import ymlInput
    from results.saveData import saveDataYaml

    cfg = ymlInput("dataManager/fatigueLoadingTemplate.yml", updateYml)
    cfg["FileName"] = file_name

    data = {
        "WorkBook": "dataManager\\catenary\\" + cfg["WorkBook"],
        "WorkSheet": cfg["WorkSheet"],
    }

    #  Save wave data for inputs to Catenary program
    SeasDF = getFatigueData(data)
    FatigueLoads, FatigueLoadingDF = prepareFatigueLoading(SeasDF, cfg)
    cfg["FatigueLoads"] = FatigueLoads
    saveDataYaml(cfg, "results\\" + "FatigueLoading_" + cfg["FileName"], None)
    FatigueLoadingDF.to_csv("results\\catenary\\" + "FatigueLoading_" + cfg["FileName"])

    #  Save data for inputs to ftg file
    FatigueProcessor = prepareFatiguePostProcessing(FatigueLoadingDF, cfg)
    saveDataYaml(
        FatigueProcessor, "results\\" + "FatigueProcess_" + cfg["FileName"], False
    )


if __name__ == "__main__":
    main()
