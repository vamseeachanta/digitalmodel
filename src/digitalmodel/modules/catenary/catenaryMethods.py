import math
import pandas as pd

import matplotlib.pyplot as plt
from digitalmodel.custom.catenary.pipeProperties import pipeProperties


def catenaryEquation(data):

    if (data["F"] != None):
        S = data["d"] * (2 * data["F"] / data["w"] - data["d"])
        #Horizontal Distance.
        X = (((data["F"] / data["w"]) - data["d"]) * math.log(
            (S +
             (data["F"] / data["w"])) / ((data["F"] / data["w"]) - data["d"])))

        #weight of the suspended chain
        W = (data["w"] * S)

        #normalized horizontal tension component
        THorizontal = (data["F"] * X / math.sqrt(S**2 + X**2))

        #catenary shape parameter
        b = (data["w"] * 9.81 / THorizontal)
        data.update({"S": S, "X": X, "W": W, "THorizontal": THorizontal})

        return data

    elif (data["X"] != None):
        raise ("Not implemented yet")

    elif (data["q"] != None):
        tanq = math.tan(math.radians(90 - data["q"]))
        BendRadius = data["d"] * (math.cos(math.radians(90 - data["q"]))) / (
            1 - math.cos(math.radians(90 - data["q"])))
        S = BendRadius * tanq
        X = BendRadius * math.asinh(tanq)

        data.update({"S": S, "X": X, "BendRadius": BendRadius})

        return data


def catenaryForces(data):
    #  Vertical load on vessel
    Fv = data['weightPerUnitLength'] * data['S']
    #  Total force along catenary
    F = Fv / (math.sin(math.radians(90 - data['q'])))
    #  Horizontal force along catenary
    Fh = F * math.cos(math.radians(90 - data['q']))

    data.update({"Fv": Fv, "F": F, "Fh": Fh})

    return data


def sagHogEquation(data):
    # Sag to Buoyancy Configuration
    BendRadius = data['HangOff']["BendRadius"]
    d = (data['HogBendAboveSeabed'] - data['SagBendElevationAboveSeabed']
        ) * abs(data['WeightPerUnitLengthWithBuoyancy']) / (
            abs(data['WeightPerUnitLengthWithBuoyancy']) +
            abs(data['WeightPerUnitLengthWithOutBuoyancy']))
    X = BendRadius * math.acosh(d / BendRadius + 1)
    S = BendRadius * math.sinh(X / BendRadius)
    data['SagToBuoyancy'] = {"d": d, "S": S, "X": X, "BendRadius": BendRadius}

    # Buoy to Hog Configuration
    BendRadius = BendRadius * data['WeightPerUnitLengthWithOutBuoyancy'] / (abs(
        data['WeightPerUnitLengthWithBuoyancy']))
    d = (data['HogBendAboveSeabed'] - data['SagBendElevationAboveSeabed']
        ) * abs(data['WeightPerUnitLengthWithOutBuoyancy']) / (
            abs(data['WeightPerUnitLengthWithBuoyancy']) +
            abs(data['WeightPerUnitLengthWithOutBuoyancy']))
    X = BendRadius * math.acosh(d / BendRadius + 1)
    S = BendRadius * math.sinh(X / BendRadius)
    data['BuoyancyToHog'] = {"d": d, "S": S, "X": X, "BendRadius": BendRadius}

    # Hog to Buoyancy Configuration
    d = data['HogBendAboveSeabed'] * abs(
        data['WeightPerUnitLengthWithOutBuoyancy']) / (
            abs(data['WeightPerUnitLengthWithBuoyancy']) +
            abs(data['WeightPerUnitLengthWithOutBuoyancy']))
    X = BendRadius * math.acosh(d / BendRadius + 1)
    S = BendRadius * math.sinh(X / BendRadius)
    data['HogToBuoyancy'] = {"d": d, "S": S, "X": X, "BendRadius": BendRadius}

    # Buoyancy to TouchDown Configuration
    BendRadius = data['HangOff']["BendRadius"]
    d = data['HogBendAboveSeabed'] * abs(
        data['WeightPerUnitLengthWithBuoyancy']) / (
            abs(data['WeightPerUnitLengthWithBuoyancy']) +
            abs(data['WeightPerUnitLengthWithOutBuoyancy']))
    X = BendRadius * math.acosh(d / BendRadius + 1)
    S = BendRadius * math.sinh(X / BendRadius)
    data['BuoyancyToTouchDown'] = {
        "d": d,
        "S": S,
        "X": X,
        "BendRadius": BendRadius
    }

    return data


def lazyWaveCatenaryEquation(data):

    #  Hang-off to Sag Section
    catenaryResult = catenaryEquation(data['HangOff'])
    data['HangOff'].update(catenaryResult)

    data = sagHogEquation(data)
    # Summarize the lazy wave catenary
    Fh = data['HangOff']["BendRadius"] * data[
        'WeightPerUnitLengthWithOutBuoyancy']
    Fv = Fh + data['WeightPerUnitLengthWithOutBuoyancy'] * data['HangOff']["S"]

    HangOffToBuoyancy = {
        "S": data['HangOff']["S"] + data['SagToBuoyancy']["S"],
        "X": data['HangOff']["X"] + data['SagToBuoyancy']["X"]
    }
    Buoyancy = {
        "S": data['BuoyancyToHog']["S"] + data['HogToBuoyancy']["S"],
        "X": data['BuoyancyToHog']["X"] + data['HogToBuoyancy']["X"]
    }
    BuoyancyToTouchDown = {
        "S": data['BuoyancyToTouchDown']["S"],
        "X": data['BuoyancyToTouchDown']["X"]
    }
    HangoffToTDP = {
        "S": HangOffToBuoyancy["S"] + Buoyancy["S"] + BuoyancyToTouchDown["S"],
        "X": HangOffToBuoyancy["X"] + Buoyancy["X"] + BuoyancyToTouchDown["X"]
    }

    data['Summary'] = {
        "HangOffToBuoyancy": HangOffToBuoyancy,
        "Buoyancy": Buoyancy,
        "BuoyancyToTouchDown": BuoyancyToTouchDown,
        "HangoffToTDP": HangoffToTDP,
        "Fh": Fh,
        "Fv": Fv
    }
    # data['Summary'] = { "HangOffToBuoyancy": HangOffToBuoyancy}

    return data


def lazyWavePlot(data, spacing=10):
    # HangoffToBuoyancy Section
    X = []
    Y = []
    BendRadius = data['lazyWaveCatenaryResult']['HangOff']['BendRadius']
    HorizontalDistance = data['lazyWaveCatenaryResult']['HangOff']['X']
    XRange = data['lazyWaveCatenaryResult']['Summary']["HangOffToBuoyancy"]['X']
    XCoordinateAdjustment = 0
    YCoordinateAdjustment = (BendRadius + data['lazyWaveCatenaryResult']['HangOff']['d'] \
        + data['LazyWaveCatenaryDefinition']['HangoffBelowMeanSeaLevel'])
    for i in range(0, spacing + 1, 1):
        X.append(XCoordinateAdjustment + i * XRange / spacing)
        Y.append(BendRadius*(math.cosh((X[i] - HorizontalDistance)/BendRadius)) \
        - YCoordinateAdjustment)

    data['lazyWaveCatenaryResult']['Summary']["HangOffToBuoyancy"][
        'PlotData'] = {
            "X": X,
            "Y": Y
        }
    PlotData = {
        "X": X,
        "Y": Y,
        "label": "Hang-off to Buoyancy",
        "SaveAndShow": False
    }
    genericPlot(PlotData)

    # Buoyancy Section
    X = []
    Y = []
    BendRadius = data['lazyWaveCatenaryResult']['BuoyancyToHog']['BendRadius']
    HorizontalDistance = data['lazyWaveCatenaryResult']['HangOff']['X'] \
                        + data['lazyWaveCatenaryResult']["SagToBuoyancy"]['X'] \
                        + data['lazyWaveCatenaryResult']["BuoyancyToHog"]['X']
    XCoordinateAdjustment = data['lazyWaveCatenaryResult']['HangOff']['X'] \
                        + data['lazyWaveCatenaryResult']["SagToBuoyancy"]['X']

    XRange = data['lazyWaveCatenaryResult']['Summary']["Buoyancy"]['X']
    YCoordinateAdjustment = -BendRadius + data['lazyWaveCatenaryResult']['HangOff']['d'] \
        - (data['LazyWaveCatenaryDefinition']['HogBendAboveSeabed']   \
        - data['LazyWaveCatenaryDefinition']['SagBendElevationAboveSeabed'])   \
        + data['LazyWaveCatenaryDefinition']['HangoffBelowMeanSeaLevel']
    for i in range(0, spacing + 1, 1):
        X.append(XCoordinateAdjustment + XRange * i / spacing)
        Y.append(-BendRadius*(math.cosh((-X[i] + HorizontalDistance)/BendRadius)) \
        - YCoordinateAdjustment)

    data['lazyWaveCatenaryResult']['Summary']["Buoyancy"]['PlotData'] = {
        "X": X,
        "Y": Y
    }
    PlotData = {"X": X, "Y": Y}
    PlotData = {"X": X, "Y": Y, "label": "Buoyancy", "SaveAndShow": False}
    genericPlot(PlotData)

    # BuoyancyToTouchDown Section
    X = []
    Y = []
    BendRadius = data['lazyWaveCatenaryResult']["BuoyancyToTouchDown"][
        'BendRadius']
    HorizontalDistance = data['lazyWaveCatenaryResult']['Summary'][
        'HangoffToTDP']['X']
    XCoordinateAdjustment = data['lazyWaveCatenaryResult']['Summary']['HangoffToTDP']['X'] \
                    - data['lazyWaveCatenaryResult']["BuoyancyToTouchDown"]['X']
    XRange = data['lazyWaveCatenaryResult']['BuoyancyToTouchDown']['X']
    YCoordinateAdjustment = (BendRadius + data['LazyWaveCatenaryDefinition']['HogBendAboveSeabed'] \
        + data['LazyWaveCatenaryDefinition']['HangoffBelowMeanSeaLevel'])
    for i in range(0, spacing + 1, 1):
        X.append(XCoordinateAdjustment + XRange * i / spacing)

        Y.append(BendRadius*math.cosh((X[i] - HorizontalDistance)/BendRadius) \
        - BendRadius - data['LazyWaveCatenaryDefinition']['VerticalDistance'] \
        - data['LazyWaveCatenaryDefinition']['HangoffBelowMeanSeaLevel'])

    data['lazyWaveCatenaryResult']['Summary']["BuoyancyToTouchDown"][
        'PlotData'] = {
            "X": X,
            "Y": Y
        }
    # PlotData = {"X": X, "Y": Y, "label": "Buoyancy to TDP", "SaveAndShow": True, "fileName": "results\\" + "SLWR_" + data['FileName']}
    PlotData = {
        "X":
            X,
        "Y":
            Y,
        "label":
            "Buoyancy to TDP",
        "SaveAndShow":
            True,
        "fileName":
            data['Analysis']['result_folder'] + "SLWR_" +
            data['Analysis']['file_name']
    }
    genericPlot(PlotData)

    return data


def simple_catenary_plot(data, spacing=10):
    # HangoffToTouchDown Section
    X = []
    Y = []
    BendRadius = data['catenaryResult']['BendRadius']
    HorizontalDistance = data['catenaryResult']['X']
    XRange = data['catenaryResult']['X']
    XCoordinateAdjustment = 0
    YCoordinateAdjustment = (BendRadius + data['catenaryResult']['d'] \
        + data['commonDefinition']['HangoffBelowMeanSeaLevel'])
    for i in range(0, spacing + 1, 1):
        X.append(XCoordinateAdjustment + i * XRange / spacing)
        Y.append(BendRadius*(math.cosh((X[i] - HorizontalDistance)/BendRadius)) \
        - YCoordinateAdjustment)

    data['catenaryResult']['PlotData'] = {"X": X, "Y": Y}
    PlotData = {
        "X":
            X,
        "Y":
            Y,
        "label":
            "Catenary",
        "SaveAndShow":
            True,
        "fileName":
            data['Analysis']['result_folder'] + "SCR_" +
            data['Analysis']['file_name']
    }
    genericPlot(PlotData)

    return data


def genericPlot(data):
    if data["label"] == None:
        plt.plot(data["X"], data["Y"])
    else:
        plt.plot(data["X"], data["Y"], label=data["label"])
    #it creates plot X-axis name, fontsize and colour
    plt.xlabel('Horizontal distance[m]',
               fontsize=12,
               fontweight='bold',
               color='black')
    #it creates plot Y-axis name, fontsize and colour
    plt.ylabel('Distance from seabed [m]',
               fontsize=12,
               fontweight='bold',
               color='black')
    #it creates plot tittle, color and size
    plt.title('Catenary Mooring Line Shape',
              fontsize=14,
              fontweight='bold',
              color='black')
    # Gridlines
    plt.grid()
    if (data['SaveAndShow'] == True):
        # plt.savefig(data["fileName"].replace(".", ""), dpi=800)
        plt.savefig(data["fileName"], dpi=800)
        plt.legend(loc='upper center',
                   bbox_to_anchor=(0.5, -0.05),
                   fancybox=True,
                   shadow=True,
                   ncol=5)


def buoyancyProperties(cfg, WeightPerUnitLengthWithOutBuoyancy):

    if cfg["commonDefinition"]["UniformBuoyancy"]["Thickness"] != None:
        cfg = pipeProperties(
            cfg,
            FluidDensity=cfg['Material']['Reference_Fluid']['Rho'],
            Buoyancy=True)
        WeightPerUnitLengthWithBuoyancy = cfg['equivalentPipe'][
            'weightPerUnitLength']
        cfg["commonDefinition"]["UniformBuoyancy"]["BuoyancyFactor"] = 1 - (
            WeightPerUnitLengthWithBuoyancy /
            WeightPerUnitLengthWithOutBuoyancy)
        # Add buoyancy Ratio and also buoyancy module detailed properties
    elif cfg["commonDefinition"]["UniformBuoyancy"]["BuoyancyFactor"] != None:
        BuoyancyOD = math.sqrt(2*cfg['catenaryResult']['Reference_FluidFilled']['weightPerUnitLength']/9.81*4/math.pi/(cfg['Material']['SeaWater']['Rho'] \
            -cfg['Material']['Buoyancy']['Rho']) + (cfg['InsulationSection']['OD']*0.0254)**2)/0.0254
        cfg["commonDefinition"]["UniformBuoyancy"]["Thickness"] = (
            BuoyancyOD - cfg['InsulationSection']['OD']) / 2
        WeightPerUnitLengthWithBuoyancy = (1 - cfg["commonDefinition"]["UniformBuoyancy"]["BuoyancyFactor"]) \
            * cfg['catenaryResult']['FluidFilled']['weightPerUnitLength']

    cfg["commonDefinition"]["UniformBuoyancy"][
        'BuoyancyOD'] = cfg['InsulationSection'][
            'OD'] + 2 * cfg["commonDefinition"]["UniformBuoyancy"]["Thickness"]

    cfg['commonDefinition']['DiscreteBuoyancy']['BuoyancyOD'] = math.sqrt(
        100 / cfg['commonDefinition']['DiscreteBuoyancy']['BuoyancyCoverage'] *
        (BuoyancyOD**2 - cfg['InsulationSection']['OD']**2) +
        cfg['InsulationSection']['OD']**2)
    cfg['commonDefinition']['DiscreteBuoyancy']['Thickness'] = (
        cfg['commonDefinition']['DiscreteBuoyancy']['BuoyancyOD'] -
        cfg['InsulationSection']['OD']) / 2

    cfg['lazyWaveCatenaryResult'] = {}
    cfg['lazyWaveCatenaryResult']['WeightPerUnitLengthWithOutBuoyancy'] = cfg[
        'catenaryResult']['FluidFilled']['weightPerUnitLength']
    cfg['lazyWaveCatenaryResult'][
        'WeightPerUnitLengthWithBuoyancy'] = WeightPerUnitLengthWithBuoyancy

    return cfg


def gethorizontalDistanceHangofftoTDPrange(cfg):
    layback_df = pd.DataFrame(
        columns=['q', 'X', 'S', 'BendRadius', 'TotalLength', 'MeshSections'])
    steps_in_degrees = 100
    if 'range' in cfg["simpleCatenaryDefinition"][
            "horizontalDistanceHangofftoTDP"] and len(
                cfg["simpleCatenaryDefinition"]
                ["horizontalDistanceHangofftoTDP"]['range']) > 0:

        layback_range = cfg["simpleCatenaryDefinition"][
            "horizontalDistanceHangofftoTDP"]['range']

        if cfg['catenaryResult']['X'] > layback_range[-1]:
            q_max = int(cfg['catenaryResult']['q']) + 1
        else:
            q_max = 45

        for q_index in range(0, q_max * steps_in_degrees):
            q = q_index / steps_in_degrees
            catenaryEquation_cfg = {
                "d": cfg["simpleCatenaryDefinition"]["verticalDistance"],
                "F": cfg["simpleCatenaryDefinition"]["axialLineForce"],
                "q": q,
                "X": None
            }
            catenaryResult = catenaryEquation(catenaryEquation_cfg)
            total_length = catenaryResult['S'] + cfg['commonDefinition'][
                'TDPToAnchor']['SCR'] + cfg['commonDefinition'][
                    'LengthBeforeHangoff']
            mesh_sections = [{
                'Length': cfg['commonDefinition']['LengthBeforeHangoff'] + 25,
                'TargetSegmentLength': 0.2
            }, {
                'Length': 5,
                'TargetSegmentLength': 0.5
            }, {
                'Length': 10,
                'TargetSegmentLength': 1.0
            }, {
                'Length': round(catenaryResult['S'] - 40 - 60, 1),
                'TargetSegmentLength': 2.0
            }, {
                'Length': 10,
                'TargetSegmentLength': 1.0
            }, {
                'Length': 180,
                'TargetSegmentLength': 0.5
            }, {
                'Length': 20,
                'TargetSegmentLength': 1.0
            }, {
                'Length': cfg['commonDefinition']['TDPToAnchor']['SCR'] - 150,
                'TargetSegmentLength': 2.0
            }]
            layback_df.loc[q_index] = [
                catenaryResult['q'], catenaryResult['X'], catenaryResult['S'],
                catenaryResult['BendRadius'], total_length, mesh_sections
            ]

            if catenaryResult['X'] > layback_range[-1]:
                break

        result_index_range = []
        for layback_value in layback_range:
            result_index_range.append(
                layback_df['X'].sub(layback_value).abs().idxmin())

        layback_df = layback_df.round(3)
        rangeresult = layback_df.iloc[result_index_range].to_dict('records')

        return rangeresult
