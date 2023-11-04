import pkgutil
import math
from collections import OrderedDict

import yaml

from assetutilities.common.saveData import saveDataYaml


def orcaflexModel(data, FEAType, LoadingIndex=0):
    model1 = OrderedDict()
    model2 = OrderedDict()

    #  Using default settings
    customData = {
        "StageDuration":
            data['FEASettings']['Stage'][FEAType]['Duration'],
        "TargetLogSampleInterval":
            data['FEASettings']['Stage'][FEAType]['TargetLogSampleInterval'],
        "ImplicitConstantTimeStep":
            data['FEASettings']['Stage'][FEAType]['ImplicitConstantTimeStep']
    }
    model1["General"] = general(customData)

    customData = []
    customData1 = {
        "Type": "BendingConnectionStiffness",
        "Name": "FJProfile",
        "FlexJointStiffness": data['FEASettings']['FlexJointStiffness'][FEAType]
    }
    customData2 = {
        "Type":
            "LineTypeOuterDiameter",
        "Name":
            "FJExtension",
        "ProfileLength":
            data['commonDefinition']['TaperJoint']['L'],
        "ThickEndOD":
            (data['MainPipe']['SteelSection']['ID'] +
             2 * data['commonDefinition']['TaperJoint']['ThickendThickness']) *
            0.0254,
        "ThinEndOD": (data['MainPipe']['SteelSection']['OD']) * 0.0254
    }
    customData.append(customData1)
    customData.append(customData2)
    model1["VariableData"] = VariableData(customData)

    customData = {
        "Density":
            data['Material']['SeaWater']['Rho'] / 1000,
        "WaterDepth":
            data['geometry']['waterDepth'],
        "SeabedNormalStiffness":
            data['FEASettings']['Seabed']['Stiffness'][FEAType] /
            ((data['MainPipe']['InsulationSection']['OD'] +
              2 * data['geometry']['Strakes']['BaseThickness']) * 0.0254)
    }
    model1["Environment"] = environment(data, customData, FEAType, LoadingIndex)

    model2["RayleighDampingCoefficients"] = []

    if data['FEASettings']['Damping'][FEAType]['Period1'] == "WavePeriod1":
        if data['EnvironmentLoad'][FEAType][LoadingIndex]['Wave']['WaveTrains'][
                0]['WaveType'] == "Dean Stream":
            customData = {
                "Name":
                    data['FEASettings']['Damping'][FEAType]['Name'],
                "Mode":
                    data['FEASettings']['Damping'][FEAType]['Mode'],
                "DampingRatio":
                    data['FEASettings']['Damping'][FEAType]['DampingRatio'],
                "Period1":
                    data['EnvironmentLoad'][FEAType][LoadingIndex]['Wave']
                    ['WaveTrains'][0]['WaveTz'],
                "ApplyToGeometricStiffness":
                    data['FEASettings']['Damping'][FEAType]
                    ['ApplyToGeometricStiffness']
            }
        elif data['EnvironmentLoad'][FEAType][LoadingIndex]['Wave'][
                'WaveTrains'][0]['WaveType'] == "JONSWAP":
            customData = {
                "Name":
                    data['FEASettings']['Damping'][FEAType]['Name'],
                "Mode":
                    data['FEASettings']['Damping'][FEAType]['Mode'],
                "DampingRatio":
                    data['FEASettings']['Damping'][FEAType]['DampingRatio'],
                "Period1":
                    data['EnvironmentLoad'][FEAType][LoadingIndex]['Wave']
                    ['WaveTrains'][0]['WaveTp'],
                "ApplyToGeometricStiffness":
                    data['FEASettings']['Damping'][FEAType]
                    ['ApplyToGeometricStiffness']
            }
        elif data['EnvironmentLoad'][FEAType][LoadingIndex]['Wave'][
                'WaveTrains'][0]['WaveType'] == "Ochi-Hubble":
            customData = {
                "Name":
                    data['FEASettings']['Damping'][FEAType]['Name'],
                "Mode":
                    data['FEASettings']['Damping'][FEAType]['Mode'],
                "DampingRatio":
                    data['FEASettings']['Damping'][FEAType]['DampingRatio'],
                "Period1":
                    1 / data['EnvironmentLoad'][FEAType][LoadingIndex]['Wave']
                    ['WaveTrains'][0]['Wavefm1'],
                "ApplyToGeometricStiffness":
                    data['FEASettings']['Damping'][FEAType]
                    ['ApplyToGeometricStiffness']
            }
    else:
        customData = {
            "Name":
                data['FEASettings']['Damping'][FEAType]['Name'],
            "Mode":
                data['FEASettings']['Damping'][FEAType]['Mode'],
            "DampingRatio":
                data['FEASettings']['Damping'][FEAType]['DampingRatio'],
            "Period1":
                data['FEASettings']['Damping'][FEAType]['Period1'],
            "ApplyToGeometricStiffness":
                data['FEASettings']['Damping'][FEAType]
                ['ApplyToGeometricStiffness']
        }

    model2["RayleighDampingCoefficients"].append(
        RayleighDampingCoefficients(customData))

    model2["LineTypes"] = []
    # Main Pipe

    customData = {
        "Name":
            "MainPipe",
        "Category":
            "General",
        "OD": (data['MainPipe']['InsulationSection']['OD'] +
               2 * data['geometry']['Strakes']['BaseThickness']) * 0.0254,
        "ID":
            data['MainPipe']['SteelSection']['ID'] * 0.0254,
        "MassPerUnitLength":
            data['MainPipe']['equivalentPipe']['WithoutBuoyancy']
            ['massPerUnitLength'] / 1000,
        "EI":
            data['Material']['Steel']['E'] * 6.894745 *
            data['MainPipe']['SteelSection']['I'] * 0.0254**4,
        "EA":
            data['Material']['Steel']['E'] * 6.894745 *
            data['MainPipe']['SteelSection']['A'] * 0.0254**2,
        "PoissonRatio":
            data['Material']['Steel']['PoissionsRatio'],
        "GJ":
            data['Material']['Steel']['G'] * 6.894745 * 2 *
            data['MainPipe']['SteelSection']['I'] * 0.0254**4,
        "Cd":
            data['FEASettings']['Hydrodynamic'][FEAType]['MainPipe']['Cd'],
        "Ca":
            data['FEASettings']['Hydrodynamic'][FEAType]['MainPipe']['Ca'],
        "StressOD":
            data['MainPipe']['SteelSection']['OD'] * 0.0254,
        "StressID":
            data['MainPipe']['SteelSection']['ID'] * 0.0254 +
            2 * data['FEASettings']['CorrosionAllowance'][FEAType] * 0.0254,
        "SeabedNormalFrictionCoefficient":
            data['FEASettings']['Seabed']['FrictionCoefficient']['Normal'],
        "SeabedAxialFrictionCoefficient":
            data['FEASettings']['Seabed']['FrictionCoefficient']['Axial'],
        "CorrosionThickness":
            0,
        "SMYS":
            data['Material']['Steel']['SMYS'] * 6.894745,
        "E":
            data['Material']['Steel']['E'] * 6.894745,
        "SMUS":
            data['Material']['Steel']['SMUS'] * 6.894745,
        "RayleighDampingCoefficients":
            data['FEASettings']['Damping'][FEAType]['Name'],
        "APISTD2RDFd1":
            data['EnvironmentLoad'][FEAType][LoadingIndex]['CodeChecks']
            ['APISTD2RDFd1'],
        "APISTD2RDFd2":
            data['EnvironmentLoad'][FEAType][LoadingIndex]['CodeChecks']
            ['APISTD2RDFd2'],
        "APISTD2RDDelta":
            data['EnvironmentLoad'][FEAType][LoadingIndex]['CodeChecks']
            ['APISTD2RDDelta'],
        "APISTD2RDAlphaFab":
            data['EnvironmentLoad'][FEAType][LoadingIndex]['CodeChecks']
            ['APISTD2RDAlphaFab'],
        "APISTD2RDk":
            data['EnvironmentLoad'][FEAType][LoadingIndex]['CodeChecks']
            ['APISTD2RDk'],
        "Pen": [2, "Solid", "Yellow"]
    }
    model2["LineTypes"].append(lineTypes(customData))
    # Buoy Pipe
    customData = {
        "Name":
            "BuoyPipe",
        "Category":
            "General",
        "OD":
            data['BuoyPipe']['BuoyancySection']['OD'] * 0.0254,
        "ID":
            data['BuoyPipe']['SteelSection']['ID'] * 0.0254,
        "MassPerUnitLength":
            data['BuoyPipe']['equivalentPipe']['WithBuoyancy']
            ['massPerUnitLength'] / 1000,
        "EI":
            data['Material']['Steel']['E'] * 6.894745 *
            data['BuoyPipe']['SteelSection']['I'] * 0.0254**4,
        "EA":
            data['Material']['Steel']['E'] * 6.894745 *
            data['BuoyPipe']['SteelSection']['A'] * 0.0254**2,
        "PoissonRatio":
            data['Material']['Steel']['PoissionsRatio'],
        "GJ":
            data['Material']['Steel']['G'] * 6.894745 * 2 *
            data['BuoyPipe']['SteelSection']['I'] * 0.0254**4,
        "Cd":
            data['FEASettings']['Hydrodynamic'][FEAType]['BuoyPipe']['Cd'],
        "Ca":
            data['FEASettings']['Hydrodynamic'][FEAType]['BuoyPipe']['Ca'],
        "StressOD":
            data['BuoyPipe']['SteelSection']['OD'] * 0.0254,
        "StressID":
            data['BuoyPipe']['SteelSection']['ID'] * 0.0254 +
            2 * data['FEASettings']['CorrosionAllowance'][FEAType] * 0.0254,
        "SeabedNormalFrictionCoefficient":
            data['FEASettings']['Seabed']['FrictionCoefficient']['Normal'],
        "SeabedAxialFrictionCoefficient":
            data['FEASettings']['Seabed']['FrictionCoefficient']['Axial'],
        "CorrosionThickness":
            0,
        "SMYS":
            data['Material']['Steel']['SMYS'] * 6.894745,
        "E":
            data['Material']['Steel']['E'] * 6.894745,
        "SMUS":
            data['Material']['Steel']['SMUS'] * 6.894745,
        "RayleighDampingCoefficients":
            data['FEASettings']['Damping'][FEAType]['Name'],
        "APISTD2RDFd1":
            data['EnvironmentLoad'][FEAType][LoadingIndex]['CodeChecks']
            ['APISTD2RDFd1'],
        "APISTD2RDFd2":
            data['EnvironmentLoad'][FEAType][LoadingIndex]['CodeChecks']
            ['APISTD2RDFd2'],
        "APISTD2RDDelta":
            data['EnvironmentLoad'][FEAType][LoadingIndex]['CodeChecks']
            ['APISTD2RDDelta'],
        "APISTD2RDAlphaFab":
            data['EnvironmentLoad'][FEAType][LoadingIndex]['CodeChecks']
            ['APISTD2RDAlphaFab'],
        "APISTD2RDk":
            data['EnvironmentLoad'][FEAType][LoadingIndex]['CodeChecks']
            ['APISTD2RDk'],
        "Pen": [2, "Solid", "Red"]
    }
    model2["LineTypes"].append(lineTypes(customData))

    # BuoyPipeStartEnd
    customData.update({
        "Name":
            "BuoyPipeStartEnd",
        "Cd":
            data['FEASettings']['Hydrodynamic'][FEAType]['BuoyPipeStartEnd']
            ['Cd'],
        "Ca":
            data['FEASettings']['Hydrodynamic'][FEAType]['BuoyPipeStartEnd']
            ['Ca'],
    })
    model2["LineTypes"].append(lineTypes(customData))
    # FJExtension
    customData = {
        "Name":
            "FJExtension1",
        "Category":
            "Homogeneous Pipe",
        "OD":
            "FJExtension",
        "ID":
            data['MainPipe']['SteelSection']['ID'] * 0.0254 +
            2 * data['FEASettings']['CorrosionAllowance'][FEAType] * 0.0254,
        "MaterialDensity":
            data['Material']['Steel']['Rho'] / 1000,
        "E":
            data['Material']['Steel']['E'] * 6.894745,
        "PoissonRatio":
            data['Material']['Steel']['PoissionsRatio'],
        "Cdn":
            1,
        "Cdz":
            0.008,
        "Cl":
            0,
        "Can":
            1,
        "Caz":
            0,
        "SeabedNormalFrictionCoefficient":
            data['FEASettings']['Seabed']['FrictionCoefficient']['Normal'],
        "SeabedAxialFrictionCoefficient":
            data['FEASettings']['Seabed']['FrictionCoefficient']['Axial'],
        "AllowableStress":
            data['Material']['Steel']['SMYS'] * 6.894745,
        "CorrosionThickness":
            0,
        "SMYS":
            data['Material']['Steel']['SMYS'] * 6.894745,
        "SMUS":
            data['Material']['Steel']['SMUS'] * 6.894745,
        "RayleighDampingCoefficients":
            data['FEASettings']['Damping'][FEAType]['Name'],
        "APISTD2RDFd1":
            data['EnvironmentLoad'][FEAType][LoadingIndex]['CodeChecks']
            ['APISTD2RDFd1'],
        "APISTD2RDFd2":
            data['EnvironmentLoad'][FEAType][LoadingIndex]['CodeChecks']
            ['APISTD2RDFd2'],
        "APISTD2RDDelta":
            data['EnvironmentLoad'][FEAType][LoadingIndex]['CodeChecks']
            ['APISTD2RDDelta'],
        "APISTD2RDAlphaFab":
            data['EnvironmentLoad'][FEAType][LoadingIndex]['CodeChecks']
            ['APISTD2RDAlphaFab'],
        "APISTD2RDk":
            data['EnvironmentLoad'][FEAType][LoadingIndex]['CodeChecks']
            ['APISTD2RDk'],
        "Pen": [2, "Solid", "$C080FF"]
    }
    model2["LineTypes"].append(lineTypes(customData))

    customData = {"Draught": data['EnvironmentLoad'][FEAType][LoadingIndex]['Vessel']['Draft'],
                "InitialPosition": [ data['EnvironmentLoad'][FEAType][LoadingIndex]['Vessel']['InitialPosition'][0] + data['EnvironmentLoad'][FEAType][LoadingIndex]['Vessel']['OffsetPercentWD']*data['geometry']['waterDepth']/100 *math.cos(math.radians(data['EnvironmentLoad'][FEAType][LoadingIndex]['Wave']['WaveTrains'][0]['WaveDirection'])), \
                data['EnvironmentLoad'][FEAType][LoadingIndex]['Vessel']['InitialPosition'][1] + data['EnvironmentLoad'][FEAType][LoadingIndex]['Vessel']['OffsetPercentWD']*data['geometry']['waterDepth']/100*math.sin(math.radians(data['EnvironmentLoad'][FEAType][LoadingIndex]['Wave']['WaveTrains'][0]['WaveDirection'])) , \
                data['EnvironmentLoad'][FEAType][LoadingIndex]['Vessel']['InitialPosition'][2] ],
                "Orientation": data['EnvironmentLoad'][FEAType][LoadingIndex]['Vessel']['Orientation']}

    model2["Vessels"] = []
    model2["Vessels"].append(vessels(customData))

    model2["Lines"] = []
    model2["Lines"].append(lines(data, FEAType, LoadingIndex))
    return model1, model2


def general(data):
    model = {}
    model.update({"UnitsSystem": "SI"})
    model.update({"StaticsMethod": "Whole System statics"})
    model.update({"BuoysIncludedInStatics": "Individually Specified"})
    model.update({"StageDuration": data['StageDuration']})
    model.update({"LogPrecision": "Single"})
    model.update({"TargetLogSampleInterval": data['TargetLogSampleInterval']})
    model.update({"SimulationIntegrationMethod": "Implicit"})
    model.update({"ImplicitUseVariableTimeStep": "No"})
    model.update({"ImplicitConstantTimeStep": data['ImplicitConstantTimeStep']})

    return model


def environment(data, customData, FEAType, LoadingIndex):
    #  Sea and Seabed
    model = {}
    model.update(seaAndSeabed(customData))
    model.update(wave(data, FEAType, LoadingIndex))
    model.update(current(data, FEAType, LoadingIndex))
    model.update(wind())

    return model


def seaAndSeabed(data):
    model = {}
    # Sea
    model.update({"WaterSurfaceZ": 0})
    model.update({"KinematicViscosity": 1.35E-6})
    model.update({"SeaTemperature": 10})
    model.update({"ReynoldsNumberCalculation": "Flow Direction"})
    # Sea Density
    model.update({"HorizontalWaterDensityFactor": "~"})
    model.update({"VerticalDensityVariation": "Constant"})
    model.update({"Density": data["Density"]})
    # Seabed
    model.update({"SeabedType": "Flat"})
    model.update({"SeabedOrigin": [0, 0]})
    model.update({"WaterDepth": data['WaterDepth']})
    model.update({"SeabedSlopeDirection": 0})
    model.update({"SeabedSlope": 0})
    model.update({"SeabedModel": "Linear"})
    model.update({"SeabedNormalStiffness": data["SeabedNormalStiffness"]})
    model.update({"SeabedShearStiffness": "~"})

    return model


def wave(data, FEAType, LoadingIndex):
    model = {}
    # Waves
    # print(data['EnvironmentLoad'][FEAType][LoadingIndex])
    if data['EnvironmentLoad'][FEAType][LoadingIndex]['Wave']['WaveTrains'][0][
            'WaveType'] == "Dean Stream":
        model.update({"SimulationTimeOrigin": 0})
        model.update({
            "WaveTrains": [{
                "Name": "Wave1",
                "WaveType": "Dean Stream",
                "WaveDirection": 180,
                "WaveHeight": 7,
                "WavePeriod": 8,
                "WaveOrigin": [0, 0],
                "WaveTimeOrigin": 0,
                "WaveStreamFunctionOrder": 5
            }]
        })
    elif data['EnvironmentLoad'][FEAType][LoadingIndex]['Wave']['WaveTrains'][
            0]['WaveType'] == "JONSWAP":
        model.update(data['EnvironmentLoad'][FEAType][LoadingIndex]['Wave'])
    elif data['EnvironmentLoad'][FEAType][LoadingIndex]['Wave']['WaveTrains'][
            0]['WaveType'] == "Ochi-Hubble":
        model.update(data['EnvironmentLoad'][FEAType][LoadingIndex]['Wave'])

    return model


def current(data, FEAType, LoadingIndex):
    model = {}
    # Current
    if data['EnvironmentLoad'][FEAType][LoadingIndex][
            "MultipleCurrentDataCanBeDefined"]:
        model.update({"MultipleCurrentDataCanBeDefined": "Yes"})
        model.update({
            "Currents":
                data['EnvironmentLoad'][FEAType][LoadingIndex]['Currents']
        })
        model.update({
            "ActiveCurrent":
                data['EnvironmentLoad'][FEAType][LoadingIndex]['ActiveCurrent']
        })
    else:
        model.update({"MultipleCurrentDataCanBeDefined": "No"})
        model.update({"CurrentRamp": "No"})
        model.update({"HorizontalCurrentFactor": "~"})
        model.update({"CurrentMethod": "Interpolated"})
        model.update({
            "RefCurrentSpeed":
                data['EnvironmentLoad'][FEAType][LoadingIndex]['Current']
                ['RefCurrentSpeed']
        })
        model.update({
            "RefCurrentDirection":
                data['EnvironmentLoad'][FEAType][LoadingIndex]['Current']
                ['RefCurrentDirection']
        })
        model.update({
            "CurrentDepth, CurrentFactor, CurrentRotation":
                data['EnvironmentLoad'][FEAType][LoadingIndex]['Current']
                ['CurrentDepth, CurrentFactor, CurrentRotation']
        })
    return model


def wind():
    model = {}
    # Wind
    model.update({"IncludeVesselWindLoads": "Yes"})
    model.update({"IncludeLineWindLoads": "Yes"})
    model.update({"IncludeBuoyWingWindLoads": "Yes"})
    model.update({"VerticalWindVariationFactor": "~"})
    model.update({"AirDensity": 0.00128})
    model.update({"WindType": "Constant"})
    model.update({"WindSpeed": 0})
    model.update({"WindDirection": 0})

    return model


def vesselTypes(data):
    pass


def supportTypes(data):
    pass


def codeChecks(data):
    model = {}

    model.update({"APIRP2RDCorrosionThickness": data['CorrosionThickness']})
    model.update({"APIRP2RDSMYS": data['SMYS']})
    model.update({"APISTD2RDE": data['E']})
    model.update({"APISTD2RDS": data['SMYS']})
    model.update({"APISTD2RDU": data['SMUS']})
    model.update({"APISTD2RDDelta": data['APISTD2RDDelta']})
    model.update({"APISTD2RDFd1": data['APISTD2RDFd1']})
    model.update({"APISTD2RDFd2": data['APISTD2RDFd2']})
    model.update({"APISTD2RDAlphaFab": data['APISTD2RDAlphaFab']})
    model.update({"APISTD2RDk": data['APISTD2RDk']})

    return model


def RayleighDampingCoefficients(data):
    model = {}

    model.update({"Name": data['Name']})
    model.update({"Mode": data['Mode']})
    model.update({"DampingRatio": data['DampingRatio']})
    model.update({"Period1": data['Period1']})
    model.update(
        {"ApplyToGeometricStiffness": data['ApplyToGeometricStiffness']})

    return model


def lineTypes(data):
    model = {}
    if data['Category'] == "General":
        model.update({"Name": data["Name"]})
        model.update({"Category": "General"})
        model.update({"OD": data["OD"]})
        model.update({"ID": data["ID"]})
        model.update({"CG": [0, 0]})
        model.update({"BulkModulus": "Infinity"})
        model.update({"MassPerUnitLength": data['MassPerUnitLength']})
        model.update({"CompressionIsLimited": "No"})
        model.update({"AllowableTension": "~"})
        model.update({"MinRadius": ["~", "~"]})
        model.update({"EI": [data['EI'], "~"]})
        model.update({"EA": data['EA']})
        model.update({"PoissonRatio": data['PoissonRatio']})
        model.update({"GJ": data['GJ']})
        model.update({"Cd": data['Cd']})
        model.update({"Cl": 0})
        model.update({"NormalDragLiftDiameter": "~"})
        model.update({"AxialDragLiftDiameter": "~"})
        model.update({"Ca": data['Ca']})
        model.update({"Cm": ["~", "~", "~"]})
        model.update({"ContactDiameter": "~"})
        model.update({"ClashStiffness": 0})
        model.update({"StressOD": data['StressOD']})
        model.update({"StressID": data['StressID']})
        model.update({"AllowableStress": "~"})
        model.update({"TensileStressLoadingFactor": 1})
        model.update({"BendingStressLoadingFactor": 1})
        model.update({"ShearStressLoadingFactor": 1})
        model.update({"TorsionalStressLoadingFactor": 1})
        model.update({
            "SeabedNormalFrictionCoefficient":
                data['SeabedNormalFrictionCoefficient']
        })
        model.update({
            "SeabedAxialFrictionCoefficient":
                data['SeabedAxialFrictionCoefficient']
        })
        model.update({
            "RayleighDampingCoefficients": data['RayleighDampingCoefficients']
        })
        model.update({"CorrosionThickness": data['CorrosionThickness']})
        model.update({"SMYS": data['SMYS']})
        model.update(codeChecks(data))
        model.update({"Pen": data['Pen']})
        # model.update({codeChecks(data)})
    elif data['Category'] == "Homogeneous Pipe":
        model.update({"Name": data['Name']})
        model.update({"Category": data['Category']})
        model.update({"OD": data['OD']})
        model.update({"ID": data['ID']})
        model.update({"MaterialDensity": data['MaterialDensity']})
        model.update({"E": data['E']})
        model.update({"PoissonRatio": data['PoissonRatio']})
        model.update({"Cdn": data['Cdn']})
        model.update({"Cdz": data['Cdz']})
        model.update({"Cl": data['Cl']})
        model.update({"Can": data['Can']})
        model.update({"Caz": data['Caz']})
        model.update({
            "SeabedNormalFrictionCoefficient":
                data['SeabedNormalFrictionCoefficient']
        })
        model.update({
            "SeabedAxialFrictionCoefficient":
                data['SeabedAxialFrictionCoefficient']
        })
        model.update({"ClashStiffness": 0})
        model.update({"AllowableStress": data['AllowableStress']})
        model.update({
            "RayleighDampingCoefficients": data['RayleighDampingCoefficients']
        })
        model.update({"CoatingThickness": 0})
        model.update({"LiningThickness": 0})
        model.update(codeChecks(data))
        model.update({"Pen": data['Pen']})
    return model


def lines(data, FEAType, LoadingIndex):

    model = {}
    if data['default']['Analysis']['SLWR']:
        model.update({"Name": "SLWR"})
    if data['default']['Analysis']['SCR']:
        model.update({"Name": "SCR"})

    model.update({"IncludeTorsion": "No"})
    model.update({"TopEnd": "End A"})
    model.update({"PyModel": "(none)"})
    model.update({"DragFormulation": "Standard"})
    model.update({"StaticsVIV": "None"})
    model.update({"DynamicsVIV": "None"})

    if data['default']['Analysis']['SLWR']:
        absolute_orientation_angle_of_riser = data['EnvironmentLoad'][FEAType][LoadingIndex]['Vessel']['Orientation'][2] \
                                              + data['LazyWaveCatenaryDefinition']['lay_azimuth_to_vessel']
        distance_from_hangoff_to_TDP = data['lazyWaveCatenaryResult']['Summary']['HangoffToTDP']['X'] \
                 + data['LazyWaveCatenaryDefinition']['TDPToAnchor'] + data['FEASettings']['AnchorAdjustment']['SLWR']
        model.update({"Connection, ConnectionX, ConnectionY, ConnectionZ, ConnectionAzm, ConnectionDec, ConnectionGamma, ReleaseStage":                    \
                [["Vessel1", data['LazyWaveCatenaryDefinition']['Hangoff']['from_midship'], data['LazyWaveCatenaryDefinition']['Hangoff']['from_centerline'], data['LazyWaveCatenaryDefinition']['Hangoff']['above_keel'], data['LazyWaveCatenaryDefinition']['lay_azimuth_to_vessel'],
                  180-data['LazyWaveCatenaryDefinition']['declinationAngle'], data['FEASettings']['EndOrientation']['SLWR']['A']['Gamma'], "~"], \
                ["Anchored", distance_from_hangoff_to_TDP*math.cos(math.radians(absolute_orientation_angle_of_riser)) + data['LazyWaveCatenaryDefinition']['Hangoff']['from_midship']
                    , data['LazyWaveCatenaryDefinition']['Hangoff']['from_centerline'] + distance_from_hangoff_to_TDP*math.sin(math.radians(absolute_orientation_angle_of_riser)), \
                data['MainPipe']['InsulationSection']['OD']*0.0254/2, data['LazyWaveCatenaryDefinition']['lay_azimuth_to_vessel'],
                 data['FEASettings']['EndOrientation']['SLWR']['B']['Declination'], data['FEASettings']['EndOrientation']['SLWR']['B']['Gamma'], "~"]]})

    if data['default']['Analysis']['SCR']:
        absolute_orientation_angle_of_riser = data['EnvironmentLoad'][FEAType][LoadingIndex]['Vessel']['Orientation'][2] \
                                              + data['commonDefinition']['lay_azimuth_to_vessel']
        distance_from_hangoff_to_TDP = data['catenaryResult']['X'] + data['commonDefinition']['TDPToAnchor']['SCR'] \
                 + data['FEASettings']['AnchorAdjustment']['SCR']
        model.update({"Connection, ConnectionX, ConnectionY, ConnectionZ, ConnectionAzm, ConnectionDec, ConnectionGamma, ReleaseStage":                    \
                [["Vessel1", data['commonDefinition']['Hangoff']['from_midship'], data['commonDefinition']['Hangoff']['from_centerline'], data['commonDefinition']['Hangoff']['above_keel'], data['commonDefinition']['lay_azimuth_to_vessel'],
                  180-data['simpleCatenaryDefinition']['declinationAngle'], data['FEASettings']['EndOrientation']['SCR']['A']['Gamma'], "~"], \
                ["Anchored", (distance_from_hangoff_to_TDP)*math.cos(math.radians(absolute_orientation_angle_of_riser)) + data['commonDefinition']['Hangoff']['from_midship']
                    , data['commonDefinition']['Hangoff']['from_centerline'] + distance_from_hangoff_to_TDP*math.sin(math.radians(absolute_orientation_angle_of_riser)), \
                data['MainPipe']['InsulationSection']['OD']*0.0254/2, data['commonDefinition']['lay_azimuth_to_vessel'],
                 data['FEASettings']['EndOrientation']['SCR']['B']['Declination'],
                 data['FEASettings']['EndOrientation']['SCR']['B']['Gamma'], "~"]]})

    model.update({
        "ConnectionStiffnessX, ConnectionStiffnessY": [[
            data['FEASettings']['FlexJointStiffness']['ConnectionLabel'],
            data['FEASettings']['FlexJointStiffness']['ConnectionLabel']
        ], [0, "~"]]
    })

    if data['default']['Analysis']['SLWR']:
        SLWR_L_Before_tdp = data['lazyWaveCatenaryResult']['Summary']['BuoyancyToTouchDown']['S'] - 30 - \
                            data['FEASettings']['Mesh']['BeforeTDP']['L']
        if SLWR_L_Before_tdp > 0:
            model.update({"LineType, Length, TargetSegmentLength":                    \
                        [["FJExtension1", data['commonDefinition']['TaperJoint']['L'], data['FEASettings']['Mesh']['Top']['Size']], \
                        ["MainPipe", 2, data['FEASettings']['Mesh']['Top']['Size']], \
                        ["MainPipe", 2, data['FEASettings']['Mesh']['Top']['Size']*2], \
                        ["MainPipe", data['FEASettings']['Mesh']['Top']['L']-data['commonDefinition']['TaperJoint']['L'] -4, data['FEASettings']['Mesh']['Top']['Size']*2*2], \
                        ["MainPipe", 10, data['FEASettings']['Mesh']['Top']['Size']*4*2],  \
                        ["MainPipe", 10, data['FEASettings']['Mesh']['Top']['Size']*8*2],  \
                        ["MainPipe", 10, data['FEASettings']['Mesh']['Top']['Size']*16*2], \
                        ["MainPipe", 10, data['FEASettings']['Mesh']['Top']['Size']*24*2], \
                        ["MainPipe", data['lazyWaveCatenaryResult']['Summary']['HangOffToBuoyancy']['S'] - data['FEASettings']['Mesh']['Top']['L']-80, 20], \
                        ["MainPipe", 10, data['FEASettings']['Mesh']['Buoyancy']['Size']*6], \
                        ["MainPipe", 10, data['FEASettings']['Mesh']['Buoyancy']['Size']*4], \
                        ["MainPipe", 10, data['FEASettings']['Mesh']['Buoyancy']['Size']*2],  \
                        ["MainPipe", 10, data['FEASettings']['Mesh']['Buoyancy']['Size']],  \
                        ["BuoyPipeStartEnd", 2, data['FEASettings']['Mesh']['Buoyancy']['Size']], \
                        ["BuoyPipe", data['lazyWaveCatenaryResult']['Summary']['Buoyancy']['S']-4, data['FEASettings']['Mesh']['Buoyancy']['Size']], \
                        ["BuoyPipeStartEnd", 2, data['FEASettings']['Mesh']['Buoyancy']['Size']], \
                        ["MainPipe", 10, data['FEASettings']['Mesh']['Buoyancy']['Size']], \
                        ["MainPipe", 10, data['FEASettings']['Mesh']['Buoyancy']['Size']*2], \
                        ["MainPipe", data['lazyWaveCatenaryResult']['Summary']['BuoyancyToTouchDown']['S'] -30 -data['FEASettings']['Mesh']['BeforeTDP']['L'], 5], \
                        ["MainPipe", 10, data['FEASettings']['Mesh']['BeforeTDP']['Size']*4], \
                        ["MainPipe", 10, data['FEASettings']['Mesh']['BeforeTDP']['Size']*2], \
                        ["MainPipe", data['FEASettings']['Mesh']['BeforeTDP']['L'] -10, data['FEASettings']['Mesh']['BeforeTDP']['Size']], \
                        ["MainPipe", data['FEASettings']['Mesh']['AfterTDP']['L'] -10, data['FEASettings']['Mesh']['AfterTDP']['Size']], \
                        ["MainPipe", 10, data['FEASettings']['Mesh']['AfterTDP']['Size']*2], \
                        ["MainPipe", 10, data['FEASettings']['Mesh']['AfterTDP']['Size']*4], \
                        ["MainPipe", 10, data['FEASettings']['Mesh']['AfterTDP']['Size']*8], \
                        ["MainPipe", 10, data['FEASettings']['Mesh']['AfterTDP']['Size']*16], \
                        ["MainPipe", data['commonDefinition']['TDPToAnchor'] -30 -data['FEASettings']['Mesh']['AfterTDP']['L'], 20]]})
        else:
            model.update({"LineType, Length, TargetSegmentLength":                    \
                        [["FJExtension1", data['commonDefinition']['TaperJoint']['L'], data['FEASettings']['Mesh']['Top']['Size']], \
                        ["MainPipe", 2, data['FEASettings']['Mesh']['Top']['Size']], \
                        ["MainPipe", 2, data['FEASettings']['Mesh']['Top']['Size']*2], \
                        ["MainPipe", data['FEASettings']['Mesh']['Top']['L']-data['commonDefinition']['TaperJoint']['L'] -4, data['FEASettings']['Mesh']['Top']['Size']*2*2], \
                        ["MainPipe", 10, data['FEASettings']['Mesh']['Top']['Size']*4*2],  \
                        ["MainPipe", 10, data['FEASettings']['Mesh']['Top']['Size']*8*2],  \
                        ["MainPipe", 10, data['FEASettings']['Mesh']['Top']['Size']*16*2], \
                        ["MainPipe", 10, data['FEASettings']['Mesh']['Top']['Size']*24*2], \
                        ["MainPipe", data['lazyWaveCatenaryResult']['Summary']['HangOffToBuoyancy']['S'] - data['FEASettings']['Mesh']['Top']['L']-80, 20], \
                        ["MainPipe", 10, data['FEASettings']['Mesh']['Buoyancy']['Size']*6], \
                        ["MainPipe", 10, data['FEASettings']['Mesh']['Buoyancy']['Size']*4], \
                        ["MainPipe", 10, data['FEASettings']['Mesh']['Buoyancy']['Size']*2],  \
                        ["MainPipe", 10, data['FEASettings']['Mesh']['Buoyancy']['Size']],  \
                        ["BuoyPipeStartEnd", 2, data['FEASettings']['Mesh']['Buoyancy']['Size']], \
                        ["BuoyPipe", data['lazyWaveCatenaryResult']['Summary']['Buoyancy']['S']-4, data['FEASettings']['Mesh']['Buoyancy']['Size']], \
                        ["BuoyPipeStartEnd", 2, data['FEASettings']['Mesh']['Buoyancy']['Size']], \
                        ["MainPipe", 10, data['FEASettings']['Mesh']['BeforeTDP']['Size']*2], \
                        ["MainPipe", data['lazyWaveCatenaryResult']['Summary']['BuoyancyToTouchDown']['S'] -10, data['FEASettings']['Mesh']['BeforeTDP']['Size']], \
                        ["MainPipe", data['FEASettings']['Mesh']['AfterTDP']['L'] -10, data['FEASettings']['Mesh']['AfterTDP']['Size']], \
                        ["MainPipe", 10, data['FEASettings']['Mesh']['AfterTDP']['Size']*2], \
                        ["MainPipe", 10, data['FEASettings']['Mesh']['AfterTDP']['Size']*4], \
                        ["MainPipe", 10, data['FEASettings']['Mesh']['AfterTDP']['Size']*8], \
                        ["MainPipe", 10, data['FEASettings']['Mesh']['AfterTDP']['Size']*16], \
                        ["MainPipe", data['commonDefinition']['TDPToAnchor'] -30 -data['FEASettings']['Mesh']['AfterTDP']['L'], 20]]})

    if data['default']['Analysis']['SCR']:
        model.update({"LineType, Length, TargetSegmentLength":                    \
                    [["FJExtension1", data['commonDefinition']['TaperJoint']['L'], data['FEASettings']['Mesh']['Top']['Size']], \
                     ["MainPipe", 2, data['FEASettings']['Mesh']['Top']['Size']], \
                     ["MainPipe", 2, data['FEASettings']['Mesh']['Top']['Size'] * 2], \
                     ["MainPipe",
                      data['FEASettings']['Mesh']['Top']['L'] - data['commonDefinition']['TaperJoint'][
                          'L'] - 4, data['FEASettings']['Mesh']['Top']['Size'] * 2], \
                     ["MainPipe", 10, data['FEASettings']['Mesh']['Top']['Size']*4], \
                    ["MainPipe", 10, data['FEASettings']['Mesh']['Top']['Size']*8], \
                    ["MainPipe", 10, data['FEASettings']['Mesh']['Top']['Size']*16], \
                    ["MainPipe", 10, data['FEASettings']['Mesh']['Top']['Size']*24], \
                    ["MainPipe", data['catenaryResult']['S'] - data['FEASettings']['Mesh']['Top']['L']- data['FEASettings']['Mesh']['BeforeTDP']['L'] -40, 5], \
                    ["MainPipe", 10, data['FEASettings']['Mesh']['BeforeTDP']['Size']*2], \
                    ["MainPipe", data['FEASettings']['Mesh']['BeforeTDP']['L'] -10, data['FEASettings']['Mesh']['BeforeTDP']['Size']], \
                    ["MainPipe", data['FEASettings']['Mesh']['AfterTDP']['L'] -10, data['FEASettings']['Mesh']['AfterTDP']['Size']], \
                    ["MainPipe", 10, data['FEASettings']['Mesh']['AfterTDP']['Size']*2], \
                    ["MainPipe", 10, data['FEASettings']['Mesh']['AfterTDP']['Size']*4], \
                    ["MainPipe", 10, data['FEASettings']['Mesh']['AfterTDP']['Size']*8], \
                    ["MainPipe", 10, data['FEASettings']['Mesh']['AfterTDP']['Size']*16], \
                    ["MainPipe", data['commonDefinition']['TDPToAnchor']['SCR'] -30 - data['FEASettings']['Mesh']['AfterTDP']['L'], 20]]})

    model.update({"ContentsMethod": "Uniform"})
    model.update({"IncludeAxialContentsInertia": "Yes"})
    model.update({"ContentsDensity": data['Material']['Fluid']['Rho'] / 1000})
    model.update({
        "ContentsPressureRefZ":
            -data['commonDefinition']['HangoffBelowMeanSeaLevel']
    })
    model.update({
        "ContentsPressure":
            data['FEASettings']['DesignPressure']['Surface'] * 6.894745
    })
    model.update({"ContentsFlowRate": 0})
    model.update({"IncludedInStatics": "Yes"})
    model.update({"StaticsStep1": "Catenary"})
    model.update({"IncludeSeabedFrictionInStatics": "Yes"})
    model.update(
        {"LayAzimuth": data['commonDefinition']['lay_azimuth_to_vessel'] + 180})
    model.update({"AsLaidTension": 0})

    return model


def VariableData(data):
    model = {}
    for i in range(0, len(data)):
        if data[i]["Type"] == "BendingConnectionStiffness":
            model["BendingConnectionStiffness"] = []
            model["BendingConnectionStiffness"].append(
                BendingConnectionStiffness(data[i]))
        elif data[i]["Type"] == "LineTypeOuterDiameter":
            model["LineTypeOuterDiameter"] = []
            model["LineTypeOuterDiameter"].append(LineTypeOuterDiameter(
                data[i]))

    return model


def BendingConnectionStiffness(data):
    model = {}

    model.update({"Name": data['Name']})
    model.update(
        {"IndependentValue, DependentValue": data["FlexJointStiffness"]})

    return model


def LineTypeOuterDiameter(data):
    model = {}

    model.update({"Name": data['Name']})
    model.update({
        "IndependentValue, DependentValue": [[0, data['ThickEndOD']],
                                             [
                                                 data['ProfileLength'],
                                                 data['ThinEndOD']
                                             ]]
    })

    return model


def vessels(data):
    model = {}

    model.update({"Name": "Vessel1"})
    model.update({"VesselType": "Vessel Type1"})
    model.update({"Draught": data['Draught']})
    model.update({"Length": "~"})
    model.update({"InitialPosition": data['InitialPosition']})
    model.update({"Orientation": data['Orientation']})
    model.update({"IncludedInStatics": "None"})
    model.update({"PrimaryMotion": "None"})
    model.update({"SuperimposedMotion": "Displacement RAOs + Harmonic Motion"})
    model.update({"IncludeAppliedLoads": "No"})
    model.update({"IncludeWaveLoad1stOrder": "No"})
    model.update({"IncludeWaveDriftLoad2ndOrder": "No"})
    model.update({"IncludeWaveDriftDamping": "No"})
    model.update({"IncludeSumFrequencyLoad": "No"})
    model.update({"IncludeAddedMassAndDamping": "No"})
    model.update({"IncludeManoeuvringLoad": "No"})
    model.update({"IncludeOtherDamping": "No"})
    model.update({"IncludeCurrentLoad": "No"})
    model.update({"IncludeWindLoad": "No"})

    return model


def winches(data):
    pass


def groups(data):
    pass


def WriteOrcaflexModel(Files, cfg):
    with open(cfg['Analysis']['fe_filename'], 'w') as f:
        for file in Files:
            if isinstance(file, str):
                with open(file, "r") as infile:
                    f.write(infile.read())
            else:
                yaml.dump(file, f)
    print('Successfully write file: "{0}"'.format(
        cfg['Analysis']['fe_filename']))


def build_model(FEAType, cfg):
    if cfg['default']['Analysis']['SLWR']:
        for LoadingIndex in range(0, len(cfg['EnvironmentLoad'][FEAType])):
            cfg['Analysis']['fe_filename'] = cfg['Analysis']['fe_folder'] + cfg[
                'Analysis']['file_name'] + '_SLWR_FE_' + cfg['EnvironmentLoad'][
                    FEAType][LoadingIndex]['Wave']['WaveTrains'][0][
                        'Name'] + '.yml'
            if cfg['default']['Analysis'][FEAType] == True:
                FEAmodel1, FEAmodel2 = orcaflexModel(cfg, FEAType, LoadingIndex)
                if FEAType == 'Extreme':
                    WriteOrcaflexModel([
                        FEAmodel1,
                        'src/digitalmodel/tests/test_data/catenary_riser/VesselTypes_Extreme.yml',
                        FEAmodel2
                    ], cfg)
                elif FEAType == 'Fatigue':
                    WriteOrcaflexModel([
                        FEAmodel1,
                        'src/digitalmodel/tests/test_data/catenary_riser/VesselTypes_Fatigue.yml',
                        FEAmodel2
                    ], cfg)

        saveDataYaml(
            cfg,
            cfg['Analysis']['result_folder'] + cfg['Analysis']['file_name'],
            False)

    if cfg['default']['Analysis']['SCR']:
        for LoadingIndex in range(0, len(cfg['EnvironmentLoad'][FEAType])):
            cfg['Analysis']['fe_filename'] = cfg['Analysis']['fe_folder'] + cfg[
                'Analysis']['file_name'] + '_SCR_FE_' + cfg['EnvironmentLoad'][
                    FEAType][LoadingIndex]['Wave']['WaveTrains'][0][
                        'Name'] + '_FE.yml'
            if cfg['default']['Analysis'][FEAType] == True:
                FEAmodel1, FEAmodel2 = orcaflexModel(cfg, FEAType, LoadingIndex)
                if FEAType == 'Extreme':
                    extreme_data = pkgutil.get_data(
                        'digitalmodel', 'tests/test_data/catenary_riser/' +
                        'VesselTypes_Extreme.yml')

                    WriteOrcaflexModel([FEAmodel1, extreme_data, FEAmodel2],
                                       cfg)
                elif FEAType == 'Fatigue':
                    fatigue_data = pkgutil.get_data(
                        'digitalmodel', 'tests/test_data/catenary_riser/' +
                        'VesselTypes_Fatigue.yml')
                    WriteOrcaflexModel([FEAmodel1, fatigue_data, FEAmodel2],
                                       cfg)

        saveDataYaml(
            cfg,
            cfg['Analysis']['result_folder'] + cfg['Analysis']['file_name'])
