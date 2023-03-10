# -*- coding: utf-8 -*-
"""
Created on September 20 2018
"""
'''
Author: Vamsee Achanta
Date Updated: 2018-09-20
Objective: To generate catenary riser shape and evaluate static configuration
Run instructions with:
 Default yml file : python APISTD2RD.py
 Update default yml with parameters in 12.yml: python APISTD2RD.py 12.yml
 Outputs: JSON file and ASCII DataFrame with outputs

 UPDATES:
 Input YML: Relocate Spacing to PlotSettings
 Rename Summary "Hangoff" to "HangOffToSag"
'''

from digitalmodel.custom.catenary.catenaryMethods import (buoyancyProperties,
                                                catenaryEquation,
                                                catenaryForces,
                                                lazyWaveCatenaryEquation,
                                                lazyWavePlot,
                                                simple_catenary_plot)
from digitalmodel.custom.catenary.orcaflexModel import build_model
from digitalmodel.custom.catenary.pipeProperties import pipeProperties



def catenary_riser(cfg):

    # Evaluate section properties and mass per unit length and related properties.
    inputData = {
        "d": cfg["simpleCatenaryDefinition"]["verticalDistance"],
        "F": cfg["simpleCatenaryDefinition"]["axialLineForce"],
        "q": cfg["simpleCatenaryDefinition"]["declinationAngle"]
    }

    catenaryResult = catenaryEquation(inputData)
    cfg['catenaryResult'] = catenaryResult
    cfg = simple_catenary_plot(cfg, cfg['simpleCatenaryDefinition']['Spacing'])

    cfg = pipeProperties(cfg, FluidDensity=cfg['Material']['Fluid']['Rho'], Buoyancy=False)
    WeightPerUnitLengthWithOutBuoyancy = cfg['equivalentPipe']['WithoutBuoyancy']['weightPerUnitLength']
    inputData = {
        'weightPerUnitLength': cfg['equivalentPipe']['WithoutBuoyancy']['weightPerUnitLength'],
        'S': cfg['catenaryResult']['S'],
        'q': cfg['catenaryResult']['q']
    }
    catenary_force = catenaryForces(inputData)
    cfg['catenaryResult']['FluidFilled'] = catenary_force

    cfg = pipeProperties(cfg, FluidDensity=cfg['Material']['Reference_Fluid']['Rho'], Buoyancy=False)
    WeightPerUnitLengthWithOutBuoyancy = cfg['equivalentPipe']['WithoutBuoyancy']['weightPerUnitLength']
    inputData = {
        'weightPerUnitLength': cfg['equivalentPipe']['WithoutBuoyancy']['weightPerUnitLength'],
        'S': cfg['catenaryResult']['S'],
        'q': cfg['catenaryResult']['q']
    }
    catenary_force = catenaryForces(inputData)
    cfg['catenaryResult']['Reference_FluidFilled'] = catenary_force

    cfg = pipeProperties(cfg, FluidDensity=cfg['Material']['SeaWater']['Rho'], Buoyancy=False)

    inputData = {
        'weightPerUnitLength': cfg['equivalentPipe']['WithoutBuoyancy']['weightPerUnitLength'],
        'S': cfg['catenaryResult']['S'],
        'q': cfg['catenaryResult']['q']
    }
    catenary_force = catenaryForces(inputData)
    cfg['catenaryResult']['SeaWaterFilled'] = catenary_force

    cfg = pipeProperties(cfg, FluidDensity=0, Buoyancy=False)
    inputData = {
        'weightPerUnitLength': cfg['equivalentPipe']['WithoutBuoyancy']['weightPerUnitLength'],
        'S': cfg['catenaryResult']['S'],
        'q': cfg['catenaryResult']['q']
    }
    catenary_force = catenaryForces(inputData)
    cfg['catenaryResult']['Empty'] = catenary_force

    # Lazy Wave Analysis
    cfg = buoyancyProperties(cfg, WeightPerUnitLengthWithOutBuoyancy)

    lazyWaveInputs = {
        "WeightPerUnitLengthWithBuoyancy": cfg['lazyWaveCatenaryResult']['WeightPerUnitLengthWithBuoyancy'],
        "WeightPerUnitLengthWithOutBuoyancy": cfg['lazyWaveCatenaryResult']['WeightPerUnitLengthWithOutBuoyancy'],
        "SagBendElevationAboveSeabed": cfg['LazyWaveCatenaryDefinition']['SagBendElevationAboveSeabed'],
        "HogBendAboveSeabed": cfg['LazyWaveCatenaryDefinition']['HogBendAboveSeabed'],
        "HangOff": {
            "d":
                cfg['LazyWaveCatenaryDefinition']['VerticalDistance'] -
                cfg['LazyWaveCatenaryDefinition']['SagBendElevationAboveSeabed'],
            "q":
                cfg["LazyWaveCatenaryDefinition"]["declinationAngle"],
            "F":
                None
        },
    }

    #  For Benchmarking/Verification
    # lazyWaveInputs = {"WeightPerUnitLengthWithBuoyancy" :  -1251.946261,
    #                 "WeightPerUnitLengthWithOutBuoyancy" :  981.97,
    #                 "SagBendElevationAboveSeabed" : cfg['LazyWaveCatenaryDefinition']['SagBendElevationAboveSeabed'],
    #                 "HogBendAboveSeabed" : cfg['LazyWaveCatenaryDefinition']['HogBendAboveSeabed'],
    #                "HangOff" :    {"d" : cfg['LazyWaveCatenaryDefinition']['VerticalDistance'] - cfg['LazyWaveCatenaryDefinition']['SagBendElevationAboveSeabed'],
    #             "q" : cfg["LazyWaveCatenaryDefinition"]["declinationAngle"], "F": None     },
    #                   "WeightPerUnitLength" :  cfg['equivalentPipe']['weightPerUnitLength']
    #              }

    cfg['lazyWaveCatenaryResult'].update(lazyWaveCatenaryEquation(lazyWaveInputs))
    cfg['lazyWaveCatenaryResult']['TotalBuoyancy'] = (
        cfg['lazyWaveCatenaryResult']['WeightPerUnitLengthWithBuoyancy'] -
        cfg['lazyWaveCatenaryResult']['WeightPerUnitLengthWithOutBuoyancy']
    ) * cfg['lazyWaveCatenaryResult']['Summary']['Buoyancy']['S']

    cfg = lazyWavePlot(cfg, cfg['LazyWaveCatenaryDefinition']['Spacing'])

    # Construct FEA Model
    cfg = pipeProperties(cfg, FluidDensity=0, Buoyancy=False)
    cfg['MainPipe'] = {
        'SteelSection': cfg['SteelSection'],
        'BuoyancySection': cfg['BuoyancySection'],
        'InsulationSection': cfg['InsulationSection'],
        'equivalentPipe': cfg['equivalentPipe']
    }
    cfg = pipeProperties(cfg, FluidDensity=0, Buoyancy=True)
    cfg['BuoyPipe'] = {
        'SteelSection': cfg['SteelSection'],
        'BuoyancySection': cfg['BuoyancySection'],
        'InsulationSection': cfg['InsulationSection'],
        'equivalentPipe': cfg['equivalentPipe']
    }

    if cfg['default']['Analysis']['Extreme']:
        FEAType = 'Extreme'
        build_model(FEAType, cfg)
    if cfg['default']['Analysis']['Fatigue']:
        FEAType = 'Fatigue'
        build_model(FEAType, cfg)

    return cfg

