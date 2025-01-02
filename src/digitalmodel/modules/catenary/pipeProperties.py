# -*- coding: utf-8 -*-
"""
Created on September 21 2018
"""
'''
Author: Vamsee Achanta
Date Updated: 2018-09-21
Objective: To generate pipe properties
'''
import math


def pipeProperties(cfg, FluidDensity, Buoyancy = False):
    cfg = steelSectionProperties(cfg)
    cfg = insulationSectionProperties(cfg)
    if Buoyancy == True:
        cfg = buoyancySectionProperties(cfg)
    else:
        cfg['BuoyancySection'] = None
    cfg = equivalentPipe(cfg, FluidDensity, Buoyancy)

    return(cfg)

def steelSectionProperties(cfg):
    OD = cfg["geometry"]["NominalOD"]
    if cfg["geometry"]["NominalID"] !=None:
        ID = cfg["geometry"]["NominalID"]
    elif cfg["geometry"]["DesignWT"] !=None:
        ID = cfg["geometry"]["NominalOD"] - 2*cfg["geometry"]["DesignWT"]

    data = {"OD": OD, "ID": ID}
    cfg['SteelSection'] = sectionProperties(data)

    return cfg

def insulationSectionProperties(cfg):
    OD = cfg["geometry"]["NominalOD"]+2*cfg["geometry"]["ExternalCoating"]['Thickness']
    ID = cfg["geometry"]["NominalOD"]
    data = {"OD": OD, "ID": ID}
    cfg['InsulationSection'] = sectionProperties(data)

    return cfg

def buoyancySectionProperties(cfg):
    OD = cfg['InsulationSection']["OD"]+2*cfg["commonDefinition"]["UniformBuoyancy"]['Thickness']
    ID = cfg['InsulationSection']["OD"]
    data = {"OD": OD, "ID": ID}
    cfg['BuoyancySection'] = sectionProperties(data)
    return cfg

def sectionProperties(data):

    A  = (math.pi/4)*(data['OD']**2-data['ID']**2)
    Ai = (math.pi/4)*(data['ID']**2)
    Ao = (math.pi/4)*(data['OD']**2)
    I = (math.pi/64)*(data['OD']**4-data['ID']**4)

    data.update({"A": A, "Ai": Ai, "Ao": Ao, "I": I})

    return data

def equivalentPipe(cfg, FluidDensity, Buoyancy = False):
    pipe_m_per_l = cfg['SteelSection']['A'] * cfg['Material']['Steel']['Rho'] * (0.0254 ** 2)
    insulation_m_per_l = cfg['InsulationSection']['A'] * cfg['Material']['ExternalCoating']['Rho'] * (0.0254 ** 2)
    internal_fluid_m_per_l = cfg['SteelSection']['Ai']*FluidDensity*(0.0254**2)
    buoyancy_due_to_insulation_m_per_l = cfg['InsulationSection']['Ao']*cfg['Material']['SeaWater']['Rho']*(0.0254**2)
    if (cfg['geometry']['Strakes']['BaseThickness'] == None and Buoyancy == False):
        cfg['equivalentPipe'] = {}
        massPerUnitLength = pipe_m_per_l + insulation_m_per_l + internal_fluid_m_per_l

        weightPerUnitLength = (pipe_m_per_l  + insulation_m_per_l + internal_fluid_m_per_l
                               - buoyancy_due_to_insulation_m_per_l)*cfg['default']['Constants']['g']

        cfg['equivalentPipe'].update({"WithoutBuoyancy":{"pipe_m_per_l": pipe_m_per_l, "insulation_m_per_l": insulation_m_per_l,
                                 "internal_fluid_m_per_l": internal_fluid_m_per_l, "buoyancy_due_to_insulation_m_per_l": buoyancy_due_to_insulation_m_per_l,
                                 "weightPerUnitLength": weightPerUnitLength, 'massPerUnitLength': massPerUnitLength}})

    elif(cfg['geometry']['Strakes']['BaseThickness'] != None and Buoyancy == False):
        cfg['equivalentPipe'] = {}
        massPerUnitLength = pipe_m_per_l + insulation_m_per_l + internal_fluid_m_per_l \
                            + cfg['Material']['Strakes']['MassPerUnitLength']


        weightPerUnitLength = (pipe_m_per_l  + insulation_m_per_l + internal_fluid_m_per_l
                        - buoyancy_due_to_insulation_m_per_l)*cfg['default']['Constants']['g']\
                        + cfg['Material']['Strakes']['WeightPerUnitLength']

        cfg['equivalentPipe'].update({
            "WithoutBuoyancy": {"pipe_m_per_l": pipe_m_per_l, "insulation_m_per_l": insulation_m_per_l,
                                "internal_fluid_m_per_l": internal_fluid_m_per_l,
                                "buoyancy_due_to_insulation_m_per_l": buoyancy_due_to_insulation_m_per_l,
                                "weightPerUnitLength": weightPerUnitLength, 'massPerUnitLength': massPerUnitLength}})

    elif(Buoyancy == True):
        massPerUnitLength = pipe_m_per_l + insulation_m_per_l \
                        + cfg['BuoyancySection']['A']*cfg['Material']['Buoyancy']['Rho']*(0.0254**2) \
                        + internal_fluid_m_per_l

        dry_buoyancy_weight =  cfg['BuoyancySection']['A']*cfg['Material']['Buoyancy']['Rho']*(0.0254**2)
        weightPerUnitLength = (pipe_m_per_l + insulation_m_per_l
                        + dry_buoyancy_weight
                        + internal_fluid_m_per_l
                        - cfg['BuoyancySection']['Ao']*cfg['Material']['SeaWater']['Rho']*(0.0254**2)
                        )*cfg['default']['Constants']['g']                                  \

        cfg['equivalentPipe'].update({"WithBuoyancy":{"pipe_m_per_l": pipe_m_per_l, "insulation_m_per_l": insulation_m_per_l,
                                 "internal_fluid_m_per_l": internal_fluid_m_per_l, "buoyancy_due_to_insulation_m_per_l": buoyancy_due_to_insulation_m_per_l,
                                 "weightPerUnitLength": weightPerUnitLength, 'massPerUnitLength': massPerUnitLength, 'dry_buoyancy_weight':dry_buoyancy_weight}})

    return(cfg)

def buoyancyFactorAndDiameter():
    pass
