# -*- coding: utf-8 -*-
"""
Created on September 21 2018
"""
"""
Author: Vamsee Achanta
Date Updated: 2018-09-21
Objective: To generate pipe properties
"""
import math

from ..models import _range

# Exact international inch, NIST SP 811 (2008), Appendix B.8, length table.
# Shared legacy boundary; geometry in inches, areas in inch squared.
INCH_TO_METRE = 0.0254


def pipeProperties(cfg, FluidDensity, Buoyancy=False):
    cfg = steelSectionProperties(cfg)
    cfg = insulationSectionProperties(cfg)
    if Buoyancy == True:
        cfg = buoyancySectionProperties(cfg)
    else:
        cfg["BuoyancySection"] = None
    cfg = equivalentPipe(cfg, FluidDensity, Buoyancy)

    return cfg


def steelSectionProperties(cfg):
    OD = cfg["geometry"]["NominalOD"]
    if cfg["geometry"]["NominalID"] != None:
        ID = cfg["geometry"]["NominalID"]
    elif cfg["geometry"]["DesignWT"] != None:
        ID = cfg["geometry"]["NominalOD"] - 2 * cfg["geometry"]["DesignWT"]

    data = {"OD": OD, "ID": ID}
    cfg["SteelSection"] = sectionProperties(data)

    return cfg


def insulationSectionProperties(cfg):
    OD = (
        cfg["geometry"]["NominalOD"]
        + 2 * cfg["geometry"]["ExternalCoating"]["Thickness"]
    )
    ID = cfg["geometry"]["NominalOD"]
    data = {"OD": OD, "ID": ID}
    cfg["InsulationSection"] = sectionProperties(data)

    return cfg


def buoyancySectionProperties(cfg):
    OD = (
        cfg["InsulationSection"]["OD"]
        + 2 * cfg["commonDefinition"]["UniformBuoyancy"]["Thickness"]
    )
    ID = cfg["InsulationSection"]["OD"]
    data = {"OD": OD, "ID": ID}
    cfg["BuoyancySection"] = sectionProperties(data)
    return cfg


def sectionProperties(data):
    """Circular areas and second moment in the supplied consistent length unit.

    Geometry definitions A=pi*(OD²-ID²)/4 and I=pi*(OD⁴-ID⁴)/64.
    A zero annulus is allowed for an absent coating, not a negative annulus.
    ``pipeProperties`` supplies inches; no unit inference is performed here.
    """
    _range("OD", data["OD"], positive=True)
    _range("ID", data["ID"])
    if data["ID"] > data["OD"]:
        raise ValueError("ID must not exceed OD")

    A = (math.pi / 4) * (data["OD"] ** 2 - data["ID"] ** 2)
    Ai = (math.pi / 4) * (data["ID"] ** 2)
    Ao = (math.pi / 4) * (data["OD"] ** 2)
    I = (math.pi / 64) * (data["OD"] ** 4 - data["ID"] ** 4)

    data.update({"A": A, "Ai": Ai, "Ao": Ao, "I": I})

    return data


def equivalentPipe(cfg, FluidDensity, Buoyancy=False):
    """Convert inch² areas and kg/m³ densities to kg/m and N/m.

    Basis: NIST SP 811 (2008), Appendix B.8, international inch=0.0254 m
    exactly; OpenStax University Physics Vol. 1 (2016), section 14.4,
    Archimedes balance (engineering-basis.md E4). Gravity is required in
    cfg['default']['Constants']['g'] [m/s²]. Strake mass and submerged weight,
    when used, are caller inputs in kg/m and N/m respectively.
    """
    _range("g", cfg["default"]["Constants"]["g"], positive=True)
    _range("FluidDensity", FluidDensity)
    for section, fields in (("SteelSection", ("A", "Ai")),
                            ("InsulationSection", ("A", "Ao"))):
        for name in fields:
            _range(f"{section}.{name}", cfg[section][name])
    for material in ("Steel", "ExternalCoating", "SeaWater"):
        _range(f"{material}.Rho", cfg["Material"][material]["Rho"])
    if Buoyancy:
        for name in ("A", "Ao"):
            _range(f"BuoyancySection.{name}", cfg["BuoyancySection"][name])
        _range("Buoyancy.Rho", cfg["Material"]["Buoyancy"]["Rho"])
    pipe_m_per_l = (
        cfg["SteelSection"]["A"] * cfg["Material"]["Steel"]["Rho"] * (INCH_TO_METRE**2)
    )
    insulation_m_per_l = (
        cfg["InsulationSection"]["A"]
        * cfg["Material"]["ExternalCoating"]["Rho"]
        * (INCH_TO_METRE**2)
    )
    internal_fluid_m_per_l = cfg["SteelSection"]["Ai"] * FluidDensity * (INCH_TO_METRE**2)
    buoyancy_due_to_insulation_m_per_l = (
        cfg["InsulationSection"]["Ao"]
        * cfg["Material"]["SeaWater"]["Rho"]
        * (INCH_TO_METRE**2)
    )
    if cfg["geometry"]["Strakes"]["BaseThickness"] == None and Buoyancy == False:
        cfg["equivalentPipe"] = {}
        massPerUnitLength = pipe_m_per_l + insulation_m_per_l + internal_fluid_m_per_l

        weightPerUnitLength = (
            pipe_m_per_l
            + insulation_m_per_l
            + internal_fluid_m_per_l
            - buoyancy_due_to_insulation_m_per_l
        ) * cfg["default"]["Constants"]["g"]

        cfg["equivalentPipe"].update(
            {
                "WithoutBuoyancy": {
                    "pipe_m_per_l": pipe_m_per_l,
                    "insulation_m_per_l": insulation_m_per_l,
                    "internal_fluid_m_per_l": internal_fluid_m_per_l,
                    "buoyancy_due_to_insulation_m_per_l": buoyancy_due_to_insulation_m_per_l,
                    "weightPerUnitLength": weightPerUnitLength,
                    "massPerUnitLength": massPerUnitLength,
                }
            }
        )

    elif cfg["geometry"]["Strakes"]["BaseThickness"] != None and Buoyancy == False:
        cfg["equivalentPipe"] = {}
        massPerUnitLength = (
            pipe_m_per_l
            + insulation_m_per_l
            + internal_fluid_m_per_l
            + cfg["Material"]["Strakes"]["MassPerUnitLength"]
        )

        weightPerUnitLength = (
            pipe_m_per_l
            + insulation_m_per_l
            + internal_fluid_m_per_l
            - buoyancy_due_to_insulation_m_per_l
        ) * cfg["default"]["Constants"]["g"] + cfg["Material"]["Strakes"][
            "WeightPerUnitLength"
        ]

        cfg["equivalentPipe"].update(
            {
                "WithoutBuoyancy": {
                    "pipe_m_per_l": pipe_m_per_l,
                    "insulation_m_per_l": insulation_m_per_l,
                    "internal_fluid_m_per_l": internal_fluid_m_per_l,
                    "buoyancy_due_to_insulation_m_per_l": buoyancy_due_to_insulation_m_per_l,
                    "weightPerUnitLength": weightPerUnitLength,
                    "massPerUnitLength": massPerUnitLength,
                }
            }
        )

    elif Buoyancy == True:
        cfg.setdefault("equivalentPipe", {})
        massPerUnitLength = (
            pipe_m_per_l
            + insulation_m_per_l
            + cfg["BuoyancySection"]["A"]
            * cfg["Material"]["Buoyancy"]["Rho"]
            * (INCH_TO_METRE**2)
            + internal_fluid_m_per_l
        )

        dry_buoyancy_weight = (
            cfg["BuoyancySection"]["A"]
            * cfg["Material"]["Buoyancy"]["Rho"]
            * (INCH_TO_METRE**2)
        )
        weightPerUnitLength = (
            pipe_m_per_l
            + insulation_m_per_l
            + dry_buoyancy_weight
            + internal_fluid_m_per_l
            - cfg["BuoyancySection"]["Ao"]
            * cfg["Material"]["SeaWater"]["Rho"]
            * (INCH_TO_METRE**2)
        ) * cfg["default"]["Constants"]["g"]
        cfg["equivalentPipe"].update(
            {
                "WithBuoyancy": {
                    "pipe_m_per_l": pipe_m_per_l,
                    "insulation_m_per_l": insulation_m_per_l,
                    "internal_fluid_m_per_l": internal_fluid_m_per_l,
                    "buoyancy_due_to_insulation_m_per_l": buoyancy_due_to_insulation_m_per_l,
                    "weightPerUnitLength": weightPerUnitLength,
                    "massPerUnitLength": massPerUnitLength,
                    "dry_buoyancy_weight": dry_buoyancy_weight,
                }
            }
        )

    return cfg


def buoyancyFactorAndDiameter():
    pass
