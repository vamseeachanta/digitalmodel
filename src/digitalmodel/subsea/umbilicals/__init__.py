# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Subsea umbilical modelling (API 17E / ISO 13628-5).

Deterministic, native-Python core for umbilical engineering:

* ``chemical_flow`` — chemical-injection delivery hydraulics through an
  umbilical tube (Darcy-Weisbach pressure drop, flow regime, delivery
  feasibility / utilisation).
* ``umbilical_sizing`` — functional-element, filler, armour and sheath build-up,
  signed weights, and supplied differential-pressure limit checks.
* ``lay_analysis`` — static parallel, bundled and suspended catenary screening
  with supplied bend-radius and touchdown residual-tension limits.

Deferred (follow-on): all-electric vs e-hydraulic control architectures,
and the ``control_systems`` FMEA / signal-loss package (API 17F).
"""

from .chemical_flow import (
    GRAVITY,
    LAMINAR_RE,
    TURBULENT_RE,
    ChemicalFluid,
    DeliveryResult,
    UmbilicalTube,
    check_delivery,
    friction_factor,
    friction_pressure_drop,
    mean_velocity,
    reynolds_number,
    static_pressure,
)
from .lay_analysis import LayCase, LayResult, analyze_lay
from .umbilical_sizing import (
    CheckResult,
    ElementPressureResult,
    FunctionalElement,
    PressureResult,
    QualifiedLimit,
    RadialBuildLayer,
    SizingResult,
    UmbilicalSection,
    check_pressure,
    size_umbilical,
)

__all__ = [
    "GRAVITY",
    "LAMINAR_RE",
    "TURBULENT_RE",
    "ChemicalFluid",
    "DeliveryResult",
    "UmbilicalTube",
    "check_delivery",
    "friction_factor",
    "friction_pressure_drop",
    "mean_velocity",
    "reynolds_number",
    "static_pressure",
    "CheckResult",
    "ElementPressureResult",
    "FunctionalElement",
    "LayCase",
    "LayResult",
    "PressureResult",
    "QualifiedLimit",
    "RadialBuildLayer",
    "SizingResult",
    "UmbilicalSection",
    "analyze_lay",
    "check_pressure",
    "size_umbilical",
]
