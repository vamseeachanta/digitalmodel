"""Subsea umbilical modelling (API 17E / ISO 13628-5).

Deterministic, native-Python core for umbilical engineering:

* ``chemical_flow`` — chemical-injection delivery hydraulics through an
  umbilical tube (Darcy-Weisbach pressure drop, flow regime, delivery
  feasibility / utilisation).

Deferred (follow-on): umbilical cross-section / pressure-rating sizing
(``umbilical_sizing.py``), installation lay-configuration analysis
(``lay_analysis.py``), all-electric vs e-hydraulic control architectures,
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
]
