# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Subsea connectors and jumpers mechanics (API 17R / ISO 13628-1).

Native Python load/capacity checks for subsea jumper connectors:
end-cap pressure thrust, combined von-Mises body utilisation, seal/preload
margin, thermal-expansion loads, bend-radius stress, and native cyclic fatigue
damage/life from signed elastic bend-radius histories with a supplied SCF.

Deferred: dynamic-jumper model export and a worked topology example remain
tracked in GitHub issue 2007.
"""

from .section_properties import PipeSection
from .connector_design import (
    ConnectorType,
    ConnectorMaterial,
    ConnectorCheckResult,
    end_cap_force,
    verify_connector,
)
from .thermal_expansion import ThermalResult, thermal_expansion_load
from .bending_analysis import BendingResult, bending_from_radius
from .jumper_catalog import JumperType, JumperSpec, JUMPER_CATALOG, get_jumper_spec
from .fatigue import JumperFatigueResult, assess_jumper_fatigue

__all__ = [
    "PipeSection",
    "ConnectorType",
    "ConnectorMaterial",
    "ConnectorCheckResult",
    "end_cap_force",
    "verify_connector",
    "ThermalResult",
    "thermal_expansion_load",
    "BendingResult",
    "bending_from_radius",
    "JumperType",
    "JumperSpec",
    "JUMPER_CATALOG",
    "get_jumper_spec",
    "JumperFatigueResult",
    "assess_jumper_fatigue",
]
