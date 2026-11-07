# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Public screening estimates with explicit applicability and supplied motions.

See docs/domains/hydrodynamics/added-resistance-analysis.md for equations,
source access limits and the reproducible synthetic example. No solver runs.
"""

from .energy import (
    SectionalEnergy,
    radiated_energy_table,
    relative_vertical_velocity,
    sectional_energy,
)
from .models import OperatingPoint, RegularWaves, ResistanceRow, Ship, Status
from .propulsion import CalmWaterCurve, SpeedLossResult, added_shaft_power, speed_loss
from .reflection import (
    ReflectionEvidence,
    ReflectionResult,
    stawave1,
    stawave1_equivalent_regular_table,
)
from .spectra import (
    SpectrumBand,
    integrate_regular_table,
    mean_added_resistance,
    pm_spectrum,
)

__all__ = [
    "CalmWaterCurve",
    "OperatingPoint",
    "ReflectionEvidence",
    "ReflectionResult",
    "RegularWaves",
    "ResistanceRow",
    "SectionalEnergy",
    "Ship",
    "SpectrumBand",
    "SpeedLossResult",
    "Status",
    "added_shaft_power",
    "integrate_regular_table",
    "mean_added_resistance",
    "pm_spectrum",
    "radiated_energy_table",
    "relative_vertical_velocity",
    "sectional_energy",
    "speed_loss",
    "stawave1",
    "stawave1_equivalent_regular_table",
]
