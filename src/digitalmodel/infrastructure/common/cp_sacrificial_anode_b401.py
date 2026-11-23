"""
ABOUTME: Backward-compat shim — cp_sacrificial_anode_b401 moved to base_solvers/hydrodynamics/.
ABOUTME: Import from digitalmodel.infrastructure.base_solvers.hydrodynamics instead.
"""

import warnings

warnings.warn(
    "digitalmodel.infrastructure.common.cp_sacrificial_anode_b401 is deprecated. "
    "Import from digitalmodel.infrastructure.base_solvers.hydrodynamics.cp_sacrificial_anode_b401",
    DeprecationWarning,
    stacklevel=2,
)

from digitalmodel.infrastructure.base_solvers.hydrodynamics.cp_sacrificial_anode_b401 import (  # noqa: F401, E402
    net_anode_mass,
    gross_anode_mass,
    anode_count,
    anode_resistance_flush,
    anode_resistance_bracelet,
    driving_voltage,
    anode_current_output,
)

__all__ = [
    "net_anode_mass",
    "gross_anode_mass",
    "anode_count",
    "anode_resistance_flush",
    "anode_resistance_bracelet",
    "driving_voltage",
    "anode_current_output",
]
