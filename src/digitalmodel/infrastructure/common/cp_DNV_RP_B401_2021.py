"""
ABOUTME: Backward-compat shim — cp_DNV_RP_B401_2021 moved to base_solvers/hydrodynamics/.
ABOUTME: Import from digitalmodel.infrastructure.base_solvers.hydrodynamics instead.
"""

import warnings

warnings.warn(
    "digitalmodel.infrastructure.common.cp_DNV_RP_B401_2021 is deprecated. "
    "Import from digitalmodel.infrastructure.base_solvers.hydrodynamics.cp_DNV_RP_B401_2021",
    DeprecationWarning,
    stacklevel=2,
)

from digitalmodel.infrastructure.base_solvers.hydrodynamics.cp_DNV_RP_B401_2021 import (  # noqa: F401, E402
    _b401_anode_requirements,
    _b401_anode_resistance,
    _b401_coating_breakdown,
    _b401_current_demand,
    _b401_current_densities,
    _b401_surface_areas,
    _b401_verify_current_output,
)

__all__ = [
    "_b401_anode_requirements",
    "_b401_anode_resistance",
    "_b401_coating_breakdown",
    "_b401_current_demand",
    "_b401_current_densities",
    "_b401_surface_areas",
    "_b401_verify_current_output",
]
