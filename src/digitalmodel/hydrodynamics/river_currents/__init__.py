# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""River / shallow-water current profile modeling.

This sub-package provides vertical current-velocity-vs-depth profile models
for rivers and shallow waterways, used in subsea/riser and barge-towing
current-loading assessments. River boundary-layer profiles differ from the
open-ocean DNV-RP-C205 power law because the velocity is bed-referenced and
vanishes (or is small) at the river bed.

See ``profiles`` for the velocity-profile models.

``bank_effects`` applies assumed lateral and bend shapes and continuity
confinement to those profiles. ``extremes`` fits Weibull and annual-maxima
Gumbel series. ``vessel_forces`` integrates steady current loading over draft
and compares tug pull and yaw capacities. ``examples`` builds a synthetic
river current analysis with inline profile visualization from one cited basis.

Deferred (follow-on): tidal reversal and unsteady flow are explicitly out of
scope (#2000); calibrated hull coefficients, tug resistance and thrust at
speed, and passing-vessel interaction during towing remain unbuilt (#2002).
"""

from .profiles import (
    power_law_profile,
    log_law_profile,
    power_law_depth_averaged,
    log_law_depth_averaged,
    river_velocity_profile,
)
from .bank_effects import (
    lateral_factor, blockage_factor, bend_factor, bend_superelevation, corrected_profile,
)
from .extremes import fit_weibull, fit_gumbel, seasonal_summary
from .vessel_forces import current_load, shallow_drag_factor, towing_assessment

__all__ = [
    "power_law_profile",
    "log_law_profile",
    "power_law_depth_averaged",
    "log_law_depth_averaged",
    "river_velocity_profile",
    "lateral_factor",
    "blockage_factor",
    "bend_factor",
    "bend_superelevation",
    "corrected_profile",
    "fit_weibull",
    "fit_gumbel",
    "seasonal_summary",
    "current_load",
    "shallow_drag_factor",
    "towing_assessment",
]
