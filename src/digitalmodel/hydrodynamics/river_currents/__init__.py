"""
River / shallow-water current profile modeling.

This sub-package provides vertical current-velocity-vs-depth profile models
for rivers and shallow waterways, used in subsea/riser and barge-towing
current-loading assessments. River boundary-layer profiles differ from the
open-ocean DNV-RP-C205 power law because the velocity is bed-referenced and
vanishes (or is small) at the river bed.

See ``profiles`` for the velocity-profile models.
"""

from .profiles import (
    power_law_profile,
    log_law_profile,
    power_law_depth_averaged,
    log_law_depth_averaged,
    river_velocity_profile,
)

__all__ = [
    "power_law_profile",
    "log_law_profile",
    "power_law_depth_averaged",
    "log_law_depth_averaged",
    "river_velocity_profile",
]
