# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Riser geometry and lazily loaded configuration screening exports."""

from importlib import import_module

from .trajectory_clearance import ClearanceResult, minimum_surface_gap

__all__ = [
    "RiserLoadCase",
    "CaseScreenResult",
    "screen_riser_cases",
    "run_riser_screening",
    "ClearanceResult",
    "minimum_surface_gap",
]


def __getattr__(name: str) -> object:
    """Load screening dependencies only when a screening export is requested."""
    if name in __all__:
        module = import_module(f"{__name__}.riser_case_screening")
        value = getattr(module, name)
        globals()[name] = value
        return value
    raise AttributeError(f"module {__name__!r} has no attribute {name!r}")
