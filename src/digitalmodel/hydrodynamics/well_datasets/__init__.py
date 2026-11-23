#!/usr/bin/env python3
"""
ABOUTME: well_datasets — The Well dataset loaders for hydrodynamics ML baselines.

Provides streaming access to turbulent flow simulation datasets from The Well
(Polymathic AI) for use as ML benchmark data sources in hydrodynamics modules.

Attribution: The Well dataset — CC BY 4.0, Polymathic AI
"""

from digitalmodel.hydrodynamics.well_datasets.shear_flow_loader import (
    WELL_AVAILABLE,
    THE_WELL_ATTRIBUTION,
    ShearFlowLoader,
)

__all__ = [
    "ShearFlowLoader",
    "WELL_AVAILABLE",
    "THE_WELL_ATTRIBUTION",
]
