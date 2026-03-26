#!/usr/bin/env python3
"""
ABOUTME: Hydrodynamics module for managing hydrodynamic coefficients, wave spectra,
and environmental loading calculations for offshore vessel response analysis.
"""

__version__ = "1.0.0"

from .models import (
    HydrodynamicMatrix,
    VesselProperties,
    WaveSpectrumType,
    WaveParameters,
    EnvironmentalConditions,
)
from .coefficient_database import CoefficientDatabase
from .wave_spectra import WaveSpectra
from .ocimf_loading import OCIMFLoading
from .interpolator import CoefficientsInterpolator

# Capytaine BEM solver (optional — requires capytaine-env)
try:
    from .capytaine import CapytaineSolver, run_bem_analysis, compute_rao
    _HAS_CAPYTAINE = True
except ImportError:
    _HAS_CAPYTAINE = False

__all__ = [
    "HydrodynamicMatrix",
    "VesselProperties",
    "WaveSpectrumType",
    "WaveParameters",
    "EnvironmentalConditions",
    "CoefficientDatabase",
    "WaveSpectra",
    "OCIMFLoading",
    "CoefficientsInterpolator",
]

if _HAS_CAPYTAINE:
    __all__ += ["CapytaineSolver", "run_bem_analysis", "compute_rao"]
