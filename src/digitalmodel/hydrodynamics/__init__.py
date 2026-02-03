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
