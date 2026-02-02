"""
Marine Engineering Module.

ABOUTME: Comprehensive marine engineering analysis tools including catenary solvers,
environmental loading calculations, hydrodynamic coefficients, mooring analysis,
and wave spectra modeling.

Submodules:
    catenary: Unified catenary analysis with BVP solver and lazy-wave support
    environmental_loading: OCIMF wind and current coefficient calculations
    hydrodynamic_coefficients: Added mass, damping matrices, and AQWA parsing
    mooring_analysis: Quasi-static mooring system analysis
    wave_spectra: JONSWAP and Pierson-Moskowitz spectrum models
"""

# Catenary analysis
from .catenary import (
    CatenarySolver,
    CatenaryInput,
    CatenaryResults,
    LazyWaveSolver,
    LazyWaveConfiguration,
    LazyWaveResults,
)

# Environmental loading (OCIMF)
from .environmental_loading import (
    OCIMFDatabase,
    EnvironmentalForces,
    EnvironmentalConditions,
    VesselGeometry,
    EnvironmentalForceResults,
)

# Hydrodynamic coefficients
from .hydrodynamic_coefficients import (
    CoefficientDatabase,
    FrequencyDependentMatrix,
    AQWAParser,
    HydrodynamicPlotter,
    DOF_NAMES,
    DOF_INDEX,
)

# Wave spectra
from .wave_spectra import (
    WaveSpectrum,
    JONSWAPSpectrum,
    PiersonMoskowitzSpectrum,
    WaveSpectrumParameters,
)

__version__ = "1.0.0"
__author__ = "DigitalModel Team"

__all__ = [
    # Catenary
    "CatenarySolver",
    "CatenaryInput",
    "CatenaryResults",
    "LazyWaveSolver",
    "LazyWaveConfiguration",
    "LazyWaveResults",
    # Environmental loading
    "OCIMFDatabase",
    "EnvironmentalForces",
    "EnvironmentalConditions",
    "VesselGeometry",
    "EnvironmentalForceResults",
    # Hydrodynamic coefficients
    "CoefficientDatabase",
    "FrequencyDependentMatrix",
    "AQWAParser",
    "HydrodynamicPlotter",
    "DOF_NAMES",
    "DOF_INDEX",
    # Wave spectra
    "WaveSpectrum",
    "JONSWAPSpectrum",
    "PiersonMoskowitzSpectrum",
    "WaveSpectrumParameters",
]
