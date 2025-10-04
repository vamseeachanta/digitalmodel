"""
Wave Spectra Module

Provides comprehensive wave spectrum modeling and analysis capabilities
for marine engineering applications. Implements industry-standard spectral
models (JONSWAP, Pierson-Moskowitz) with spectral moment calculations.

Classes:
    WaveSpectrumParameters: Parameters defining wave spectrum
    WaveSpectrum: Abstract base class for wave spectrum models
    JONSWAPSpectrum: JONSWAP wave spectrum implementation
    PiersonMoskowitzSpectrum: Pierson-Moskowitz spectrum implementation
"""

from .spectra import (
    WaveSpectrumParameters,
    WaveSpectrum,
    JONSWAPSpectrum,
    PiersonMoskowitzSpectrum,
)

# Aliases for backward compatibility
JONSWAP = JONSWAPSpectrum
PM = PiersonMoskowitzSpectrum

__all__ = [
    'WaveSpectrumParameters',
    'WaveSpectrum',
    'JONSWAPSpectrum',
    'PiersonMoskowitzSpectrum',
    'JONSWAP',  # Alias
    'PM',  # Alias
]

__version__ = '0.1.0'
