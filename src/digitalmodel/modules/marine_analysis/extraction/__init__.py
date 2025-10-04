"""Data Extraction Module for Marine Engineering.

This module provides tools for extracting hydrodynamic coefficients,
OCIMF data, and mooring component specifications from various sources.

Modules:
    extract_ocimf: OCIMF database extraction utilities
    extract_hydro: Hydrodynamic coefficient extraction from AQWA/OrcaFlex
    extract_mooring: Mooring component database extraction
    run_extraction: Batch extraction convenience wrapper

Example:
    >>> from digitalmodel.modules.marine_analysis.extraction import extract_ocimf
    >>> data = extract_ocimf.extract_vessel_data('tanker_vlcc')
    >>> print(data.wind_coefficients)
"""

from .extract_ocimf import *
from .extract_hydro import *
from .extract_mooring import *
from .run_extraction import *

__all__ = [
    'extract_ocimf',
    'extract_hydro',
    'extract_mooring',
    'run_extraction'
]
