"""OrcFxAPI-dependent solver subpackage.

This subpackage requires OrcaFlex/OrcaWave to be installed.
Import will fail cleanly if the OrcFxAPI DLL is not available.
All license-free code lives in the parent diffraction/ package.
"""
import OrcFxAPI  # Fails immediately if DLL not available

from .orcawave_converter import OrcaWaveConverter, convert_orcawave_results
from .orcawave_data_extraction import (
    OrcaWaveDataExtractor,
    extract_all_rao_data,
    extract_all_added_mass,
    extract_all_damping,
)
from .report_extractors import (
    extract_report_data_from_owr,
    build_report_data_from_solver_results,
)

__all__ = [
    "OrcFxAPI",
    "OrcaWaveConverter",
    "convert_orcawave_results",
    "OrcaWaveDataExtractor",
    "extract_all_rao_data",
    "extract_all_added_mass",
    "extract_all_damping",
    "extract_report_data_from_owr",
    "build_report_data_from_solver_results",
]
