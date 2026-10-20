"""Solver exports with an explicit boundary around licensed interfaces.

Importing this package or its report helpers does not request OrcFxAPI.
Accessing a licensed export is an explicit opt-in; unavailable optional exports
retain the historical None fallback. Direct converter submodule imports still
require the interface and retain their normal import errors.
"""

from importlib import import_module

from .report_extractors import (
    extract_report_data_from_owr,
    build_report_data_from_solver_results,
)

_LICENSED_EXPORTS = {
    "OrcaWaveConverter": ".orcawave_converter",
    "convert_orcawave_results": ".orcawave_converter",
    "OrcaWaveDataExtractor": ".orcawave_data_extraction",
    "extract_all_rao_data": ".orcawave_data_extraction",
    "extract_all_added_mass": ".orcawave_data_extraction",
    "extract_all_damping": ".orcawave_data_extraction",
}


def __getattr__(name):
    """Resolve licensed exports only when requested (PEP 562, 2017)."""
    if name == "OrcFxAPI":
        try:
            return import_module(name)
        except ImportError:
            return None
    if name in _LICENSED_EXPORTS:
        try:
            module = import_module(_LICENSED_EXPORTS[name], __name__)
        except ImportError:
            return None
        return getattr(module, name)
    raise AttributeError(f"module {__name__!r} has no attribute {name!r}")


def __dir__():
    return sorted(set(globals()) | set(__all__))


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
