"""OrcFxAPI-dependent solver subpackage.

This subpackage hosts code that ultimately calls into OrcaFlex/OrcaWave.
Importing the subpackage itself MUST NOT fail when OrcFxAPI is absent,
because pure-Python helpers (e.g. ``build_report_data_from_solver_results``
in its minimal, no-OWR path) live alongside the licensed code and are
exercised by license-free callers (BenchmarkRunner, the diffraction CLI).

Strategy: each OrcFxAPI-dependent submodule is imported defensively. When
OrcFxAPI is unavailable on the host, the failing submodule's public names
are set to ``None`` and any direct invocation surfaces a clear
``ModuleNotFoundError`` from the call site (deferring the failure from
import time to call time). The OrcFxAPI-free helpers in
``report_extractors`` (most importantly ``build_report_data_from_solver_results``)
remain importable and callable on Linux without OrcFxAPI.

See workspace-hub #2614 (Cat A) for the regression this guards against:
hard ``import OrcFxAPI`` here previously broke 8 BenchmarkRunner tests
on any non-licensed-Windows machine via report_generator's optional-import
fallback collapsing the helper to ``None``.
"""
try:  # pragma: no cover - exercised only when OrcFxAPI is installed
    import OrcFxAPI  # type: ignore[import-not-found]
except ImportError:
    OrcFxAPI = None  # type: ignore[assignment]

# OrcFxAPI-dependent converters: import defensively so the subpackage
# still loads on hosts without the DLL. Submodules call ``import OrcFxAPI``
# at module level, so a missing DLL raises here.
try:
    from .orcawave_converter import OrcaWaveConverter, convert_orcawave_results
except ImportError:
    OrcaWaveConverter = None  # type: ignore[assignment]
    convert_orcawave_results = None  # type: ignore[assignment]

try:
    from .orcawave_data_extraction import (
        OrcaWaveDataExtractor,
        extract_all_rao_data,
        extract_all_added_mass,
        extract_all_damping,
    )
except ImportError:
    OrcaWaveDataExtractor = None  # type: ignore[assignment]
    extract_all_rao_data = None  # type: ignore[assignment]
    extract_all_added_mass = None  # type: ignore[assignment]
    extract_all_damping = None  # type: ignore[assignment]

# report_extractors is OrcFxAPI-aware but its module-level import is now
# also lazy (see report_extractors.py). The minimal path of
# build_report_data_from_solver_results does NOT touch OrcFxAPI at all,
# so this re-export must succeed on Linux.
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
