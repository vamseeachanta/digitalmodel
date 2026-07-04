"""
See https://github.com/vamseeachanta/digitalmodel/ for more information.

Domain groups::

    digitalmodel.data_systems        - Data acquisition & validation
    digitalmodel.hydrodynamics       - Wave & vessel dynamics (AQWA, OrcaWave, BEMRosetta)
    digitalmodel.infrastructure      - Core framework & shared utilities
    digitalmodel.marine_ops          - Marine operations & offshore engineering
    digitalmodel.signal_processing   - Signal analysis & time series
    digitalmodel.solvers             - Solver integrations (OrcaFlex, GMSH, Blender)
    digitalmodel.specialized         - Domain-specific apps (GIS, finance, marketing)
    digitalmodel.power               - Power systems (data center topology, UPS/STS)
    digitalmodel.structural          - Structural analysis & fatigue
    digitalmodel.subsea              - Subsea systems (mooring, risers, VIV)
    digitalmodel.visualization       - Dashboards & reporting
    digitalmodel.workflows           - Automation & agent orchestration
"""

import warnings

# Version of package
__version__ = "0.0.9"

__all__ = [
    "data_systems",
    "hydrodynamics",
    "infrastructure",
    "marine_ops",
    "power",
    "signal_processing",
    "solvers",
    "specialized",
    "structural",
    "subsea",
    "visualization",
    "workflows",
]

# Install Layer 2 group redirect finder (flat -> grouped paths)
from ._compat import _FLAT_TO_GROUP, install_group_redirect, warn_flat_import

install_group_redirect()

# Backward-compatibility class re-exports, resolved LAZILY (PEP 562 __getattr__).
#
# These were previously imported eagerly here, which dragged the entire heavy
# hydrodynamics / subsea / solver stack (xarray, scipy, scrapy, sympy, ...) into
# EVERY `import digitalmodel` — Python runs this __init__ in full before any
# submodule import, so a bare import took ~580s and every single-file pytest paid
# it (issue #1142). Mapping name -> (module, attribute) and importing on first
# access keeps `import digitalmodel` sub-second while `from digitalmodel import
# OrcaFlex` (and `digitalmodel.OrcaFlex`) still work, resolved on demand.
_LAZY_COMPAT_ATTRS = {
    "Aqwa": ("digitalmodel.hydrodynamics.aqwa.aqwa_router", "Aqwa"),
    "MooringDesigner": ("digitalmodel.subsea.mooring_analysis", "MooringDesigner"),
    "OrcaFlex": ("digitalmodel.solvers.orcaflex.orcaflex", "OrcaFlex"),
    "CTHydraulics": (
        "digitalmodel.marine_ops.ct_hydraulics.ct_hydraulics",
        "CTHydraulics",
    ),
    "PipeCapacity": (
        "digitalmodel.structural.pipe_capacity.pipe_capacity",
        "PipeCapacity",
    ),
    "Pipeline": ("digitalmodel.subsea.pipeline.pipeline", "Pipeline"),
    "RAOAnalysis": (
        "digitalmodel.hydrodynamics.rao_analysis.rao_analysis",
        "RAOAnalysis",
    ),
    "TimeSeriesAnalysis": (
        "digitalmodel.signal_processing.time_series.time_series_analysis",
        "TimeSeriesAnalysis",
    ),
    "Transformation": (
        "digitalmodel.infrastructure.transformation.transformation",
        "Transformation",
    ),
    "vertical_riser": (
        "digitalmodel.subsea.vertical_riser.vertical_riser",
        "vertical_riser",
    ),
    "VIVAnalysis": ("digitalmodel.subsea.viv_analysis.viv_analysis", "VIVAnalysis"),
    "DigitalMarketing": (
        "digitalmodel.specialized.digitalmarketing.digitalmarketing",
        "DigitalMarketing",
    ),
}


def __getattr__(name):
    """Lazily resolve backward-compat re-exports and flat module redirects.

    Catches both the compat class re-exports (``_LAZY_COMPAT_ATTRS``, imported on
    first access — issue #1142) and ``from digitalmodel import X`` where X is a
    module that has been moved to ``digitalmodel.<group>.X``.
    """
    if name in _LAZY_COMPAT_ATTRS:
        import importlib

        module_path, attr = _LAZY_COMPAT_ATTRS[name]
        try:
            value = getattr(importlib.import_module(module_path), attr)
        except Exception as e:  # noqa: BLE001
            # Preserve the prior soft-fail contract: an optional/broken module
            # must not make `from digitalmodel import X` raise — it yields None.
            warnings.warn(f"Could not import digitalmodel.{name}: {e}")
            value = None
        # Cache on the module so __getattr__ is not re-invoked for this name.
        globals()[name] = value
        return value

    if name in _FLAT_TO_GROUP:
        import importlib

        group = _FLAT_TO_GROUP[name]
        try:
            mod = importlib.import_module(f"digitalmodel.{group}.{name}")
        except ImportError:
            # Module not yet moved to group — fall through to normal resolution
            pass
        else:
            warn_flat_import(name, group)
            return mod
    raise AttributeError(f"module 'digitalmodel' has no attribute {name!r}")


def __dir__():
    return sorted(set(globals()) | set(__all__) | set(_LAZY_COMPAT_ATTRS))
