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
    digitalmodel.structural          - Structural analysis & fatigue
    digitalmodel.subsea              - Subsea systems (mooring, risers, VIV)
    digitalmodel.visualization       - Dashboards & reporting
    digitalmodel.workflows           - Automation & agent orchestration
"""

# Version of package
__version__ = "0.0.9"

__all__ = [
    "data_systems",
    "hydrodynamics",
    "infrastructure",
    "marine_ops",
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

# Expose modules at top level for backward compatibility with engine.py imports
try:
    from digitalmodel.hydrodynamics.aqwa.aqwa_router import Aqwa
    from digitalmodel.subsea.mooring_analysis import MooringDesigner
    from digitalmodel.solvers.orcaflex.orcaflex import OrcaFlex
    from digitalmodel.marine_ops.ct_hydraulics.ct_hydraulics import CTHydraulics
    from digitalmodel.structural.pipe_capacity.pipe_capacity import PipeCapacity
    from digitalmodel.subsea.pipeline.pipeline import Pipeline
    from digitalmodel.hydrodynamics.rao_analysis.rao_analysis import RAOAnalysis
    from digitalmodel.signal_processing.time_series.time_series_analysis import TimeSeriesAnalysis
    from digitalmodel.infrastructure.transformation.transformation import Transformation
    from digitalmodel.subsea.vertical_riser.vertical_riser import vertical_riser
    from digitalmodel.subsea.viv_analysis.viv_analysis import VIVAnalysis
except ImportError as e:
    import warnings
    warnings.warn(f"Could not import some digitalmodel modules: {e}")

try:
    from digitalmodel.specialized.digitalmarketing.digitalmarketing import DigitalMarketing
except ImportError:
    DigitalMarketing = None


def __getattr__(name):
    """Redirect flat module access to grouped paths.

    Catches `from digitalmodel import X` where X is a module that
    has been moved to digitalmodel.<group>.X.
    """
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
