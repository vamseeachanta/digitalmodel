"""
See https://github.com/vamseeachanta/digitalmodel/ for more information.
"""

# Version of package
__version__ = "0.0.9"

# Import modules subpackage for test patching support
from . import modules

# Install Layer 2 group redirect finder (flat -> grouped paths)
from ._compat import _FLAT_TO_GROUP, install_group_redirect

install_group_redirect()

# Expose modules at top level for backward compatibility with engine.py imports
try:
    from digitalmodel.aqwa.aqwa_router import Aqwa
    from digitalmodel.mooring_analysis import MooringDesigner
    from digitalmodel.orcaflex.orcaflex import OrcaFlex
    from digitalmodel.ct_hydraulics.ct_hydraulics import CTHydraulics
    from digitalmodel.pipe_capacity.pipe_capacity import PipeCapacity
    from digitalmodel.pipeline.pipeline import Pipeline
    from digitalmodel.rao_analysis.rao_analysis import RAOAnalysis
    from digitalmodel.time_series.time_series_analysis import TimeSeriesAnalysis
    from digitalmodel.transformation.transformation import Transformation
    from digitalmodel.vertical_riser.vertical_riser import vertical_riser
    from digitalmodel.viv_analysis.viv_analysis import VIVAnalysis
except ImportError as e:
    import warnings
    warnings.warn(f"Could not import some digitalmodel modules: {e}")

try:
    from digitalmodel.digitalmarketing.digitalmarketing import DigitalMarketing
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
            return importlib.import_module(f"digitalmodel.{group}.{name}")
        except ImportError:
            # Module not yet moved to group — fall through to normal resolution
            pass
    raise AttributeError(f"module 'digitalmodel' has no attribute {name!r}")
