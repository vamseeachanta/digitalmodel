"""

See https://github.com/vamseeachanta/digitalmodel/ for more information.
"""

# Version of package
__version__ = "0.0.9"

# Import modules subpackage for test patching support
from . import modules

# Expose modules at top level for backward compatibility with engine.py imports
try:
    from digitalmodel.modules.aqwa.aqwa_router import Aqwa
    from digitalmodel.modules.mooring_analysis import MooringDesigner
    from digitalmodel.modules.orcaflex.orcaflex import OrcaFlex
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
