"""

See https://github.com/vamseeachanta/digitalmodel/ for more information.
"""

# Version of package
__version__ = "0.0.9"

# Expose modules at top level for backward compatibility with engine.py imports
try:
    from digitalmodel.modules.aqwa.aqwa_router import Aqwa
    from digitalmodel.modules.mooring.mooring import Mooring
    from digitalmodel.modules.orcaflex.orcaflex import OrcaFlex
    from digitalmodel.modules.pipe_capacity.pipe_capacity import PipeCapacity
    from digitalmodel.modules.pipeline.pipeline import Pipeline
    from digitalmodel.modules.rao_analysis.rao_analysis import RAOAnalysis
    from digitalmodel.modules.time_series.time_series_analysis import TimeSeriesAnalysis
    from digitalmodel.modules.transformation.transformation import Transformation
    from digitalmodel.modules.vertical_riser.vertical_riser import vertical_riser
    from digitalmodel.modules.viv_analysis.viv_analysis import VIVAnalysis
except ImportError as e:
    import warnings
    warnings.warn(f"Could not import some digitalmodel modules: {e}")
