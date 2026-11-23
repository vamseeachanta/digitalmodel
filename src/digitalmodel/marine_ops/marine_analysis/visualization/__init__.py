"""Marine engineering visualization tools, loaded only when requested.

Modules:
    integration_charts: System integration and architecture visualizations.
    ocimf_charts: OCIMF wind/current coefficient visualizations.

Example:
    >>> from digitalmodel.marine_ops.marine_analysis.visualization import ocimf_charts
    >>> ocimf_charts.plot_polar_diagrams(data, filename="polar.png", output_dir="charts")
"""

from importlib import import_module

__all__ = ["integration_charts", "ocimf_charts"]


def __getattr__(name):
    """Load a chart submodule on explicit attribute access."""
    if name in __all__:
        module = import_module(f"{__name__}.{name}")
        globals()[name] = module
        return module
    raise AttributeError(f"module {__name__!r} has no attribute {name!r}")
