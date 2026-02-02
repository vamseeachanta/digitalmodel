"""Backward compatibility shim for digitalmodel.modules.reporting.report_generator.

Use digitalmodel.reporting.report_generator instead.
"""
import warnings

warnings.warn(
    "digitalmodel.modules.reporting.report_generator is deprecated. "
    "Use digitalmodel.reporting.report_generator instead.",
    DeprecationWarning,
    stacklevel=2,
)

from digitalmodel.reporting.report_generator import PlotlyReportGenerator

__all__ = ["PlotlyReportGenerator"]
