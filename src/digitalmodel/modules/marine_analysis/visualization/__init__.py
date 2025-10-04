"""Visualization Module for Marine Engineering Data.

This module provides specialized visualization tools for marine engineering
data including integration charts, OCIMF data visualization, and
hydrodynamic response plots.

Modules:
    integration_charts: System integration and architecture visualizations
    ocimf_charts: OCIMF wind/current coefficient visualizations

Example:
    >>> from digitalmodel.modules.marine_analysis.visualization import ocimf_charts
    >>> ocimf_charts.plot_wind_coefficients(vessel_data)
    >>> ocimf_charts.save_polar_diagrams('output/polar.png')
"""

from .integration_charts import *
from .ocimf_charts import *

__all__ = [
    'integration_charts',
    'ocimf_charts'
]
