"""
Utility functions for marine engineering tutorials.

This package provides helper functions for:
- Professional plotting and visualization
- Report generation
- Data validation
- Excel comparison tools
"""

from .plotting_utils import (
    apply_plot_style,
    plot_catenary_shape,
    plot_wave_spectrum,
    plot_mooring_layout,
    plot_environmental_forces,
    create_interactive_3d_surface,
    save_figure_multiple_formats
)

__all__ = [
    'apply_plot_style',
    'plot_catenary_shape',
    'plot_wave_spectrum',
    'plot_mooring_layout',
    'plot_environmental_forces',
    'create_interactive_3d_surface',
    'save_figure_multiple_formats'
]
