# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Signal visualization module.

Plotting and visualization utilities for signal analysis:
- Time series plots
- Spectrograms
- Rainflow histograms
- Interactive visualizations
"""

from .filter_comparison import plot_filter_comparison

__all__ = ["plot_filter_comparison"]
