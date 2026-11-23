# ABOUTME: Pipe cross-section analysis module for offshore pipelines, risers, and piping.
# ABOUTME: Calculates geometry, weight, buoyancy, and generates visualization reports.

"""
Pipe Cross-Section Analysis Module
==================================

Comprehensive analysis module for multi-layer coated pipe cross-sections,
supporting offshore pipelines, risers, and general piping applications.

Key Features:
    - Multi-layer geometry calculations (steel + coatings)
    - Weight in air and submerged weight calculations
    - Buoyancy analysis for subsea applications
    - Interactive HTML reports with Plotly
    - Static PNG/SVG exports for engineering reports
    - CSV/JSON data export

Standards Compliance:
    - DNV-ST-F101: Submarine Pipeline Systems
    - API 5L: Line Pipe Specification
    - ISO 21809: Petroleum and Natural Gas Industries - External Coatings

Usage:
    >>> from digitalmodel.structural.pipe_cross_section import PipeCrossSection

    >>> # Create from metric units
    >>> pipe = PipeCrossSection(
    ...     steel_od_mm=609.6,
    ...     steel_wt_mm=14.29,
    ...     lpp_thickness_mm=3.5,
    ...     concrete_thickness_mm=80.0
    ... )

    >>> # Create from imperial units
    >>> pipe = PipeCrossSection.from_inches(
    ...     steel_od_inch=24,
    ...     steel_wt_inch=0.5625,
    ...     lpp_thickness_mm=3.5,
    ...     concrete_thickness_inch=3.15
    ... )

    >>> # Get summary
    >>> print(f"Submerged weight: {pipe.submerged_weight_kg_m:.1f} kg/m")
    Submerged weight: 256.9 kg/m

    >>> # Generate HTML report
    >>> from digitalmodel.structural.pipe_cross_section import generate_html_report
    >>> generate_html_report(pipe, "output/report.html")

CLI Usage:
    $ pipe-cross-section --steel-od-inch 24 --steel-wt-inch 0.5625 --lpp 3.5 --concrete 80
    $ pipe-cross-section --config pipe_config.yaml --output-dir ./results
"""

__version__ = "1.0.0"
__author__ = "DigitalModel Team"

# Core classes
from .models import (
    PipeLayer,
    PipeCrossSectionConfig,
    BuoyancyResult,
    CoatingType,
    InternalContents,
    inch_to_mm,
    mm_to_inch,
)

from .calculator import PipeCrossSection

# Visualization
from .visualization import (
    PipeCrossSectionVisualizer,
    generate_html_report,
    export_static_image,
    export_csv,
)

# CLI
from .cli import main as cli_main

__all__ = [
    # Version
    "__version__",
    # Models
    "PipeLayer",
    "PipeCrossSectionConfig",
    "BuoyancyResult",
    "CoatingType",
    "InternalContents",
    # Unit conversion
    "inch_to_mm",
    "mm_to_inch",
    # Calculator
    "PipeCrossSection",
    # Visualization
    "PipeCrossSectionVisualizer",
    "generate_html_report",
    "export_static_image",
    "export_csv",
    # CLI
    "cli_main",
]
