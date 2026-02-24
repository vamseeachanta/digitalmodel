# ABOUTME: Schematics sub-package — SVG/PNG field development layout diagrams
"""
digitalmodel.field_development.schematics

Provides schematic generators for:
- SubseaTiebackSchematic  — subsea templates + flowlines to host
- PlatformSchematic       — stand-alone platform with conductors
- FpsoSpreadSchematic     — FPSO with spread mooring and SPS
"""

from .subsea_tieback import SubseaTiebackSchematic
from .platform_standalone import PlatformSchematic
from .fpso_spread import FpsoSpreadSchematic
from .renderer import render_figure

__all__ = [
    "SubseaTiebackSchematic",
    "PlatformSchematic",
    "FpsoSpreadSchematic",
    "render_figure",
]
