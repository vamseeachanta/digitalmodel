#!/usr/bin/env python3
"""
Diffraction Report Data Models

ABOUTME: Pydantic data models and constants for diffraction report data.
Extracted from report_generator.py as part of WRK-591 God Object split.

No imports from other report_* modules — this is the leaf dependency.
"""

from __future__ import annotations

from datetime import datetime
from typing import Any, Dict, List, Optional

from pydantic import BaseModel, Field

DOF_NAMES = ["surge", "sway", "heave", "roll", "pitch", "yaw"]
DOF_UNITS = ["m/m", "m/m", "m/m", "deg/m", "deg/m", "deg/m"]
LOAD_RAO_UNITS = ["N/m", "N/m", "N/m", "N.m/m", "N.m/m", "N.m/m"]


class HydrostaticData(BaseModel):
    """Hydrostatic properties extracted from OrcaWave."""

    volume: float = Field(description="Displaced volume (m^3)")
    mass: float = Field(description="Vessel mass (kg or te depending on units)")
    centre_of_buoyancy: List[float] = Field(description="CoB [x, y, z]")
    centre_of_mass: List[float] = Field(description="CoM [x, y, z]")
    waterplane_area: float = Field(description="Waterplane area Awp (m^2)")
    Lxx: float = Field(description="Waterplane moment of inertia Ixx")
    Lyy: float = Field(description="Waterplane moment of inertia Iyy")
    Lxy: float = Field(description="Waterplane product of inertia Ixy")
    centre_of_floatation: List[float] = Field(description="CoF [x, y]")
    restoring_matrix: List[List[float]] = Field(
        description="6x6 hydrostatic stiffness matrix C"
    )
    inertia_matrix: List[List[float]] = Field(
        description="6x6 inertia matrix"
    )

    class Config:
        arbitrary_types_allowed = True


class RollDampingData(BaseModel):
    """Roll critical damping analysis data."""

    frequencies_rad_s: List[float]
    periods_s: List[float]
    roll_damping_percent_critical: List[float] = Field(
        description="B_44 as % of critical damping vs frequency"
    )
    B_44: List[float] = Field(description="Roll radiation damping vs frequency")
    A_44: List[float] = Field(description="Roll added mass vs frequency")
    C_44: float = Field(description="Hydrostatic restoring C(4,4)")
    I_44: float = Field(description="Roll moment of inertia I(4,4)")
    peak_roll_rao_period: Optional[float] = Field(
        None, description="Period of peak roll RAO (s)"
    )
    zeta_at_peak: Optional[float] = Field(
        None, description="Critical damping ratio at peak roll RAO period"
    )

    class Config:
        arbitrary_types_allowed = True


class LoadRAOData(BaseModel):
    """Wave excitation force/moment (load RAOs) from diffraction."""

    method: str = Field(description="Calculation method: diffraction or haskind")
    frequencies_rad_s: List[float]
    periods_s: List[float]
    headings_deg: List[float]
    amplitude: Dict[str, List[List[float]]] = Field(
        description="Per-DOF amplitude [nfreq x nheading]"
    )
    phase_deg: Dict[str, List[List[float]]] = Field(
        description="Per-DOF phase in degrees [nfreq x nheading]"
    )

    class Config:
        arbitrary_types_allowed = True


class MeshQualityData(BaseModel):
    """Panel mesh quality summary."""

    panel_count: int
    mean_area: float
    min_area: float
    max_area: float
    area_ratio: float = Field(description="max_area / min_area")
    vertex_count: Optional[int] = None

    class Config:
        arbitrary_types_allowed = True


class DiffractionReportData(BaseModel):
    """Complete data model for a standardized diffraction report."""

    schema_version: str = "1.0"
    vessel_name: str
    report_date: str = Field(
        default_factory=lambda: datetime.now().strftime("%Y-%m-%d %H:%M:%S")
    )
    report_title: Optional[str] = None
    report_subtitle: Optional[str] = None

    # Hydrostatics
    hydrostatics: Optional[HydrostaticData] = None

    # Roll critical damping
    roll_damping: Optional[RollDampingData] = None

    # Load RAOs (wave excitation)
    load_raos: Optional[LoadRAOData] = None

    # Mesh quality
    mesh_quality: Optional[MeshQualityData] = None

    # Solver comparison metadata
    solver_names: List[str] = Field(default_factory=list)
    source_file: Optional[str] = None

    # Raw frequency/heading grids
    frequencies_rad_s: List[float] = Field(default_factory=list)
    periods_s: List[float] = Field(default_factory=list)
    headings_deg: List[float] = Field(default_factory=list)

    # Infinite frequency added mass
    infinite_freq_added_mass: Optional[List[List[float]]] = None

    # Stability (computed from hydrostatics)
    gm_transverse: Optional[float] = None       # metres
    gm_longitudinal: Optional[float] = None     # metres
    bm_transverse: Optional[float] = None       # metres
    bm_longitudinal: Optional[float] = None     # metres
    kb: Optional[float] = None                  # metres
    radii_of_gyration: Optional[Dict[str, float]] = None  # r_xx, r_yy, r_zz

    # Natural periods (computed from restoring + inertia + added mass)
    natural_periods: Optional[Dict[str, Optional[float]]] = None

    # Peak responses (from RAO scan)
    peak_responses: Optional[Dict[str, Dict[str, Any]]] = None

    # Coupling significance (off-diagonal check)
    coupling_significance: Optional[Dict[str, float]] = None

    # Executive summary warnings
    executive_warnings: List[str] = Field(default_factory=list)

    # Hull type for interpretation
    hull_type: Optional[str] = None

    # Full added mass / damping diagonals for coefficient plots
    added_mass_diagonal: Optional[Dict[str, List[float]]] = None
    damping_diagonal: Optional[Dict[str, List[float]]] = None

    # Mode control
    mode: str = "full"  # "full" or "compact"

    # Multi-solver benchmark sections (pre-built HTML from BenchmarkPlotter)
    benchmark_html_sections: Optional[Dict[str, str]] = None

    # Notes (from benchmark or standalone analysis)
    notes: List[str] = Field(default_factory=list)

    class Config:
        arbitrary_types_allowed = True
