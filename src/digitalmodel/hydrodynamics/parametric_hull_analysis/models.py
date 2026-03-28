"""
ABOUTME: Data models for parametric hull analysis — configuration, sweep
results, shallow water classification, and bank effect containers.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from enum import Enum
from typing import Any, Optional

import numpy as np
from pydantic import BaseModel, Field


# ---------------------------------------------------------------------------
# Enums
# ---------------------------------------------------------------------------


class DepthClassification(str, Enum):
    """Shallow water classification per DNV-RP-C205 / PIANC 121."""

    DEEP = "deep"                  # h/T > 3.0
    MEDIUM = "medium"              # 2.0 < h/T <= 3.0
    SHALLOW = "shallow"            # 1.5 < h/T <= 2.0
    VERY_SHALLOW = "very_shallow"  # h/T <= 1.5


class BankSlopeType(str, Enum):
    """PIANC 121 bank slope categories."""

    GENTLE = "gentle"      # >= 1:3 (or flatter)
    MODERATE = "moderate"  # ~1:2
    STEEP = "steep"        # 1:1 or vertical


# ---------------------------------------------------------------------------
# Configuration models
# ---------------------------------------------------------------------------


class SweepConfig(BaseModel):
    """Full configuration for a parametric hull BEM sweep."""

    # Hull parametric space
    base_hull_id: str
    ranges: dict[str, Any] = Field(
        default_factory=dict,
        description="Dict of param_name -> ParametricRange",
    )
    fixed_params: dict[str, float] = Field(default_factory=dict)

    # Wave conditions
    t_min: float = Field(3.0, description="Min wave period [s]")
    t_max: float = Field(25.0, description="Max wave period [s]")
    n_periods: int = Field(30, description="Number of wave periods")
    headings_deg: list[float] = Field(
        default_factory=lambda: [0.0, 45.0, 90.0, 135.0, 180.0],
    )
    water_depth: float = Field(
        default=float("inf"), description="Water depth [m], inf=deep"
    )
    rho: float = 1025.0

    # Mesh generation
    target_panels: int = Field(1000, ge=100, le=10000)
    symmetry: bool = True

    # Solver
    n_jobs: int = Field(1, ge=1)

    # Forward speed (optional)
    forward_speed_ms: Optional[float] = Field(
        None, description="Forward speed [m/s], None=stationary"
    )


class PassingShipSweepConfig(BaseModel):
    """Configuration for parametric passing ship force sweeps."""

    separations_m: list[float] = Field(
        ..., description="Lateral separation distances [m]"
    )
    speeds_ms: list[float] = Field(
        ..., description="Passing ship speeds [m/s]"
    )
    water_depths_m: list[float] = Field(
        ..., description="Water depths to evaluate [m]"
    )
    stagger_n_points: int = Field(
        50, ge=10, description="Points along track for time history"
    )
    parallel: bool = True


# ---------------------------------------------------------------------------
# Result containers
# ---------------------------------------------------------------------------


@dataclass
class SweepResultEntry:
    """One hull variation's BEM + RAO results."""

    variation_id: str
    hull_params: dict[str, float]
    length_bp: float
    beam: float
    draft: float
    block_coefficient: Optional[float]
    bem_result: Any  # capytaine.BEMResult
    rao_result: Any  # capytaine.RAOResult
    depth_class: DepthClassification
    forward_speed_ms: float = 0.0
    metadata: dict = field(default_factory=dict)


@dataclass
class PassingShipSweepEntry:
    """One passing ship scenario result."""

    variation_id: str
    hull_params: dict[str, float]
    separation_m: float
    speed_ms: float
    water_depth_m: float
    peak_surge_N: float
    peak_sway_N: float
    peak_yaw_Nm: float
    depth_class: DepthClassification


@dataclass
class BankEffectResult:
    """Bank suction force result per PIANC 121."""

    lateral_force_N: float
    yaw_moment_Nm: float
    bank_clearance_m: float
    slope_type: BankSlopeType
    speed_ms: float
    water_depth_m: float


# ---------------------------------------------------------------------------
# Utility functions
# ---------------------------------------------------------------------------


def classify_depth(water_depth: float, draft: float) -> DepthClassification:
    """Classify water depth ratio per DNV-RP-C205 convention.

    Parameters
    ----------
    water_depth : float
        Water depth in metres.  Use ``float('inf')`` for deep water.
    draft : float
        Vessel draft in metres (must be > 0).

    Returns
    -------
    DepthClassification
    """
    if not np.isfinite(water_depth):
        return DepthClassification.DEEP
    ratio = water_depth / draft
    if ratio > 3.0:
        return DepthClassification.DEEP
    elif ratio > 2.0:
        return DepthClassification.MEDIUM
    elif ratio > 1.5:
        return DepthClassification.SHALLOW
    else:
        return DepthClassification.VERY_SHALLOW


__all__ = [
    "DepthClassification",
    "BankSlopeType",
    "SweepConfig",
    "PassingShipSweepConfig",
    "SweepResultEntry",
    "PassingShipSweepEntry",
    "BankEffectResult",
    "classify_depth",
]
