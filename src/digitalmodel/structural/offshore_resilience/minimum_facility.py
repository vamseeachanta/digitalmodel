"""
Minimum Facility Platform — modular platform design parameterization.

Provides data models for minimum-facility (marginal-field) platform concepts
with BSEE platform classification mapping and modular vs. conventional cost
comparison.

References
----------
BSEE OCS Platform Inventory (Gulf of Mexico).
API RP 2A-WSD, Section 1.7 — Platform Classification.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from enum import Enum
from typing import Tuple


# ---------------------------------------------------------------------------
# Enumerations
# ---------------------------------------------------------------------------

class InstallationMethod(str, Enum):
    """Offshore platform installation method."""
    CRANE_BARGE = "crane_barge"
    HEAVY_LIFT = "heavy_lift"
    SELF_INSTALL = "self_install"


class BseePlatformClass(str, Enum):
    """BSEE Gulf of Mexico platform classification by size and water depth."""
    SMALL_FIXED = "small_fixed"
    LARGE_FIXED = "large_fixed"
    FLOATING = "floating"


# ---------------------------------------------------------------------------
# Allowed platform types
# ---------------------------------------------------------------------------

VALID_PLATFORM_TYPES = frozenset({"wellhead", "riser", "process_minimum"})

# BSEE classification thresholds (approximate GoM boundaries)
_FLOATING_DEPTH_THRESHOLD_FT = 1500.0
_LARGE_FIXED_DEPTH_THRESHOLD_FT = 500.0
_LARGE_FIXED_TOPSIDES_THRESHOLD_T = 2000.0


# ---------------------------------------------------------------------------
# Data models
# ---------------------------------------------------------------------------

@dataclass
class MinimumFacilityPlatform:
    """
    Minimum facility (marginal-field) platform concept.

    Parameters
    ----------
    platform_type : str
        One of "wellhead", "riser", "process_minimum".
    topsides_weight_tonnes : float
        Integrated topsides weight (t).
    jacket_weight_tonnes : float
        Steel jacket weight (t).
    water_depth_range_ft : tuple[float, float]
        (min, max) applicable water depth (ft).
    installation_method : InstallationMethod
        Primary planned installation method.
    max_crane_capacity_tonnes : float
        Maximum hook load of available crane barge (t).
        Used to assess whether direct crane-lift is feasible.
    well_slots : int
        Number of well conductor slots.
    design_life_years : int
        Nominal design service life (years).
    expansion_capability : bool
        True if the platform can accept additional modules post-installation.
    """

    platform_type: str
    topsides_weight_tonnes: float
    jacket_weight_tonnes: float
    water_depth_range_ft: Tuple[float, float]
    installation_method: InstallationMethod
    max_crane_capacity_tonnes: float
    well_slots: int
    design_life_years: int
    expansion_capability: bool

    def __post_init__(self) -> None:
        if self.platform_type not in VALID_PLATFORM_TYPES:
            raise ValueError(
                f"platform_type must be one of {sorted(VALID_PLATFORM_TYPES)}; "
                f"got '{self.platform_type}'"
            )
        min_depth, max_depth = self.water_depth_range_ft
        if min_depth >= max_depth:
            raise ValueError(
                "water_depth_range_ft: min must be strictly less than max; "
                f"got ({min_depth}, {max_depth})"
            )

    @property
    def total_weight_tonnes(self) -> float:
        """Combined topsides + jacket steel weight (t)."""
        return self.topsides_weight_tonnes + self.jacket_weight_tonnes

    @property
    def crane_installable(self) -> bool:
        """True when total weight does not exceed available crane capacity."""
        return self.total_weight_tonnes <= self.max_crane_capacity_tonnes

    @property
    def water_depth_midpoint_ft(self) -> float:
        """Midpoint of the applicable water depth range (ft)."""
        return (self.water_depth_range_ft[0] + self.water_depth_range_ft[1]) / 2.0


@dataclass
class ModularCostComparison:
    """
    Modular vs. conventional platform CAPEX comparison.

    Parameters
    ----------
    conventional_capex_usd : float
        Full conventional platform installed cost (USD).
    minimum_facility_capex_usd : float
        Minimum-facility concept installed cost (USD).
    heavy_lift_vessel_savings_usd : float
        Avoided heavy-lift vessel spread cost if direct crane lift is used.
    installation_weather_window_days : int
        Required installation weather window (days).
    """

    conventional_capex_usd: float
    minimum_facility_capex_usd: float
    heavy_lift_vessel_savings_usd: float
    installation_weather_window_days: int

    def __post_init__(self) -> None:
        if self.conventional_capex_usd <= 0.0:
            raise ValueError(
                "conventional_capex_usd must be positive; "
                f"got {self.conventional_capex_usd}"
            )

    @property
    def net_savings_usd(self) -> float:
        """
        Net savings vs. conventional approach.

        net = (conventional - minimum_facility) + heavy_lift_savings
        """
        return (
            self.conventional_capex_usd
            - self.minimum_facility_capex_usd
            + self.heavy_lift_vessel_savings_usd
        )

    @property
    def savings_fraction(self) -> float:
        """Net savings as a fraction of conventional CAPEX."""
        return self.net_savings_usd / self.conventional_capex_usd


# ---------------------------------------------------------------------------
# BSEE classification helper
# ---------------------------------------------------------------------------

def classify_bsee_platform(
    water_depth_ft: float,
    topsides_weight_tonnes: float,
) -> BseePlatformClass:
    """
    Map a platform concept to a BSEE Gulf of Mexico size category.

    Classification rules (approximate):
    - FLOATING    : water depth > 1 500 ft (independent of topsides weight)
    - LARGE_FIXED : depth in (500, 1500] ft  OR  topsides > 2 000 t
    - SMALL_FIXED : depth <= 500 ft  AND  topsides <= 2 000 t

    Parameters
    ----------
    water_depth_ft : float
        Mean water depth at platform location (ft).
    topsides_weight_tonnes : float
        Integrated topsides weight (t).

    Returns
    -------
    BseePlatformClass
    """
    if water_depth_ft > _FLOATING_DEPTH_THRESHOLD_FT:
        return BseePlatformClass.FLOATING
    if (
        water_depth_ft > _LARGE_FIXED_DEPTH_THRESHOLD_FT
        or topsides_weight_tonnes > _LARGE_FIXED_TOPSIDES_THRESHOLD_T
    ):
        return BseePlatformClass.LARGE_FIXED
    return BseePlatformClass.SMALL_FIXED


# ---------------------------------------------------------------------------
# Lifecycle value index
# ---------------------------------------------------------------------------

_SCALE_MIN = 1
_SCALE_MAX = 5


def lifecycle_value_index(
    installation_difficulty: int,
    operational_flexibility: int,
    decommissioning_complexity: int,
    expansion_headroom: int,
) -> float:
    """
    Score a platform concept on lifecycle value (0 = poor, 1 = excellent).

    All inputs use a 1–5 scale where:
    - installation_difficulty  : 1 = easy, 5 = very difficult
    - operational_flexibility  : 1 = rigid, 5 = highly flexible
    - decommissioning_complexity: 1 = simple, 5 = very complex
    - expansion_headroom       : 1 = none, 5 = ample

    Positive drivers (flexibility, headroom) increase score.
    Negative drivers (difficulty, complexity) decrease score.

    Returns
    -------
    float
        Lifecycle value index in [0, 1].
    """
    for name, val in [
        ("installation_difficulty", installation_difficulty),
        ("operational_flexibility", operational_flexibility),
        ("decommissioning_complexity", decommissioning_complexity),
        ("expansion_headroom", expansion_headroom),
    ]:
        if not (_SCALE_MIN <= val <= _SCALE_MAX):
            raise ValueError(
                f"{name} must be in [{_SCALE_MIN}, {_SCALE_MAX}]; got {val}"
            )

    # Normalise each input to [0, 1]
    span = float(_SCALE_MAX - _SCALE_MIN)

    # Higher values for positive drivers are better
    pos_install = 1.0 - (installation_difficulty - _SCALE_MIN) / span
    pos_flexibility = (operational_flexibility - _SCALE_MIN) / span
    pos_decom = 1.0 - (decommissioning_complexity - _SCALE_MIN) / span
    pos_headroom = (expansion_headroom - _SCALE_MIN) / span

    # Equal-weight composite
    return (pos_install + pos_flexibility + pos_decom + pos_headroom) / 4.0
