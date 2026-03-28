"""Data models for subsea structure installation analysis.

Defines the core data structures for installation engineering:
structures, vessels, crane configurations, installation cases, and results.

References:
    DNV-RP-H103 (2011) -- Modelling and Analysis of Marine Operations
    DNV-ST-N001 (2021) -- Marine Operations and Marine Warranty
"""
from __future__ import annotations

import numpy as np
from dataclasses import dataclass, field
from enum import Enum
from typing import NamedTuple


class InstallationPhase(Enum):
    """Phases of a subsea structure installation."""

    LIFT_OFF = "lift_off"
    IN_AIR = "in_air"
    SPLASH_ZONE = "splash_zone"
    LOWERING = "lowering"
    LANDING = "landing"


class GoNoGoState(Enum):
    """Decision state for installation operations."""

    GO = "go"
    NO_GO = "no_go"
    MARGINAL = "marginal"


@dataclass
class Structure:
    """Subsea structure being installed.

    Attributes
    ----------
    name : str
        Structure identifier.
    length_m : float
        Overall length [m].
    width_m : float
        Overall width [m].
    height_m : float
        Overall height [m].
    mass_air_kg : float
        Mass in air [kg].
    cog_m : tuple[float, float, float]
        Centre of gravity (x, y, z) in structure local frame [m].
    C_s : float
        Slamming coefficient per DNV-RP-H103 §4.3.4. Default 5.0 for flat bottom.
    C_D : float
        Drag coefficient for lowering. Default 2.0 (bluff body).
    C_a : float
        Added mass coefficient for submerged phase. Default 1.0 (box-like).
    A_projected_m2 : float | None
        Projected area for slamming [m2]. Defaults to length * width.
    """

    name: str
    length_m: float
    width_m: float
    height_m: float
    mass_air_kg: float
    cog_m: tuple[float, float, float] = (0.0, 0.0, 0.0)
    C_s: float = 5.0
    C_D: float = 2.0
    C_a: float = 1.0
    A_projected_m2: float | None = None

    def __post_init__(self):
        if self.A_projected_m2 is None:
            self.A_projected_m2 = self.length_m * self.width_m

    @property
    def weight_air_N(self) -> float:
        return self.mass_air_kg * 9.80665

    def submerged_weight_N(self, rho_w: float = 1025.0) -> float:
        """Net submerged weight [N]."""
        volume = self.length_m * self.width_m * self.height_m
        buoyancy = rho_w * 9.80665 * volume
        return self.weight_air_N - buoyancy


@dataclass
class CraneTipConfig:
    """Crane tip position relative to vessel CoG.

    The geometric transfer function uses this position vector to convert
    vessel RAOs at CoG to crane tip motions.

    Attributes
    ----------
    x_m : float
        Longitudinal offset from vessel CoG [m]. Positive forward.
    y_m : float
        Transverse offset from vessel CoG [m]. Positive port.
    z_m : float
        Vertical offset from vessel CoG [m]. Positive up.
    """

    x_m: float
    y_m: float
    z_m: float

    @property
    def position_vector(self) -> np.ndarray:
        return np.array([self.x_m, self.y_m, self.z_m])


@dataclass
class CraneCurve:
    """Crane load-radius curve.

    Attributes
    ----------
    radii_m : np.ndarray
        Operating radii [m].
    capacities_te : np.ndarray
        Corresponding lift capacities [te] at each radius.
    max_hook_load_te : float
        Maximum allowable hook load [te] for the operating radius.
    """

    radii_m: np.ndarray
    capacities_te: np.ndarray
    max_hook_load_te: float

    def capacity_at_radius(self, radius_m: float) -> float:
        """Interpolated crane capacity [te] at a given radius."""
        return float(np.interp(radius_m, self.radii_m, self.capacities_te))


@dataclass
class Vessel:
    """Installation vessel with RAO library and crane data.

    Attributes
    ----------
    name : str
        Vessel identifier.
    rao_frequencies : np.ndarray
        RAO frequencies [rad/s], shape (n_freq,).
    rao_headings : np.ndarray
        RAO headings [deg], shape (n_hdg,).
    rao_data : dict[str, dict[str, np.ndarray]]
        RAO amplitudes and phases per DOF. Keys: surge, sway, heave, roll, pitch, yaw.
        Each value: {'amplitude': (n_freq, n_hdg), 'phase': (n_freq, n_hdg)}.
    crane_tip : CraneTipConfig
        Crane tip position relative to vessel CoG.
    crane_curve : CraneCurve | None
        Crane capacity curve.
    displacement_te : float
        Vessel displacement [te].
    """

    name: str
    rao_frequencies: np.ndarray
    rao_headings: np.ndarray
    rao_data: dict[str, dict[str, np.ndarray]]
    crane_tip: CraneTipConfig
    crane_curve: CraneCurve | None = None
    displacement_te: float = 0.0

    def has_complete_raos(self) -> bool:
        required = {"surge", "sway", "heave", "roll", "pitch", "yaw"}
        return required.issubset(self.rao_data.keys())


@dataclass
class InstallationCriteria:
    """Allowable limits for installation operations per DNV-ST-N001.

    Attributes
    ----------
    max_crane_tip_heave_m : float
        Maximum allowable single-amplitude crane tip heave [m].
    max_crane_tip_velocity_m_s : float
        Maximum allowable crane tip velocity [m/s].
    max_hook_load_factor : float
        Maximum dynamic amplification factor on hook load.
    max_tilt_deg : float
        Maximum allowable structure tilt during lowering [deg].
    alpha_operational : float
        DNV-ST-N001 operational contingency factor (typically 1.0–1.5).
    """

    max_crane_tip_heave_m: float = 2.0
    max_crane_tip_velocity_m_s: float = 0.5
    max_hook_load_factor: float = 1.3
    max_tilt_deg: float = 3.0
    alpha_operational: float = 1.0


class OperabilityResult(NamedTuple):
    """Result of operability assessment for a vessel/structure pair.

    Attributes
    ----------
    hs_limit_m : float
        Maximum allowable significant wave height [m].
    tp_limits_s : np.ndarray
        Tp range [s] at the Hs limit.
    governing_criterion : str
        Which criterion governs (e.g. 'crane_tip_heave', 'hook_load').
    operability_pct : float
        Percentage operability from scatter diagram (0–100).
    details : dict
        Intermediate calculation results for audit trail.
    """

    hs_limit_m: float
    tp_limits_s: np.ndarray
    governing_criterion: str
    operability_pct: float
    details: dict


@dataclass
class SplashZoneResult:
    """Results from splash zone assessment per DNV-RP-H103 §4.

    Attributes
    ----------
    slamming_force_N : float
        Slamming force at water entry [N].
    varying_buoyancy_N : float
        Peak varying buoyancy force in splash zone [N].
    total_hydrodynamic_force_N : float
        Combined slamming + varying buoyancy [N].
    min_crane_load_N : float
        Minimum hook load in splash zone (snap risk) [N].
    max_crane_load_N : float
        Maximum hook load in splash zone [N].
    daf : float
        Dynamic Amplification Factor.
    details : dict
        Intermediate values for audit trail.
    """

    slamming_force_N: float
    varying_buoyancy_N: float
    total_hydrodynamic_force_N: float
    min_crane_load_N: float
    max_crane_load_N: float
    daf: float
    details: dict


@dataclass
class InstallationCase:
    """Complete installation analysis case.

    Bundles a structure, vessel, criteria, and environmental conditions
    for a single assessment run.
    """

    structure: Structure
    vessel: Vessel
    criteria: InstallationCriteria
    wave_hs_m: float
    wave_tp_s: float
    wave_heading_deg: float = 0.0
    current_m_s: float = 0.0
    water_depth_m: float = 100.0
    rho_w_kg_m3: float = 1025.0
