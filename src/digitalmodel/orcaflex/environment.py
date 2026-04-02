"""Environmental data handling for OrcaFlex analysis workflows.

Provides current profile definitions (power law, uniform, sheared),
wave spectrum parameters (JONSWAP, Pierson-Moskowitz), wind profiles,
and environmental load combination matrices per API/ISO standards.

Standards references:
    - API RP 2MET: Derivation of Metocean Design and Operating Conditions
    - DNV-RP-C205: Environmental Conditions and Environmental Loads
    - ISO 19901-1: Metocean design and operating considerations
    - API RP 2A-WSD: Planning, Designing and Constructing Fixed Offshore Platforms

Does NOT require OrcFxAPI — produces configuration dicts compatible
with OrcaFlex model data format.
"""

import math
from enum import Enum
from typing import Dict, List, Optional, Tuple

import numpy as np
from pydantic import BaseModel, Field


# ---------------------------------------------------------------------------
# Enums
# ---------------------------------------------------------------------------

class CurrentProfileType(str, Enum):
    """Supported current profile types."""
    UNIFORM = "uniform"
    POWER_LAW = "power_law"
    LINEAR = "linear"
    BILINEAR = "bilinear"


class WaveSpectrumType(str, Enum):
    """Supported wave spectrum types."""
    JONSWAP = "JONSWAP"
    PIERSON_MOSKOWITZ = "Pierson-Moskowitz"
    ISSC = "ISSC"


class WindProfileType(str, Enum):
    """Wind profile vertical variation types."""
    POWER_LAW = "power_law"
    LOG_LAW = "log_law"


# ---------------------------------------------------------------------------
# Current profiles
# ---------------------------------------------------------------------------

class CurrentProfilePoint(BaseModel):
    """Single depth–speed pair in a current profile."""
    depth: float = Field(..., description="Depth below water surface (m), positive downward")
    speed: float = Field(..., ge=0.0, description="Current speed (m/s)")
    direction: float = Field(0.0, ge=0.0, lt=360.0, description="Current direction (deg from N, going to)")


class CurrentProfile(BaseModel):
    """Discrete current profile (depth vs speed).

    Reference: DNV-RP-C205 Section 7.
    """
    profile_type: CurrentProfileType = CurrentProfileType.POWER_LAW
    surface_speed: float = Field(1.0, ge=0.0, description="Surface current speed (m/s)")
    water_depth: float = Field(1000.0, gt=0.0, description="Water depth (m)")
    direction: float = Field(0.0, ge=0.0, lt=360.0, description="Direction (deg from N)")
    power_law_exponent: float = Field(1.0 / 7.0, gt=0.0, description="Power law exponent (typically 1/7)")
    seabed_speed_fraction: float = Field(0.0, ge=0.0, le=1.0, description="Fraction of surface speed at seabed")
    n_points: int = Field(21, ge=2, description="Number of depth points to generate")

    def generate_profile(self) -> List[CurrentProfilePoint]:
        """Return a list of (depth, speed) points.

        For power-law profiles: V(z) = V_s * ((d - z) / d)^alpha
        where z is depth below surface, d is water depth, V_s is surface speed.
        """
        depths = np.linspace(0.0, self.water_depth, self.n_points)
        points: List[CurrentProfilePoint] = []

        for z in depths:
            if self.profile_type == CurrentProfileType.UNIFORM:
                speed = self.surface_speed
            elif self.profile_type == CurrentProfileType.POWER_LAW:
                ratio = max(0.0, (self.water_depth - z) / self.water_depth)
                speed = self.surface_speed * ratio ** self.power_law_exponent
            elif self.profile_type == CurrentProfileType.LINEAR:
                fraction = 1.0 - z / self.water_depth * (1.0 - self.seabed_speed_fraction)
                speed = self.surface_speed * fraction
            elif self.profile_type == CurrentProfileType.BILINEAR:
                mid_depth = self.water_depth * 0.5
                if z <= mid_depth:
                    speed = self.surface_speed
                else:
                    frac = (z - mid_depth) / (self.water_depth - mid_depth)
                    speed = self.surface_speed * (1.0 - frac * (1.0 - self.seabed_speed_fraction))
                    speed = max(0.0, speed)
            else:
                speed = self.surface_speed

            points.append(CurrentProfilePoint(depth=float(z), speed=float(speed), direction=self.direction))

        return points

    def to_orcaflex_dict(self) -> Dict:
        """Export as OrcaFlex-compatible dict for environment current data."""
        profile = self.generate_profile()
        return {
            "CurrentDepth": [p.depth for p in profile],
            "CurrentSpeed": [p.speed for p in profile],
            "CurrentDirection": [p.direction for p in profile],
        }


# ---------------------------------------------------------------------------
# Wave spectra
# ---------------------------------------------------------------------------

class WaveSpectrumParams(BaseModel):
    """Wave spectrum parameters.

    Reference: DNV-RP-C205 Section 3.5 (JONSWAP), Section 3.5.2 (PM).
    """
    spectrum_type: WaveSpectrumType = WaveSpectrumType.JONSWAP
    hs: float = Field(3.0, gt=0.0, description="Significant wave height Hs (m)")
    tp: float = Field(10.0, gt=0.0, description="Peak spectral period Tp (s)")
    gamma: float = Field(3.3, gt=1.0, description="JONSWAP peak enhancement factor")
    direction: float = Field(0.0, ge=0.0, lt=360.0, description="Wave direction (deg from N)")
    spreading_exponent: float = Field(2.0, ge=1.0, description="Cos^n directional spreading exponent")

    @property
    def fp(self) -> float:
        """Peak frequency (Hz)."""
        return 1.0 / self.tp

    @property
    def tz(self) -> float:
        """Zero-crossing period estimate (s). Tz ≈ Tp / 1.2859 for gamma=3.3."""
        if self.spectrum_type == WaveSpectrumType.PIERSON_MOSKOWITZ:
            return self.tp / 1.408
        # JONSWAP approximation
        return self.tp / (1.0 + 0.0336 * self.gamma - 0.185 / (1.9 + self.gamma))

    def spectral_density(self, f: np.ndarray) -> np.ndarray:
        """Compute spectral density S(f) (m^2/Hz).

        JONSWAP: S(f) = alpha * g^2 / (16 pi^4 f^5) * exp(-5/4 (fp/f)^4) * gamma^r
        Simplified parameterisation from DNV-RP-C205.
        """
        g = 9.80665
        fp = self.fp
        alpha_pm = (5.0 / 16.0) * self.hs**2 * fp**4

        with np.errstate(divide="ignore", invalid="ignore"):
            pm_part = alpha_pm / f**5 * np.exp(-1.25 * (fp / f) ** 4)

        if self.spectrum_type == WaveSpectrumType.PIERSON_MOSKOWITZ:
            return np.where(f > 0, pm_part, 0.0)

        # JONSWAP enhancement
        sigma = np.where(f <= fp, 0.07, 0.09)
        r = np.exp(-0.5 * ((f - fp) / (sigma * fp)) ** 2)
        jonswap = pm_part * self.gamma**r

        # Normalise to preserve Hs
        df = np.mean(np.diff(f)) if len(f) > 1 else 1.0
        m0 = np.trapz(jonswap[f > 0], f[f > 0]) if np.any(f > 0) else 1.0
        if m0 > 0:
            target_m0 = (self.hs / 4.0) ** 2
            jonswap *= target_m0 / m0

        return np.where(f > 0, jonswap, 0.0)

    def to_orcaflex_dict(self) -> Dict:
        """Export as OrcaFlex-compatible wave dict."""
        return {
            "WaveType": "JONSWAP" if self.spectrum_type == WaveSpectrumType.JONSWAP else "Dean stream",
            "WaveHs": self.hs,
            "WaveTp": self.tp,
            "WaveGamma": self.gamma if self.spectrum_type == WaveSpectrumType.JONSWAP else None,
            "WaveDirection": self.direction,
        }


# ---------------------------------------------------------------------------
# Wind profiles
# ---------------------------------------------------------------------------

class WindProfile(BaseModel):
    """Vertical wind profile definition.

    Reference: API RP 2MET Section 5, DNV-RP-C205 Section 2.3.
    """
    profile_type: WindProfileType = WindProfileType.POWER_LAW
    reference_speed: float = Field(20.0, ge=0.0, description="Wind speed at reference height (m/s)")
    reference_height: float = Field(10.0, gt=0.0, description="Reference height above MSL (m)")
    power_law_exponent: float = Field(0.12, gt=0.0, description="Power law exponent (API: 0.12 for 1-hr mean)")
    roughness_length: float = Field(0.002, gt=0.0, description="Surface roughness length z0 (m) for log law")
    direction: float = Field(0.0, ge=0.0, lt=360.0, description="Wind direction (deg from N)")

    def speed_at_height(self, z: float) -> float:
        """Wind speed at height z (m) above MSL.

        Power law: V(z) = V_ref * (z / z_ref)^alpha
        Log law:   V(z) = V_ref * ln(z/z0) / ln(z_ref/z0)
        """
        if z <= 0:
            return 0.0
        if self.profile_type == WindProfileType.POWER_LAW:
            return self.reference_speed * (z / self.reference_height) ** self.power_law_exponent
        else:
            # Log law
            return self.reference_speed * math.log(z / self.roughness_length) / math.log(
                self.reference_height / self.roughness_length
            )

    def generate_profile(self, heights: Optional[List[float]] = None) -> List[Tuple[float, float]]:
        """Generate (height, speed) pairs."""
        if heights is None:
            heights = [5, 10, 20, 30, 50, 80, 100, 120, 150]
        return [(h, self.speed_at_height(h)) for h in heights]


# ---------------------------------------------------------------------------
# Environmental load combination matrix
# ---------------------------------------------------------------------------

class EnvironmentalLoadCase(BaseModel):
    """Single environmental load combination case.

    Reference: API RP 2SK Section 6, ISO 19901-7.
    """
    case_id: str = Field(..., description="Load case identifier")
    wave_hs: float = Field(..., ge=0.0, description="Significant wave height (m)")
    wave_tp: float = Field(..., gt=0.0, description="Peak period (s)")
    wave_direction: float = Field(0.0, description="Wave direction (deg)")
    current_speed: float = Field(0.0, ge=0.0, description="Surface current speed (m/s)")
    current_direction: float = Field(0.0, description="Current direction (deg)")
    wind_speed: float = Field(0.0, ge=0.0, description="1-hr mean wind speed at 10 m (m/s)")
    wind_direction: float = Field(0.0, description="Wind direction (deg)")
    return_period_years: float = Field(100.0, gt=0.0, description="Return period (years)")
    condition: str = Field("intact", description="Condition: intact, damaged, transient")


class EnvironmentalMatrix(BaseModel):
    """Environmental load combination matrix per API/ISO.

    Generates combinations of wave, current, and wind for ULS/ALS/SLS
    per API RP 2SK Table 2 methodology: associated vs. independent extremes.

    Reference: API RP 2SK Section 6.3, DNV-OS-E301 Section 2.
    """
    wave_hs_100yr: float = Field(6.0, gt=0.0, description="100-yr Hs (m)")
    wave_tp_100yr: float = Field(12.0, gt=0.0, description="100-yr Tp (s)")
    current_100yr: float = Field(1.5, gt=0.0, description="100-yr surface current (m/s)")
    wind_100yr: float = Field(30.0, gt=0.0, description="100-yr 1-hr wind speed at 10 m (m/s)")
    headings: List[float] = Field(default=[0, 45, 90, 135, 180, 225, 270, 315],
                                  description="Headings to consider (deg)")
    hs_tp_ratio: float = Field(2.0, description="Tp/Hs scaling ratio for non-100yr cases")

    def _scale_to_return_period(self, value_100yr: float, target_rp: float) -> float:
        """Scale 100-yr value to other return periods via Gumbel."""
        # Gumbel: V_rp = V_100 * [1 + 0.1 * ln(rp/100)]  (simplified)
        if target_rp <= 0:
            return 0.0
        return value_100yr * (1.0 + 0.1 * math.log(target_rp / 100.0))

    def generate_uls_cases(self) -> List[EnvironmentalLoadCase]:
        """Generate ULS (100-yr return) load combination matrix.

        Per API RP 2SK Table 2:
        - Case A: 100-yr wave + associated current + associated wind
        - Case B: 100-yr current + associated wave + associated wind
        """
        cases: List[EnvironmentalLoadCase] = []
        assoc_factor = 0.6  # associated extreme factor

        for i, heading in enumerate(self.headings):
            # Case A: extreme wave
            cases.append(EnvironmentalLoadCase(
                case_id=f"ULS-A-{heading:03.0f}",
                wave_hs=self.wave_hs_100yr,
                wave_tp=self.wave_tp_100yr,
                wave_direction=heading,
                current_speed=self.current_100yr * assoc_factor,
                current_direction=heading,
                wind_speed=self.wind_100yr * assoc_factor,
                wind_direction=heading,
                return_period_years=100.0,
                condition="intact",
            ))
            # Case B: extreme current
            hs_assoc = self.wave_hs_100yr * assoc_factor
            cases.append(EnvironmentalLoadCase(
                case_id=f"ULS-B-{heading:03.0f}",
                wave_hs=hs_assoc,
                wave_tp=max(5.0, hs_assoc * self.hs_tp_ratio),
                wave_direction=heading,
                current_speed=self.current_100yr,
                current_direction=heading,
                wind_speed=self.wind_100yr * assoc_factor,
                wind_direction=heading,
                return_period_years=100.0,
                condition="intact",
            ))

        return cases

    def generate_als_cases(self) -> List[EnvironmentalLoadCase]:
        """Generate ALS (damaged) load combination cases with 100-yr environment."""
        cases: List[EnvironmentalLoadCase] = []

        for heading in self.headings:
            cases.append(EnvironmentalLoadCase(
                case_id=f"ALS-{heading:03.0f}",
                wave_hs=self.wave_hs_100yr,
                wave_tp=self.wave_tp_100yr,
                wave_direction=heading,
                current_speed=self.current_100yr * 0.6,
                current_direction=heading,
                wind_speed=self.wind_100yr * 0.6,
                wind_direction=heading,
                return_period_years=100.0,
                condition="damaged",
            ))

        return cases

    def generate_all_cases(self) -> List[EnvironmentalLoadCase]:
        """Generate complete load combination matrix (ULS + ALS)."""
        return self.generate_uls_cases() + self.generate_als_cases()
