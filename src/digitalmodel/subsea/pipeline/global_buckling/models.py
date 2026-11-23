"""
Data models for closed-form global (lateral) buckling of submarine pipelines.

Reference
---------
Hobbs, R.E. (1984). "In-Service Buckling of Heated Pipelines."
ASCE Journal of Transportation Engineering 110(2), 175-189.
DOI: 10.1061/(ASCE)0733-947X(1984)110:2(175)

Conventions
-----------
* All quantities are SI.  Axial force is **positive in compression**.
* ``L`` is the Hobbs characteristic buckle length (the lobe length of the
  mode shape), NOT the total length of pipeline affected by the buckle.
* Soil resistance is fully-mobilised Coulomb friction; axial and lateral
  coefficients are kept separate because they differ materially in practice
  (axial breakout is typically 0.3-0.6, lateral 0.5-1.2 on soft clay).
"""
from __future__ import annotations

import math
from dataclasses import dataclass
from enum import Enum


class HobbsMode(Enum):
    """Hobbs (1984) lateral post-buckling mode shapes.

    Modes 1-4 are the finite (localised) lobed shapes of Fig. 3; ``INFINITE``
    is the periodic (sinusoidal, infinite-mode) solution of Eqs. 20-25.
    """

    MODE_1 = "1"
    MODE_2 = "2"
    MODE_3 = "3"
    MODE_4 = "4"
    INFINITE = "infinite"


@dataclass(frozen=True)
class HobbsConstants:
    """Mode coefficients from Hobbs (1984) Table 1.

    k1  buckle force coefficient        P  = k1 EI / L^2
    k2  force-release coefficient       (inside the radical, see hobbs_lateral)
    k3  axial slip coefficient          (zero for the periodic mode)
    k4  amplitude coefficient           y_hat = k4 q_L L^4 / EI
    k5  bending moment coefficient      M_hat = k5 q_L L^2
    k6  maximum slope coefficient       periodic mode only (Eq. 25), else None
    """

    k1: float
    k2: float
    k3: float
    k4: float
    k5: float
    k6: float | None = None


# Hobbs (1984) Table 1 for the lateral modes; the periodic ("infinite") row
# follows Eqs. 20-25.  These are published mathematical mode coefficients.
MODE_CONSTANTS: dict[HobbsMode, HobbsConstants] = {
    HobbsMode.MODE_1: HobbsConstants(80.76, 6.391e-5, 0.500, 2.407e-3, 6.938e-2),
    HobbsMode.MODE_2: HobbsConstants(4.0 * math.pi**2, 1.743e-4, 1.000, 5.532e-3, 1.088e-1),
    HobbsMode.MODE_3: HobbsConstants(34.06, 1.668e-4, 1.294, 1.032e-2, 1.434e-1),
    HobbsMode.MODE_4: HobbsConstants(28.20, 2.144e-4, 1.608, 1.047e-2, 1.483e-1),
    HobbsMode.INFINITE: HobbsConstants(
        4.0 * math.pi**2, 4.7050e-5, 0.0, 4.4495e-3, 5.066e-2, 1.267e-2
    ),
}


@dataclass(frozen=True)
class PipeSection:
    """Homogeneous elastic steel section carrying the buckling load.

    Parameters
    ----------
    e_modulus_pa           Young's modulus E [Pa]
    area_m2                steel cross-sectional area A_s [m^2]
    inertia_m4             steel second moment of area I [m^4]
    outer_radius_m         extreme-fibre radius for bending stress [m]
    submerged_weight_N_m   submerged weight w of the pipe **including
                           coatings and contents** [N/m]
    thermal_expansion_per_K  alpha [1/K]

    ``area_m2`` and ``inertia_m4`` are stored explicitly (rather than always
    derived) so that published rounded section properties can be reproduced
    exactly when benchmarking.  Use :meth:`from_dimensions` for the usual case.
    """

    e_modulus_pa: float
    area_m2: float
    inertia_m4: float
    outer_radius_m: float
    submerged_weight_N_m: float
    thermal_expansion_per_K: float = 1.17e-5

    def __post_init__(self) -> None:
        for name in (
            "e_modulus_pa",
            "area_m2",
            "inertia_m4",
            "outer_radius_m",
            "submerged_weight_N_m",
            "thermal_expansion_per_K",
        ):
            value = getattr(self, name)
            if not math.isfinite(value) or value <= 0.0:
                raise ValueError(f"{name} must be finite and positive, got {value!r}")

    @classmethod
    def from_dimensions(
        cls,
        *,
        e_modulus_pa: float,
        od_m: float,
        wt_m: float,
        submerged_weight_N_m: float,
        thermal_expansion_per_K: float = 1.17e-5,
    ) -> PipeSection:
        """Build from the steel annulus: A = pi/4 (D^2 - d^2), I = pi/64 (D^4 - d^4)."""
        if not math.isfinite(od_m) or od_m <= 0.0:
            raise ValueError("od_m must be finite and positive")
        if not math.isfinite(wt_m) or wt_m <= 0.0:
            raise ValueError("wt_m must be finite and positive")
        if 2.0 * wt_m >= od_m:
            raise ValueError("wt_m must be less than half of od_m")
        id_m = od_m - 2.0 * wt_m
        return cls(
            e_modulus_pa=e_modulus_pa,
            area_m2=math.pi / 4.0 * (od_m**2 - id_m**2),
            inertia_m4=math.pi / 64.0 * (od_m**4 - id_m**4),
            outer_radius_m=od_m / 2.0,
            submerged_weight_N_m=submerged_weight_N_m,
            thermal_expansion_per_K=thermal_expansion_per_K,
        )

    @property
    def EA(self) -> float:
        """Axial stiffness E*A_s [N]."""
        return self.e_modulus_pa * self.area_m2

    @property
    def EI(self) -> float:
        """Bending stiffness E*I [N m^2]."""
        return self.e_modulus_pa * self.inertia_m4

    def fully_restrained_thermal_force(self, temperature_rise_K: float) -> float:
        """Compressive force E*A*alpha*dT in a fully-restrained line [N]."""
        if not math.isfinite(temperature_rise_K) or temperature_rise_K < 0.0:
            raise ValueError("temperature_rise_K must be finite and non-negative")
        return self.EA * self.thermal_expansion_per_K * temperature_rise_K


@dataclass(frozen=True)
class SoilResistance:
    """Fully-mobilised Coulomb friction coefficients.

    axial_friction     phi_A [-], resists feed-in along the pipe axis
    lateral_friction   phi_L [-], resists sideways sweep of the buckle
    """

    axial_friction: float
    lateral_friction: float

    def __post_init__(self) -> None:
        for name in ("axial_friction", "lateral_friction"):
            value = getattr(self, name)
            if not math.isfinite(value) or value <= 0.0:
                raise ValueError(f"{name} must be finite and positive, got {value!r}")

    def axial_resistance_N_m(self, submerged_weight_N_m: float) -> float:
        """q_A = phi_A * w [N/m]."""
        return self.axial_friction * submerged_weight_N_m

    def lateral_resistance_N_m(self, submerged_weight_N_m: float) -> float:
        """q_L = phi_L * w [N/m]."""
        return self.lateral_friction * submerged_weight_N_m


@dataclass(frozen=True)
class LateralBuckleState:
    """One point on a Hobbs lateral post-buckling equilibrium path.

    buckle_length_m     L, the characteristic lobe length [m]
    buckle_force_N      P, compressive force inside the buckle [N]
    far_field_force_N   P0, compressive force in the fully-restrained line [N]
    temperature_rise_K  thermal-only dT that generates P0 (= P0 / (EA alpha))
    amplitude_m         y_hat, peak lateral displacement [m]
    max_moment_Nm       M_hat, peak bending moment magnitude [N m]
    axial_stress_pa     P / A_s [Pa]
    bending_stress_pa   M_hat * r_o / I [Pa]
    combined_stress_pa  axial + extreme-fibre bending [Pa]
    max_slope           peak |dy/dx| [-]; periodic mode only, else None
    """

    mode: HobbsMode
    buckle_length_m: float
    buckle_force_N: float
    far_field_force_N: float
    temperature_rise_K: float
    amplitude_m: float
    max_moment_Nm: float
    axial_stress_pa: float
    bending_stress_pa: float
    combined_stress_pa: float
    max_slope: float | None = None


@dataclass(frozen=True)
class ModeSusceptibility:
    """Screening outcome for one mode against a given driving force.

    critical_state       the minimum-P0 turning point on the path
    driving_force_N      the effective compressive force offered by the line
    utilisation          driving_force / critical far-field force [-]
    susceptible          True when the line can reach the turning point
    """

    mode: HobbsMode
    critical_state: LateralBuckleState
    driving_force_N: float
    utilisation: float
    susceptible: bool
