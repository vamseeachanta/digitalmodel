"""VIV screening per DNV-RP-C205 for risers and pipelines.

Calculates reduced velocity, checks lock-in conditions, estimates
natural frequencies for risers/pipelines, and provides screening
pass/fail assessment.

Does NOT require OrcFxAPI — all calculations are analytical.

References:
    - DNV-RP-C205 (2019): Environmental Conditions and Environmental Loads, Section 9
    - DNV-RP-F105 (2017): Free Spanning Pipelines (VIV of pipeline spans)
    - Blevins (1990): Flow-Induced Vibrations
"""

import math
from enum import Enum
from typing import Dict, List, Optional, Tuple

import numpy as np
from pydantic import BaseModel, Field


# ---------------------------------------------------------------------------
# Enums
# ---------------------------------------------------------------------------

class VIVDirection(str, Enum):
    """VIV oscillation direction."""
    CROSS_FLOW = "cross_flow"
    IN_LINE = "in_line"


class BoundaryCondition(str, Enum):
    """End boundary conditions for natural frequency calculation."""
    PINNED_PINNED = "pinned_pinned"
    FIXED_FIXED = "fixed_fixed"
    FIXED_FREE = "fixed_free"
    FIXED_PINNED = "fixed_pinned"


# ---------------------------------------------------------------------------
# Strouhal number
# ---------------------------------------------------------------------------

def strouhal_number(reynolds: float) -> float:
    """Strouhal number as function of Reynolds number.

    Reference: DNV-RP-C205 Table 9-1, Blevins (1990).

    Args:
        reynolds: Reynolds number.

    Returns:
        Strouhal number.
    """
    if reynolds < 300:
        return 0.21
    elif reynolds < 3e5:
        return 0.20
    elif reynolds < 3.5e6:
        return 0.20  # Subcritical to critical — broadband
    else:
        return 0.25  # Supercritical


# ---------------------------------------------------------------------------
# VIV screening input/output
# ---------------------------------------------------------------------------

class VIVScreeningInput(BaseModel):
    """Input for VIV screening assessment.

    Reference: DNV-RP-C205 Section 9.1.
    """
    outer_diameter: float = Field(0.273, gt=0.0, description="Hydrodynamic diameter (m)")
    current_speed: float = Field(0.8, ge=0.0, description="Current speed (m/s)")
    kinematic_viscosity: float = Field(1.19e-6, gt=0.0,
                                        description="Seawater kinematic viscosity (m^2/s)")
    mass_ratio: float = Field(1.5, gt=0.0,
                               description="Mass ratio m* = (m + m_a) / (rho * pi/4 * D^2)")
    damping_ratio: float = Field(0.01, ge=0.0, description="Structural damping ratio zeta")
    lock_in_vr_range: Tuple[float, float] = Field(
        (4.0, 8.0),
        description="Reduced velocity range for cross-flow lock-in",
    )

    @property
    def reynolds_number(self) -> float:
        """Reynolds number Re = V*D/nu."""
        return self.current_speed * self.outer_diameter / self.kinematic_viscosity

    @property
    def st(self) -> float:
        """Strouhal number."""
        return strouhal_number(self.reynolds_number)

    @property
    def vortex_shedding_frequency(self) -> float:
        """Vortex shedding frequency (Hz): f_vs = St * V / D."""
        return self.st * self.current_speed / self.outer_diameter

    @property
    def stability_parameter(self) -> float:
        """Stability parameter Ks = 2 * m* * zeta.

        High Ks (>16) means VIV suppressed.
        Reference: DNV-RP-C205 Section 9.1.3.
        """
        return 2.0 * self.mass_ratio * self.damping_ratio

    def check_reduced_velocity(self, natural_freq: float) -> Dict[str, float]:
        """Check if reduced velocity falls in lock-in range.

        Vr = V / (fn * D)
        Cross-flow lock-in typically: 4 < Vr < 8.
        In-line lock-in typically: 1 < Vr < 3.5.

        Args:
            natural_freq: Natural frequency of the structure (Hz).

        Returns:
            Dict with reduced velocity and lock-in assessment.
        """
        if natural_freq <= 0:
            vr = float("inf")
        else:
            vr = self.current_speed / (natural_freq * self.outer_diameter)

        cf_lock_in = self.lock_in_vr_range[0] <= vr <= self.lock_in_vr_range[1]
        il_lock_in = 1.0 <= vr <= 3.5

        return {
            "reduced_velocity": round(vr, 3),
            "vortex_shedding_freq_Hz": round(self.vortex_shedding_frequency, 4),
            "natural_freq_Hz": round(natural_freq, 4),
            "reynolds_number": round(self.reynolds_number, 0),
            "strouhal_number": round(self.st, 3),
            "cross_flow_lock_in": cf_lock_in,
            "in_line_lock_in": il_lock_in,
            "stability_parameter_Ks": round(self.stability_parameter, 3),
        }


# ---------------------------------------------------------------------------
# Natural frequency estimation
# ---------------------------------------------------------------------------

class BeamProperties(BaseModel):
    """Beam properties for natural frequency estimation.

    Reference: DNV-RP-F105 Section 4.
    """
    length: float = Field(100.0, gt=0.0, description="Span/riser length (m)")
    outer_diameter: float = Field(0.273, gt=0.0, description="Outer diameter (m)")
    inner_diameter: float = Field(0.203, ge=0.0, description="Inner diameter (m)")
    youngs_modulus: float = Field(207e9, gt=0.0, description="Young's modulus (Pa)")
    mass_per_length: float = Field(120.0, gt=0.0, description="Mass per unit length incl. added mass (kg/m)")
    effective_tension: float = Field(0.0, description="Effective tension (N). Positive = tensile.")
    boundary: BoundaryCondition = BoundaryCondition.PINNED_PINNED

    @property
    def moment_of_inertia(self) -> float:
        """Second moment of area I (m^4)."""
        return math.pi / 64.0 * (self.outer_diameter**4 - self.inner_diameter**4)

    @property
    def bending_stiffness(self) -> float:
        """EI (N.m^2)."""
        return self.youngs_modulus * self.moment_of_inertia

    def boundary_constants(self) -> Tuple[float, float]:
        """Return (C_n, C_tension) for the n-th mode.

        C_n: coefficient for beam natural frequency without tension.
        Reference: Blevins (1990) Table 7-2.
        """
        constants = {
            BoundaryCondition.PINNED_PINNED: (math.pi**2, 1.0),
            BoundaryCondition.FIXED_FIXED: (22.373, 0.9),  # 4.73^2
            BoundaryCondition.FIXED_FREE: (3.516, 1.0),    # 1.875^2
            BoundaryCondition.FIXED_PINNED: (15.418, 0.95),  # 3.927^2
        }
        return constants.get(self.boundary, (math.pi**2, 1.0))

    def natural_frequency(self, mode: int = 1) -> float:
        """Estimate natural frequency for mode n (Hz).

        For a tensioned beam:
        f_n = (n^2 * C1 / (2*pi*L^2)) * sqrt(EI/m) * sqrt(1 + T*L^2/(n^2*pi^2*EI))

        Simplified (DNV-RP-F105 Eq. 4.3):
        f_n = (C1 / (2*pi)) * sqrt(EI / (m * L^4)) * sqrt(1 + C2 * S_eff)

        where S_eff = T * L^2 / (pi^2 * EI) is the effective tension parameter.

        Args:
            mode: Mode number (1, 2, 3, ...).

        Returns:
            Natural frequency (Hz).

        Reference: DNV-RP-F105 Section 4.3.
        """
        EI = self.bending_stiffness
        m = self.mass_per_length
        L = self.length
        T = self.effective_tension

        C1, C_t = self.boundary_constants()

        # Beam frequency (no tension)
        f_beam = (mode**2 * C1) / (2.0 * math.pi * L**2) * math.sqrt(EI / m)

        # Tension correction
        S_eff = T * L**2 / (mode**2 * math.pi**2 * EI) if EI > 0 else 0
        tension_factor = math.sqrt(max(0, 1.0 + C_t * S_eff))

        return f_beam * tension_factor

    def natural_frequencies(self, n_modes: int = 5) -> List[Dict[str, float]]:
        """Calculate natural frequencies for the first n modes.

        Args:
            n_modes: Number of modes to compute.

        Returns:
            List of dicts with mode number and frequency.
        """
        return [
            {"mode": n, "frequency_Hz": round(self.natural_frequency(n), 4)}
            for n in range(1, n_modes + 1)
        ]


# ---------------------------------------------------------------------------
# VIV screening assessment
# ---------------------------------------------------------------------------

class VIVScreeningResult(BaseModel):
    """Overall VIV screening result."""
    screening_pass: bool = Field(..., description="True if VIV screening passes (no lock-in)")
    modes_checked: int = Field(..., description="Number of modes checked")
    critical_mode: Optional[int] = Field(None, description="Mode number at risk of lock-in")
    critical_vr: Optional[float] = Field(None, description="Reduced velocity of critical mode")
    details: List[Dict] = Field(default_factory=list, description="Per-mode check details")


def viv_screening(
    viv_input: VIVScreeningInput,
    beam: BeamProperties,
    n_modes: int = 10,
) -> VIVScreeningResult:
    """Perform VIV screening assessment for a riser or pipeline span.

    Checks reduced velocity for each mode against lock-in criteria.

    Args:
        viv_input: VIV screening parameters.
        beam: Beam properties for natural frequency calculation.
        n_modes: Number of modes to check.

    Returns:
        VIVScreeningResult with pass/fail and details.

    Reference: DNV-RP-C205 Section 9, DNV-RP-F105 Section 5.
    """
    details = []
    critical_mode = None
    critical_vr = None
    overall_pass = True

    for mode_n in range(1, n_modes + 1):
        fn = beam.natural_frequency(mode_n)
        check = viv_input.check_reduced_velocity(fn)
        check["mode"] = mode_n

        if check["cross_flow_lock_in"] or check["in_line_lock_in"]:
            overall_pass = False
            if critical_mode is None:
                critical_mode = mode_n
                critical_vr = check["reduced_velocity"]

        details.append(check)

    return VIVScreeningResult(
        screening_pass=overall_pass,
        modes_checked=n_modes,
        critical_mode=critical_mode,
        critical_vr=critical_vr,
        details=details,
    )


def estimate_response_amplitude(
    reduced_velocity: float,
    outer_diameter: float,
    stability_parameter: float,
) -> float:
    """Estimate cross-flow VIV response amplitude A/D.

    Simplified empirical relation from DNV-RP-C205 Figure 9-3:
    A/D ≈ f(Vr, Ks)

    For Ks < 1:  A/D ≈ 1.0 in lock-in range
    For Ks > 16: A/D ≈ 0 (VIV suppressed)

    Args:
        reduced_velocity: Reduced velocity Vr.
        outer_diameter: Hydrodynamic diameter (m).
        stability_parameter: Stability parameter Ks.

    Returns:
        Estimated A/D ratio (amplitude / diameter).

    Reference: DNV-RP-C205 Figure 9-3.
    """
    # Check if in lock-in range (cross-flow: 4-8)
    if reduced_velocity < 4.0 or reduced_velocity > 8.0:
        return 0.0

    # Peak A/D vs Ks (simplified exponential decay)
    if stability_parameter <= 0:
        a_d_peak = 1.0
    else:
        a_d_peak = 1.0 * math.exp(-0.15 * stability_parameter)

    # Parabolic shape within lock-in range
    vr_center = 6.0
    vr_width = 2.0
    shape = max(0, 1.0 - ((reduced_velocity - vr_center) / vr_width) ** 2)

    return round(a_d_peak * shape, 4)
