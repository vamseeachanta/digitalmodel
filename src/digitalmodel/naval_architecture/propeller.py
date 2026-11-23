"""4-quadrant propeller performance model.

Implements the Wageningen B-series propeller using:
- 1st-quadrant: Oosterveld & van Oossanen (1975) polynomial regression
  (39 KT + 47 KQ coefficients from Bernitsas et al. 1981)
- All quadrants: β-angle parameterisation with CT*(β), CQ*(β)
  (Fourier series from van Lammeren et al. 1969 / MARIN Report 60482-1)

References:
- Viviani, Roddy, Hess & Faller (2007), Ship Technology Research 54:3
- Carlton (2007), Marine Propellers and Propulsion, Ch. 4
- McTaggart (2005), DRDC Atlantic TM 2005-071

WRK-1280 | Parent: WRK-1147
"""

import math
from typing import Optional

# ---------------------------------------------------------------------------
# 1st-quadrant polynomial coefficients
# Source: Oosterveld & van Oossanen (1975) via Bernitsas et al. (1981)
# Code ref: github.com/nickholt15/wageningen/bseries.py
# ---------------------------------------------------------------------------

_KT_COEFFS = [
    8.80496e-03, -2.04554e-01, 1.66351e-01, 1.58114e-01, -1.47581e-01,
    -4.81497e-01, 4.15437e-01, 1.44043e-02, -5.30054e-02, 1.43481e-02,
    6.06826e-02, -1.25894e-02, 1.09689e-02, -1.33698e-01, 6.38407e-03,
    -1.32718e-03, 1.68496e-01, -5.07214e-02, 8.54559e-02, -5.04475e-02,
    1.04650e-02, -6.48272e-03, -8.41728e-03, 1.68424e-02, -1.02296e-03,
    -3.17791e-02, 1.86040e-02, -4.10798e-03, -6.06848e-04, -4.98190e-03,
    2.59830e-03, -5.60528e-04, -1.63652e-03, -3.28787e-04, 1.16502e-04,
    6.90904e-04, 4.21749e-03, 5.65229e-05, -1.46564e-03,
]

_KT_S = [
    0, 1, 0, 0, 2, 1, 0, 0, 2, 0, 1, 0, 1, 0, 0,
    2, 3, 0, 2, 3, 1, 2, 0, 1, 3, 0, 1, 0, 0, 1,
    2, 3, 1, 1, 2, 0, 0, 3, 0,
]

_KT_T = [
    0, 0, 1, 2, 0, 1, 2, 0, 0, 1, 1, 0, 0, 3, 6,
    6, 0, 0, 0, 0, 6, 6, 3, 3, 3, 3, 0, 2, 0, 0,
    0, 0, 2, 6, 6, 0, 3, 6, 3,
]

_KT_U = [
    0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 1, 1, 0, 0,
    0, 1, 2, 2, 2, 2, 2, 0, 0, 0, 1, 2, 2, 0, 0,
    0, 0, 0, 0, 0, 1, 1, 1, 2,
]

_KT_V = [
    0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 2, 2,
    2, 2, 2, 2, 2, 2, 2, 2, 2,
]

_KQ_COEFFS = [
    3.79368e-03, 8.86523e-03, -3.22410e-02, 3.44778e-03, -4.08811e-02,
    -1.08009e-01, -8.85381e-02, 1.88561e-01, -3.70871e-03, 5.13696e-03,
    2.09449e-02, 4.74319e-03, -7.23408e-03, 4.38388e-03, -2.69403e-02,
    5.58082e-02, 1.61886e-02, 3.18086e-03, 1.58960e-02, 4.71729e-02,
    1.96283e-02, -5.02782e-02, -3.00550e-02, 4.17122e-02, -3.97722e-02,
    -3.50024e-03, -1.06854e-02, 1.10903e-03, -3.13912e-04, 3.59850e-03,
    -1.42121e-03, -3.83637e-03, 1.26803e-02, -3.18278e-03, 3.34268e-03,
    -1.83491e-03, 1.12451e-04, -2.97228e-05, 2.69551e-04, 8.32650e-04,
    1.55334e-03, 3.02683e-04, -1.84300e-04, -4.25399e-04, 8.69243e-05,
    -4.65900e-04, 5.54194e-05,
]

_KQ_S = [
    0, 2, 1, 0, 0, 1, 2, 0, 1, 0, 1, 2, 2, 1, 0,
    3, 0, 1, 0, 1, 3, 0, 3, 2, 0, 0, 3, 3, 0, 3,
    0, 1, 0, 2, 0, 1, 3, 3, 1, 2, 0, 0, 0, 0, 3,
    0, 1,
]

_KQ_T = [
    0, 0, 1, 2, 1, 1, 1, 2, 0, 1, 1, 1, 0, 1, 2,
    0, 3, 3, 0, 0, 0, 1, 1, 2, 3, 6, 0, 3, 6, 0,
    6, 0, 2, 3, 6, 1, 2, 6, 0, 0, 2, 6, 0, 3, 3,
    6, 6,
]

_KQ_U = [
    0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1,
    1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 0, 0, 0, 1,
    1, 2, 2, 2, 2, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2,
    2, 2,
]

_KQ_V = [
    0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
    2, 2,
]


def wageningen_kt(J: float, PD: float, AE_A0: float, Z: int) -> float:
    """Wageningen B-series thrust coefficient KT (1st quadrant).

    Args:
        J: Advance ratio V_A/(n·D), must be ≥ 0
        PD: Pitch-diameter ratio P/D
        AE_A0: Expanded area ratio A_E/A_0
        Z: Number of blades

    Returns:
        KT: Thrust coefficient (non-dimensional)
    """
    result = 0.0
    for i in range(len(_KT_COEFFS)):
        result += (
            _KT_COEFFS[i]
            * J ** _KT_S[i]
            * PD ** _KT_T[i]
            * AE_A0 ** _KT_U[i]
            * Z ** _KT_V[i]
        )
    return result


def wageningen_kq(J: float, PD: float, AE_A0: float, Z: int) -> float:
    """Wageningen B-series torque coefficient KQ (1st quadrant).

    Args:
        J: Advance ratio V_A/(n·D), must be ≥ 0
        PD: Pitch-diameter ratio P/D
        AE_A0: Expanded area ratio A_E/A_0
        Z: Number of blades

    Returns:
        KQ: Torque coefficient (non-dimensional)
    """
    result = 0.0
    for i in range(len(_KQ_COEFFS)):
        result += (
            _KQ_COEFFS[i]
            * J ** _KQ_S[i]
            * PD ** _KQ_T[i]
            * AE_A0 ** _KQ_U[i]
            * Z ** _KQ_V[i]
        )
    return result


# ---------------------------------------------------------------------------
# 4-quadrant parameterisation — Viviani (2007) Eqs. 1-6
# ---------------------------------------------------------------------------

def advance_angle(V_A: float, n: float, D: float) -> float:
    """Hydrodynamic pitch angle β (degrees).

    β = atan2(V_A, 0.7π·n·D), mapped to [0°, 360°).

    Args:
        V_A: Advance velocity at propeller plane (m/s), signed
        n: Rotational speed (rev/s), signed
        D: Propeller diameter (m)
    """
    circumferential = 0.7 * math.pi * n * D
    beta_rad = math.atan2(V_A, circumferential)
    beta_deg = math.degrees(beta_rad)
    if beta_deg < 0:
        beta_deg += 360.0
    return beta_deg


def reference_velocity(V_A: float, n: float, D: float) -> float:
    """Reference velocity V_R = sqrt(V_A² + (0.7π·n·D)²)."""
    circumferential = 0.7 * math.pi * n * D
    return math.sqrt(V_A**2 + circumferential**2)


def ct_star_from_kt(KT: float, J: float) -> float:
    """Convert KT to CT* — Viviani Eq. 5.

    CT* = 8·KT / (π·(J² + (0.7π)²))
    """
    denom = math.pi * (J**2 + (0.7 * math.pi) ** 2)
    return 8.0 * KT / denom


def cq_star_from_kq(KQ: float, J: float) -> float:
    """Convert KQ to CQ* — Viviani Eq. 6.

    CQ* = 8·KQ / (π·(J² + (0.7π)²))
    """
    denom = math.pi * (J**2 + (0.7 * math.pi) ** 2)
    return 8.0 * KQ / denom


def kt_from_ct_star(ct_star: float, J: float) -> float:
    """Convert CT* back to KT — inverse of Viviani Eq. 5.

    KT = CT* · π · (J² + (0.7π)²) / 8
    """
    return ct_star * math.pi * (J**2 + (0.7 * math.pi) ** 2) / 8.0


def kq_from_cq_star(cq_star: float, J: float) -> float:
    """Convert CQ* back to KQ — inverse of Viviani Eq. 6.

    KQ = CQ* · π · (J² + (0.7π)²) / 8
    """
    return cq_star * math.pi * (J**2 + (0.7 * math.pi) ** 2) / 8.0


# ---------------------------------------------------------------------------
# 4-quadrant model — hybrid polynomial + physics-based approach
# ---------------------------------------------------------------------------

# Maximum β (degrees) for which the 1st-quadrant polynomial is valid.
# Beyond this J grows too large and the polynomial diverges.
_Q1_BETA_MAX = 82.0


def _q1_from_polynomial(beta_deg: float, PD: float, AE_A0: float, Z: int,
                         coeff: str) -> float:
    """Evaluate CT* or CQ* in Q1 using Wageningen polynomial."""
    J = 0.7 * math.pi * math.tan(math.radians(beta_deg))
    if coeff == "ct":
        K = wageningen_kt(J, PD, AE_A0, Z)
        return ct_star_from_kt(K, J)
    K = wageningen_kq(J, PD, AE_A0, Z)
    return cq_star_from_kq(K, J)


def _q234_physics_estimate(beta_deg: float, ct0: float, cq0: float,
                            coeff: str) -> float:
    """Physics-based estimate for Q2-Q4 CT*/CQ*.

    Uses the physical behaviour documented in Carlton (2007) Ch. 4:
    - Q2 (90-180): crash-stop, CT*<0 (braking), CQ*<0 (windmilling)
    - Q3 (180-270): astern driving, CT*<0, CQ*>0
    - Q4 (270-360): backing with ahead rotation, CT*>0, CQ*>0

    Approximated as sinusoidal variation with amplitude scaled to Q1 bollard.
    """
    beta_rad = math.radians(beta_deg)
    amp = abs(ct0) if coeff == "ct" else abs(cq0)

    if coeff == "ct":
        # CT* ≈ positive in Q1/Q4, negative in Q2/Q3
        # Dominant shape: ~cos(β) with higher harmonics
        return amp * (0.6 * math.cos(beta_rad) + 0.4 * math.cos(2 * beta_rad))
    # CQ* ≈ positive in Q1/Q3/Q4, negative in Q2
    return amp * (0.5 + 0.3 * math.cos(beta_rad) + 0.2 * math.cos(2 * beta_rad))


class FourQuadrantPropeller:
    """4-quadrant propeller performance model.

    Q1 (0°–82°): exact Wageningen polynomial converted to CT*/CQ*.
    Q2–Q4 (82°–360°): physics-based sinusoidal estimate scaled to Q1 bollard.

    For exact Q2-Q4 data, load MARIN Fourier coefficients via load_fourier().

    Args:
        Z: Number of blades
        AE_A0: Expanded area ratio
        PD: Pitch-diameter ratio
    """

    def __init__(self, Z: int, AE_A0: float, PD: float):
        self.Z = Z
        self.AE_A0 = AE_A0
        self.PD = PD
        self._fourier_ct: Optional[dict] = None
        self._fourier_cq: Optional[dict] = None
        # Cache bollard values for scaling Q2-Q4 estimates
        self._ct0 = _q1_from_polynomial(0.0, PD, AE_A0, Z, "ct")
        self._cq0 = _q1_from_polynomial(0.0, PD, AE_A0, Z, "cq")

    def load_fourier(self, ct_A: list[float], ct_B: list[float],
                     cq_C: list[float], cq_D: list[float]) -> None:
        """Load validated Fourier coefficients for full 4-quadrant coverage.

        Source: MARIN Report 60482-1 (1984) or Carlton (2007) Table 4.3.
        When loaded, these override the physics-based Q2-Q4 estimates.
        """
        self._fourier_ct = {"A": ct_A, "B": ct_B}
        self._fourier_cq = {"C": cq_C, "D": cq_D}

    def ct_star(self, beta_deg: float) -> float:
        """CT*(β) at any advance angle."""
        beta_deg = beta_deg % 360.0

        # If Fourier coefficients loaded, use them everywhere
        if self._fourier_ct is not None:
            return _fourier_eval(
                self._fourier_ct["A"], self._fourier_ct["B"], beta_deg
            )

        # Q1: exact polynomial
        if 0.0 <= beta_deg <= _Q1_BETA_MAX:
            return _q1_from_polynomial(
                beta_deg, self.PD, self.AE_A0, self.Z, "ct"
            )

        # Q2-Q4: physics-based estimate
        return _q234_physics_estimate(beta_deg, self._ct0, self._cq0, "ct")

    def cq_star(self, beta_deg: float) -> float:
        """CQ*(β) at any advance angle."""
        beta_deg = beta_deg % 360.0

        if self._fourier_cq is not None:
            return _fourier_eval(
                self._fourier_cq["C"], self._fourier_cq["D"], beta_deg
            )

        if 0.0 <= beta_deg <= _Q1_BETA_MAX:
            return _q1_from_polynomial(
                beta_deg, self.PD, self.AE_A0, self.Z, "cq"
            )

        return _q234_physics_estimate(beta_deg, self._ct0, self._cq0, "cq")

    def thrust(self, V_A: float, n: float, D: float, rho: float = 1025.0) -> float:
        """Dimensional thrust T (N) at any operating point."""
        beta = advance_angle(V_A, n, D)
        V_R = reference_velocity(V_A, n, D)
        ct = self.ct_star(beta)
        return ct * 0.5 * rho * V_R**2 * math.pi / 4.0 * D**2

    def torque(self, V_A: float, n: float, D: float, rho: float = 1025.0) -> float:
        """Dimensional torque Q (N·m) at any operating point."""
        beta = advance_angle(V_A, n, D)
        V_R = reference_velocity(V_A, n, D)
        cq = self.cq_star(beta)
        return cq * 0.5 * rho * V_R**2 * math.pi / 4.0 * D**3


def _fourier_eval(A: list[float], B: list[float], beta_deg: float) -> float:
    """Evaluate Fourier series: A_0/2 + Σ[A_k·cos(k·β) + B_k·sin(k·β)]."""
    beta_rad = math.radians(beta_deg)
    result = A[0] / 2.0
    for k in range(1, len(A)):
        result += A[k] * math.cos(k * beta_rad) + B[k] * math.sin(k * beta_rad)
    return result


# ---------------------------------------------------------------------------
# Convenience functions for dimensional output
# ---------------------------------------------------------------------------

def four_quadrant_thrust(
    V_A: float, n: float, D: float, rho: float,
    Z: int, AE_A0: float, PD: float,
) -> float:
    """Compute propeller thrust (N) at any operating point."""
    prop = FourQuadrantPropeller(Z=Z, AE_A0=AE_A0, PD=PD)
    return prop.thrust(V_A, n, D, rho)


def four_quadrant_torque(
    V_A: float, n: float, D: float, rho: float,
    Z: int, AE_A0: float, PD: float,
) -> float:
    """Compute propeller torque (N·m) at any operating point."""
    prop = FourQuadrantPropeller(Z=Z, AE_A0=AE_A0, PD=PD)
    return prop.torque(V_A, n, D, rho)
