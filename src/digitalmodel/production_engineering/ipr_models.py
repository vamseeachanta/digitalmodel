# ABOUTME: Inflow Performance Relationship (IPR) models for production engineering
# ABOUTME: Implements Vogel (1968), Fetkovich (1973), Linear PI, and Composite IPR

"""
Inflow Performance Relationship (IPR) Models
=============================================
IPR curves describe the relationship between bottomhole flowing pressure (Pwf)
and liquid production rate (q). Each model applies to specific reservoir
and fluid conditions.

Models implemented:
- LinearIpr    — PI model; valid above bubble point (undersaturated oil)
- VogelIpr     — Solution-gas drive below bubble point (Vogel 1968, JPT)
- FetkovichIpr — Isochronal test-based, generalised flow exponent (SPE 4529)
- CompositeIpr — Linear above Pb + Vogel below Pb (Klins-Clark composite)

All models expose the same interface:
    flow_rate(pwf_psi)    → q (bopd)
    flowing_pressure(q)   → Pwf (psi)
"""

from __future__ import annotations

import math
from dataclasses import dataclass
from typing import Optional

from scipy.optimize import brentq


@dataclass
class ReservoirConditions:
    """Static reservoir parameters for IPR calculations."""

    reservoir_pressure_psi: float
    bubble_point_psi: float
    productivity_index_bopd_psi: float  # PI = dq/d(Pr - Pwf) above bubble point


# ---------------------------------------------------------------------------
# Linear IPR (PI model) — above bubble point
# ---------------------------------------------------------------------------

class LinearIpr:
    """
    Linear (Productivity Index) IPR.
    q = PI * (Pr - Pwf)

    Valid for undersaturated oil (Pwf > bubble point pressure).
    PI = dq/dΔP is measured from build-up or flow tests.
    """

    def __init__(self, reservoir: ReservoirConditions, pi_bopd_psi: float) -> None:
        self.reservoir = reservoir
        self.pi = pi_bopd_psi

    def flow_rate(self, pwf_psi: float) -> float:
        """Return liquid rate (bopd) at flowing bottomhole pressure Pwf."""
        q = self.pi * max(0.0, self.reservoir.reservoir_pressure_psi - pwf_psi)
        return max(0.0, q)

    def flowing_pressure(self, q_bopd: float) -> float:
        """Return Pwf (psi) at given liquid rate."""
        return self.reservoir.reservoir_pressure_psi - q_bopd / self.pi


# ---------------------------------------------------------------------------
# Vogel IPR — solution-gas drive below bubble point
# ---------------------------------------------------------------------------

class VogelIpr:
    """
    Vogel (1968) IPR for solution-gas drive reservoirs.
    q/qmax = 1 - 0.2*(Pwf/Pr) - 0.8*(Pwf/Pr)^2

    Reference: Vogel, J.V. (1968). Inflow Performance Relationships for
    Solution-Gas Drive Wells. JPT, 20(1), 83-93.
    """

    def __init__(
        self,
        reservoir: ReservoirConditions,
        qmax_bopd: float,
    ) -> None:
        self.reservoir = reservoir
        self.qmax_bopd = qmax_bopd

    @classmethod
    def from_productivity_index(
        cls,
        reservoir: ReservoirConditions,
        pi_bopd_psi: float,
    ) -> "VogelIpr":
        """
        Build Vogel IPR from PI measured at test conditions.
        Standard derivation: qmax = PI * Pr / 1.8
        """
        qmax = pi_bopd_psi * reservoir.reservoir_pressure_psi / 1.8
        return cls(reservoir=reservoir, qmax_bopd=qmax)

    def flow_rate(self, pwf_psi: float) -> float:
        """Return liquid rate (bopd) at flowing bottomhole pressure Pwf."""
        pr = self.reservoir.reservoir_pressure_psi
        ratio = max(0.0, min(1.0, pwf_psi / pr))
        q = self.qmax_bopd * (1.0 - 0.2 * ratio - 0.8 * ratio**2)
        return max(0.0, q)

    def flowing_pressure(self, q_bopd: float) -> float:
        """Return Pwf (psi) at given liquid rate using quadratic formula."""
        pr = self.reservoir.reservoir_pressure_psi
        # 0.8*(Pwf/Pr)^2 + 0.2*(Pwf/Pr) + (q/qmax - 1) = 0
        # Use quadratic: x = Pwf/Pr
        a, b = 0.8, 0.2
        c = q_bopd / self.qmax_bopd - 1.0
        discriminant = b**2 - 4 * a * c
        if discriminant < 0:
            return 0.0
        x = (-b + math.sqrt(discriminant)) / (2 * a)
        return max(0.0, min(pr, x * pr))


# ---------------------------------------------------------------------------
# Fetkovich IPR — isochronal / multirate tests
# ---------------------------------------------------------------------------

class FetkovichIpr:
    """
    Fetkovich (1973) empirical IPR for oil and gas wells.
    q = C * (Pr^2 - Pwf^2)^n

    n is the flow exponent:
    - n = 1.0: Darcy (laminar) flow
    - n = 0.5: fully turbulent flow
    - 0.5 < n < 1.0: transitional flow

    Reference: Fetkovich, M.J. (1973). The Isochronal Testing of Oil Wells.
    SPE 4529.
    """

    def __init__(
        self,
        reservoir: ReservoirConditions,
        c_coeff: float,
        n_exponent: float = 1.0,
    ) -> None:
        self.reservoir = reservoir
        self.c = c_coeff
        self.n = n_exponent

    def flow_rate(self, pwf_psi: float) -> float:
        """Return liquid rate (bopd) at Pwf."""
        pr = self.reservoir.reservoir_pressure_psi
        delta_p_squared = max(0.0, pr**2 - pwf_psi**2)
        return self.c * delta_p_squared**self.n

    def flowing_pressure(self, q_bopd: float) -> float:
        """Return Pwf (psi) at given rate using inverse of Fetkovich equation."""
        pr = self.reservoir.reservoir_pressure_psi
        if self.c <= 0 or q_bopd <= 0:
            return pr
        delta_p_sq = (q_bopd / self.c) ** (1.0 / self.n)
        pwf_sq = max(0.0, pr**2 - delta_p_sq)
        return math.sqrt(pwf_sq)


# ---------------------------------------------------------------------------
# Composite IPR — Klins-Clark: linear above Pb, Vogel below Pb
# ---------------------------------------------------------------------------

class CompositeIpr:
    """
    Composite IPR (Klins-Clark, 1993).
    - Above bubble point (Pwf ≥ Pb): linear, q = PI*(Pr - Pwf)
    - Below bubble point (Pwf < Pb): Vogel extension from q_b

    This properly models the kink at bubble point where dissolved gas
    comes out of solution and significantly changes the flow behaviour.
    """

    def __init__(self, reservoir: ReservoirConditions) -> None:
        self.reservoir = reservoir
        self._pi = reservoir.productivity_index_bopd_psi
        pr = reservoir.reservoir_pressure_psi
        pb = reservoir.bubble_point_psi
        # Rate at bubble point from linear segment
        self._q_b = self._pi * max(0.0, pr - pb)
        # AOF contribution from Vogel extension below Pb
        # q_max_vogel = q_b * (1 + PI*Pb/1.8/q_b) when q_b > 0
        # Standard formula: qmax = q_b + PI*Pb/1.8
        self._qmax = self._q_b + self._pi * pb / 1.8

    def flow_rate(self, pwf_psi: float) -> float:
        """Return liquid rate (bopd) at Pwf."""
        pb = self.reservoir.bubble_point_psi

        if pwf_psi >= pb:
            # Linear region above bubble point
            pr = self.reservoir.reservoir_pressure_psi
            return max(0.0, self._pi * (pr - pwf_psi))

        # Vogel region below bubble point
        ratio = pwf_psi / pb
        vogel_factor = 1.0 - 0.2 * ratio - 0.8 * ratio**2
        q = self._q_b + (self._qmax - self._q_b) * vogel_factor
        return max(0.0, q)

    def flowing_pressure(self, q_bopd: float) -> Optional[float]:
        """Return Pwf (psi) at given rate (bisection search)."""
        pr = self.reservoir.reservoir_pressure_psi
        if q_bopd <= 0:
            return pr
        if q_bopd >= self._qmax:
            return 0.0

        def residual(pwf: float) -> float:
            return self.flow_rate(pwf) - q_bopd

        return brentq(residual, 0.0, pr, xtol=0.1, maxiter=100)
