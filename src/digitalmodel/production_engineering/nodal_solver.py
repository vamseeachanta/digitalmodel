# ABOUTME: Nodal analysis solver — finds IPR/VLP intersection (operating point)
# ABOUTME: Returns operating point with confidence bounds linked to test quality score

"""
Nodal Analysis Solver
=====================
Finds the well operating point: the (q, Pwf) pair where inflow (IPR) equals
outflow (VLP). The intersection is found by bisection on the residual:

    residual(q) = Pwf_IPR(q) - Pwf_VLP(q)

IPR is a decreasing function of q (more drawdown at higher rate).
VLP is an increasing function of q (more friction at higher rate).
The curves cross exactly once for a well-behaved system.

Confidence bounds are derived from the test quality score:
- Green (≥80): ±5% uncertainty on rate
- Amber (50-79): ±15% uncertainty on rate
- Red (<50):   ±30% uncertainty on rate
- No score:    Amber default (±15%)
"""

from __future__ import annotations

from dataclasses import dataclass
from enum import Enum
from typing import Optional, Protocol

from scipy.optimize import brentq

from digitalmodel.production_engineering.vlp_correlations import (
    FlowConditions,
    FluidProperties,
    TubingConfig,
    hagedorn_brown_pwf,
)


# ---------------------------------------------------------------------------
# Confidence classification
# ---------------------------------------------------------------------------

class ConfidenceBound(Enum):
    GREEN = "Green"
    AMBER = "Amber"
    RED = "Red"


_QUALITY_BOUNDS: dict[ConfidenceBound, float] = {
    ConfidenceBound.GREEN: 0.05,   # ±5%
    ConfidenceBound.AMBER: 0.15,   # ±15%
    ConfidenceBound.RED: 0.30,     # ±30%
}


def _classify_confidence(quality_score: Optional[float]) -> ConfidenceBound:
    if quality_score is None:
        return ConfidenceBound.AMBER
    if quality_score >= 80:
        return ConfidenceBound.GREEN
    if quality_score >= 50:
        return ConfidenceBound.AMBER
    return ConfidenceBound.RED


# ---------------------------------------------------------------------------
# Protocol for IPR models (structural typing)
# ---------------------------------------------------------------------------

class IprModel(Protocol):
    def flow_rate(self, pwf_psi: float) -> float: ...
    def flowing_pressure(self, q_bopd: float) -> float: ...


# ---------------------------------------------------------------------------
# Operating point result
# ---------------------------------------------------------------------------

@dataclass
class NodalOperatingPoint:
    """Result of nodal analysis — well operating point."""

    q_bopd: float               # Operating liquid rate (bbl/d)
    pwf_psi: float              # Bottomhole flowing pressure (psi)
    confidence: ConfidenceBound
    q_uncertainty_fraction: float  # Fractional uncertainty (e.g. 0.10 = ±10%)

    @property
    def q_low_bopd(self) -> float:
        return self.q_bopd * (1.0 - self.q_uncertainty_fraction)

    @property
    def q_high_bopd(self) -> float:
        return self.q_bopd * (1.0 + self.q_uncertainty_fraction)


# ---------------------------------------------------------------------------
# Solver
# ---------------------------------------------------------------------------

class NodalSolver:
    """
    Finds the well operating point by solving IPR(q) = VLP(q).

    Uses Brent's method on the rate residual. The search bracket is
    [0, qmax_ipr] where qmax_ipr = IPR(Pwf=0).
    """

    def solve(
        self,
        ipr: IprModel,
        tubing: TubingConfig,
        fluid: FluidProperties,
        watercut: float,
        gor_scf_per_bbl: float,
        whp_psi: float,
        test_quality_score: Optional[float] = None,
        correlation: str = "hagedorn_brown",
        q_search_max_bopd: float = 10_000.0,
    ) -> NodalOperatingPoint:
        """
        Solve for operating point where IPR equals VLP.

        Args:
            ipr:                  IPR model (Vogel, Fetkovich, Composite, etc.)
            tubing:               Tubing geometry
            fluid:                Fluid PVT properties
            watercut:             Water fraction (0-1)
            gor_scf_per_bbl:      Producing GOR
            whp_psi:              Wellhead pressure (psi)
            test_quality_score:   Optional quality score (0-100); drives confidence bounds
            correlation:          VLP correlation to use
            q_search_max_bopd:    Upper bound for rate search

        Returns:
            NodalOperatingPoint with rate, pressure, and confidence bounds.
        """

        def vlp_pwf(q: float) -> float:
            flow = FlowConditions(
                q_l_bopd=max(1.0, q),
                watercut=watercut,
                gor_scf_per_bbl=gor_scf_per_bbl,
            )
            return hagedorn_brown_pwf(flow, tubing, fluid, whp_psi)

        def ipr_pwf(q: float) -> float:
            return ipr.flowing_pressure(q)

        def residual(q: float) -> float:
            return ipr_pwf(q) - vlp_pwf(q)

        # Find rate bounds where residual changes sign
        q_low, q_high = self._find_bracket(
            residual, q_search_max_bopd, ipr
        )

        q_op = brentq(residual, q_low, q_high, xtol=0.1, maxiter=100)
        pwf_op = vlp_pwf(q_op)

        confidence = _classify_confidence(test_quality_score)
        uncertainty = _QUALITY_BOUNDS[confidence]

        return NodalOperatingPoint(
            q_bopd=q_op,
            pwf_psi=pwf_op,
            confidence=confidence,
            q_uncertainty_fraction=uncertainty,
        )

    def _find_bracket(
        self,
        residual_fn,
        q_search_max: float,
        ipr: IprModel,
    ) -> tuple[float, float]:
        """
        Find (q_low, q_high) such that residual changes sign.
        IPR > VLP at low q (reservoir pressure > backpressure required).
        IPR < VLP at high q (reservoir depleted below what VLP needs).
        """
        q_low = 0.1  # avoid zero exactly
        # Scan upward until residual turns negative
        q_high = min(q_search_max, ipr.flow_rate(0.0) * 1.1)
        q_high = max(q_high, 10.0)

        # Verify bracket validity
        r_low = residual_fn(q_low)
        r_high = residual_fn(q_high)

        if r_low * r_high > 0:
            # Same sign — adjust bracket by scanning
            for q_test in range(10, int(q_high), int(q_high / 20)):
                r_test = residual_fn(float(q_test))
                if r_test * r_low < 0:
                    q_high = float(q_test)
                    break

        return q_low, q_high
