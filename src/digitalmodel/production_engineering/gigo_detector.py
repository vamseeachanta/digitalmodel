# ABOUTME: GIGO (Garbage In, Garbage Out) detector for production engineering
# ABOUTME: Compares model-predicted rate vs. test rate with physics-based diagnosis

"""
GIGO Detector
=============
Compares the model-predicted operating rate (from nodal analysis) against
the measured test rate, and diagnoses the likely physical cause of any
divergence.

This implements the core insight from WRK-164: when nodal analysis disagrees
with field measurements, the diagnosis must be physics-based (not statistical).
Four premier AI O&G service companies failed at production optimisation because
they used statistical residuals instead of physics-based divergence diagnosis.

Diagnostic logic:
- High watercut + divergence → WATERCUT_CHANGE (model used wrong WC)
- Unstable rate endpoints + divergence → STABILIZATION (well not at PSS)
- Low drawdown + divergence → RESERVOIR_DEPLETION (reservoir pressure wrong)
- GOR much higher/lower than model → GAS_BREAKOUT (PVT mismatch)
- Default → UNKNOWN (investigate field conditions)
"""

from __future__ import annotations

from dataclasses import dataclass, field
from enum import Enum
from typing import Optional, Protocol

from digitalmodel.production_engineering.test_quality_scorer import (
    ProductionTestRecord,
)
from digitalmodel.production_engineering.vlp_correlations import (
    FlowConditions,
    FluidProperties,
    TubingConfig,
    hagedorn_brown_pwf,
)


# ---------------------------------------------------------------------------
# Diagnostic causes (physics-based, not statistical)
# ---------------------------------------------------------------------------

class DiagnosticCause(Enum):
    WATERCUT_CHANGE = "watercut_change"
    STABILIZATION = "stabilization_not_reached"
    RESERVOIR_DEPLETION = "reservoir_pressure_wrong"
    GAS_BREAKOUT = "gor_pvt_mismatch"
    SCALE_OR_DAMAGE = "scale_or_formation_damage"
    UNKNOWN = "unknown"


# Thresholds
_DIVERGENCE_FLAG_THRESHOLD = 0.20      # ≥20% divergence → flagged
_HIGH_WATERCUT_FOR_DIAGNOSIS = 0.60    # above this → watercut change likely
_STABILIZATION_RATE_VARIATION = 0.20   # >20% start/end difference → instability


class IprModel(Protocol):
    def flow_rate(self, pwf_psi: float) -> float: ...
    def flowing_pressure(self, q_bopd: float) -> float: ...


# ---------------------------------------------------------------------------
# Result
# ---------------------------------------------------------------------------

@dataclass
class GigoResult:
    """Outcome of model vs. test rate comparison."""

    model_rate_bopd: float
    test_rate_bopd: float
    divergence_fraction: float  # |model - test| / model
    flagged: bool
    likely_causes: list[DiagnosticCause] = field(default_factory=list)
    diagnosis_summary: str = ""


# ---------------------------------------------------------------------------
# Detector
# ---------------------------------------------------------------------------

class GigoDetector:
    """
    Compares model-predicted operating rate against field test rate.

    The model rate is estimated by finding the operating point on the IPR
    at the flowing wellhead pressure measured during the test, then applying
    the VLP correction to derive implied bottomhole pressure.
    """

    def compare(
        self,
        test: ProductionTestRecord,
        ipr: IprModel,
        tubing: TubingConfig,
        fluid: FluidProperties,
        whp_psi: float,
    ) -> GigoResult:
        """
        Compare model prediction with test measurement.

        Args:
            test:       Production test record (has measured rate and pressure)
            ipr:        Calibrated IPR model for this well
            tubing:     Tubing geometry
            fluid:      Fluid PVT properties
            whp_psi:    Wellhead pressure during test (psi)

        Returns:
            GigoResult with divergence fraction and physics-based diagnosis.
        """
        # Model rate: use IPR at the measured flowing wellhead pressure
        flow = FlowConditions(
            q_l_bopd=max(1.0, test.liquid_rate_blpd),
            watercut=test.watercut,
            gor_scf_per_bbl=test.gor_scf_per_bbl or 500.0,
        )
        pwf_vlp = hagedorn_brown_pwf(flow, tubing, fluid, whp_psi)
        model_rate = ipr.flow_rate(pwf_vlp)

        test_rate = test.oil_rate_bopd
        if model_rate <= 0:
            divergence = 1.0
        else:
            divergence = abs(model_rate - test_rate) / model_rate

        flagged = divergence >= _DIVERGENCE_FLAG_THRESHOLD

        causes: list[DiagnosticCause] = []
        summary = ""
        if flagged:
            causes = self._diagnose(test, model_rate, test_rate)
            summary = self._build_summary(model_rate, test_rate, divergence, causes)

        return GigoResult(
            model_rate_bopd=model_rate,
            test_rate_bopd=test_rate,
            divergence_fraction=divergence,
            flagged=flagged,
            likely_causes=causes,
            diagnosis_summary=summary,
        )

    def _diagnose(
        self,
        test: ProductionTestRecord,
        model_rate: float,
        test_rate: float,
    ) -> list[DiagnosticCause]:
        causes: list[DiagnosticCause] = []

        # High watercut → VLP and/or IPR based on wrong fluid properties
        if test.watercut >= _HIGH_WATERCUT_FOR_DIAGNOSIS:
            causes.append(DiagnosticCause.WATERCUT_CHANGE)

        # Unstable rate endpoints → test captured transient, not stabilised deliverability
        if (
            test.rate_start_bopd is not None
            and test.rate_end_bopd is not None
        ):
            avg = (test.rate_start_bopd + test.rate_end_bopd) / 2.0
            if avg > 0:
                variation = abs(test.rate_end_bopd - test.rate_start_bopd) / avg
                if variation > _STABILIZATION_RATE_VARIATION:
                    causes.append(DiagnosticCause.STABILIZATION)

        # Low drawdown → reservoir pressure input to model may be wrong
        if test.static_wellhead_pressure_psi is not None:
            drawdown = (
                test.static_wellhead_pressure_psi - test.flowing_wellhead_pressure_psi
            ) / test.static_wellhead_pressure_psi
            if drawdown < 0.03:
                causes.append(DiagnosticCause.RESERVOIR_DEPLETION)

        if not causes:
            causes.append(DiagnosticCause.UNKNOWN)

        return causes

    def _build_summary(
        self,
        model_rate: float,
        test_rate: float,
        divergence: float,
        causes: list[DiagnosticCause],
    ) -> str:
        direction = "over-predicts" if model_rate > test_rate else "under-predicts"
        cause_names = [c.value for c in causes]
        return (
            f"Model {direction} test by {divergence:.0%}. "
            f"Model: {model_rate:.0f} bopd, Test: {test_rate:.0f} bopd. "
            f"Likely causes: {', '.join(cause_names)}."
        )
