# ABOUTME: Test-to-model reconciliation workflow for production engineering
# ABOUTME: Structured pipeline: QC → nonlinearity detect → GIGO → confidence → recommend

"""
Test-to-Model Reconciliation Workflow
======================================
Runs the full structured pipeline from a raw production test record to
a confidence-classified nodal model.

Pipeline stages:
    1. QC (quality score)     — score the raw test record
    2. Nonlinearity detection — flag physics-based reliability issues
    3. GIGO comparison        — compare model prediction vs. test rate
    4. Confidence assignment  — combine QC score + GIGO into overall confidence
    5. Recommendations        — specific, actionable next steps

This is the end-to-end workflow a production engineer would use to
answer: "Can I trust the nodal analysis output for this well?"

References:
    Optimization path: Good test → Reliable model → Valid nodal → Real optimization.
    (WRK-164 user voice — breaking this chain at step 1 corrupts all downstream.)
"""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import Optional, Protocol

from digitalmodel.production_engineering.gigo_detector import (
    GigoDetector,
    GigoResult,
)
from digitalmodel.production_engineering.nonlinearity_flags import (
    NonlinearityDetector,
    NonlinearityFlag,
)
from digitalmodel.production_engineering.test_quality_scorer import (
    ProductionTestQualityScorer,
    ProductionTestRecord,
    QualityFlag,
    QualityScore,
)
from digitalmodel.production_engineering.vlp_correlations import (
    FluidProperties,
    TubingConfig,
)


class IprModel(Protocol):
    def flow_rate(self, pwf_psi: float) -> float: ...
    def flowing_pressure(self, q_bopd: float) -> float: ...


class WorkflowStage:
    QC = "qc"
    NONLINEARITY = "nonlinearity"
    GIGO = "gigo"
    CONFIDENCE = "confidence"
    RECOMMENDATIONS = "recommendations"


@dataclass
class ReconciliationResult:
    """
    Full output of the test-to-model reconciliation workflow.

    Attributes:
        qc_score:             Quality score from the scorer (0-100 + flags)
        nonlinearity_flags:   Physics-based reliability flags
        gigo_result:          Model vs. test rate comparison
        confidence:           Overall confidence classification (Green/Amber/Red)
        recommendations:      Actionable next steps for the engineer
    """

    qc_score: Optional[QualityScore] = None
    nonlinearity_flags: Optional[list[NonlinearityFlag]] = None
    gigo_result: Optional[GigoResult] = None
    confidence: str = "Red"
    recommendations: list[str] = field(default_factory=list)


class ReconciliationWorkflow:
    """
    Orchestrates the full QC → confidence → recommendation pipeline.

    Usage:
        workflow = ReconciliationWorkflow()
        result = workflow.run(test=test_record, ipr=ipr_model, ...)
    """

    def __init__(self) -> None:
        self._scorer = ProductionTestQualityScorer()
        self._nl_detector = NonlinearityDetector()
        self._gigo = GigoDetector()

    def run(
        self,
        test: ProductionTestRecord,
        ipr: IprModel,
        tubing: TubingConfig,
        fluid: FluidProperties,
    ) -> ReconciliationResult:
        """
        Run the full reconciliation pipeline.

        Args:
            test:    Production test record to evaluate
            ipr:     IPR model calibrated for this well
            tubing:  Tubing geometry
            fluid:   Fluid PVT properties

        Returns:
            ReconciliationResult with all stage outputs and recommendations.
        """
        result = ReconciliationResult()

        # Stage 1: QC scoring
        result.qc_score = self._scorer.score(test)

        # Stage 2: Nonlinearity detection
        result.nonlinearity_flags = self._nl_detector.detect(test)

        # Stage 3: GIGO comparison
        result.gigo_result = self._gigo.compare(
            test=test,
            ipr=ipr,
            tubing=tubing,
            fluid=fluid,
            whp_psi=test.flowing_wellhead_pressure_psi,
        )

        # Stage 4: Overall confidence
        result.confidence = self._assign_confidence(
            result.qc_score, result.gigo_result
        )

        # Stage 5: Recommendations
        result.recommendations = self._build_recommendations(
            result.qc_score, result.nonlinearity_flags, result.gigo_result
        )

        return result

    def _assign_confidence(
        self, qc: QualityScore, gigo: GigoResult
    ) -> str:
        """Combine QC score and GIGO divergence into overall confidence."""
        # Start from QC confidence
        conf = qc.confidence  # Green / Amber / Red

        # Downgrade if GIGO flagged a large divergence
        if gigo.flagged:
            if conf == "Green":
                conf = "Amber"
            elif conf == "Amber":
                conf = "Red"

        # Downgrade if multiple nonlinearity flags (handled in recommendations)
        return conf

    def _build_recommendations(
        self,
        qc: QualityScore,
        nl_flags: list[NonlinearityFlag],
        gigo: GigoResult,
    ) -> list[str]:
        """Generate specific, actionable recommendations."""
        recs: list[str] = []

        # Duration issues
        if QualityFlag.TEST_DURATION_SHORT in qc.flags:
            recs.append(
                "Extend test duration to meet minimum stabilization time for well type."
            )

        # Stabilization issues
        if QualityFlag.STABILIZATION_INSUFFICIENT in qc.flags:
            recs.append(
                "Re-test: rate variation during test exceeds 10%. "
                "Well was not at pseudo-steady state."
            )
        elif QualityFlag.RATE_UNSTABLE in qc.flags:
            recs.append(
                "Rate variation (5-10%) suggests marginal stabilization. "
                "Consider longer test or verify separator conditions."
            )

        # Drawdown issues
        if QualityFlag.PRESSURE_DRAWDOWN_LOW in qc.flags:
            recs.append(
                "Drawdown < 5% — test may be choke-limited rather than reservoir-limited. "
                "Open choke further or check for near-wellbore damage."
            )
        if QualityFlag.NO_STATIC_PRESSURE in qc.flags:
            recs.append(
                "Record static wellhead pressure before opening well for future tests."
            )

        # Separator back-pressure
        if QualityFlag.SEPARATOR_BACK_PRESSURE_HIGH in qc.flags:
            recs.append(
                "Separator back-pressure exceeds 80% of flowing WHP. "
                "Check separator liquid level, gas handling capacity, and choke size."
            )

        # Gas lift instability
        if QualityFlag.GAS_LIFT_UNSTABLE in qc.flags:
            recs.append(
                "Gas lift injection rate unstable during test. "
                "Stabilise injection before recording test rates."
            )

        # Nonlinearity flags
        if NonlinearityFlag.TRANSIENT_FLOW in nl_flags:
            recs.append(
                "Test duration insufficient for pseudo-steady state. "
                "Rates reflect transient deliverability, not stabilised performance."
            )

        if NonlinearityFlag.SLUG_FLOW_LIKELY in nl_flags:
            recs.append(
                "Slug flow conditions indicated (low GOR + watercut > 30%). "
                "Instantaneous test rates may not represent average deliverability."
            )

        if NonlinearityFlag.HIGH_WATERCUT in nl_flags:
            recs.append(
                "Watercut > 80%: VLP correlations lose accuracy at high water fractions. "
                "Consider water-based VLP correlation or empirical tubing performance data."
            )

        # GIGO divergence
        if gigo.flagged:
            recs.append(
                f"Model/test divergence {gigo.divergence_fraction:.0%}. "
                f"Diagnosis: {gigo.diagnosis_summary}"
            )

        if not recs:
            recs.append(
                "Test data quality is acceptable. Model and test are consistent. "
                "Proceed with nodal analysis calibration."
            )

        return recs
