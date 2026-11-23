# ABOUTME: Production test quality scorer — physics-based evaluation of well test data
# ABOUTME: Scores stabilization, duration, drawdown, separator, gas lift criteria

"""
Production Test Quality Scorer
===============================
Evaluates a production test record against physics-based criteria and produces
a quality score (0-100) plus a flag taxonomy for identified issues.

The key insight (WRK-164): bad input data makes nodal analysis unreliable.
This module surfaces that unreliability explicitly rather than letting it
propagate silently into model outputs.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from enum import Enum
from typing import Optional


class WellType(Enum):
    FLOWING = "flowing"
    GAS_LIFT = "gas_lift"
    ESP = "esp"
    ROD_PUMP = "rod_pump"


class QualityFlag(Enum):
    TEST_DURATION_SHORT = "test_duration_short"
    STABILIZATION_INSUFFICIENT = "stabilization_insufficient"
    RATE_UNSTABLE = "rate_unstable"
    PRESSURE_DRAWDOWN_LOW = "pressure_drawdown_low"
    NO_STATIC_PRESSURE = "no_static_pressure"
    SEPARATOR_BACK_PRESSURE_HIGH = "separator_back_pressure_high"
    GAS_LIFT_UNSTABLE = "gas_lift_unstable"


# Minimum test duration (hours) by well type — based on industry practice
_MIN_DURATION_HOURS: dict[WellType, float] = {
    WellType.FLOWING: 4.0,
    WellType.GAS_LIFT: 8.0,
    WellType.ESP: 6.0,
    WellType.ROD_PUMP: 12.0,
}

# Scoring weights — must sum to 100
_WEIGHTS: dict[str, float] = {
    "duration": 25.0,
    "stabilization": 30.0,
    "drawdown": 20.0,
    "separator": 15.0,
    "gas_lift": 10.0,
}

# Thresholds
_RATE_STABLE_THRESHOLD = 0.05       # 5% variation → stable
_RATE_MARGINAL_THRESHOLD = 0.10     # 10% variation → partial credit
_DRAWDOWN_MIN_FRACTION = 0.05       # 5% of static WHP
_SEPARATOR_MAX_FRACTION = 0.80      # separator pressure < 80% of FWHP
_GAS_LIFT_STABLE_THRESHOLD = 0.10   # 10% variation → stable GL


@dataclass
class ProductionTestRecord:
    """
    A single production well test record.

    All rates are at stock-tank conditions. Pressures at wellhead.
    """

    well_id: str
    test_date: str
    well_type: WellType
    duration_hours: float
    oil_rate_bopd: float
    gas_rate_mscfd: float
    water_rate_bwpd: float
    flowing_wellhead_pressure_psi: float
    separator_pressure_psi: float
    static_wellhead_pressure_psi: Optional[float] = None
    rate_start_bopd: Optional[float] = None
    rate_end_bopd: Optional[float] = None
    gas_lift_rate_start_mscfd: Optional[float] = None
    gas_lift_rate_end_mscfd: Optional[float] = None

    @property
    def liquid_rate_blpd(self) -> float:
        return self.oil_rate_bopd + self.water_rate_bwpd

    @property
    def watercut(self) -> float:
        if self.liquid_rate_blpd == 0:
            return 0.0
        return self.water_rate_bwpd / self.liquid_rate_blpd

    @property
    def gor_scf_per_bbl(self) -> Optional[float]:
        if self.oil_rate_bopd == 0:
            return None
        return self.gas_rate_mscfd * 1000.0 / self.oil_rate_bopd


@dataclass
class QualityScore:
    """Quality evaluation result for a production test record."""

    score: float  # 0-100
    flags: list[QualityFlag] = field(default_factory=list)

    @property
    def confidence(self) -> str:
        """Green ≥ 80, Amber 50-79, Red < 50."""
        if self.score >= 80:
            return "Green"
        if self.score >= 50:
            return "Amber"
        return "Red"


class ProductionTestQualityScorer:
    """
    Physics-based quality scorer for production well tests.

    Evaluates five criteria:
    1. Test duration (adequate time to approach pseudo-steady state)
    2. Rate stabilization (rate change during test)
    3. Pressure drawdown (measurable depletion signal)
    4. Separator back-pressure (reasonable flow conditions)
    5. Gas lift stability (consistent injection rate, if applicable)
    """

    def score(self, test: ProductionTestRecord) -> QualityScore:
        """Evaluate test and return quality score with flags."""
        flags: list[QualityFlag] = []
        points = 0.0

        points += self._score_duration(test, flags)
        points += self._score_stabilization(test, flags)
        points += self._score_drawdown(test, flags)
        points += self._score_separator(test, flags)
        points += self._score_gas_lift(test, flags)

        return QualityScore(score=min(100.0, max(0.0, points)), flags=flags)

    def _score_duration(
        self, test: ProductionTestRecord, flags: list[QualityFlag]
    ) -> float:
        min_hours = _MIN_DURATION_HOURS[test.well_type]
        if test.duration_hours >= min_hours:
            return _WEIGHTS["duration"]
        flags.append(QualityFlag.TEST_DURATION_SHORT)
        return 0.0

    def _score_stabilization(
        self, test: ProductionTestRecord, flags: list[QualityFlag]
    ) -> float:
        if test.rate_start_bopd is None or test.rate_end_bopd is None:
            # No endpoint data — partial credit only
            return _WEIGHTS["stabilization"] * 0.5

        avg = (test.rate_start_bopd + test.rate_end_bopd) / 2.0
        if avg <= 0:
            flags.append(QualityFlag.STABILIZATION_INSUFFICIENT)
            return 0.0

        variation = abs(test.rate_end_bopd - test.rate_start_bopd) / avg
        if variation <= _RATE_STABLE_THRESHOLD:
            return _WEIGHTS["stabilization"]
        if variation <= _RATE_MARGINAL_THRESHOLD:
            flags.append(QualityFlag.RATE_UNSTABLE)
            return _WEIGHTS["stabilization"] * 0.5
        flags.append(QualityFlag.STABILIZATION_INSUFFICIENT)
        return 0.0

    def _score_drawdown(
        self, test: ProductionTestRecord, flags: list[QualityFlag]
    ) -> float:
        if test.static_wellhead_pressure_psi is None:
            flags.append(QualityFlag.NO_STATIC_PRESSURE)
            return 0.0

        drawdown = (
            test.static_wellhead_pressure_psi - test.flowing_wellhead_pressure_psi
        ) / test.static_wellhead_pressure_psi

        if drawdown >= _DRAWDOWN_MIN_FRACTION:
            return _WEIGHTS["drawdown"]
        flags.append(QualityFlag.PRESSURE_DRAWDOWN_LOW)
        return 0.0

    def _score_separator(
        self, test: ProductionTestRecord, flags: list[QualityFlag]
    ) -> float:
        threshold = _SEPARATOR_MAX_FRACTION * test.flowing_wellhead_pressure_psi
        if test.separator_pressure_psi < threshold:
            return _WEIGHTS["separator"]
        flags.append(QualityFlag.SEPARATOR_BACK_PRESSURE_HIGH)
        return 0.0

    def _score_gas_lift(
        self, test: ProductionTestRecord, flags: list[QualityFlag]
    ) -> float:
        if test.well_type != WellType.GAS_LIFT:
            return _WEIGHTS["gas_lift"]  # not applicable → full credit

        if (
            test.gas_lift_rate_start_mscfd is None
            or test.gas_lift_rate_end_mscfd is None
        ):
            flags.append(QualityFlag.GAS_LIFT_UNSTABLE)
            return 0.0

        avg = (test.gas_lift_rate_start_mscfd + test.gas_lift_rate_end_mscfd) / 2.0
        if avg <= 0:
            flags.append(QualityFlag.GAS_LIFT_UNSTABLE)
            return 0.0

        variation = (
            abs(test.gas_lift_rate_end_mscfd - test.gas_lift_rate_start_mscfd) / avg
        )
        if variation <= _GAS_LIFT_STABLE_THRESHOLD:
            return _WEIGHTS["gas_lift"]
        flags.append(QualityFlag.GAS_LIFT_UNSTABLE)
        return 0.0
