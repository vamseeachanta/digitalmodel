# ABOUTME: Nonlinearity flag detector for production test conditions
# ABOUTME: Identifies flow conditions that degrade test data quality and nodal reliability

"""
Nonlinearity Flags
==================
Detects operating conditions where multiphase flow nonlinearities make test
data inherently unreliable, independent of test execution quality.

These flags explain *why* a test might be unreliable even when the test
procedure was followed correctly. The key physics:

- Transient flow: reservoir hasn't reached pseudo-steady state → test rate
  does not represent stabilised deliverability
- Slug flow: intermittent liquid slugging makes instantaneous rates
  unrepresentative of average deliverability
- Gas lift instability: oscillating injection changes VLP and invalidates
  the assumed operating point
- Choke criticality: flow is choke-controlled rather than reservoir-controlled;
  small pressure changes cause large rate changes
- High watercut: multiphase correlations lose accuracy above ~80% watercut
"""

from __future__ import annotations

from enum import Enum

from digitalmodel.production_engineering.test_quality_scorer import (
    ProductionTestRecord,
    WellType,
    _MIN_DURATION_HOURS,
)


class NonlinearityFlag(Enum):
    TRANSIENT_FLOW = "transient_flow"
    SLUG_FLOW_LIKELY = "slug_flow_likely"
    GAS_LIFT_INSTABILITY = "gas_lift_instability"
    CHOKE_NEAR_CRITICAL = "choke_near_critical"
    HIGH_WATERCUT = "high_watercut"


# Transient threshold: test must be ≥ 2× minimum duration to approach PSS
_TRANSIENT_DURATION_MULTIPLIER = 2.0

# Slug flow conditions: low-to-medium GOR with watercut above 30%
# (elevated gas fraction prevents slug formation; low GOR + water → slugging)
_SLUG_GOR_THRESHOLD_SCF_BBL = 500.0    # below this → slug flow risk
_SLUG_WATERCUT_THRESHOLD = 0.30         # above this → slug flow risk

# Gas lift instability: injection rate varies > 15% during test
_GAS_LIFT_INSTABILITY_THRESHOLD = 0.15

# Choke criticality: for gas-dominated flow, critical pressure ratio ≈ 0.528
# For liquid-dominated flow, choke criticality is different but use as proxy
_CHOKE_CRITICAL_PRESSURE_RATIO = 0.528

# High watercut threshold
_HIGH_WATERCUT_THRESHOLD = 0.80


class NonlinearityDetector:
    """
    Detects operating conditions where multiphase nonlinearities degrade
    production test reliability.

    Returns a list of NonlinearityFlag instances (empty list = clean test).
    Does not replace the quality scorer — works alongside it to explain
    physics-based sources of unreliability.
    """

    def detect(self, test: ProductionTestRecord) -> list[NonlinearityFlag]:
        """Identify nonlinear flow conditions in this test record."""
        flags: list[NonlinearityFlag] = []

        self._check_transient_flow(test, flags)
        self._check_slug_flow(test, flags)
        self._check_gas_lift_instability(test, flags)
        self._check_choke_criticality(test, flags)
        self._check_high_watercut(test, flags)

        return flags

    def _check_transient_flow(
        self, test: ProductionTestRecord, flags: list[NonlinearityFlag]
    ) -> None:
        min_duration = _MIN_DURATION_HOURS[test.well_type]
        threshold = min_duration * _TRANSIENT_DURATION_MULTIPLIER
        if test.duration_hours < threshold:
            flags.append(NonlinearityFlag.TRANSIENT_FLOW)

    def _check_slug_flow(
        self, test: ProductionTestRecord, flags: list[NonlinearityFlag]
    ) -> None:
        gor = test.gor_scf_per_bbl
        if gor is None:
            return
        if (
            test.watercut > _SLUG_WATERCUT_THRESHOLD
            and gor < _SLUG_GOR_THRESHOLD_SCF_BBL
        ):
            flags.append(NonlinearityFlag.SLUG_FLOW_LIKELY)

    def _check_gas_lift_instability(
        self, test: ProductionTestRecord, flags: list[NonlinearityFlag]
    ) -> None:
        if test.well_type != WellType.GAS_LIFT:
            return
        if (
            test.gas_lift_rate_start_mscfd is None
            or test.gas_lift_rate_end_mscfd is None
        ):
            return

        avg = (test.gas_lift_rate_start_mscfd + test.gas_lift_rate_end_mscfd) / 2.0
        if avg <= 0:
            return

        variation = (
            abs(test.gas_lift_rate_end_mscfd - test.gas_lift_rate_start_mscfd) / avg
        )
        if variation > _GAS_LIFT_INSTABILITY_THRESHOLD:
            flags.append(NonlinearityFlag.GAS_LIFT_INSTABILITY)

    def _check_choke_criticality(
        self, test: ProductionTestRecord, flags: list[NonlinearityFlag]
    ) -> None:
        if test.flowing_wellhead_pressure_psi <= 0:
            return
        pressure_ratio = (
            test.separator_pressure_psi / test.flowing_wellhead_pressure_psi
        )
        if pressure_ratio > _CHOKE_CRITICAL_PRESSURE_RATIO:
            flags.append(NonlinearityFlag.CHOKE_NEAR_CRITICAL)

    def _check_high_watercut(
        self, test: ProductionTestRecord, flags: list[NonlinearityFlag]
    ) -> None:
        if test.watercut >= _HIGH_WATERCUT_THRESHOLD:
            flags.append(NonlinearityFlag.HIGH_WATERCUT)
