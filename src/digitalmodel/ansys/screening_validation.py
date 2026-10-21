# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Offline content checks for supported, in-plane screening result digests.

Criteria are required inputs; no material allowable or acceptance tolerance is
inferred. A content pass establishes neither execution provenance nor native
qualification, mesh convergence, bearing capacity or out-of-plane equilibrium.
"""

from __future__ import annotations

import math
from dataclasses import dataclass
from typing import Literal

from digitalmodel.ansys.results_extractor import ResultsExtractor

CheckStatus = Literal["PASS", "FAIL", "NOT_EVALUATED"]


@dataclass(frozen=True)
class ScreeningCriteria:
    """Independent case inputs and absolute tolerances, with their basis.

    ``source`` identifies the basis of every input, including the tolerances;
    it is a caller declaration, not independently authenticated provenance.
    Stress quantities use MPa; force quantities use N; unity is dimensionless.
    """

    allowable_mpa: float
    yield_strength_mpa: float
    expected_applied_fx_n: float
    expected_applied_fy_n: float
    force_tolerance_n: float
    stress_tolerance_mpa: float
    unity_tolerance: float
    source: str

    def __post_init__(self) -> None:
        """Require finite case inputs and nonnegative absolute tolerances."""
        for name, value in vars(self).items():
            if name == "source":
                continue
            if isinstance(value, bool) or not isinstance(value, (int, float)):
                raise ValueError(f"{name} must be a finite number")
            if not math.isfinite(value):
                raise ValueError(f"{name} must be finite")
        if self.allowable_mpa <= 0 or self.yield_strength_mpa <= 0:
            raise ValueError("allowable and yield strength must be positive")
        tolerances = (
            self.force_tolerance_n,
            self.stress_tolerance_mpa,
            self.unity_tolerance,
        )
        if min(tolerances) < 0:
            raise ValueError("absolute tolerances must be nonnegative")
        load = math.hypot(self.expected_applied_fx_n, self.expected_applied_fy_n)
        if not math.isfinite(load) or load <= 0:
            raise ValueError("supported case requires a finite nonzero applied load")
        if not isinstance(self.source, str) or not self.source.strip():
            raise ValueError("an input and tolerance source is required")


@dataclass(frozen=True)
class ScreeningCheck:
    """Disposition of one content check, with its criterion or missing fields."""

    status: CheckStatus
    detail: str


@dataclass(frozen=True)
class ScreeningAssessment:
    """Content dispositions, kept separate from unestablished qualification."""

    checks: dict[str, ScreeningCheck]
    criteria_source: str

    @property
    def content_status(self) -> CheckStatus:
        """Return failure first, then incomplete evidence, then content pass."""
        statuses = {check.status for check in self.checks.values()}
        if "FAIL" in statuses:
            return "FAIL"
        if "NOT_EVALUATED" in statuses:
            return "NOT_EVALUATED"
        return "PASS"

    @property
    def native_qualification(self) -> CheckStatus:
        """Content checks cannot establish native execution or qualification."""
        return "NOT_EVALUATED"


def _outcome(passed: bool, detail: str) -> ScreeningCheck:
    return ScreeningCheck("PASS" if passed else "FAIL", detail)


def _missing(values: dict[str, float], keys: tuple[str, ...]) -> ScreeningCheck | None:
    missing = [key for key in keys if key not in values]
    if missing:
        return ScreeningCheck("NOT_EVALUATED", "Missing fields: " + ", ".join(missing))
    return None


def _stress(values: dict[str, float], criteria: ScreeningCriteria) -> ScreeningCheck:
    missing = _missing(values, ("max_seqv_mpa", "allowable_mpa"))
    if missing:
        return missing
    peak, allowable = values["max_seqv_mpa"], values["allowable_mpa"]
    passed = (
        allowable > 0
        and abs(allowable - criteria.allowable_mpa) <= criteria.stress_tolerance_mpa
        and 0 <= peak < min(criteria.allowable_mpa, criteria.yield_strength_mpa)
    )
    # Issue 2094 (2026): compliant sub-yield examples; limits are inputs.
    return _outcome(
        passed, "Peak below independent allowable and yield; allowable matches input"
    )


def _unity(values: dict[str, float], criteria: ScreeningCriteria) -> ScreeningCheck:
    missing = _missing(values, ("max_seqv_mpa", "uc"))
    if missing:
        return missing
    # Definition used by padeye.py and issue 2094 (2026): UC = peak / allowable.
    expected = values["max_seqv_mpa"] / criteria.allowable_mpa
    passed = (
        0 <= values["uc"] < 1
        and abs(values["uc"] - expected) <= criteria.unity_tolerance
    )
    return _outcome(
        passed, "UC equals peak / independent allowable within input tolerance; UC < 1"
    )


def _applied_load(
    values: dict[str, float], criteria: ScreeningCriteria
) -> ScreeningCheck:
    missing = _missing(values, ("applied_fx_n", "applied_fy_n"))
    if missing:
        return missing
    error = math.hypot(
        values["applied_fx_n"] - criteria.expected_applied_fx_n,
        values["applied_fy_n"] - criteria.expected_applied_fy_n,
    )
    return _outcome(
        error <= criteria.force_tolerance_n,
        "Applied vector matches independent case within input tolerance",
    )


_FORCE_FIELDS = ("applied_fx_n", "applied_fy_n", "reaction_fx_n", "reaction_fy_n")


def _force_residual(values: dict[str, float]) -> float:
    # Engineering Statics, online edition consulted 2026, section 5.3, eq. 5.3.1:
    # sum(F) = 0; the in-plane Euclidean norm measures the signed force residual.
    # https://engineeringstatics.org/Chapter_05-equations-of-equilibrium.html
    return math.hypot(
        values["applied_fx_n"] + values["reaction_fx_n"],
        values["applied_fy_n"] + values["reaction_fy_n"],
    )


def _force_balance(
    values: dict[str, float], criteria: ScreeningCriteria
) -> ScreeningCheck:
    missing = _missing(values, _FORCE_FIELDS)
    if missing:
        return missing
    return _outcome(
        _force_residual(values) <= criteria.force_tolerance_n,
        "Signed force residual <= input force tolerance",
    )


def _residual_record(
    values: dict[str, float], criteria: ScreeningCriteria
) -> ScreeningCheck:
    missing = _missing(values, _FORCE_FIELDS + ("force_residual_n",))
    if missing:
        return missing
    return _outcome(
        values["force_residual_n"] >= 0
        and abs(values["force_residual_n"] - _force_residual(values))
        <= criteria.force_tolerance_n,
        "Recorded residual is nonnegative and agrees with the independent norm",
    )


def assess_supported_digest(
    csv_text: str, criteria: ScreeningCriteria
) -> ScreeningAssessment:
    """Assess supported in-plane content against independently supplied inputs.

    A missing quantity yields NOT_EVALUATED for each affected check. Malformed
    text raises ValueError through the shared production reader. Self-balanced
    pressure-vessel and mudmat conventions are outside this function's scope.
    """
    values = ResultsExtractor().parse_result_digest(csv_text)
    checks = {
        "stress": _stress(values, criteria),
        "unity": _unity(values, criteria),
        "applied_load": _applied_load(values, criteria),
        "force_balance": _force_balance(values, criteria),
        "residual_record": _residual_record(values, criteria),
    }
    return ScreeningAssessment(checks, criteria.source)
