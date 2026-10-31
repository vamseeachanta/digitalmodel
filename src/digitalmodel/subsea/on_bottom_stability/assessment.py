"""Explicit-input static screening, weight design and reproducible reporting.

Equations A1–A4 are derived in the module analysis. They check arithmetic
equilibrium, not edition-specific F109 compliance. In particular, the passive
balance is not the standard generalised displacement method. No coefficient,
soil resistance or safety-class factor is inferred from an inaccessible table.
"""

from __future__ import annotations

from collections.abc import Sequence
from dataclasses import dataclass, field, replace
from datetime import date
import math

from ._validation import _number, _stability
from .dnv_rp_f109 import absolute_stability_check, generalized_stability_check


_METHODS = {"absolute", "passive_balance", "simplified"}


def _text(name: str, value: str) -> None:
    if not isinstance(value, str) or not value.strip():
        raise ValueError(f"{name} must be nonempty text")


def _method(value: str) -> None:
    if value not in _METHODS:
        raise ValueError(
            "method must be absolute, passive_balance or simplified; dynamic is not implemented"
        )


@dataclass(frozen=True)
class StaticCase:
    """SI simultaneous forces, explicit factors and their supplied provenance.

    A signed horizontal force is reduced to its magnitude for assessment.
    Passive resistance is an input, not a prediction of a calibrated soil law.
    Negative finite weight represents a buoyant pipe and fails the assessment.
    """

    label: str
    weight_n_m: float
    horizontal_n_m: float
    lift_n_m: float
    friction: float
    passive_n_m: float
    safety_factor: float
    factor_source: str

    def __post_init__(self):
        _text("label", self.label)
        _text("factor_source", self.factor_source)
        _stability(
            self.weight_n_m,
            self.horizontal_n_m,
            self.lift_n_m,
            self.friction,
            self.passive_n_m,
            self.safety_factor,
        )


@dataclass(frozen=True)
class StaticResult:
    """Arithmetic verdict only; missing normative evidence cannot be a pass."""

    case: StaticCase
    method: str
    lateral_utilisation: float
    vertical_utilisation: float
    utilisation: float
    arithmetic_pass: bool
    governing_check: str
    standard_status: str = field(default="NOT_EVALUATED", init=False)
    dynamic_status: str = field(default="NOT_EVALUATED", init=False)
    qualified: bool = field(default=False, init=False)


def assess_static(case: StaticCase, *, method: str) -> StaticResult:
    """Evaluate A1/A2/A3 plus A4 uplift and positive bed contact.

    Unity is the exact demand/capacity boundary by definition, not a supplied
    empirical limit. All comparisons use unrounded values. A1 ignores passive
    credit; A2 includes supplied passive credit; A3 fixes that credit to zero.
    """
    _method(method)
    if method == "absolute":
        lateral = absolute_stability_check(
            case.weight_n_m,
            case.horizontal_n_m,
            case.lift_n_m,
            case.friction,
            case.safety_factor,
        ).utilisation
    else:
        passive = case.passive_n_m if method == "passive_balance" else 0
        lateral = generalized_stability_check(
            case.weight_n_m,
            case.horizontal_n_m,
            case.lift_n_m,
            case.friction,
            passive,
            case.safety_factor,
        ).utilisation
    vertical = (
        case.safety_factor * case.lift_n_m / case.weight_n_m
        if case.weight_n_m > 0
        else math.inf
    )
    contact = case.weight_n_m > case.lift_n_m
    utilisation = max(lateral, vertical) if contact else math.inf
    governing = (
        "contact" if not contact else ("uplift" if vertical > lateral else "lateral")
    )
    return StaticResult(
        case, method, lateral, vertical, utilisation, utilisation <= 1, governing
    )


@dataclass(frozen=True)
class WeightSolution:
    """Feasible endpoint of the supplied bracket; fixed loads and geometry."""

    weight_n_m: float | None
    lower_n_m: float
    upper_n_m: float
    residual_n_m: float | None
    iterations: int
    converged: bool
    status: str


def _margin(case: StaticCase, method: str) -> float:
    # Analysis A1–A4, all margins in N/m. Zero is equilibrium by definition.
    if method == "absolute":
        lateral = case.friction * case.weight_n_m - case.safety_factor * (
            abs(case.horizontal_n_m) + case.friction * case.lift_n_m
        )
    else:
        passive = case.passive_n_m if method == "passive_balance" else 0
        lateral = (
            case.friction * (case.weight_n_m - case.lift_n_m)
            + passive
            - case.safety_factor * abs(case.horizontal_n_m)
        )
    return min(lateral, case.weight_n_m - case.safety_factor * case.lift_n_m)


def solve_required_weight(
    case: StaticCase,
    *,
    method: str,
    lower_n_m: float,
    upper_n_m: float,
    tolerance_n_m: float,
    max_iterations: int = 100,
) -> WeightSolution:
    """Bisect fixed-force A1–A4 and retain the assessed feasible upper endpoint.

    The caller supplies bounds and numerical resolution. The iteration default
    is a numerical work limit, unrelated to standard factors. Loads, diameter,
    embedment and resistance stay fixed: this is not a coating-thickness solver.
    """
    _method(method)
    _number("lower weight", lower_n_m)
    _number("upper weight", upper_n_m, positive=True)
    _number("weight tolerance", tolerance_n_m, positive=True)
    if upper_n_m <= lower_n_m or type(max_iterations) is not int or max_iterations < 1:
        raise ValueError(
            "an increasing weight bracket and positive iteration count are required"
        )

    def feasible(weight):
        return assess_static(
            replace(case, weight_n_m=weight), method=method
        ).arithmetic_pass

    def solution(weight, iteration, status):
        residual = (
            _margin(replace(case, weight_n_m=weight), method)
            if weight is not None
            else None
        )
        return WeightSolution(
            weight,
            lower_n_m,
            upper_n_m,
            residual,
            iteration,
            weight is not None,
            status,
        )

    if feasible(lower_n_m):
        return solution(lower_n_m, 0, "lower_feasible")
    if not feasible(upper_n_m):
        return solution(None, 0, "unbracketed")
    for iteration in range(1, max_iterations + 1):
        if upper_n_m - lower_n_m <= tolerance_n_m:
            return solution(upper_n_m, iteration - 1, "converged")
        midpoint = lower_n_m + (upper_n_m - lower_n_m) / 2
        if midpoint == lower_n_m or midpoint == upper_n_m:
            return solution(None, iteration, "precision_limit")
        if feasible(midpoint):
            upper_n_m = midpoint
        else:
            lower_n_m = midpoint
    if upper_n_m - lower_n_m <= tolerance_n_m:
        return solution(upper_n_m, max_iterations, "converged")
    return solution(None, max_iterations, "not_converged")


def _cell(value: str) -> str:
    return " ".join(value.split()).replace("|", "\\|").replace("`", "'")


def render_report(
    cases: Sequence[StaticCase],
    *,
    method: str,
    report_date: str,
    revision: str,
    previous_revisions: Sequence[tuple[str, str, str]] = (),
) -> str:
    """Render inputs, arithmetic verdicts, unrounded governing case and limits.

    Display numbers use one decimal place. Verdicts and governing-case selection
    always use full precision. Earlier supplied revision rows retain their
    content and order; a reissue appends the current issue's row.
    """
    _method(method)
    date.fromisoformat(report_date)
    _text("revision", revision)
    history = []
    seen = {revision}
    for old_revision, old_date, description in previous_revisions:
        _text("previous revision", old_revision)
        _text("revision description", description)
        date.fromisoformat(old_date)
        if old_revision in seen:
            raise ValueError("revision identifiers must be distinct")
        seen.add(old_revision)
        history.append(f"| {_cell(old_revision)} | {old_date} | {_cell(description)} |")
    if not cases or len({case.label for case in cases}) != len(cases):
        raise ValueError("nonempty cases with distinct labels are required")
    results = [assess_static(case, method=method) for case in cases]
    governing = max(results, key=lambda result: result.utilisation)
    lines = [
        "# On-bottom stability analysis",
        "",
        "## Summary",
        "",
        f"Method: {method}. Governing case: {_cell(governing.case.label)}.",
        "Results are static arithmetic screens. Standard qualification and dynamic displacement: NOT_EVALUATED.",
        "",
        "## Revision history",
        "",
        "| Revision | Date | Change and reason |",
        "|---|---|---|",
        *history,
        f"| {_cell(revision)} | {report_date} | Calculation issue with explicit input provenance |",
        "",
        "## Inputs",
        "",
        "Forces and weight are N/m; friction and safety factor are dimensionless.",
        "",
        "| Case | Weight | Horizontal | Lift | Friction | Passive | Safety factor | Source |",
        "|---|---:|---:|---:|---:|---:|---:|---|",
    ]
    for case in cases:
        values = (
            case.weight_n_m,
            case.horizontal_n_m,
            case.lift_n_m,
            case.friction,
            case.passive_n_m,
            case.safety_factor,
        )
        lines.append(
            f"| {_cell(case.label)} | "
            + " | ".join(f"{value:.1f}" for value in values)
            + f" | {_cell(case.factor_source)} |"
        )
    lines += [
        "",
        "## Utilisation",
        "",
        "Ratios display one decimal place; verdicts use unrounded demand <= capacity.",
        "",
        "| Case | Lateral | Uplift | Governing | Arithmetic | Standard |",
        "|---|---:|---:|---|---|---|",
    ]
    for result in results:
        verdict = "ARITHMETIC_PASS" if result.arithmetic_pass else "ARITHMETIC_FAIL"
        lines.append(
            f"| {_cell(result.case.label)} | {result.lateral_utilisation:.1f} | "
            f"{result.vertical_utilisation:.1f} | {result.governing_check} | "
            f"{verdict} | {result.standard_status} |"
        )
    lines += [
        "",
        "## Method and limits",
        "",
        "Equations A1–A4: module on-bottom-stability-analysis.md, independent force balance.",
        "Absolute uses gamma*(abs(H)+mu*L)/(mu*W), without passive credit.",
        "Passive balance uses gamma*abs(H)/(mu*(W-L)+FR); simplified sets FR=0.",
        "Uplift uses gamma*L/W and bed contact requires W>L.",
        "Standard editions, clauses, safety-class factors and calibrated soil resistance are not qualified.",
        "No generalised displacement envelope or dynamic time history is calculated.",
        "Use reviewed factors and an applicable soil/dynamic assessment before design acceptance.",
        "",
    ]
    return "\n".join(lines)
