"""Correctness contract for rainflow cycle counting, and the registry of paths.

Issue: https://github.com/vamseeachanta/workspace-hub/issues/3839
Plan:  workspace-hub ``docs/plans/2026-09-11-issue-3839-rainflow-divergence.md``

Seven cycle-counting implementations exist in this repository. Four of them
extract a maximum stress range **below** the signal's peak-to-valley span.
Fatigue damage scales as range cubed, so those four understate damage — the
non-conservative direction.

This module supplies three things:

1. **The contract** — four clauses a cycle count must satisfy, callable
   independently of any test framework (:func:`check_counting_contract`,
   :func:`assert_counting_contract`, :func:`assert_exact_distribution`).
2. **The registry** — every counting path, its entry point, its declared
   residual policy, its recorded verdict, and the damage entry points bound to
   it (:data:`COUNTING_PATHS`).
3. **The refusal** — :func:`refuse_damage_calculation`, called from the damage
   entry points of the non-conforming paths so that a failing path cannot be
   used to produce a damage or fatigue-life number while consolidation is
   deferred to the successor issue.

Scope limitation, stated rather than implied: the contract is scoped to this
project's declared convention, not to a standard. ASTM E1049-85(2023) is
paywalled and was not read, so no clause here is presented as a standards
requirement.

Clause 1 is **conditional on a declared residual policy**. An implementation
that discards residuals rather than counting them as half cycles can correctly
omit the global span; such an implementation is not condemned by this contract.
Every path registered below declares ``RETAIN``.
"""

from __future__ import annotations

import ast
import re
import tempfile
from dataclasses import dataclass
from enum import Enum
from pathlib import Path
from typing import Any, Callable, Iterable, Mapping, NoReturn, Sequence

import numpy as np

__all__ = [
    "ISSUE_NUMBER",
    "ISSUE_URL",
    "ResidualPolicy",
    "ContractViolation",
    "NonConservativeCountingError",
    "CycleCount",
    "CountingPath",
    "COUNTING_PATHS",
    "COUNTING_SCAN_PACKAGES",
    "CONTRACT_SIGNALS",
    "CONTRACT_EXACT",
    "reversals_with_endpoints",
    "peak_to_valley_span",
    "check_counting_contract",
    "assert_counting_contract",
    "check_exact_distribution",
    "assert_exact_distribution",
    "refuse_damage_calculation",
    "discover_counting_callables",
    "unregistered_counting_callables",
]

ISSUE_NUMBER = 3839
ISSUE_URL = "https://github.com/vamseeachanta/workspace-hub/issues/3839"

#: Relative tolerance on range comparisons. Loose enough to admit an
#: implementation that rounds its aggregation key to six decimals
#: (``fatigue_apps/rainflow_counter.py`` does), tight enough that every
#: recorded failure — the smallest of which understates the span by 4.3 % —
#: is rejected by four orders of magnitude.
DEFAULT_RTOL = 1e-6
DEFAULT_ATOL = 1e-9

#: Count tolerance. Counts are multiples of 0.5 and exactly representable.
COUNT_ATOL = 1e-9


class ResidualPolicy(str, Enum):
    """What an implementation does with the residual after loop closure."""

    #: Residual turning points are counted as half cycles. The global peak and
    #: valley are necessarily a reversal pair, so the span is emitted.
    RETAIN = "retain"

    #: Residual turning points are discarded. The span may legitimately be
    #: absent from the emitted ranges.
    DISCARD = "discard"


class ContractViolation(AssertionError):
    """A cycle count violates the counting contract."""


class NonConservativeCountingError(RuntimeError):
    """Damage calculation refused: the bound counting path understates range."""


@dataclass(frozen=True)
class CycleCount:
    """A normalised cycle count.

    Attributes
    ----------
    ranges
        Stress/load ranges, one per emitted row.
    counts
        Cycle counts, one per row; 0.5 for a half cycle, 1.0 for a full cycle,
        or an aggregated sum of those.
    means
        Mean stress per row, or ``None`` when the path does not report means.
        Clause 4 is only applicable when means are reported.
    """

    ranges: np.ndarray
    counts: np.ndarray
    means: np.ndarray | None = None

    def __post_init__(self) -> None:
        object.__setattr__(self, "ranges", np.asarray(self.ranges, dtype=float).ravel())
        object.__setattr__(self, "counts", np.asarray(self.counts, dtype=float).ravel())
        if self.means is not None:
            object.__setattr__(
                self, "means", np.asarray(self.means, dtype=float).ravel()
            )
        if self.ranges.shape != self.counts.shape:
            raise ValueError("ranges and counts must have the same length")
        if self.means is not None and self.means.shape != self.ranges.shape:
            raise ValueError("means, when given, must have the same length as ranges")

    @property
    def total_counts(self) -> float:
        return float(np.sum(self.counts)) if self.counts.size else 0.0


# ---------------------------------------------------------------------------
# Reversals — defined once, here, so every clause refers to the same object
# ---------------------------------------------------------------------------


def reversals_with_endpoints(signal: Any) -> np.ndarray:
    """Turning points of ``signal``, including the first and last points.

    Consecutive equal samples are collapsed first, so a plateau contributes one
    point rather than several. An interior point is a reversal when the sign of
    the difference changes across it. The first and last surviving points are
    always retained: they bound the sequence and participate in the count.
    """
    x = np.asarray(signal, dtype=float).ravel()
    if x.size == 0:
        return x
    keep = np.concatenate(([True], x[1:] != x[:-1]))
    x = x[keep]
    if x.size <= 2:
        return x
    d = np.diff(x)
    interior = np.sign(d[:-1]) != np.sign(d[1:])
    mask = np.concatenate(([True], interior, [True]))
    return x[mask]


def peak_to_valley_span(signal: Any) -> float:
    """The largest excursion present in ``signal``."""
    x = np.asarray(signal, dtype=float).ravel()
    if x.size == 0:
        return 0.0
    return float(np.max(x) - np.min(x))


# ---------------------------------------------------------------------------
# The contract
# ---------------------------------------------------------------------------


def check_counting_contract(
    result: CycleCount,
    signal: Any,
    residual_policy: ResidualPolicy,
    *,
    rtol: float = DEFAULT_RTOL,
    atol: float = DEFAULT_ATOL,
) -> list[str]:
    """Return a list of clause violations; an empty list means conforming.

    Clauses
    -------
    1. *(RETAIN only)* the maximum extracted range equals the peak-to-valley
       span. The global peak and valley are necessarily a reversal pair, so a
       retain-residual count emits that range as a full or half cycle.
    2. no extracted range exceeds the span — nothing is counted that the signal
       does not contain.
    3. count conservation. Each reversal participates in exactly one extracted
       range, so for RETAIN ``2 * sum(counts) == len(reversals) - 1`` exactly;
       for DISCARD the equality relaxes to ``<=``, since discarding removes
       counts but may never manufacture them. Every count is a positive
       multiple of 0.5, and no zero-range cycle is emitted for a signal that
       actually moves.

    4. exact distribution — checked separately by
       :func:`check_exact_distribution`, because it requires a hand-derived
       reference table rather than the signal alone.

    Zero-range rows
    ---------------
    A constant signal genuinely contains one zero-range excursion, and the
    conforming implementations disagree about whether to emit it: pyLife and
    the PyPI ``rainflow`` package emit ``(0.0, value, 0.5)``, while
    ``fatigue_apps/rainflow_counter.py`` drops it. Both are defensible, since a
    zero range contributes exactly zero damage. The contract therefore treats
    zero-range rows as inert — excluded from clauses 1, 2 and the conservation
    equality — but forbids them outright when the span is non-zero, which is
    the case the sub-clause exists to catch: a counter padding its distribution
    with zero-range rows to satisfy conservation.
    """
    violations: list[str] = []
    span = peak_to_valley_span(signal)
    reversals = reversals_with_endpoints(signal)
    expected_half_cycles = max(len(reversals) - 1, 0)
    tol = atol + rtol * abs(span)

    all_ranges = result.ranges
    all_counts = result.counts
    inert = all_ranges <= atol if all_ranges.size else np.zeros(0, dtype=bool)
    ranges = all_ranges[~inert]
    counts = all_counts[~inert]
    max_range = float(np.max(ranges)) if ranges.size else 0.0

    # -- clause 1 ---------------------------------------------------------
    if residual_policy is ResidualPolicy.RETAIN:
        if ranges.size == 0:
            if span > tol:
                violations.append(
                    f"clause 1: no cycles emitted but the signal spans {span:.6g}"
                )
        elif abs(max_range - span) > tol:
            violations.append(
                f"clause 1: maximum extracted range {max_range:.6g} is not the "
                f"peak-to-valley span {span:.6g} "
                f"(shortfall {span - max_range:.6g}; declared policy RETAIN)"
            )

    # -- clause 2 ---------------------------------------------------------
    if ranges.size and max_range > span + tol:
        violations.append(
            f"clause 2: maximum extracted range {max_range:.6g} exceeds the "
            f"peak-to-valley span {span:.6g}"
        )

    # -- clause 3 ---------------------------------------------------------
    total = 2.0 * float(np.sum(counts)) if counts.size else 0.0
    if residual_policy is ResidualPolicy.RETAIN:
        if abs(total - expected_half_cycles) > COUNT_ATOL:
            violations.append(
                f"clause 3: 2*sum(counts) = {total:.6g}, expected "
                f"{expected_half_cycles} (= len(reversals) - 1); declared policy RETAIN"
            )
    else:
        if total - expected_half_cycles > COUNT_ATOL:
            violations.append(
                f"clause 3: 2*sum(counts) = {total:.6g} exceeds "
                f"{expected_half_cycles} (= len(reversals) - 1); a discard-residual "
                f"count may omit cycles but never manufacture them"
            )

    if all_counts.size:
        if np.any(all_counts <= 0.0):
            violations.append("clause 3: a non-positive count was emitted")
        halves = all_counts * 2.0
        off = np.abs(halves - np.rint(halves)) > COUNT_ATOL
        if np.any(off):
            violations.append(
                f"clause 3: counts are not multiples of 0.5: {all_counts[off][:5]}"
            )
    if span > tol and np.any(inert):
        violations.append(
            f"clause 3: {int(np.count_nonzero(inert))} zero-range cycle(s) emitted "
            f"for a signal that spans {span:.6g}"
        )

    return violations


def assert_counting_contract(
    result: CycleCount,
    signal: Any,
    residual_policy: ResidualPolicy,
    *,
    rtol: float = DEFAULT_RTOL,
    atol: float = DEFAULT_ATOL,
    label: str = "",
) -> None:
    """Raise :class:`ContractViolation` when ``result`` violates any clause."""
    violations = check_counting_contract(
        result, signal, residual_policy, rtol=rtol, atol=atol
    )
    if violations:
        prefix = f"{label}: " if label else ""
        raise ContractViolation(
            prefix
            + f"cycle count violates the counting contract ({ISSUE_URL})\n  "
            + "\n  ".join(violations)
        )


def _aggregate(
    ranges: np.ndarray,
    means: np.ndarray,
    counts: np.ndarray,
    decimals: int = 9,
) -> list[tuple[float, float, float]]:
    acc: dict[tuple[float, float], float] = {}
    for rng, mean, cnt in zip(ranges, means, counts):
        key = (round(float(rng), decimals), round(float(mean), decimals))
        acc[key] = acc.get(key, 0.0) + float(cnt)
    return sorted((rng, mean, cnt) for (rng, mean), cnt in acc.items())


def check_exact_distribution(
    result: CycleCount,
    expected: Sequence[tuple[float, float, float]],
    *,
    rtol: float = DEFAULT_RTOL,
    atol: float = DEFAULT_ATOL,
) -> list[str]:
    """Clause 4 — the emitted ``(range, mean, count)`` table, exactly.

    Clauses 1 to 3 are aggregate: a counter that fabricates counts can satisfy
    some of them. This clause pins the whole distribution against a table
    derived by hand.
    """
    if result.means is None:
        return ["clause 4: path does not report mean stress; not applicable"]

    got = _aggregate(result.ranges, result.means, result.counts)
    want = _aggregate(
        np.array([e[0] for e in expected], dtype=float),
        np.array([e[1] for e in expected], dtype=float),
        np.array([e[2] for e in expected], dtype=float),
    )

    if len(got) != len(want):
        return [
            f"clause 4: {len(got)} distinct (range, mean) rows emitted, "
            f"{len(want)} expected\n    got:      {got}\n    expected: {want}"
        ]

    violations: list[str] = []
    for (g_rng, g_mean, g_cnt), (w_rng, w_mean, w_cnt) in zip(got, want):
        if not np.isclose(g_rng, w_rng, rtol=rtol, atol=atol):
            violations.append(f"clause 4: range {g_rng:.10g} != {w_rng:.10g}")
        if not np.isclose(g_mean, w_mean, rtol=rtol, atol=atol):
            violations.append(
                f"clause 4: mean {g_mean:.10g} != {w_mean:.10g} at range {w_rng:.10g}"
            )
        if abs(g_cnt - w_cnt) > COUNT_ATOL:
            violations.append(
                f"clause 4: count {g_cnt:g} != {w_cnt:g} at "
                f"(range {w_rng:.10g}, mean {w_mean:.10g})"
            )
    return violations


def assert_exact_distribution(
    result: CycleCount,
    expected: Sequence[tuple[float, float, float]],
    *,
    rtol: float = DEFAULT_RTOL,
    atol: float = DEFAULT_ATOL,
    label: str = "",
) -> None:
    """Raise :class:`ContractViolation` when the distribution differs."""
    violations = check_exact_distribution(result, expected, rtol=rtol, atol=atol)
    if violations:
        prefix = f"{label}: " if label else ""
        raise ContractViolation(
            prefix
            + f"cycle distribution differs from the hand-derived table ({ISSUE_URL})\n  "
            + "\n  ".join(violations)
        )


# ---------------------------------------------------------------------------
# The refusal
# ---------------------------------------------------------------------------


def refuse_damage_calculation(path_key: str, entry_point: str) -> NoReturn:
    """Refuse to compute fatigue damage through a non-conservative counter.

    Called from the damage entry points bound to a path whose recorded verdict
    is ``non-conservative``. Cycle counting itself is left callable: it has
    legitimate diagnostic uses (statistics, histograms, exported cycle tables).
    What is refused is turning those cycles into a damage or fatigue-life
    number, because the ranges are understated and damage scales as range
    cubed.
    """
    path = COUNTING_PATHS.get(path_key)
    shortfall = path.note if path is not None else ""
    raise NonConservativeCountingError(
        f"{entry_point} refuses to compute fatigue damage.\n"
        f"It is bound to cycle-counting path '{path_key}', which extracts a "
        f"maximum stress range below the signal's peak-to-valley span. Fatigue "
        f"damage scales as range cubed, so the resulting damage is understated "
        f"— the non-conservative direction.\n"
        f"{shortfall}\n"
        f"Issue #{ISSUE_NUMBER}: {ISSUE_URL}\n"
        f"Cycle counting through this path remains available for diagnostic use. "
        f"For damage, use a path that satisfies digitalmodel.fatigue."
        f"counting_contract: pylife_4pt, pypi_rainflow or fapps_counter."
    )


# ---------------------------------------------------------------------------
# Path adapters — lazy imports, so registering a path costs nothing until used
# ---------------------------------------------------------------------------

_TMP_OUTPUT: list[str] = []


def _scratch_dir() -> str:
    if not _TMP_OUTPUT:
        _TMP_OUTPUT.append(tempfile.mkdtemp(prefix="digitalmodel-counting-contract-"))
    return _TMP_OUTPUT[0]


def _adapt_pylife_4pt(signal: Any) -> CycleCount:
    from digitalmodel.fatigue.rainflow import rainflow_count

    df = rainflow_count(np.asarray(signal, dtype=float))
    return CycleCount(
        ranges=df["stress_range"].to_numpy(dtype=float),
        counts=df["cycles"].to_numpy(dtype=float),
        means=df["mean_stress"].to_numpy(dtype=float),
    )


def _adapt_pypi_rainflow(signal: Any) -> CycleCount:
    import rainflow as _rainflow

    rows = list(_rainflow.extract_cycles(np.asarray(signal, dtype=float)))
    if not rows:
        empty = np.array([], dtype=float)
        return CycleCount(ranges=empty, counts=empty.copy(), means=empty.copy())
    return CycleCount(
        ranges=np.array([r[0] for r in rows], dtype=float),
        counts=np.array([r[2] for r in rows], dtype=float),
        means=np.array([r[1] for r in rows], dtype=float),
    )


def _adapt_sigproc_astm(signal: Any) -> CycleCount:
    from digitalmodel.signal_processing.signal_analysis.core.rainflow import (
        RainflowCounter,
    )

    df = RainflowCounter().count_cycles(
        np.asarray(signal, dtype=float), extract_info=False
    )
    return CycleCount(
        ranges=df["range"].to_numpy(dtype=float),
        counts=df["count"].to_numpy(dtype=float),
        means=df["mean"].to_numpy(dtype=float),
    )


def _adapt_calm_buoy(signal: Any) -> CycleCount:
    from digitalmodel.marine_ops.marine_engineering.calm_buoy_fatigue import (
        RainflowFatigue,
    )

    ranges, counts = RainflowFatigue().count_cycles(np.asarray(signal, dtype=float))
    return CycleCount(ranges=ranges, counts=counts)


def _adapt_fapps_counting(signal: Any) -> CycleCount:
    from digitalmodel.structural.fatigue_apps.rainflow_counting import RainflowCounter

    result = RainflowCounter(output_dir=_scratch_dir()).process_time_series(
        np.asarray(signal, dtype=float)
    )
    return CycleCount(ranges=result["ranges"], counts=result["counts"])


def _adapt_fapps_counter(signal: Any) -> CycleCount:
    from digitalmodel.structural.fatigue_apps.rainflow_counter import RainflowCounter

    ranges, counts = RainflowCounter().count_cycles(np.asarray(signal, dtype=float))
    return CycleCount(ranges=ranges, counts=counts)


def _adapt_struct_fatigue(signal: Any) -> CycleCount:
    from digitalmodel.structural.fatigue.rainflow import RainflowCounter

    result = RainflowCounter().count_cycles(np.asarray(signal, dtype=float))
    return CycleCount(ranges=result["ranges"], counts=result["counts"])


# ---------------------------------------------------------------------------
# The registry
# ---------------------------------------------------------------------------


@dataclass(frozen=True)
class CountingPath:
    """One cycle-counting implementation, with its declared convention.

    Attributes
    ----------
    key
        Stable identifier used in tests, reports and refusal messages.
    module
        Dotted module path of the implementation.
    entry_point
        The public callable exercised by the contract.
    residual_policy
        Declared, not inferred. Clause 1 applies only to ``RETAIN``.
    contract_status
        Recorded verdict: ``conforming`` or ``non-conservative``.
    implementations
        Every ``module:qualname`` that belongs to this path, used by the
        registry-completeness scan so a counting callable cannot be added in
        the known packages without being registered.
    damage_entry_points
        Callables that turn this path's cycles into a damage or fatigue-life
        number. Empty for a conforming path; each one refuses for a
        non-conservative path.
    """

    key: str
    module: str
    entry_point: str
    residual_policy: ResidualPolicy
    contract_status: str
    adapter: Callable[[Any], CycleCount]
    implementations: tuple[str, ...] = ()
    damage_entry_points: tuple[str, ...] = ()
    note: str = ""

    def count(self, signal: Any) -> CycleCount:
        """Run this path's public entry point and normalise the result."""
        return self.adapter(signal)


_PATHS: tuple[CountingPath, ...] = (
    CountingPath(
        key="pylife_4pt",
        module="digitalmodel.fatigue.rainflow",
        entry_point="rainflow_count",
        residual_policy=ResidualPolicy.RETAIN,
        contract_status="conforming",
        adapter=_adapt_pylife_4pt,
        implementations=("digitalmodel.fatigue.rainflow:rainflow_count",),
        note="Wraps pyLife FourPointDetector; residual turning points counted as half cycles.",
    ),
    CountingPath(
        key="pypi_rainflow",
        module="digitalmodel.signal_processing.time_series.time_series_components",
        entry_point="TimeSeriesComponents.count_cycles",
        residual_policy=ResidualPolicy.RETAIN,
        contract_status="conforming",
        adapter=_adapt_pypi_rainflow,
        implementations=(
            "digitalmodel.signal_processing.time_series.time_series_components:"
            "TimeSeriesComponents.count_cycles",
        ),
        note="Delegates to the third-party `rainflow` PyPI package (ASTM E1049-85).",
    ),
    CountingPath(
        key="sigproc_astm",
        module="digitalmodel.signal_processing.signal_analysis.core.rainflow",
        entry_point="RainflowCounter.count_cycles",
        residual_policy=ResidualPolicy.RETAIN,
        contract_status="non-conservative",
        adapter=_adapt_sigproc_astm,
        implementations=(
            "digitalmodel.signal_processing.signal_analysis.core.rainflow:"
            "RainflowCounter.count_cycles",
            "digitalmodel.signal_processing.signal_analysis.core.rainflow:"
            "RainflowCounter._rainflow_algorithm",
            "digitalmodel.signal_processing.signal_analysis.core.rainflow:"
            "RainflowCounter._combine_half_cycles",
            "digitalmodel.signal_processing.signal_analysis.adapters:"
            "TimeSeriesComponentsAdapter.get_rainflow_count_from_time_series",
            "digitalmodel.signal_processing.signal_analysis.adapters:"
            "FatigueAnalysisAdapter.get_rainflow_from_timetrace",
            "digitalmodel.signal_processing.signal_analysis.adapters:"
            "FatigueAnalysisAdapter.damage_from_rainflow_cycles",
            "digitalmodel.signal_processing.signal_analysis.adapters:"
            "OrcaFlexAdapter.RainflowHalfCycles",
        ),
        damage_entry_points=(
            "digitalmodel.solvers.orcaflex.time_trace_processor:"
            "OrcaFlexTimeTraceProcessor._perform_fatigue_analysis",
            "digitalmodel.solvers.orcaflex.time_trace_processor:"
            "OrcaFlexTimeTraceProcessor._analyze_single_trace",
        ),
        note=(
            "Mechanism, read from source at core/rainflow.py:162-164: "
            "`if range_XY >= range_YZ: cycle_range = range_YZ` inverts both halves of "
            "ASTM 5.4.4 — the rule fires when the NEWER range is larger and extracts "
            "the OLDER (inner) pair. It also emits 0.5 for every extraction and always "
            "discards stack[-2], so the full-cycle rule is never applied. Correcting "
            "the inversion alone does not restore the span (controlled pair: 7.0 of "
            "9.0), so the defect is structural; the exact arithmetic behind 6.0 and "
            "125.2664 is not established. Measured shortfall: 6.0 of 9.0 (ASTM "
            "fixture), 125.2664 of 177.1224 (broadband)."
        ),
    ),
    CountingPath(
        key="calm_buoy",
        module="digitalmodel.marine_ops.marine_engineering.calm_buoy_fatigue",
        entry_point="RainflowFatigue.count_cycles",
        residual_policy=ResidualPolicy.RETAIN,
        contract_status="non-conservative",
        adapter=_adapt_calm_buoy,
        implementations=(
            "digitalmodel.marine_ops.marine_engineering.calm_buoy_fatigue:"
            "RainflowFatigue.count_cycles",
            "digitalmodel.marine_ops.marine_engineering.calm_buoy_fatigue:"
            "RainflowFatigue._rainflow_algorithm",
        ),
        damage_entry_points=(
            "digitalmodel.marine_ops.marine_engineering.calm_buoy_fatigue:"
            "compute_fatigue_life",
            "digitalmodel.marine_ops.marine_engineering.calm_buoy_fatigue:"
            "ScatterDiagramFatigue.compute",
        ),
        note=(
            "calm_buoy_fatigue.py:262-270 makes the three-point comparison "
            "correctly, which is why its failure is milder and differently "
            "shaped. Two lines are wrong: :267 `stack.pop(-2)` discards the "
            "SECOND point of the counted pair where ASTM rule 2 discards the "
            "first, and :268 `break` exits the loop instead of re-checking the "
            "shortened stack. Controlled pair changing only those two: the sine "
            "goes 50.0 -> 100.0 and the ASTM fixture 7.0/3.5 -> 9.0/4.0. This "
            "path therefore diverges in COUNT as well as range on the ASTM "
            "fixture, unlike the other six."
        ),
    ),
    CountingPath(
        key="fapps_counting",
        module="digitalmodel.structural.fatigue_apps.rainflow_counting",
        entry_point="RainflowCounter.process_time_series",
        residual_policy=ResidualPolicy.RETAIN,
        contract_status="non-conservative",
        adapter=_adapt_fapps_counting,
        implementations=(
            "digitalmodel.structural.fatigue_apps.rainflow_counting:"
            "RainflowCounter.rainflow_counting_astm",
        ),
        damage_entry_points=(
            "digitalmodel.structural.fatigue_apps.rainflow_counting:"
            "RainflowCounter.process_batch",
        ),
        note=(
            "No in-process damage consumer exists: no module in src/, tests/, "
            "docs/ or scripts/ imports this class. Its hand-off to damage is by "
            "file — `process_batch` is the documented Module 2 step whose "
            "`*_cycles.csv` output feeds the Module 3 damage calculator "
            "(fatigue_apps/INPUT_FILE_STRUCTURE.md:77,116,275) — so the refusal "
            "is placed there. Mechanism: rainflow_counting.py:154-163 extracts "
            "only when the stack holds four or more points and breaks otherwise, "
            "so ASTM rule 2 never fires, the start point is never shed, and the "
            "full-cycle rule discards a reversal ASTM retains. Same defect shape "
            "as struct_fatigue, where a controlled pair confirms it; the two "
            "report identical maxima. Measured shortfall: 169.556 of 177.1224."
        ),
    ),
    CountingPath(
        key="fapps_counter",
        module="digitalmodel.structural.fatigue_apps.rainflow_counter",
        entry_point="RainflowCounter.count_cycles",
        residual_policy=ResidualPolicy.RETAIN,
        contract_status="conforming",
        adapter=_adapt_fapps_counter,
        implementations=(
            "digitalmodel.structural.fatigue_apps.rainflow_counter:"
            "RainflowCounter.count_cycles",
            "digitalmodel.structural.fatigue_apps.rainflow_counter:"
            "RainflowCounter._rainflow_algorithm",
            "digitalmodel.structural.fatigue_apps.rainflow_counter:"
            "RainflowCounter._aggregate_cycles",
            "digitalmodel.structural.fatigue_apps.rainflow_counter:rainflow_count",
        ),
        note=(
            "Aggregates on a range key rounded to six decimals, so the emitted "
            "maximum can differ from the span by up to 5e-7; DEFAULT_RTOL admits "
            "that and nothing larger."
        ),
    ),
    CountingPath(
        key="struct_fatigue",
        module="digitalmodel.structural.fatigue.rainflow",
        entry_point="RainflowCounter.count_cycles",
        residual_policy=ResidualPolicy.RETAIN,
        contract_status="non-conservative",
        adapter=_adapt_struct_fatigue,
        implementations=(
            "digitalmodel.structural.fatigue.rainflow:RainflowCounter.count_cycles",
            "digitalmodel.structural.fatigue.rainflow:RainflowCounter._rainflow_count",
            "digitalmodel.structural.fatigue.rainflow:RainflowCounter._rainflow_with_means",
            "digitalmodel.structural.fatigue.rainflow:RainflowCounter."
            "extract_cycles_with_means",
            "digitalmodel.structural.fatigue.rainflow:_rainflow_counting_numba",
            "digitalmodel.structural.fatigue.rainflow:rainflow_count",
            "digitalmodel.structural.fatigue.rainflow:rainflow_with_means",
            "digitalmodel.structural.fatigue.analysis:"
            "FatigueAnalysisEngine._setup_rainflow_counter",
        ),
        damage_entry_points=(
            "digitalmodel.structural.fatigue.analysis:"
            "FatigueAnalysisEngine.analyze_time_series",
            "digitalmodel.structural.fatigue.analysis:quick_time_domain_analysis",
            "digitalmodel.structural.fatigue:quick_fatigue_analysis",
        ),
        note=(
            "`_rainflow_counting_numba` (rainflow.py:183-196) extracts only when "
            "stack_size >= 4 and breaks otherwise, so ASTM rule 2 never fires, "
            "the start point is never shed, and rule 3 discards BOTH points of a "
            "pair where rule 2 was due — throwing away a reversal ASTM retains. "
            "Controlled pair on broadband, changing only that branch: 169.556 -> "
            "177.1224, total counts unchanged at 1370.0, and the signal's global "
            "valley goes from discarded to retained. Measured shortfall: 169.556 "
            "of a 177.1224 span."
        ),
    ),
)

#: Every counting path in the repository, keyed by its stable identifier.
COUNTING_PATHS: Mapping[str, CountingPath] = {p.key: p for p in _PATHS}


# ---------------------------------------------------------------------------
# Contract signals and the hand-derived fixture
# ---------------------------------------------------------------------------


def _narrow_band_sine() -> np.ndarray:
    return 50.0 * np.sin(np.linspace(0.0, 10 * 2 * np.pi, 640, endpoint=False))


def _broadband_random() -> np.ndarray:
    return np.random.default_rng(20260911).normal(0.0, 25.0, 4096)


def _residual_dominated() -> np.ndarray:
    return np.linspace(0.0, 200.0, 512) + 8.0 * np.sin(
        np.linspace(0.0, 40 * np.pi, 512)
    )


#: The four deterministic signals the contract is applied over. The broadband
#: case is the one nothing in the existing suite tested; every failing
#: implementation passes the narrow-band case.
CONTRACT_SIGNALS: Mapping[str, np.ndarray] = {
    "astm_example": np.array([-2, 1, -3, 5, -1, 3, -4, 4, -2], dtype=float),
    "narrow_band_sine": _narrow_band_sine(),
    "broadband_random": _broadband_random(),
    "residual_dominated": _residual_dominated(),
}


# ---------------------------------------------------------------------------
# CONTRACT_EXACT — hand-derived, committed as literals, never captured
# ---------------------------------------------------------------------------
#
# Signal: [-2, 1, -3, 5, -1, 3, -4, 4, -2]
#
# Every sample is a reversal: the sequence alternates up, down, up, down, up,
# down, up, down. So reversals = the signal itself, 9 points, 8 half cycles to
# distribute. Span = 5 - (-4) = 9.
#
# Rule applied (ASTM E1049-85 §5.4.4, three-point form, residual retained):
#   X = the newest range, formed by the two most recent points.
#   Y = the range immediately preceding X.
#   (1) X <  Y                       -> count nothing, read the next point.
#   (2) X >= Y and Y holds the start -> count Y as a HALF cycle, discard the
#                                       first point of Y, the start moves on.
#   (3) X >= Y and Y is interior     -> count Y as a FULL cycle, discard both
#                                       points of Y.
#   At the end, every remaining range in the residual is a HALF cycle.
#
# Trace (stack shown after each step):
#
#  push -2          [-2]
#  push  1          [-2, 1]
#  push -3          [-2, 1, -3]     Y=|-2-1|=3, X=|1-(-3)|=4, X>=Y, Y holds start
#                                   -> HALF cycle: range 3, mean (-2+1)/2 = -0.5
#                                   drop -2                      [1, -3]
#  push  5          [1, -3, 5]      Y=|1-(-3)|=4, X=|-3-5|=8, X>=Y, Y holds start
#                                   -> HALF cycle: range 4, mean (1+(-3))/2 = -1.0
#                                   drop 1                       [-3, 5]
#  push -1          [-3, 5, -1]     Y=8, X=|5-(-1)|=6, X<Y  -> nothing
#  push  3          [-3, 5, -1, 3]  Y=|5-(-1)|=6, X=|-1-3|=4, X<Y -> nothing
#  push -4          [-3,5,-1,3,-4]  Y=|-1-3|=4, X=|3-(-4)|=7, X>=Y, Y interior
#                                   -> FULL cycle: range 4, mean (-1+3)/2 = 1.0
#                                   drop -1 and 3                [-3, 5, -4]
#                                   re-check: Y=|-3-5|=8, X=|5-(-4)|=9, X>=Y,
#                                   Y holds start
#                                   -> HALF cycle: range 8, mean (-3+5)/2 = 1.0
#                                   drop -3                      [5, -4]
#  push  4          [5, -4, 4]      Y=|5-(-4)|=9, X=|-4-4|=8, X<Y -> nothing
#  push -2          [5,-4,4,-2]     Y=|-4-4|=8, X=|4-(-2)|=6, X<Y -> nothing
#
# Residual [5, -4, 4, -2], each adjacent range a HALF cycle:
#   |5-(-4)| = 9, mean (5+(-4))/2  =  0.5
#   |-4-4|   = 8, mean (-4+4)/2    =  0.0
#   |4-(-2)| = 6, mean (4+(-2))/2  =  1.0
#
# Checks on the derivation itself:
#   sum(counts) = 0.5+0.5+1.0+0.5 + 0.5+0.5+0.5 = 4.0
#   2*4.0 = 8 = len(reversals) - 1                        (clause 3)
#   max(range) = 9 = span                                 (clause 1)
#
# Cross-check, NOT the source: collapsing means, this gives
# range 3 -> 0.5, range 4 -> 1.5, range 6 -> 0.5, range 8 -> 1.0, range 9 -> 0.5,
# which is the table ASTM E1049-85 publishes for this worked sequence and the
# documented output of the `rainflow` PyPI package. The literals below were
# written from the trace above before any counter was run.
# ---------------------------------------------------------------------------

CONTRACT_EXACT: Mapping[str, Any] = {
    "signal": [-2.0, 1.0, -3.0, 5.0, -1.0, 3.0, -4.0, 4.0, -2.0],
    # (range, mean, count)
    "expected": (
        (3.0, -0.5, 0.5),
        (4.0, -1.0, 0.5),
        (4.0, 1.0, 1.0),
        (6.0, 1.0, 0.5),
        (8.0, 0.0, 0.5),
        (8.0, 1.0, 0.5),
        (9.0, 0.5, 0.5),
    ),
}


# ---------------------------------------------------------------------------
# Registry completeness — a name-pattern scan, honestly scoped
# ---------------------------------------------------------------------------

#: Package directories, relative to ``src/``, that the completeness scan covers.
#: A counting implementation under a novel name, or in a package not listed
#: here, escapes the scan. The gate does not claim otherwise.
COUNTING_SCAN_PACKAGES: tuple[str, ...] = (
    "digitalmodel/fatigue",
    "digitalmodel/signal_processing",
    "digitalmodel/structural/fatigue",
    "digitalmodel/structural/fatigue_apps",
    "digitalmodel/marine_ops/marine_engineering",
)

#: Callable names that denote cycle counting rather than anything else.
_EXACT_COUNTING_NAMES = frozenset(
    {"count_cycles", "extract_cycles", "rainflow_with_means"}
)
_RAINFLOW = re.compile(r"rainflow", re.IGNORECASE)
_COUNTING_VERB = re.compile(r"count|cycle", re.IGNORECASE)


def _is_counting_name(name: str) -> bool:
    if name in _EXACT_COUNTING_NAMES:
        return True
    return bool(_RAINFLOW.search(name)) and bool(_COUNTING_VERB.search(name))


def _src_root() -> Path:
    # .../src/digitalmodel/fatigue/counting_contract.py -> .../src
    return Path(__file__).resolve().parents[2]


def _iter_defs(tree: ast.Module) -> Iterable[tuple[str, str]]:
    """Yield ``(qualname, name)`` for module-level and class-level defs."""
    for node in tree.body:
        if isinstance(node, (ast.FunctionDef, ast.AsyncFunctionDef)):
            yield node.name, node.name
        elif isinstance(node, ast.ClassDef):
            for sub in node.body:
                if isinstance(sub, (ast.FunctionDef, ast.AsyncFunctionDef)):
                    yield f"{node.name}.{sub.name}", sub.name


def discover_counting_callables(
    packages: Sequence[str] = COUNTING_SCAN_PACKAGES,
) -> list[str]:
    """Return ``module:qualname`` for every counting-named callable found.

    The scan is static (``ast``), not import-based: importing these modules has
    side effects — one of them replaces ``sys.stdout`` — and a completeness gate
    must not depend on those.
    """
    root = _src_root()
    found: set[str] = set()
    for pkg in packages:
        base = root / Path(pkg)
        if not base.is_dir():
            continue
        for py in sorted(base.rglob("*.py")):
            rel = py.relative_to(root).with_suffix("")
            module = ".".join(rel.parts)
            try:
                tree = ast.parse(py.read_text(encoding="utf-8", errors="replace"))
            except SyntaxError:
                continue
            for qualname, name in _iter_defs(tree):
                if _is_counting_name(name):
                    found.add(f"{module}:{qualname}")
    return sorted(found)


def registered_implementations() -> set[str]:
    """Every ``module:qualname`` claimed by a registered path."""
    claimed: set[str] = set()
    for path in COUNTING_PATHS.values():
        claimed.update(path.implementations)
    return claimed


def unregistered_counting_callables(
    packages: Sequence[str] = COUNTING_SCAN_PACKAGES,
) -> list[str]:
    """Counting-named callables in the known packages that no path claims."""
    claimed = registered_implementations()
    return [c for c in discover_counting_callables(packages) if c not in claimed]
