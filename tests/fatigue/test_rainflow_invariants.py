"""Correctness gate for every cycle-counting path in the repository.

Issue: https://github.com/vamseeachanta/workspace-hub/issues/3839
Plan:  workspace-hub docs/plans/2026-09-11-issue-3839-rainflow-divergence.md

The contract under test lives in ``digitalmodel.fatigue.counting_contract`` and is
callable independently of pytest. This module applies it to every registered path
and, in tests 8 to 10, tests the contract itself: without those the contract is
unfalsifiable.

Test map (the plan's 14 rows):

===  ======================================================================
  1  exact distribution over a conforming path against the hand-derived table
  2  contract over ``pylife_4pt``, four signals
  3  contract over ``pypi_rainflow``, four signals
  4  contract over ``fapps_counter``, four signals
  5  contract over ``sigproc_astm``, broadband — xfail(strict)
  6  contract over ``calm_buoy``, narrow-band sine — xfail(strict)
  7  contract over ``fapps_counting`` and ``struct_fatigue``, broadband — xfail(strict)
  8  a counter returning one full-span half cycle plus fabricated counts
  9  a counter returning the correct count total but wrong ranges
 10  a declared discard-residual counter omitting the span
 11  a constant signal
 12  a two-point signal
 13  each failing path invoked for damage calculation
 14  registry completeness over the known fatigue and signal-processing packages
===  ======================================================================
"""

from __future__ import annotations

import numpy as np
import pytest

from digitalmodel.fatigue.counting_contract import (
    CONTRACT_EXACT,
    CONTRACT_SIGNALS,
    COUNTING_PATHS,
    ContractViolation,
    CycleCount,
    NonConservativeCountingError,
    ResidualPolicy,
    assert_counting_contract,
    assert_exact_distribution,
    check_counting_contract,
    peak_to_valley_span,
    reversals_with_endpoints,
    unregistered_counting_callables,
)

ISSUE = "https://github.com/vamseeachanta/workspace-hub/issues/3839"

SIGNAL_KEYS = tuple(CONTRACT_SIGNALS)


def run_path(key: str, signal: np.ndarray) -> CycleCount:
    return COUNTING_PATHS[key].count(signal)


def assert_path_contract(key: str, signal_key: str) -> None:
    path = COUNTING_PATHS[key]
    signal = CONTRACT_SIGNALS[signal_key]
    assert_counting_contract(path.count(signal), signal, path.residual_policy)


# ---------------------------------------------------------------------------
# Registry sanity — every path in the issue is registered with a declared policy
# ---------------------------------------------------------------------------


def test_registry_declares_all_seven_paths_with_a_residual_policy():
    expected = {
        "pylife_4pt",
        "pypi_rainflow",
        "sigproc_astm",
        "calm_buoy",
        "fapps_counting",
        "fapps_counter",
        "struct_fatigue",
    }
    assert set(COUNTING_PATHS) == expected
    for path in COUNTING_PATHS.values():
        assert isinstance(path.residual_policy, ResidualPolicy)


# ---------------------------------------------------------------------------
# 1 — exact distribution on the hand-derived fixture
# ---------------------------------------------------------------------------


def test_1_exact_distribution_matches_hand_derived_table():
    """The literals in CONTRACT_EXACT are hand-derived, never captured."""
    signal = np.asarray(CONTRACT_EXACT["signal"], dtype=float)
    result = COUNTING_PATHS["pylife_4pt"].count(signal)
    assert_exact_distribution(result, CONTRACT_EXACT["expected"])


def test_1b_hand_derived_table_is_self_consistent():
    """The committed table must itself satisfy the contract it is used to check."""
    signal = np.asarray(CONTRACT_EXACT["signal"], dtype=float)
    expected = CONTRACT_EXACT["expected"]
    total = sum(count for _, _, count in expected)
    assert 2 * total == len(reversals_with_endpoints(signal)) - 1
    assert max(rng for rng, _, _ in expected) == pytest.approx(
        peak_to_valley_span(signal)
    )


# ---------------------------------------------------------------------------
# 2, 3, 4 — the conforming paths
# ---------------------------------------------------------------------------


@pytest.mark.parametrize("signal_key", SIGNAL_KEYS)
def test_2_contract_pylife_4pt(signal_key):
    assert_path_contract("pylife_4pt", signal_key)


@pytest.mark.parametrize("signal_key", SIGNAL_KEYS)
def test_3_contract_pypi_rainflow(signal_key):
    assert_path_contract("pypi_rainflow", signal_key)


@pytest.mark.parametrize("signal_key", SIGNAL_KEYS)
def test_4_contract_fapps_counter(signal_key):
    assert_path_contract("fapps_counter", signal_key)


# ---------------------------------------------------------------------------
# 5, 6, 7 — the non-conservative paths
#
# strict=True: a repair reports as a FAILURE demanding this marker's removal,
# rather than passing silently as XPASS.
# ---------------------------------------------------------------------------


@pytest.mark.xfail(
    strict=True,
    reason=(
        "sigproc_astm does not implement the three-point rule: core/rainflow.py:"
        "162-164 inverts both the comparison and the extracted range relative to "
        "ASTM 5.4.4, and every extraction emits 0.5 and pops stack[-2], so the "
        f"full-cycle rule never fires — {ISSUE}"
    ),
)
def test_5_contract_sigproc_astm_broadband():
    assert_path_contract("sigproc_astm", "broadband_random")


@pytest.mark.xfail(
    strict=True,
    reason=(
        "calm_buoy_fatigue.py:267 discards the second point of the counted pair "
        "where ASTM rule 2 discards the first, and :268 breaks instead of "
        "re-checking the shortened stack; on a pure sine it reports 50.0 of a "
        f"100.0 span — {ISSUE}"
    ),
)
def test_6_contract_calm_buoy_narrow_band():
    assert_path_contract("calm_buoy", "narrow_band_sine")


@pytest.mark.xfail(
    strict=True,
    reason=f"fapps_counting understates the maximum range on broadband input — {ISSUE}",
)
def test_7a_contract_fapps_counting_broadband():
    assert_path_contract("fapps_counting", "broadband_random")


@pytest.mark.xfail(
    strict=True,
    reason=f"struct_fatigue understates the maximum range on broadband input — {ISSUE}",
)
def test_7b_contract_struct_fatigue_broadband():
    assert_path_contract("struct_fatigue", "broadband_random")


# Recorded verdict per (path, signal), measured 2026-09-11 and committed as the
# behaviour record the deferral argument rests on. `True` means the path
# satisfies every clause on that signal. Any change — a repair, a regression, or
# a change to the contract itself — moves a cell and fails here, so the record
# cannot go stale silently. See
# docs/domains/fatigue/rainflow-path-exposure-2026-09-11.md.
RECORDED_VERDICTS = {
    ("pylife_4pt", "astm_example"): True,
    ("pylife_4pt", "narrow_band_sine"): True,
    ("pylife_4pt", "broadband_random"): True,
    ("pylife_4pt", "residual_dominated"): True,
    ("pypi_rainflow", "astm_example"): True,
    ("pypi_rainflow", "narrow_band_sine"): True,
    ("pypi_rainflow", "broadband_random"): True,
    ("pypi_rainflow", "residual_dominated"): True,
    ("fapps_counter", "astm_example"): True,
    ("fapps_counter", "narrow_band_sine"): True,
    ("fapps_counter", "broadband_random"): True,
    ("fapps_counter", "residual_dominated"): True,
    # clause 1 on three signals; on the sine the maximum range is correct but
    # 4.5 cycles are emitted as zero-range rows and clause 3 fails instead.
    ("sigproc_astm", "astm_example"): False,
    ("sigproc_astm", "narrow_band_sine"): False,
    ("sigproc_astm", "broadband_random"): False,
    ("sigproc_astm", "residual_dominated"): False,
    # clause 1 on all four; clause 3 as well on the ASTM fixture (3.5 of 4.0).
    ("calm_buoy", "astm_example"): False,
    ("calm_buoy", "narrow_band_sine"): False,
    ("calm_buoy", "broadband_random"): False,
    ("calm_buoy", "residual_dominated"): False,
    # conforming on the two signals the existing suite covered; clause 1 fails
    # on broadband and on the residual-dominated ramp.
    ("fapps_counting", "astm_example"): True,
    ("fapps_counting", "narrow_band_sine"): True,
    ("fapps_counting", "broadband_random"): False,
    ("fapps_counting", "residual_dominated"): False,
    ("struct_fatigue", "astm_example"): True,
    ("struct_fatigue", "narrow_band_sine"): True,
    ("struct_fatigue", "broadband_random"): False,
    ("struct_fatigue", "residual_dominated"): False,
}


@pytest.mark.parametrize(
    ("path_key", "signal_key"),
    sorted(RECORDED_VERDICTS),
    ids=[f"{p}-{s}" for p, s in sorted(RECORDED_VERDICTS)],
)
def test_7c_recorded_verdict_matrix(path_key, signal_key):
    path = COUNTING_PATHS[path_key]
    signal = CONTRACT_SIGNALS[signal_key]
    violations = check_counting_contract(path.count(signal), signal, path.residual_policy)
    conforms = not violations
    expected = RECORDED_VERDICTS[(path_key, signal_key)]
    assert conforms is expected, (
        f"{path_key} on {signal_key}: recorded {expected}, measured {conforms}.\n"
        + "\n".join(violations)
        + f"\nUpdate RECORDED_VERDICTS and the exposure record together — {ISSUE}"
    )


# ---------------------------------------------------------------------------
# 8, 9, 10 — the contract's own tests
# ---------------------------------------------------------------------------


def test_8_fabricated_counts_fail_clause_3():
    """Clauses 1 and 2 alone are not sufficient.

    This fake counter emits exactly one half cycle at the full peak-to-valley
    span — satisfying clause 1 and clause 2 — and then fabricates additional
    counts at smaller ranges. Only the exact count-conservation equality of
    clause 3 rejects it.
    """
    signal = CONTRACT_SIGNALS["astm_example"]
    span = peak_to_valley_span(signal)
    fabricated = CycleCount(
        ranges=np.array([span, span / 3.0]),
        counts=np.array([0.5, 7.0]),
        means=np.array([0.5, 0.0]),
    )

    violations = check_counting_contract(fabricated, signal, ResidualPolicy.RETAIN)
    assert violations, "the contract accepted a fabricated count distribution"
    assert any(v.startswith("clause 3") for v in violations), violations
    assert not any(v.startswith("clause 1") for v in violations), violations
    assert not any(v.startswith("clause 2") for v in violations), violations

    with pytest.raises(ContractViolation):
        assert_counting_contract(fabricated, signal, ResidualPolicy.RETAIN)


def test_9_correct_total_wrong_ranges_fails():
    """Correct count conservation does not license arbitrary ranges."""
    signal = CONTRACT_SIGNALS["astm_example"]
    span = peak_to_valley_span(signal)
    n_rev = len(reversals_with_endpoints(signal))
    total = (n_rev - 1) / 2.0

    wrong = CycleCount(
        ranges=np.full(int(total * 2), span / 2.0),
        counts=np.full(int(total * 2), 0.5),
        means=np.zeros(int(total * 2)),
    )

    violations = check_counting_contract(wrong, signal, ResidualPolicy.RETAIN)
    assert any(v.startswith("clause 1") for v in violations), violations
    assert not any(v.startswith("clause 3") for v in violations), violations

    with pytest.raises(ContractViolation):
        assert_exact_distribution(wrong, CONTRACT_EXACT["expected"])


def test_10_declared_discard_residual_counter_passes():
    """Clause 1 is conditional, and must not condemn a valid convention.

    A discard-residual implementation legitimately omits the global span when
    the span appears only in the residual. The contract must accept it.
    """
    signal = CONTRACT_SIGNALS["astm_example"]
    span = peak_to_valley_span(signal)

    # The closed cycles of the ASTM fixture only: the span (9.0) lives in the
    # residual, so a discard-residual counter never emits it.
    discarding = CycleCount(
        ranges=np.array([4.0]),
        counts=np.array([1.0]),
        means=np.array([1.0]),
    )

    assert max(discarding.ranges) < span
    assert check_counting_contract(discarding, signal, ResidualPolicy.DISCARD) == []
    assert_counting_contract(discarding, signal, ResidualPolicy.DISCARD)

    # The identical result under a RETAIN declaration must be rejected.
    retained_violations = check_counting_contract(
        discarding, signal, ResidualPolicy.RETAIN
    )
    assert any(v.startswith("clause 1") for v in retained_violations)


def test_10b_discard_residual_may_not_manufacture_cycles():
    """DISCARD relaxes clause 3's equality to an inequality, not to nothing."""
    signal = CONTRACT_SIGNALS["astm_example"]
    n_rev = len(reversals_with_endpoints(signal))
    too_many = CycleCount(
        ranges=np.full(20, 4.0),
        counts=np.full(20, 1.0),
        means=np.zeros(20),
    )
    assert 2 * float(np.sum(too_many.counts)) > n_rev - 1
    violations = check_counting_contract(too_many, signal, ResidualPolicy.DISCARD)
    assert any(v.startswith("clause 3") for v in violations), violations


# ---------------------------------------------------------------------------
# 11, 12 — degenerate signals
# ---------------------------------------------------------------------------


@pytest.mark.parametrize("key", ["pylife_4pt", "pypi_rainflow", "fapps_counter"])
def test_11_constant_signal(key):
    """Zero span is handled without division and without a damaging cycle.

    The three conforming paths disagree here, and both conventions are
    admitted: pyLife and the PyPI package emit one zero-range half cycle,
    ``fapps_counter`` emits nothing. A zero range contributes exactly zero
    damage, so neither is wrong; what the contract must reject is a zero-range
    row emitted for a signal that moves, which test_11b covers.
    """
    signal = np.full(64, 7.5)
    assert peak_to_valley_span(signal) == 0.0
    assert len(reversals_with_endpoints(signal)) == 1

    result = COUNTING_PATHS[key].count(signal)
    damaging = result.ranges[result.ranges > 0.0]
    assert damaging.size == 0
    assert_counting_contract(result, signal, COUNTING_PATHS[key].residual_policy)


def test_11b_zero_range_row_is_rejected_when_the_signal_moves():
    signal = CONTRACT_SIGNALS["astm_example"]
    padded = CycleCount(
        ranges=np.array([9.0, 0.0, 0.0]),
        counts=np.array([0.5, 3.0, 0.5]),
        means=np.array([0.5, 0.0, 0.0]),
    )
    violations = check_counting_contract(padded, signal, ResidualPolicy.RETAIN)
    assert any("zero-range" in v for v in violations), violations


@pytest.mark.parametrize("key", ["pylife_4pt", "fapps_counter"])
def test_12_two_point_signal(key):
    """A single ramp is one half cycle at the full span."""
    signal = np.array([0.0, 10.0])
    assert peak_to_valley_span(signal) == 10.0
    assert len(reversals_with_endpoints(signal)) == 2

    result = COUNTING_PATHS[key].count(signal)
    assert float(np.sum(result.counts)) == pytest.approx(0.5)
    assert_counting_contract(result, signal, COUNTING_PATHS[key].residual_policy)


@pytest.mark.xfail(
    strict=True,
    reason=(
        "Divergence found by this gate, not by the issue: rainflow 3.2.0's "
        "extract_cycles returns nothing for a two-sample series, so the single "
        "half cycle at the full span is lost. Third-party behaviour, degenerate "
        "input, recorded rather than hidden — see the exposure record for #3839."
    ),
)
def test_12b_pypi_rainflow_two_point_signal():
    signal = np.array([0.0, 10.0])
    path = COUNTING_PATHS["pypi_rainflow"]
    assert_counting_contract(path.count(signal), signal, path.residual_policy)


# ---------------------------------------------------------------------------
# 13 — refusal of damage calculation
# ---------------------------------------------------------------------------


def _dummy_self(cls):
    """An uninitialised instance, sufficient because refusal is the first statement."""
    return cls.__new__(cls)


def _damage_calls():
    """(path_key, label, thunk) for every damage entry point bound to a failing path."""
    calls = []

    def sigproc_perform():
        from digitalmodel.solvers.orcaflex.time_trace_processor import (
            OrcaFlexTimeTraceProcessor,
        )

        OrcaFlexTimeTraceProcessor._perform_fatigue_analysis(
            _dummy_self(OrcaFlexTimeTraceProcessor), None
        )

    def sigproc_single():
        from digitalmodel.solvers.orcaflex.time_trace_processor import (
            OrcaFlexTimeTraceProcessor,
        )

        OrcaFlexTimeTraceProcessor._analyze_single_trace(
            None, "x", 1.0, {}, None, 25.0, 1.0
        )

    calls += [
        ("sigproc_astm", "OrcaFlexTimeTraceProcessor._perform_fatigue_analysis", sigproc_perform),
        ("sigproc_astm", "OrcaFlexTimeTraceProcessor._analyze_single_trace", sigproc_single),
    ]

    def calm_compute_life():
        from digitalmodel.marine_ops.marine_engineering.calm_buoy_fatigue import (
            CHAIN_SN_SEAWATER,
            compute_fatigue_life,
        )

        compute_fatigue_life(None, CHAIN_SN_SEAWATER)

    def calm_scatter():
        from digitalmodel.marine_ops.marine_engineering.calm_buoy_fatigue import (
            CHAIN_SN_SEAWATER,
            ScatterDiagramFatigue,
        )

        ScatterDiagramFatigue(sn_curve=CHAIN_SN_SEAWATER).compute(None, {})

    calls += [
        ("calm_buoy", "calm_buoy_fatigue.compute_fatigue_life", calm_compute_life),
        ("calm_buoy", "ScatterDiagramFatigue.compute", calm_scatter),
    ]

    def fapps_batch():
        from digitalmodel.structural.fatigue_apps.rainflow_counting import (
            RainflowCounter,
        )

        RainflowCounter.process_batch(_dummy_self(RainflowCounter), ".")

    calls += [("fapps_counting", "rainflow_counting.RainflowCounter.process_batch", fapps_batch)]

    def struct_engine():
        from digitalmodel.structural.fatigue.analysis import FatigueAnalysisEngine

        FatigueAnalysisEngine.analyze_time_series(
            _dummy_self(FatigueAnalysisEngine), None
        )

    def struct_quick_tds():
        from digitalmodel.structural.fatigue.analysis import quick_time_domain_analysis

        quick_time_domain_analysis(None)

    def struct_quick():
        from digitalmodel.structural.fatigue import quick_fatigue_analysis

        quick_fatigue_analysis(None)

    calls += [
        ("struct_fatigue", "FatigueAnalysisEngine.analyze_time_series", struct_engine),
        ("struct_fatigue", "analysis.quick_time_domain_analysis", struct_quick_tds),
        ("struct_fatigue", "structural.fatigue.quick_fatigue_analysis", struct_quick),
    ]

    return calls


@pytest.mark.parametrize(
    ("path_key", "label", "thunk"),
    _damage_calls(),
    ids=[f"{k}::{label}" for k, label, _ in _damage_calls()],
)
def test_13_failing_path_refuses_damage_calculation(path_key, label, thunk):
    with pytest.raises(NonConservativeCountingError) as excinfo:
        thunk()
    message = str(excinfo.value)
    assert "3839" in message, message
    assert path_key in message, message


def test_13b_every_failing_path_has_at_least_one_refusing_entry_point():
    failing = {
        key for key, path in COUNTING_PATHS.items() if path.contract_status == "non-conservative"
    }
    covered = {key for key, _, _ in _damage_calls()}
    assert failing == covered, (failing, covered)


def test_13c_conforming_paths_are_not_blocked():
    """Refusal must not spread to a path the contract accepts."""
    for key in ("pylife_4pt", "pypi_rainflow", "fapps_counter"):
        assert COUNTING_PATHS[key].damage_entry_points == ()


def test_13d_cycle_counting_itself_remains_callable():
    """Refusal targets damage, not the diagnostic use of the counters."""
    for key in COUNTING_PATHS:
        result = COUNTING_PATHS[key].count(CONTRACT_SIGNALS["astm_example"])
        assert len(result.ranges) > 0


# ---------------------------------------------------------------------------
# 14 — registry completeness
# ---------------------------------------------------------------------------


def test_14_registry_completeness_over_known_packages():
    """A counting-named callable in the known packages cannot be added unregistered.

    Scope, stated honestly: this is a name-pattern scan over the fatigue and
    signal-processing packages named in COUNTING_SCAN_PACKAGES. A counting
    implementation under a novel name, or in an unrelated package, escapes it.
    The gate does not claim otherwise.
    """
    unregistered = unregistered_counting_callables()
    assert unregistered == [], (
        "counting-named callables found that no registered path claims:\n  "
        + "\n  ".join(unregistered)
        + f"\n\nRegister each in COUNTING_PATHS[...].implementations — {ISSUE}"
    )
