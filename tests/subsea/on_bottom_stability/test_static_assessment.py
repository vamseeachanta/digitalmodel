"""Slices 3–5: independent force-balance envelopes and design roots.

Basis: analysis equations A1–A4 (algebraic equilibrium, not qualified DNV
clauses). Every coefficient and force below is a synthetic assumed input.
Unless overridden W=200, abs(H)=40, L=10, mu=.5, FR=10, gamma=2, in SI.
Absolute demand=2*(40+.5*10)=90; capacity=.5*200=100; U=.9.
Passive demand=80; capacity=.5*(200-10)+10=105; U=80/105.
Simplified capacity=.5*(200-10)=95; U=80/95. Vertical U=20/200=.1.
The static ratio historically called generalized is not a dynamic envelope.
"""

from dataclasses import replace
import math

import pytest

from digitalmodel.subsea.on_bottom_stability.assessment import (
    StaticCase,
    assess_static,
    render_report,
    solve_required_weight,
)


def case(**changes):
    return replace(
        StaticCase(
            "synthetic",
            200,
            40,
            10,
            0.5,
            10,
            2,
            "assumed arithmetic inputs; analysis A1–A4",
        ),
        **changes,
    )


@pytest.mark.parametrize(
    "method,expected",
    [
        ("absolute", 0.9),
        ("passive_balance", 80 / 105),
        ("simplified", 80 / 95),
    ],
)
def test_hand_calculated_envelopes(method, expected):
    result = assess_static(case(), method=method)
    assert result.lateral_utilisation == pytest.approx(expected)
    assert result.vertical_utilisation == pytest.approx(0.1)
    assert result.utilisation == pytest.approx(expected)
    assert result.arithmetic_pass
    assert not result.qualified
    assert result.standard_status == "NOT_EVALUATED"
    assert result.dynamic_status == "NOT_EVALUATED"


@pytest.mark.parametrize(
    "method,weight",
    [
        ("absolute", 180),
        ("passive_balance", 150),
        ("simplified", 170),
    ],
)
def test_exact_unity_and_both_sides(method, weight):
    """Solve A1–A3 by hand: absolute W=2*(40/.5+10)=180;
    passive W=(2*40-10)/.5+10=150; simplified W=2*40/.5+10=170.
    Acceptance at unity is the chosen demand<=capacity definition (A4).
    """
    result = assess_static(case(weight_n_m=weight), method=method)
    assert result.utilisation == 1
    assert result.arithmetic_pass
    assert not assess_static(case(weight_n_m=weight - 1), method=method).arithmetic_pass
    assert assess_static(case(weight_n_m=weight + 1), method=method).arithmetic_pass


@pytest.mark.parametrize("method", ["absolute", "passive_balance", "simplified"])
def test_monotonic_weight_friction_force_and_direction(method):
    baseline = assess_static(case(), method=method).utilisation
    assert assess_static(case(weight_n_m=300), method=method).utilisation < baseline
    assert assess_static(case(friction=0.8), method=method).utilisation < baseline
    assert assess_static(case(horizontal_n_m=80), method=method).utilisation > baseline
    assert (
        assess_static(case(horizontal_n_m=-40), method=method).utilisation == baseline
    )
    assert assess_static(case(lift_n_m=20), method=method).utilisation > baseline


def test_simplified_can_be_less_or_more_conservative_than_absolute():
    """A1/A3: Us<Ua iff H<mu*(W-L), for L>0. Here threshold=95.
    H=40 is below it; H=100 is above it. Neither method bounds the other.
    """
    for horizontal, less in [(40, True), (100, False)]:
        c = case(horizontal_n_m=horizontal)
        assert (
            assess_static(c, method="simplified").utilisation
            < assess_static(c, method="absolute").utilisation
        ) is less


def test_uplift_is_separate_from_lateral_balance():
    """Assumed H=0, W=150, L=100, gamma=2: uplift U=200/150>1,
    even with an assumed passive force 1000. W<=L also loses contact.
    """
    result = assess_static(
        case(weight_n_m=150, horizontal_n_m=0, lift_n_m=100, passive_n_m=1000),
        method="passive_balance",
    )
    assert not result.arithmetic_pass
    assert result.governing_check == "uplift"
    lost = assess_static(
        case(weight_n_m=10, lift_n_m=20, passive_n_m=1000), method="passive_balance"
    )
    assert not lost.arithmetic_pass
    assert lost.governing_check == "contact"


@pytest.mark.parametrize("method", ["generalized", "dynamic", "", None])
def test_unimplemented_or_unknown_method_is_rejected(method):
    with pytest.raises(ValueError):
        assess_static(case(), method=method)


@pytest.mark.parametrize(
    "field,value",
    [
        ("weight_n_m", math.nan),
        ("horizontal_n_m", math.inf),
        ("lift_n_m", -1),
        ("friction", -0.5),
        ("passive_n_m", -1),
        ("safety_factor", 0),
        ("safety_factor", True),
        ("label", ""),
        ("factor_source", ""),
    ],
)
def test_invalid_static_case_rejected(field, value):
    with pytest.raises(ValueError):
        case(**{field: value})


@pytest.mark.parametrize(
    "method,expected",
    [
        ("absolute", 180),
        ("passive_balance", 150),
        ("simplified", 170),
    ],
)
def test_weight_solver_against_hand_solvable_root(method, expected):
    """Roots derived in test_exact_unity_and_both_sides, independent of solver.
    A numerical interval of 1e-7 N/m is a test resolution, not a design margin.
    """
    result = solve_required_weight(
        case(), method=method, lower_n_m=0, upper_n_m=500, tolerance_n_m=1e-7
    )
    assert result.converged
    assert expected <= result.weight_n_m <= expected + 1e-7
    assert result.upper_n_m - result.lower_n_m <= 1e-7
    assert result.residual_n_m >= 0
    assert assess_static(
        case(weight_n_m=result.weight_n_m), method=method
    ).arithmetic_pass


def test_weight_solver_uplift_root():
    """H=0, L=100, gamma=2 => W>=200, independently of passive credit."""
    result = solve_required_weight(
        case(horizontal_n_m=0, lift_n_m=100),
        method="passive_balance",
        lower_n_m=0,
        upper_n_m=500,
        tolerance_n_m=1e-7,
    )
    assert result.weight_n_m == pytest.approx(200, abs=1e-7)


def test_solver_bracket_endpoints_failure_and_iteration_limit():
    """Absolute closed-form root=180. Feasible endpoints must stay feasible."""
    kwargs = dict(method="absolute", tolerance_n_m=1e-7)
    assert (
        solve_required_weight(case(), lower_n_m=180, upper_n_m=500, **kwargs).weight_n_m
        == 180
    )
    assert (
        solve_required_weight(case(), lower_n_m=0, upper_n_m=180, **kwargs).weight_n_m
        == 180
    )
    failed = solve_required_weight(case(), lower_n_m=0, upper_n_m=100, **kwargs)
    assert failed.status == "unbracketed" and failed.weight_n_m is None
    limited = solve_required_weight(
        case(), lower_n_m=0, upper_n_m=500, max_iterations=1, **kwargs
    )
    assert limited.status == "not_converged" and limited.weight_n_m is None


@pytest.mark.parametrize(
    "changes",
    [
        {"lower_n_m": -1},
        {"upper_n_m": 0},
        {"upper_n_m": math.inf},
        {"tolerance_n_m": 0},
        {"max_iterations": True},
        {"max_iterations": 0},
    ],
)
def test_invalid_solver_controls_rejected(changes):
    kwargs = dict(method="absolute", lower_n_m=0, upper_n_m=500, tolerance_n_m=1e-7)
    kwargs.update(changes)
    with pytest.raises(ValueError):
        solve_required_weight(case(), **kwargs)


def test_report_contains_inputs_method_unrounded_governing_case_and_limits():
    """W=180 is unity, W=179 fails though both display 1.0 at one decimal."""
    rows = [case(label="unity", weight_n_m=180), case(label="over", weight_n_m=179)]
    report = render_report(
        rows, method="absolute", report_date="2026-10-22", revision="0"
    )
    assert "Governing case: over" in report
    assert report.index("## Revision history") < report.index("## Inputs")
    assert "180.0" in report and "179.0" in report
    assert "ARITHMETIC_FAIL" in report and "ARITHMETIC_PASS" in report
    assert "NOT_EVALUATED" in report
    assert "assumed arithmetic inputs" in report
    assert "passive" in report and "safety factor" in report


def test_report_rejects_empty_and_duplicate_cases():
    for rows in ([], [case(), case()]):
        with pytest.raises(ValueError):
            render_report(
                rows, method="absolute", report_date="2026-10-22", revision="0"
            )


def test_report_reissue_preserves_earlier_revision_rows():
    history = (("0", "2026-10-21", "Initial issue with assumed inputs"),)
    report = render_report(
        [case()],
        method="absolute",
        report_date="2026-10-22",
        revision="1",
        previous_revisions=history,
    )
    assert "| 0 | 2026-10-21 | Initial issue with assumed inputs |" in report
    assert "| 1 | 2026-10-22 |" in report
    assert history == (("0", "2026-10-21", "Initial issue with assumed inputs"),)
    with pytest.raises(ValueError, match="revision"):
        render_report(
            [case()],
            method="absolute",
            report_date="2026-10-22",
            revision="0",
            previous_revisions=history,
        )
