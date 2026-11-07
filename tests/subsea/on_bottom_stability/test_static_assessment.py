"""Slices 3–5: independent force-balance envelopes and design roots.

Basis: analysis equations A1–A4 (algebraic equilibrium, not qualified DNV
clauses). Every coefficient and force below is a synthetic assumed input.
Unless overridden W=200, H=40, L=10, mu=.5, FR=0, gamma=2, in SI.
Absolute demand=2*(40+.5*10)=90; capacity=.5*200=100; U=.9.
Passive demand=90; capacity=.5*200+10=110; U=90/110.
Vertical U=20/200=.1; no unfactored-lift method is supported.
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
            0,
            2,
            "assumed arithmetic inputs; analysis A1–A4",
        ),
        **changes,
    )


@pytest.mark.parametrize(
    "method,expected",
    [
        ("absolute", 0.9),
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
    ],
)
def test_exact_unity_and_both_sides(method, weight):
    """Solve A1 by hand without passive credit: W=2*(40/.5+10)=180.
    Acceptance at unity is the chosen demand<=capacity definition (A4).
    """
    result = assess_static(case(weight_n_m=weight), method=method)
    assert result.utilisation == 1
    assert result.arithmetic_pass
    assert not assess_static(case(weight_n_m=weight - 1), method=method).arithmetic_pass
    assert assess_static(case(weight_n_m=weight + 1), method=method).arithmetic_pass


@pytest.mark.parametrize("method", ["absolute"])
def test_monotonic_weight_friction_force_and_direction(method):
    baseline = assess_static(case(), method=method).utilisation
    assert assess_static(case(weight_n_m=300), method=method).utilisation < baseline
    assert assess_static(case(friction=0.8), method=method).utilisation < baseline
    assert assess_static(case(horizontal_n_m=80), method=method).utilisation > baseline
    with pytest.raises(ValueError):
        case(horizontal_n_m=-40)
    assert assess_static(case(lift_n_m=20), method=method).utilisation > baseline


def test_absolute_with_qualified_passive_resistance():
    """Factored demand=90, capacity=100+10=110; root=(90-10)/.5=160."""
    c = case(passive_n_m=10, passive_resistance_basis="initial_penetration")
    result = assess_static(c, method="absolute")
    assert result.lateral_utilisation == pytest.approx(90 / 110)
    solution = solve_required_weight(
        c, method="absolute", lower_n_m=0, upper_n_m=500, tolerance_n_m=1e-7
    )
    assert 160 <= solution.weight_n_m <= 160 + 1e-7


@pytest.mark.parametrize("basis", [None, "motion_generated", ""])
def test_passive_credit_requires_qualified_basis(basis):
    with pytest.raises(ValueError, match="passive_resistance_basis"):
        case(passive_n_m=10, passive_resistance_basis=basis)


def test_uplift_is_separate_from_lateral_balance():
    """Assumed H=0, W=150, L=100, gamma=2: uplift U=200/150>1,
    even with an assumed passive force 1000. W<=L also loses contact.
    """
    result = assess_static(
        case(weight_n_m=150, horizontal_n_m=0, lift_n_m=100, passive_n_m=1000, passive_resistance_basis="prior_loading"),
        method="absolute",
    )
    assert not result.arithmetic_pass
    assert result.governing_check == "uplift"
    lost = assess_static(
        case(weight_n_m=10, lift_n_m=20, passive_n_m=1000, passive_resistance_basis="prior_loading"), method="absolute"
    )
    assert not lost.arithmetic_pass
    assert lost.governing_check == "contact"


@pytest.mark.parametrize("method", ["generalized", "dynamic", "passive_balance", "simplified", "", None])
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
        method="absolute",
        lower_n_m=0,
        upper_n_m=500,
        tolerance_n_m=1e-7,
    )
    assert result.weight_n_m == pytest.approx(200, abs=1e-7)


@pytest.mark.parametrize("factor", [0.98, 1.0])
def test_weight_solver_contact_residual_uses_the_strict_boundary(factor):
    """With H=0 and gamma<=1 the contact infimum is L=101, not gamma*L."""
    c = case(horizontal_n_m=0, lift_n_m=101, safety_factor=factor)
    result = solve_required_weight(
        c, method="absolute", lower_n_m=0, upper_n_m=200,
        tolerance_n_m=1e-7,
    )
    assert result.converged
    assert 101 < result.weight_n_m <= 101 + 1e-7
    assert 0 < result.residual_n_m <= 1e-7
    assert result.residual_n_m == pytest.approx(result.weight_n_m - 101)
    assert assess_static(replace(c, weight_n_m=result.weight_n_m), method="absolute").arithmetic_pass
    assert not assess_static(replace(c, weight_n_m=101), method="absolute").arithmetic_pass


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
    assert limited.status == "not_converged" and not limited.converged
    assert limited.weight_n_m == limited.upper_n_m == 250
    assert limited.residual_n_m >= 0
    assert assess_static(case(weight_n_m=limited.weight_n_m), method="absolute").arithmetic_pass


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
    """W=180 is unity and W=179 fails; selection uses unrounded ratios."""
    rows = [case(label="unity", weight_n_m=180), case(label="over", weight_n_m=179)]
    report = render_report(
        rows, method="absolute", report_date="2026-10-22", revision="0", revision_description="Initial arithmetic issue"
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
                rows, method="absolute", report_date="2026-10-22", revision="0", revision_description="Initial arithmetic issue"
            )


def test_report_reissue_preserves_earlier_revision_rows():
    history = (("0", "2026-10-21", "Initial issue with assumed inputs"),)
    report = render_report(
        [case()],
        method="absolute",
        report_date="2026-10-22",
        revision="1", revision_description="Update synthetic calculation inputs",
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
            revision="0", revision_description="Initial arithmetic issue",
            previous_revisions=history,
        )


def test_precision_limit_retains_feasible_endpoint():
    lower = math.nextafter(180.0, -math.inf)
    result = solve_required_weight(
        case(), method="absolute", lower_n_m=lower, upper_n_m=180,
        tolerance_n_m=1e-20,
    )
    assert result.status == "precision_limit"
    assert not result.converged
    assert result.weight_n_m == result.upper_n_m == 180
    assert result.residual_n_m == 0


def test_report_preserves_dimensionless_inputs_and_near_unity_verdicts():
    # With L=0, gamma=1.15, mu=.25, W=460, U=H/100.
    rows = [case(label=label, weight_n_m=460, horizontal_n_m=h, lift_n_m=0,
                 friction=.25, safety_factor=1.15)
            for label, h in [("over", 104), ("under", 96)]]
    report = render_report(rows, method="absolute", report_date="2026-11-06", revision="1", revision_description="Update synthetic calculation inputs")
    assert "| 0.25 | 0.0 | 1.15 |" in report
    assert "| over | 1.040 | 0.000 | lateral | ARITHMETIC_FAIL |" in report
    assert "| under | 0.960 | 0.000 | lateral | ARITHMETIC_PASS |" in report


@pytest.mark.parametrize("weight,lift,factor", [(100, 50, 1), (7, 3, 1), (100, 99.999, 0.98)])
def test_pure_lift_report_uses_the_canonical_governor(weight, lift, factor):
    c = case(label="pure lift", weight_n_m=weight, horizontal_n_m=0,
             lift_n_m=lift, safety_factor=factor)
    result = assess_static(c, method="absolute")
    assert result.governing_check == "contact"
    assert result.utilisation == pytest.approx(lift / weight)
    report = render_report(
        [c], method="absolute", report_date="2026-11-06", revision="1", revision_description="Update synthetic calculation inputs"
    )
    ratio = lift / weight
    assert f"| pure lift | {factor * ratio:.3f} | {factor * ratio:.3f} | contact | ARITHMETIC_PASS |" in report


def test_report_appends_the_supplied_revision_description():
    history = (
        ("0", "2026-10-21", "Initial issue with assumed inputs"),
        ("1", "2026-10-22", "Align peak checks and report precision with supplied inputs"),
    )
    report = render_report(
        [case()], method="absolute", report_date="2026-11-06", revision="2",
        revision_description="Record strict bed contact and canonical report governors",
        previous_revisions=history,
    )
    for revision, report_date, description in history:
        assert f"| {revision} | {report_date} | {description} |" in report
    assert "| 2 | 2026-11-06 | Record strict bed contact and canonical report governors |" in report
    assert report.count("Align peak checks and report precision with supplied inputs") == 1


@pytest.mark.parametrize("description", [None, "", "   "])
def test_report_rejects_invalid_revision_description(description):
    with pytest.raises(ValueError, match="revision description"):
        render_report(
            [case()], method="absolute", report_date="2026-11-06", revision="2",
            revision_description=description,
        )


def test_report_requires_revision_description():
    with pytest.raises(TypeError, match="revision_description"):
        render_report(
            [case()], method="absolute", report_date="2026-11-06", revision="2"
        )
