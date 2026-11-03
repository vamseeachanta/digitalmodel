# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Independent catenary worksheets, based on Catenary Curve (2019).

Equations (3), (4), (9), (10), (32), (34); all loads and limits are synthetic.
"""

import math
from dataclasses import replace

import pytest

from digitalmodel.subsea.umbilicals import LayCase, QualifiedLimit, analyze_lay


def case(**changes):
    """A synthetic touchdown case; limits are supplied worksheet assumptions."""
    values = dict(
        configuration="parallel",
        submerged_weight=10.0,
        horizontal_tension=1000.0,
        vertical_drop=100.0,
        available_length=200.0,
        minimum_bend_radius=QualifiedLimit(100, "Synthetic hand worksheet"),
        minimum_residual_tension=QualifiedLimit(1000, "Synthetic hand worksheet"),
    )
    values.update(changes)
    return LayCase(**values)


def test_touchdown_geometry_and_checks():
    """a=H/w=100 m; z=100 -> s=sqrt(100²+2*100*100)=100*sqrt(3).

    cosh(x/a)=2 -> x=100*ln(2+sqrt(3)); T=H+w*z=2000 N.
    Eq. (32) gives minimum R=a=100 at touchdown; residual T=H=1000.
    Equality passes both supplied limits. No production equation is reused.
    """
    result = analyze_lay(case())
    assert result.suspended_length == pytest.approx(100 * math.sqrt(3))
    assert result.horizontal_span == pytest.approx(100 * math.log(2 + math.sqrt(3)))
    assert result.top_tension == pytest.approx(2000)
    assert result.minimum_radius == pytest.approx(100)
    assert result.touchdown_tension == pytest.approx(1000)
    assert result.touchdown_curvature == pytest.approx(0.01)
    assert result.bend_check.status == "PASS"
    assert result.residual_check.status == "PASS"
    assert result.length_check.status == "PASS"
    assert result.contact_check.status == "PASS"
    assert result.standard_qualification.status == "NOT_EVALUATED"


def test_bundled_uses_total_weight():
    """10+10=20 N/m -> a=50, T=1000+20*100=3000, s=100*sqrt(2)."""
    result = analyze_lay(case(configuration="bundled", companion_weight=10.0))
    assert result.effective_weight == 20
    assert result.minimum_radius == pytest.approx(50)
    assert result.top_tension == pytest.approx(3000)
    assert result.suspended_length == pytest.approx(100 * math.sqrt(2))
    assert result.bend_check.status == "FAIL"
    assert result.configuration_check.status == "NOT_EVALUATED"


def test_length_and_residual_failures():
    """Known hand case needs 173.2 m and supplies 1000 N at touchdown."""
    result = analyze_lay(
        case(
            available_length=100,
            minimum_residual_tension=QualifiedLimit(1001, "Synthetic hand worksheet"),
        )
    )
    assert result.length_check.status == "FAIL"
    assert result.residual_check.status == "FAIL"


def test_equal_height_suspended_span():
    """a=100, X=200*ln(2+sqrt(3)) -> u_end=+-acosh(2).

    Eq. (9): sag=100*(2-1)=100; clearance=150-100=50 m.
    Eq. (10): total s=200*sqrt(3); each support T=1000*2=2000 N.
    No touchdown exists, so residual touchdown tension cannot be evaluated.
    """
    result = analyze_lay(
        case(
            configuration="suspended",
            vertical_drop=0,
            horizontal_span=200 * math.log(2 + math.sqrt(3)),
            lower_support_clearance=150,
            available_length=400,
        )
    )
    assert result.suspended_length == pytest.approx(200 * math.sqrt(3))
    assert result.minimum_clearance == pytest.approx(50)
    assert result.top_tension == pytest.approx(2000)
    assert result.minimum_radius == pytest.approx(100)
    assert result.touchdown_tension is None
    assert result.touchdown_curvature is None
    assert result.residual_check.status == "NOT_EVALUATED"
    assert result.contact_check.status == "PASS"


def test_unequal_supports_with_vertex_outside_span():
    """Set u1=ln(2+sqrt(3)), u2=ln(3+sqrt(8)), a=100.

    z=100*(3-2)=100, X=100*(u2-u1), s=100*(sqrt(8)-sqrt(3)).
    Minimum radius at lower support is a*cosh(u1)^2=400 m;
    minimum height equals the lower support height of 25 m.
    """
    result = analyze_lay(
        case(
            configuration="suspended",
            horizontal_span=100
            * (math.log(3 + math.sqrt(8)) - math.log(2 + math.sqrt(3))),
            lower_support_clearance=25,
        )
    )
    assert result.minimum_radius == pytest.approx(400)
    assert result.minimum_clearance == pytest.approx(25)
    assert result.suspended_length == pytest.approx(100 * (math.sqrt(8) - math.sqrt(3)))
    assert result.top_tension == pytest.approx(3000)


def test_suspended_seabed_intersection_is_failed():
    """The equal-level worksheet sags 100 m below supports at 50 m."""
    result = analyze_lay(
        case(
            configuration="suspended",
            vertical_drop=0,
            horizontal_span=200 * math.log(2 + math.sqrt(3)),
            lower_support_clearance=50,
        )
    )
    assert result.minimum_clearance == pytest.approx(-50)
    assert result.contact_check.status == "FAIL"


def test_missing_limits_do_not_pass():
    result = analyze_lay(case(minimum_bend_radius=None, minimum_residual_tension=None))
    assert result.bend_check.status == "NOT_EVALUATED"
    assert result.residual_check.status == "NOT_EVALUATED"


@pytest.mark.parametrize(
    "changes",
    [
        {"configuration": "unknown"},
        {"submerged_weight": 0},
        {"horizontal_tension": -1},
        {"vertical_drop": -1},
        {"available_length": math.nan},
        {"submerged_weight": math.inf},
        {"configuration": "bundled"},
        {"companion_weight": 1},
        {"configuration": "bundled", "companion_weight": -10},
        {"configuration": "suspended"},
        {"configuration": "suspended", "horizontal_span": 10},
        {
            "configuration": "suspended",
            "horizontal_span": 0,
            "lower_support_clearance": 10,
        },
        {
            "configuration": "suspended",
            "horizontal_span": 10,
            "lower_support_clearance": -1,
        },
        {"horizontal_span": 10},
        {"vertical_drop": 0},
    ],
)
def test_invalid_or_incomplete_lay_inputs(changes):
    with pytest.raises(ValueError):
        analyze_lay(case(**changes))


def test_numerical_overflow_rejected_as_invalid_case():
    model = replace(
        case(),
        configuration="suspended",
        horizontal_span=1e300,
        lower_support_clearance=1,
    )
    with pytest.raises(ValueError, match="numerical range"):
        analyze_lay(model)


def test_worked_example(capsys):
    """Run the documented inputs and compare printed hand worksheet answers.

    Mass=2.625*pi, wet weight=17.25*pi, H=1725*pi, Ttop=3450*pi;
    rounding each to one decimal gives the strings below. Geometry uses
    a=100 m, z=100 m, hence length=100*sqrt(3), x=100*ln(2+sqrt(3)).
    """
    import runpy
    from pathlib import Path

    example = (
        Path(__file__).resolve().parents[3]
        / "examples/subsea/umbilical_sizing_and_lay.py"
    )
    runpy.run_path(str(example), run_name="__main__")
    assert capsys.readouterr().out.splitlines() == [
        "Outer diameter: 60.0 mm",
        "Filled mass: 8.2 kg/m",
        "Air weight: 82.5 N/m",
        "Water weight: 54.2 N/m",
        "External pressure: 1.0 MPa",
        "Burst / collapse: PASS / PASS",
        "Suspended length: 173.2 m",
        "Touchdown offset: 131.7 m",
        "Minimum radius: 100.0 m",
        "Top effective tension: 10838.5 N",
        "Touchdown effective tension: 5419.2 N",
        "Bend / residual: PASS / PASS",
        "Standard qualification: NOT_EVALUATED",
    ]
