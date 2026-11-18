# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Independent hand calculations for a uniform, inextensible pinned line.

The synthetic fixture derives from x=a*u, z=a*(cosh(u)-1), s=a*sinh(u),
H=w*a and V=w*s. These identities follow by integrating dz/dx=sinh(u)
and ds/dx=cosh(u), with u=x/a. No solver output defines an expectation.
"""

from math import cosh, inf, log, sinh, sqrt, ulp

import pytest

from digitalmodel.marine_ops.marine_analysis.catenary.solver import (
    CatenaryInput,
    CatenarySolver,
)


# Round-off allowance: square root of binary64 spacing at unity.
ROUND_OFF = sqrt(ulp(1.0))


@pytest.fixture(params=["rising", "sagging", "descending", "level"])
def hand_case(request):
    """Choose a=80 m, w=2.5 N/m, H=200 N; these are synthetic inputs.

    At u=ln(2): exp(u)=2, exp(-u)=1/2, so cosh(u)=5/4,
    sinh(u)=3/4, x=80*ln(2) m, z=20 m, s=60 m, V=150 N,
    and T=H*cosh(u)=250 N.
    At u=ln(4)=2*ln(2): cosh(u)=17/8, sinh(u)=15/8,
    x=160*ln(2) m, z=90 m, s=150 m, V=375 N, T=425 N.
    Negative u reverses x, s and V, while z and T are unchanged.
    Differences of signed s give lengths 90, 210, 90 and 120 m.
    Infinity for EA removes extension, rather than approximating rigidity.
    """
    near_x, far_x = 80.0 * log(2.0), 160.0 * log(2.0)
    cases = {
        "rising": ((near_x, 20.0), (far_x, 90.0), 90.0, 150.0, 375.0, 250.0, 425.0),
        "sagging": ((-near_x, 20.0), (far_x, 90.0), 210.0, -150.0, 375.0, 250.0, 425.0),
        "descending": ((-far_x, 90.0), (-near_x, 20.0), 90.0, -375.0, -150.0, 425.0, 250.0),
        "level": ((-near_x, 20.0), (near_x, 20.0), 120.0, -150.0, 150.0, 250.0, 250.0),
    }
    start, end, length, start_v, end_v, start_t, end_t = cases[request.param]
    return {
        "start": start, "end": end, "length": length,
        "submerged_weight": 2.5, "parameter": 80.0, "horizontal": 200.0,
        "start_v": start_v, "end_v": end_v, "start_t": start_t, "end_t": end_t,
    }


def test_pinned_catenary_geometry(hand_case):
    """Check both endpoints, full shape, parameter and tension independently."""
    from examples.subsea.mooring_analysis.pinned_catenary import solve_pinned_catenary

    case = hand_case
    result = solve_pinned_catenary(
        case["start"], case["end"], case["length"], case["submerged_weight"],
    )

    assert result.converged
    assert result.elongation == 0.0
    assert result.catenary_parameter == pytest.approx(case["parameter"], rel=ROUND_OFF)
    assert result.horizontal_tension == pytest.approx(case["horizontal"], rel=ROUND_OFF)
    assert (result.shape_x[0], result.shape_y[0]) == pytest.approx(case["start"], rel=ROUND_OFF)
    assert (result.shape_x[-1], result.shape_y[-1]) == pytest.approx(case["end"], rel=ROUND_OFF)
    assert all(right > left for left, right in zip(result.shape_x, result.shape_x[1:]))
    # The hand fixture's vertex is (0, 0). Evaluate its entire analytic curve,
    # using the prescribed a and H, never parameters recovered by the solver.
    expected_z = [case["parameter"] * (cosh(x / case["parameter"]) - 1.0)
                  for x in result.shape_x]
    expected_t = [case["horizontal"] * cosh(x / case["parameter"])
                  for x in result.shape_x]
    assert result.shape_y == pytest.approx(expected_z, rel=ROUND_OFF, abs=ROUND_OFF)
    assert result.tension_distribution == pytest.approx(expected_t, rel=ROUND_OFF)
    # Integrate the hand curve using s=a*sinh(u), u=x/a, to check length.
    a = case["parameter"]
    suspended = a * (sinh(result.shape_x[-1] / a) - sinh(result.shape_x[0] / a))
    assert suspended == pytest.approx(case["length"], rel=ROUND_OFF)


def test_pinned_catenary_force_balance(hand_case):
    """Signed end-force difference equals weight of the suspended length."""
    from examples.subsea.mooring_analysis.pinned_catenary import solve_pinned_catenary

    case = hand_case
    result = solve_pinned_catenary(
        case["start"], case["end"], case["length"], case["submerged_weight"],
    )
    assert result.total_tension_anchor == pytest.approx(case["start_t"], rel=ROUND_OFF)
    assert result.total_tension_fairlead == pytest.approx(case["end_t"], rel=ROUND_OFF)
    # The API exposes the start resultant, not its signed vertical component.
    # Recover magnitude by Pythagoras; the hand geometry supplies its sign.
    # Do not derive V_start from V_end-w*L: that would assume this check.
    start_magnitude = sqrt(result.total_tension_anchor**2 - result.horizontal_tension**2)
    start_v = start_magnitude if case["start_v"] > 0.0 else -start_magnitude
    assert start_v == pytest.approx(case["start_v"], rel=ROUND_OFF)
    assert result.vertical_tension_fairlead == pytest.approx(case["end_v"], rel=ROUND_OFF)
    assert result.vertical_tension_fairlead - start_v == pytest.approx(
        case["submerged_weight"] * case["length"], rel=ROUND_OFF,
    )


def test_taut_pinned_geometry_is_rejected():
    """A synthetic 3-4-5 triangle has no finite-tension weighted catenary."""
    params = CatenaryInput(
        length=5.0, horizontal_span=3.0, vertical_span=4.0,
        weight_per_length=2.5, ea_stiffness=inf,
    )
    with pytest.raises(ValueError, match="Length must exceed straight distance"):
        CatenarySolver().solve(params)


@pytest.mark.parametrize(
    "start,end,length,weight,message",
    [
        # Synthetic spans: unit horizontal/vertical distances and line length.
        ((0.0, 0.0), (0.0, 1.0), 2.0, 2.5, "Horizontal span must be positive"),
        ((1.0, 0.0), (0.0, 1.0), 2.0, 2.5, "Horizontal span must be positive"),
        ((0.0, 0.0), (1.0, 1.0), 1.0, 2.5, "straight distance"),
        ((0.0, 0.0), (1.0, 1.0), 2.0, 0.0, "Weight per length must be positive"),
        ((0.0, 0.0), (inf, 1.0), 2.0, 2.5, "finite"),
        ((0.0, 0.0), (1.0, 1.0), inf, 2.5, "finite"),
        ((0.0, 0.0), (1.0, 1.0), 2.0, float("nan"), "finite"),
    ],
)
def test_unsupported_pinned_geometry(start, end, length, weight, message):
    """Unsupported inputs give a diagnostic instead of a numerical result."""
    from examples.subsea.mooring_analysis.pinned_catenary import solve_pinned_catenary

    with pytest.raises(ValueError, match=message):
        solve_pinned_catenary(start, end, length, weight)
