"""Closed-form regressions for a uniform, inextensible touchdown catenary.

Source: General Physics I: Classical Mechanics, section 19.3, The Catenary
(2024 web edition), https://phys.libretexts.org/@go/page/92208.
The identities z=a(cosh(x/a)-1), s=a*sinh(x/a), a=H/w give
s²=z²+2az, H=w(s²-z²)/(2z), and T=H+wz by force balance.
"""

from math import atan, degrees, log, sqrt

import pytest

from digitalmodel.subsea.mooring_analysis.catenary import CatenaryAnalyzer
from digitalmodel.subsea.mooring_analysis.models import LineType, MooringLineProperties


# Acceptance: issue 2561 requires agreement within 0.1%, i.e. relative 0.001.
REL_TOL = 0.001


@pytest.fixture
def line():
    """Synthetic inputs, not material data; only length and weight enter here.

    Use 1000 kg/m so the existing __init__ gravity convention (9.81 m/s²)
    gives w=9.81 kN/m. The remaining properties are arbitrary positive inputs.
    """
    return MooringLineProperties(
        line_type=LineType.CHAIN, length=500.0, diameter=1.0,
        mbl=1.0, weight_water=1000.0, ea=1.0,
    )


@pytest.mark.parametrize(
    "height, suspended, tension, reach, top_tension, slope",
    [
        (100.0, 100.0 * sqrt(3.0), 981.0,
         100.0 * log(2.0 + sqrt(3.0)), 1962.0, sqrt(3.0)),
        (50.0, 150.0, 1962.0, 200.0 * log(2.0), 2452.5, 0.75),
    ],
)
def test_vertical_span_closed_form(
    line, height, suspended, tension, reach, top_tension, slope,
):
    """Independent hand calculations from the section-19.3 identities above.

    Case A: z=100 m, s=100*sqrt(3) m; H=9.81*(30000-10000)/200
    =981 kN, a=100 m, cosh(x/a)=2, x=100*ln(2+sqrt(3)) m,
    T=981+9.81*100=1962 kN, tan(theta)=sqrt(3).
    Case B: z=50 m, s=150 m; H=9.81*(22500-2500)/100=1962 kN,
    a=200 m, cosh(x/a)=1.25, x=200*ln(2) m,
    T=1962+9.81*50=2452.5 kN, tan(theta)=150/200=0.75.
    Both leave grounded length 500-s m; all inputs are synthetic.
    """
    result = CatenaryAnalyzer(water_depth=height).solve_catenary(line, tension)

    assert result.arc_length == pytest.approx(suspended, rel=REL_TOL)
    assert result.horizontal_distance == pytest.approx(reach, rel=REL_TOL)
    assert result.fairlead_tension == pytest.approx(top_tension, rel=REL_TOL)
    assert result.fairlead_angle == pytest.approx(degrees(atan(slope)), rel=REL_TOL)
    assert result.grounded_length == pytest.approx(line.length - suspended, rel=REL_TOL)
    # Recover H from the returned length and prescribed vertical span.
    recovered = 9.81 * (result.arc_length**2 - height**2) / (2.0 * height)
    assert recovered == pytest.approx(tension, rel=REL_TOL)


def test_explicit_height_overrides_water_depth(line):
    """Case B above with a different depth; the explicit 50 m span governs."""
    result = CatenaryAnalyzer(water_depth=100.0).solve_catenary(
        line, horizontal_tension=1962.0, vertical_height=50.0,
    )
    assert result.arc_length == pytest.approx(150.0, rel=REL_TOL)
    assert result.vertical_height == 50.0


def test_inverse_tension_matches_hand_calculation(line):
    """Case A above: T=1962 and wz=981 kN require H=981 kN.

    The solver's existing default tolerance is used; acceptance remains the
    issue's 0.1%, applied to independently calculated H, T, s and x.
    """
    result = CatenaryAnalyzer(water_depth=100.0).solve_for_horizontal_tension(
        line, target_fairlead_tension=1962.0,
    )
    assert result.horizontal_tension == pytest.approx(981.0, rel=REL_TOL)
    assert result.fairlead_tension == pytest.approx(1962.0, rel=REL_TOL)
    assert result.arc_length == pytest.approx(100.0 * sqrt(3.0), rel=REL_TOL)
    assert result.horizontal_distance == pytest.approx(
        100.0 * log(2.0 + sqrt(3.0)), rel=REL_TOL,
    )


def test_buoyant_line_has_no_touchdown_solution(line):
    """Section 19.3 assumes downward weight; reverse the synthetic weight.

    A buoyant line cannot rise from a horizontal seabed tangent under its own
    weight. It requires a different boundary-value model, not a NaN result.
    """
    line.weight_water = -line.weight_water
    with pytest.raises(ValueError, match="positive submerged weight"):
        CatenaryAnalyzer(water_depth=100.0).solve_catenary(line, 981.0)
