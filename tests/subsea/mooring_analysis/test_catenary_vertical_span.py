# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Independent catenary hand calculations; all line data are synthetic.

Basis: Classical Mechanics, section 18.3 (LibreTexts, 2017), equations
18.3.4-18.3.6 and 18.3.9; https://phys.libretexts.org/@go/page/7046.
Set y=a+z: s²=z²+2az, H=w*a, T=w*(a+z), tan(theta)=s/a.
The mass conversion retains g=9.81 m/s², the existing API convention:
NIST 2022 CODATA standard gravity, rounded to two decimal places.
https://physics.nist.gov/cgi-bin/cuu/Value?gn
"""

import json
import math

import pytest
from click.testing import CliRunner

from digitalmodel.subsea.mooring_analysis import (
    CatenaryAnalyzer,
    LineType,
    MooringLineProperties,
)

# Issue #2561 acceptance criterion: agreement within 0.1 percent.
RELATIVE_LIMIT = 0.001


def line_with_weight(weight_kn_m):
    """Synthetic 100 m line; unused diameter, MBL and EA are zero placeholders."""
    return MooringLineProperties(
        line_type=LineType.CHAIN,
        length=100.0,
        diameter=0.0,
        mbl=0.0,
        weight_water=weight_kn_m * 1000.0 / 9.81,
        ea=0.0,
    )


@pytest.mark.parametrize(
    "weight,height,suspended,horizontal,tension,reach,angle",
    [
        (1.0, 10.0, 30.0, 40.0, 50.0, 40.0 * math.log(2),
         math.degrees(math.atan(3 / 4))),
        (2.0, 20.0, 40.0, 60.0, 100.0, 30.0 * math.log(3),
         math.degrees(math.atan(4 / 3))),
    ],
)
def test_vertical_span_hand_calculations(
    weight, height, suspended, horizontal, tension, reach, angle
):
    """Recover H from vertical span z and suspended length s independently.

    Case A: w=1, z=10, s=30 -> a=(900-100)/20=40, H=40, T=50.
    cosh(x/40)=5/4 -> exp(x/40)=2 -> x=40*ln(2); tan(theta)=3/4.
    Case B: w=2, z=20, s=40 -> a=(1600-400)/40=30, H=60, T=100.
    cosh(x/30)=5/3 -> exp(x/30)=3 -> x=30*ln(3); tan(theta)=4/3.
    Lengths are m, weights kN/m, forces kN. Equations cited above.
    """
    line = line_with_weight(weight)
    prescribed_h = weight * (suspended**2 - height**2) / (2 * height)
    # Different default depth proves the explicitly supplied vertical span is used.
    result = CatenaryAnalyzer(water_depth=line.length).solve_catenary(
        line, prescribed_h, vertical_height=height
    )

    for actual, expected in (
        (result.horizontal_tension, horizontal),
        (result.touchdown_tension, horizontal),
        (result.fairlead_tension, tension),
        (result.arc_length, suspended),
        (result.horizontal_distance, reach),
        (result.fairlead_angle, angle),
        (result.grounded_length, line.length - suspended),
        (result.vertical_height, height),
    ):
        assert actual == pytest.approx(expected, rel=RELATIVE_LIMIT, abs=0)


@pytest.mark.parametrize("horizontal,height", [(40.0, 10.0), (0.01, 20.0)])
def test_inverse_tension_at_fixed_height(horizontal, height):
    """w=1: T=H+z, so (H,z,T)=(40,10,50) or (0.01,20,20.01).

    Classical Mechanics (2017), equation 18.3.9; dT/dH=1 at fixed z.
    The second synthetic case has a target near the hanging-weight limit.
    """
    result = CatenaryAnalyzer(height).solve_for_horizontal_tension(
        line_with_weight(1.0), horizontal + height
    )
    assert result.horizontal_tension == pytest.approx(
        horizontal, rel=RELATIVE_LIMIT, abs=0
    )
    assert result.fairlead_tension == pytest.approx(
        horizontal + height, rel=RELATIVE_LIMIT, abs=0
    )


@pytest.mark.parametrize(
    "option,value", [("--horizontal-tension", "40"), ("--fairlead-tension", "50")]
)
def test_cli_reports_hand_calculated_geometry(option, value, tmp_path):
    """Both CLI routes reproduce hand-calculation A, including JSON units.

    Synthetic w=1 kN/m, z=10 m, H=40 kN -> s=30 m and T=50 kN;
    x=40*ln(2) m, as derived in test_vertical_span_hand_calculations.
    This exercises geometry output only; stiffness is not independently checked.
    """
    from digitalmodel.subsea.mooring_analysis.cli import cli

    output = tmp_path / "catenary.json"
    result = CliRunner().invoke(cli, [
        "catenary", "--water-depth", "10", "--line-length", "100",
        "--line-weight", str(1000.0 / 9.81), option, value,
        "--output", str(output),
    ])
    assert result.exit_code == 0, result.output
    geometry = json.loads(output.read_text())["catenary"]
    for key, expected in (
        ("horizontal_tension", 40.0),
        ("fairlead_tension", 50.0),
        ("arc_length", 30.0),
        ("horizontal_distance", 40 * math.log(2)),
        ("grounded_length", 70.0),
    ):
        assert geometry[key] == pytest.approx(expected, rel=RELATIVE_LIMIT, abs=0)


@pytest.mark.parametrize("height,horizontal", [(1000.0, 1.0), (1e-10, 1e8)])
def test_deep_and_shallow_spans(height, horizontal):
    """Synthetic limits of the same equations, without exponential evaluation.

    w=1, a=H. Deep: s=sqrt(1000*1002), T=1001, x=asinh(s).
    Shallow: s=sqrt(1e-20+0.02), x=a*ln(s/a+sqrt(1+(s/a)²)).
    Evaluate the logarithm as log1p to retain the shallow-span increment.
    These are rearrangements of equations 18.3.4-18.3.6 and 18.3.9.
    """
    expected_s = math.sqrt(height**2 + 2 * horizontal * height)
    ratio = expected_s / horizontal
    expected_x = horizontal * math.log1p(
        ratio + ratio**2 / (math.sqrt(1 + ratio**2) + 1)
    )
    result = CatenaryAnalyzer(height).solve_catenary(
        line_with_weight(1.0), horizontal
    )
    assert result.arc_length == pytest.approx(expected_s, rel=RELATIVE_LIMIT, abs=0)
    assert result.horizontal_distance == pytest.approx(
        expected_x, rel=RELATIVE_LIMIT, abs=0
    )
    assert result.fairlead_tension == pytest.approx(
        horizontal + height, rel=RELATIVE_LIMIT, abs=0
    )


@pytest.mark.parametrize("horizontal", [0.0, -1.0, math.nan, math.inf])
def test_reject_invalid_horizontal_tension(horizontal):
    """H=w*a must be finite and positive for the cited touchdown catenary."""
    with pytest.raises(ValueError, match="Horizontal tension"):
        CatenaryAnalyzer(10.0).solve_catenary(line_with_weight(1.0), horizontal)


@pytest.mark.parametrize("weight", [0.0, -1.0, math.nan, math.inf])
def test_reject_nonpositive_or_nonfinite_weight(weight):
    """A lower touchdown with H>0 requires finite w>0; no buoyant solution."""
    with pytest.raises(ValueError, match="weight"):
        CatenaryAnalyzer(10.0).solve_catenary(line_with_weight(weight), 40.0)


@pytest.mark.parametrize("height", [-1.0, math.nan, math.inf])
def test_reject_invalid_height(height):
    """z=y-a is nonnegative above the lowest point (equation 18.3.4)."""
    with pytest.raises(ValueError, match="Vertical height"):
        CatenaryAnalyzer(height).solve_catenary(line_with_weight(1.0), 40.0)


@pytest.mark.parametrize("target", [0.0, 9.0, 10.0, math.nan, math.inf])
def test_reject_unattainable_target(target):
    """For synthetic w=1 and z=10, T must exceed 10 kN to give H>0."""
    with pytest.raises(ValueError, match="Target fairlead tension"):
        CatenaryAnalyzer(10.0).solve_for_horizontal_tension(
            line_with_weight(1.0), target
        )


def test_zero_height_limit():
    """z=0 implies s=x=theta=0 and T=H (equations cited above)."""
    line = line_with_weight(1.0)
    result = CatenaryAnalyzer(0.0).solve_catenary(line, 40.0)
    assert result.arc_length == result.horizontal_distance == result.fairlead_angle == 0
    assert result.fairlead_tension == result.touchdown_tension == 40.0
    assert result.grounded_length == line.length
