# ABOUTME: Reject nonfinite pipe geometry inputs before range validation
# ABOUTME: Check finite effective thicknesses against synthetic hand arithmetic

"""Geometry validation and independent arithmetic controls.

Source of all finite inputs: synthetic geometry chosen for hand calculation,
with diameter 0.4 m, wall 0.02 m, allowance 0 or 0.004 m, and caller-supplied
fabrication reduction 0 or 0.2 (dimensionless). These are test inputs, not
standard-derived factors. The negative diameter is the sign-reversed fixture.
The zero wall is an invalid range control.

Hand calculations: subtracting the allowance leaves 0.02 or 0.016 m;
retaining 80 percent gives 0.016 or 0.0128 m. With zero reduction, retain the
full corroded thickness. Subtracting two walls from the diameter gives
0.4 - 2 * 0.02 = 0.36 m; dividing diameter by wall gives 0.4 / 0.02 = 20.
No production helper supplies an expected value.
"""

import math

import pytest

from digitalmodel.structural.analysis.wall_thickness import PipeGeometry


_FIELDS = (
    ("outer_diameter", "Outer diameter"),
    ("wall_thickness", "Wall thickness"),
    ("corrosion_allowance", "Corrosion allowance"),
    ("fabrication_tolerance", "Fabrication tolerance"),
)


@pytest.mark.parametrize("field, label", _FIELDS)
@pytest.mark.parametrize(
    "value", [math.nan, math.inf, -math.inf], ids=["nan", "posinf", "neginf"]
)
def test_pipe_geometry_rejects_nonfinite_fields(field, label, value):
    """Every constructor field requires a finite value and identifies itself."""
    inputs = dict(
        outer_diameter=0.4,
        wall_thickness=0.02,
        corrosion_allowance=0.004,
        fabrication_tolerance=0.2,
    )
    inputs[field] = value

    with pytest.raises(ValueError, match=f"^{label} must be finite$"):
        PipeGeometry(**inputs)


@pytest.mark.parametrize("field, label", _FIELDS)
def test_finite_validation_precedes_range_validation(field, label):
    """An unrelated finite range error must not mask a nonfinite field."""
    inputs = dict(
        outer_diameter=-0.4,
        wall_thickness=0.0,
        corrosion_allowance=0.004,
        fabrication_tolerance=0.2,
    )
    inputs[field] = math.nan

    with pytest.raises(ValueError, match=f"^{label} must be finite$"):
        PipeGeometry(**inputs)


@pytest.mark.parametrize(
    "allowance, tolerance, expected_t1, expected_t2",
    [
        (0.004, 0.2, 0.0128, 0.016),
        (0.0, 0.2, 0.016, 0.02),
        (0.004, 0.0, 0.016, 0.016),
        (0.0, 0.0, 0.02, 0.02),
    ],
    ids=["both-deductions", "zero-allowance", "zero-tolerance", "both-zero"],
)
def test_effective_thicknesses_match_hand_arithmetic(
    allowance, tolerance, expected_t1, expected_t2
):
    """Compare all geometry properties with the hand calculations above."""
    geometry = PipeGeometry(
        outer_diameter=0.4,
        wall_thickness=0.02,
        corrosion_allowance=allowance,
        fabrication_tolerance=tolerance,
    )

    assert geometry.t1 == pytest.approx(expected_t1)
    assert geometry.t2 == pytest.approx(expected_t2)
    assert geometry.inner_diameter == pytest.approx(0.36)
    assert geometry.d_over_t == pytest.approx(20.0)
