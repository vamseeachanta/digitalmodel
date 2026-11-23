# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Independent hydrostatic hand calculations for straight-sided wedge strips.

Let x be transverse distance [m] and z height above the keel [m]. For side
slope m = tan(beta), z = m*abs(x). At keel submergence h [m], the submerged
width at height z is 2*z/m, hence the displaced cross-sectional area is

    A = integral(2*z/m, z=0..h) = h**2/m  [m**2].

Independently, gauge pressure on a side is p(x) = rho*g*(h-m*x) [Pa], with
waterline half-width b = h/m. The vertical projection of a side element per
unit longitudinal length is dx, so the upward force per length is

    F' = 2*integral(p(x), x=0..b)
       = 2*rho*g*(h*b - m*b**2/2) = rho*g*A  [N/m].

Thus F'(k*h)/F'(h) = k**2 while the waterline stays below the chine. For
h <= 0 there is no wetted side and the gauge-pressure integral is zero.
The factors 2 account for both sides or integration of a linear function.

Gravity convention: NIST standard acceleration is exactly 9.80665 m/s**2;
use its rounded value 9.81 m/s**2, rather than a local gravity measurement:
https://physics.nist.gov/cgi-bin/cuu/Value?gn

All geometry, density, depth and scale values below are synthetic test inputs,
not measured properties. The slopes at 30, 45 and 60 degrees follow from the
elementary right triangles: 1/sqrt(3), 1 and sqrt(3), respectively. Expected
forces use the pressure integral above, without production geometry helpers.
"""

import math

import pytest

from digitalmodel.hydrodynamics.planing_hull.geometry import PlaningHullGeometry
from digitalmodel.hydrodynamics.planing_hull.strip_model import PlaningStripModel


GRAVITY = 9.81  # m/s**2; rounded NIST convention cited above.
# Allow 64 binary64 epsilons for degree conversion, trigonometry, the analytic
# slope and the short arithmetic chains. These angles are well conditioned;
# there is no discretisation error or empirical tolerance in this comparison.
REL_TOL = 64 * math.ulp(1.0)
WEDGE_SIDES = [(30.0, 1 / math.sqrt(3)), (45.0, 1.0), (60.0, math.sqrt(3))]


def _synthetic_strip(deadrise_degrees, side_slope, density):
    """Use a 3 m strip layout with 2 m beam and a chine on each straight side."""
    geometry = PlaningHullGeometry(
        length=3.0,  # m; synthetic, with centre at half the length.
        beam=2.0,  # m; synthetic, so each chine is 1 m from the centreline.
        deadrise=deadrise_degrees,
        chine_height=side_slope,  # m; side height at transverse distance 1 m.
        lcg=1.5,  # m; synthetic centre at half the length.
    )
    # Four strips give dx = 0.75 m, exposing an erroneous strip-length factor
    # in a result specified per unit length. Density is supplied in kg/m**3.
    return PlaningStripModel(geometry, n_strips=4, rho=density)


@pytest.mark.parametrize("deadrise_degrees,side_slope", WEDGE_SIDES)
@pytest.mark.parametrize("depth", [0.125, 0.25])  # m; synthetic submerged cases.
@pytest.mark.parametrize("density", [1000.0, 1030.0])  # kg/m**3; synthetic.
def test_wedge_buoyancy_matches_hand_integral(deadrise_degrees, side_slope,
                                           depth, density):
    """Check absolute upward N/m against the integrated hydrostatic pressure."""
    model = _synthetic_strip(deadrise_degrees, side_slope, density)
    half_width = depth / side_slope  # m; straight-side waterline intersection.
    assert depth < model.geometry.chine_height
    assert half_width < model.geometry.beam / 2
    expected = 2 * density * GRAVITY * (
        depth * half_width - side_slope * half_width**2 / 2
    )

    actual = model.buoyancy_force_per_length(depth)

    assert actual == pytest.approx(expected, rel=REL_TOL, abs=0.0)


@pytest.mark.parametrize("depth", [-0.25, -0.125, 0.0])  # m; synthetic dry cases.
def test_dry_wedge_has_zero_buoyancy(depth):
    """An emerged keel or a keel at the waterline has zero displaced area."""
    model = _synthetic_strip(45.0, 1.0, 1000.0)

    assert model.buoyancy_force_per_length(depth) == 0.0


@pytest.mark.parametrize("deadrise_degrees,side_slope", WEDGE_SIDES)
@pytest.mark.parametrize("depth_scale", [2.0, 3.0])  # Synthetic deeper cases.
def test_wedge_buoyancy_depth_scaling(deadrise_degrees, side_slope, depth_scale):
    """The independently integrated area gives a quadratic depth relation."""
    model = _synthetic_strip(deadrise_degrees, side_slope, 1000.0)
    shallow_depth = 0.0625  # m; synthetic, below the chine even after scaling.
    deeper_depth = depth_scale * shallow_depth
    assert deeper_depth < model.geometry.chine_height

    shallow_force = model.buoyancy_force_per_length(shallow_depth)
    deeper_force = model.buoyancy_force_per_length(deeper_depth)

    assert shallow_force > 0.0
    assert deeper_force == pytest.approx(
        shallow_force * depth_scale**2, rel=REL_TOL, abs=0.0
    )
