# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Independent radial-balance checks for bend water levels.

Physical basis: OpenStax, University Physics Volume 1 (2016), section 6.3,
equation 6.3 (radial force), and section 14.1, equation 14.4 (hydrostatic
pressure). Combining them gives g*d(eta)/dr = U**2/r. Integrating between
the banks gives eta_outer-eta_inner = U**2/g * ln(r_outer/r_inner).
All dimensions and gravity values below are synthetic inputs, not empirical
coefficients or recommended design values.

https://openstax.org/books/university-physics-volume-1/pages/6-3-centripetal-force
https://openstax.org/books/university-physics-volume-1/pages/14-1-fluids-density-and-pressure
"""

import math
from decimal import Decimal, localcontext

import pytest

from digitalmodel.hydrodynamics.river_currents.bank_effects import (
    bend_superelevation,
)


def test_representative_bend_against_hand_integral():
    """U=2, g=10, W=100, R=200: (4/10)*integral(1/r,150,250).

    The independent integral is (2/5)*ln(5/3) metres, from the OpenStax
    radial/hydrostatic balance cited above. Decimal evaluates that expression
    without using the implementation's floating-point bank geometry.
    """
    with localcontext() as ctx:
        # Reference arithmetic uses 60 decimal digits to exceed binary64 precision.
        ctx.prec = 60
        expected = float(Decimal(2) / Decimal(5) * (Decimal(5) / Decimal(3)).ln())
    actual = bend_superelevation(speed=2, width=100, radius=200, gravity=10)
    # Numerical acceptance is one representable step, defined by Python 3 math.ulp:
    # https://docs.python.org/3/library/math.html#math.ulp (not a hydraulic limit).
    assert abs(actual - expected) <= math.ulp(expected)


def test_gentle_bend_retains_nonzero_superelevation():
    """Synthetic W=1, R=10**16, U=g=1 tests cancellation, not river calibration.

    Write x=W/(2R)=5e-17. The hand integral is ln((1+x)/(1-x))
    = 2*(x+x**3/3+...), so its leading term is 1e-16 m and the next
    term is below 1e-48 m. Thus it rounds to float(1e-16).
    This is the Taylor expansion of the cited radial-balance integral.
    """
    expected = 1e-16
    actual = bend_superelevation(speed=1, width=1, radius=1e16, gravity=1)
    # One representable step: Python 3 math.ulp, cited in the preceding test.
    assert abs(actual - expected) <= math.ulp(expected)


def test_large_radii_do_not_overflow_outer_bank_sum():
    """Synthetic R=1.5e308, W=1e308, U=g=1: ln(2e308/1e308)=ln(2).

    The outer radius exceeds binary64 range; the dimensionless integral and
    water-level difference remain finite. Expected ln(2) follows by hand from
    the cited radial balance, without forming either bank in binary64.
    """
    actual = bend_superelevation(speed=1, width=1e308, radius=1.5e308, gravity=1)
    expected = math.log(2)
    # One representable step: Python 3 math.ulp, cited above.
    assert abs(actual - expected) <= math.ulp(expected)


@pytest.mark.parametrize("speed", [0, 2, -2])
def test_speed_sign_and_still_water(speed):
    """U=0 gives zero; U=+/-2 gives (4/10)*ln(3) for banks at 50 and 150 m.

    Speed enters as U squared in OpenStax (2016), equation 6.3: changing its
    sign cannot change the outer-minus-inner water level for the same bend.
    """
    actual = bend_superelevation(speed, width=100, radius=100, gravity=10)
    with localcontext() as ctx:
        # Match the 60-digit independent reference arithmetic above.
        ctx.prec = 60
        expected = float(Decimal(speed * speed) / Decimal(10) * Decimal(3).ln())
    # One representable step: Python 3 math.ulp, cited above; zero is exact.
    assert abs(actual - expected) <= math.ulp(expected)


@pytest.mark.parametrize("field", ["width", "radius", "gravity"])
@pytest.mark.parametrize("value", [0, -1, math.nan, math.inf, -math.inf])
def test_invalid_positive_inputs(field, value):
    """Geometry and gravity must be finite and positive for the cited integral."""
    inputs = dict(speed=2, width=100, radius=200, gravity=10)
    inputs[field] = value
    with pytest.raises(ValueError, match=field):
        bend_superelevation(**inputs)


@pytest.mark.parametrize("speed", [math.nan, math.inf, -math.inf])
def test_nonfinite_speed(speed):
    """The cited radial-force equation needs finite speed."""
    with pytest.raises(ValueError, match="speed"):
        bend_superelevation(speed, width=100, radius=200, gravity=10)


@pytest.mark.parametrize("radius", [49, 50])
def test_inner_bank_must_have_positive_radius(radius):
    """For W=100, R<=50 reaches the singularity of integral(1/r)."""
    with pytest.raises(ValueError, match="radius"):
        bend_superelevation(speed=2, width=100, radius=radius, gravity=10)
