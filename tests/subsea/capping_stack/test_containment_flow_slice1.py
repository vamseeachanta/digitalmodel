"""Independent hand calculations for containment-flow slice 1.

Sources opened for this slice:
* NIST DLMF (2010), current v1.2.8, section 3.8(iii), bisection:
  https://dlmf.nist.gov/3.8.iii
* Multiphase Flow Production Model, TR94-12 (1994), section 2.4.1,
  equation 2-7 (linear PI):
  https://www.bsee.gov/sites/bsee.gov/files/tap-technical-assessment-program/300ae.pdf
* DOE-HDBK-1012/3-92 (1992), HT-03 equations 3-1, 3-12 and 3-14:
  https://www.energy.gov/documents/doe-hdbk-1012-92vol3

All case values below are synthetic mathematical inputs, not equipment data.
Each expected result is derived in its test docstring without calling another
production function. Arithmetic comparison policy: allow 64 binary64 epsilons
for nested roots and reconstructed pressures, with no engineering tolerance.
This declared test budget is not a value prescribed by a standard.
"""

import math

import pytest

from digitalmodel.subsea.capping_stack.containment_flow import (
    _bisect,
    linear_inflow,
    solve_operating_point,
)


# Budget source: this module's arithmetic comparison policy above (slice 1).
# Epsilon source: Python 3.11 Library Reference, math.ulp and sys.float_info:
# https://docs.python.org/3.11/library/math.html#math.ulp
# https://docs.python.org/3.11/library/sys.html#sys.float_info
ROUNDING_BUDGET = 64 * math.ulp(1.0)


def friction_case():
    """Supply the independent q=1 benchmark, including a synthetic linear choke.

    In SI units choose D=2/sqrt(pi), so A=pi*D²/4=1. With L=100,
    rho=1000, f=D/50, Darcy resistance is
    R=f*(L/D)*rho/(2*A²)=(1/50)*100*1000/2=1000.
    Choose g=h=1: hydrostatic head H=1000*1*1=1000 Pa.
    PI=1/2000 and p_res=6000 give p_bh=6000-2000*q.
    The imposed linear choke q=(p_wh-1000)/1000 gives p_wh=1000+1000*q.
    Lift gives p_bh=p_wh+1000+1000*q².
    Combining: 6000-2000*q=2000+1000*q+1000*q², or q²+3*q-4=0.
    Roots are 1 and -4; forward flow is q=1 m³/s, p_wh=2000 Pa,
    p_bh=4000 Pa. The two residuals are zero.
    """
    # Inputs and algebraic coefficients: friction_case hand calculation above;
    # equations from TR94-12 (1994), 2-7 and DOE (1992), 3-1/3-12/3-14.
    diameter = 2 / math.sqrt(math.pi)
    return dict(
        inflow=lambda p: linear_inflow(p, 6000.0, 1 / 2000),
        reservoir_pressure=6000.0,
        column_density=1000.0,
        vertical_depth=1.0,
        gravity=1.0,
        bore_length=100.0,
        bore_diameter=diameter,
        friction_factor=diameter / 50,
        outlet_pressure=1000.0,
        choke_capacity=lambda p: (p - 1000) / 1000,
    )


def test_frictional_ipr_vlp_intersection_has_independent_expected_values():
    """friction_case derives q=1, p_wh=2000, p_bh=4000 and zero residuals."""
    # Expected values: friction_case hand calculation, not solver outputs.
    result = solve_operating_point(opening=1.0, **friction_case())
    assert result.flow_rate_m3_s == pytest.approx(1.0, rel=ROUNDING_BUDGET, abs=0)
    assert result.wellhead_pressure_pa == pytest.approx(2000.0, rel=ROUNDING_BUDGET, abs=0)
    assert result.bottomhole_pressure_pa == pytest.approx(4000.0, rel=ROUNDING_BUDGET, abs=0)
    assert result.ipr_residual_m3_s == pytest.approx(0.0, abs=ROUNDING_BUDGET, rel=0)
    assert result.choke_residual_m3_s == pytest.approx(0.0, abs=ROUNDING_BUDGET, rel=0)


def test_closed_frictional_well_has_independent_static_expected_values():
    """q=0 eliminates Darcy loss; p_bh=6000 and p_wh=6000-1000=5000 Pa."""
    # Zero closure and pressure values: this hand calculation; DOE (1992), 3-12.
    result = solve_operating_point(opening=0.0, **friction_case())
    assert result.flow_rate_m3_s == 0.0
    assert result.wellhead_pressure_pa == 5000.0
    assert result.bottomhole_pressure_pa == 6000.0


# Synthetic root cases; expected roots are the literal r in f(x)=x-r (DLMF
# 3.8(iii), 2010). The last case is the smallest positive binary64 subnormal,
# constructed from the format rather than an empirical numerical constant.
@pytest.mark.parametrize(
    "low,high,root",
    [
        (0.0, 1.0, 1e-100),
        (1e308, 1.5e308, 1.25e308),
        (-1.5e308, -1e308, -1.25e308),
        (-1e308, 1e308, 0.0),
        (0.0, 1.0, math.ulp(0.0)),
    ],
)
def test_root_scale_does_not_set_an_arbitrary_iteration_limit(low, high, root):
    """Solving x-r=0 must return r exactly, even with huge or subnormal r."""
    assert _bisect(lambda x: x - root, low, high) == root


def test_sign_product_underflow_must_not_accept_an_invalid_bracket():
    """f(x)=1e-200*(1+x)>0 on [0,1]; endpoint product 2e-400 rounds to zero."""
    # Hand-calculated positive endpoints above; DLMF (2010), 3.8(iii) requires
    # opposite signs, independent of whether their product is representable.
    with pytest.raises(ValueError, match="opposite-sign"):
        _bisect(lambda x: 1e-200 * (1 + x), 0.0, 1.0)


# Invalid numerical inputs, not physical limits (DLMF 3.8(iii), 2010, a < b).
@pytest.mark.parametrize("low,high", [(2.0, 1.0), (0.0, math.inf), (math.nan, 1.0)])
def test_invalid_intervals_fail_before_calling_the_callback(low, high):
    """Reversed or nonfinite bounds cannot define a finite ordered bracket."""
    def unexpected(_):
        pytest.fail("invalid bounds reached the callback")

    with pytest.raises(ValueError, match="finite ordered"):
        _bisect(unexpected, low, high)


# Exact endpoint roots: f(x)=x-r on [0,1], DLMF (2010), 3.8(iii).
@pytest.mark.parametrize("root", [0.0, 1.0])
def test_endpoint_roots(root):
    """At x=r the linear residual is exactly zero; no iteration is needed."""
    assert _bisect(lambda x: x - root, 0.0, 1.0) == root


def test_adjacent_floats_return_the_endpoint_with_smaller_residual():
    """Exact root is 1+ulp(1)/3, between adjacent floats; lower is nearer.

    Residuals are -ulp(1)/3 at 1 and 2*ulp(1)/3 at nextafter(1,+inf).
    There is no representable interior point; the lower endpoint minimizes |f|.
    """
    # All constants follow this linear hand calculation and midpoint rule from
    # NIST DLMF (2010), 3.8(iii); no tolerance is used for endpoint selection.
    low = 1.0
    high = math.nextafter(low, math.inf)
    offset = math.ulp(low) / 3
    assert _bisect(lambda x: (x - low) - offset, low, high) == low
    assert _bisect(lambda x: (x - low) - 2 * offset, low, high) == high


def test_nonfinite_interior_residual_is_rejected():
    """Residuals -1,+1 bracket a root, but f(0.5)=NaN is undefined."""
    # Synthetic residual values: this test docstring, DLMF (2010), 3.8(iii).
    with pytest.raises(ValueError, match="non-finite"):
        _bisect(lambda x: math.nan if x == 0.5 else x - 0.5, 0.0, 1.0)


def test_root_endpoint_does_not_hide_nonfinite_other_endpoint():
    """f(0)=0 and f(1)=NaN do not define a finite residual bracket."""
    # Synthetic endpoint values: this test docstring, DLMF (2010), 3.8(iii).
    with pytest.raises(ValueError, match="non-finite"):
        _bisect(lambda x: 0.0 if x == 0.0 else math.nan, 0.0, 1.0)
