"""Independent collapse-root regressions for the cubic reported in issue #1920.

Synthetic inputs use the issue's diameter, elastic constants, yield strength,
ovality and reference wall. A thinner wall exercises the reverse ordering of
characteristic pressures. These are numerical examples, not design factors.

Writing a = p_el, b = p_p and c = a*b*f_0*D/t, the expanded polynomial is
F(p) = p**3 - a*p**2 - (b**2 + c)*p + a*b**2. Its derivative is
F'(p) = 3*p**2 - 2*a*p - (b**2 + c). The test reference uses Decimal derivative
iteration on this expansion, independently of the production bisection.
For c > 0, F(0) > 0 and F(min(a, b)) < 0. The two roots of the quadratic
F' have opposite signs, so F decreases to a single positive minimum then rises:
there is exactly one root below min(a, b). At c = 0, the factorisation gives
the exact limiting root min(a, b).
"""

from decimal import Decimal, localcontext

import pytest

from digitalmodel.structural.analysis.wall_thickness import (
    DesignLoads,
    PipeGeometry,
    PipeMaterial,
)
from digitalmodel.structural.analysis.wall_thickness_codes.iso_13623 import (
    Iso13623Strategy,
)
from digitalmodel.structural.analysis.wall_thickness_codes.pd_8010_2 import (
    Pd80102Strategy,
)

# Issue #1920 synthetic reference inputs, in SI units. The thinner wall is a
# test choice for p_el < p_p; the reference wall gives p_el > p_p.
DIAMETER = Decimal("0.3239")
YOUNGS_MODULUS = Decimal("207e9")
POISSONS_RATIO = Decimal("0.3")
YIELD_STRENGTH = Decimal("448e6")
OVALITY = Decimal("0.005")
WALLS = [pytest.param("0.0097", id="elastic-limit"),
         pytest.param("0.018", id="plastic-limit")]

# Numerical controls: a millipascal bracket tolerance gives ample margin for a
# dimensionless residual bound of one part per billion at these pressures.
PRESSURE_TOLERANCE = 0.001
RESIDUAL_TOLERANCE = 1e-9


@pytest.fixture(params=[Iso13623Strategy, Pd80102Strategy], ids=["ISO", "PD"])
def strategy(request):
    return request.param()


def reference_root(wall):
    """Calculate the expected root at 60-digit precision without solver helpers."""
    with localcontext() as context:
        context.prec = 60  # Retain margin over the float solver's precision.
        t = Decimal(wall)
        a = 2 * YOUNGS_MODULUS * (t / DIAMETER) ** 3 / (1 - POISSONS_RATIO**2)
        b = 2 * YIELD_STRENGTH * t / DIAMETER
        c = a * b * OVALITY * DIAMETER / t
        p = min(a, b)  # Start at the zero-imperfection physical limit.
        for _ in range(100):  # Numerical budget, not an engineering parameter.
            residual = p**3 - a * p**2 - (b**2 + c) * p + a * b**2
            derivative = 3 * p**2 - 2 * a * p - (b**2 + c)
            updated = p - residual / derivative
            if abs(updated - p) < Decimal("1e-40"):
                p = updated
                break
            p = updated
        else:
            raise AssertionError("Independent derivative calculation did not converge")
        assert 0 < p < min(a, b)
        assert abs(p**3 - a * p**2 - (b**2 + c) * p + a * b**2) / (a * b**2) < Decimal("1e-45")
        return float(a), float(b), float(p)


def assert_root_and_residual(p, p_el, p_p, wall, expected):
    """Check the physical bound, independent pressure and factored polynomial."""
    assert 0 < p < min(p_el, p_p)
    assert p == pytest.approx(expected, rel=0, abs=PRESSURE_TOLERANCE)
    coefficient = p_el * p_p * float(OVALITY) * float(DIAMETER) / float(wall)
    residual = (p - p_el) * (p**2 - p_p**2) - p * coefficient
    assert abs(residual) / (p_el * p_p**2) < RESIDUAL_TOLERANCE


@pytest.mark.parametrize("wall", WALLS)
def test_smallest_positive_root(strategy, wall):
    p_el, p_p, expected = reference_root(wall)
    assert (p_el < p_p) == (wall == "0.0097")
    p = strategy._solve_collapse_pressure(
        p_el, p_p, float(OVALITY), float(DIAMETER), float(wall),
        tol=PRESSURE_TOLERANCE,
    )
    assert_root_and_residual(p, p_el, p_p, wall, expected)


@pytest.mark.parametrize("wall", WALLS)
def test_zero_imperfection_reaches_characteristic_limit(strategy, wall):
    p_el, p_p, _ = reference_root(wall)
    p = strategy._solve_collapse_pressure(
        p_el, p_p, 0.0, float(DIAMETER), float(wall),
    )
    assert p == min(p_el, p_p)
    assert (p - p_el) * (p**2 - p_p**2) == 0


@pytest.mark.parametrize("wall", WALLS)
def test_collapse_check_uses_physical_root(strategy, wall):
    _, _, expected = reference_root(wall)
    geometry = PipeGeometry(outer_diameter=float(DIAMETER), wall_thickness=float(wall))
    material = PipeMaterial(
        grade="synthetic", smys=float(YIELD_STRENGTH),
        smts=float(YIELD_STRENGTH), youngs_modulus=float(YOUNGS_MODULUS),
        poissons_ratio=float(POISSONS_RATIO),
    )
    _, details = strategy._check_collapse(geometry, material, DesignLoads())
    # The public check uses the solver's default one-pascal tolerance.
    assert 0 < details["p_c"] < min(details["p_el"], details["p_p"])
    assert details["p_c"] == pytest.approx(expected, rel=0, abs=1.0)


def test_iteration_exhaustion_is_explicit(strategy):
    p_el, p_p, _ = reference_root("0.018")
    with pytest.raises(RuntimeError, match="converge"):
        strategy._solve_collapse_pressure(
            p_el, p_p, float(OVALITY), float(DIAMETER), 0.018, max_iter=1,
        )


def test_last_allowed_bisection_can_converge(strategy):
    p_el, p_p, expected = reference_root("0.018")
    # Halving the bracket once gives this width; this verifies that a final
    # successful iteration is accepted rather than reported as exhaustion.
    tolerance = min(p_el, p_p) / 2
    p = strategy._solve_collapse_pressure(
        p_el, p_p, float(OVALITY), float(DIAMETER), 0.018,
        tol=tolerance, max_iter=1,
    )
    assert 0 < p < min(p_el, p_p)
    assert abs(p - expected) <= tolerance / 2


@pytest.mark.parametrize("tol", [0.0, -1.0, float("inf"), float("nan")])
def test_invalid_tolerance_is_rejected(strategy, tol):
    p_el, p_p, _ = reference_root("0.018")
    with pytest.raises(ValueError, match="tol"):
        strategy._solve_collapse_pressure(
            p_el, p_p, float(OVALITY), float(DIAMETER), 0.018, tol=tol,
        )
