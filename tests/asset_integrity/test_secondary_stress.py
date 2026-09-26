"""Tests for secondary (residual) stress in Kr (#2157 P2; owner cards G01, G02, B11).

Comparator classes: closed-form (uniform-stress K, superposition of a polynomial
profile), invariants (monotonic in residual stress) and contracts (named methods,
required bases, no silent defaults).
"""

import math

import pytest

from digitalmodel.asset_integrity.assessment.secondary_stress import (
    PolynomialResidual,
    UniformResidual,
    kr_api_phi,
    kr_bs7910_rho,
    kr_total,
    residual_screening_bounds,
)


# --- residual profiles [closed-form + contract] -------------------------------------
def test_uniform_secondary_k():
    r = UniformResidual(stress_mpa=100.0, y=1.12, basis="screening: yield magnitude")
    assert r.k(4.0) == pytest.approx(1.12 * 100.0 * math.sqrt(math.pi * 0.004))


def test_uniform_requires_basis_and_geometry_factor():
    with pytest.raises(ValueError):
        UniformResidual(stress_mpa=100.0, y=1.12, basis="")
    with pytest.raises(ValueError):
        UniformResidual(stress_mpa=100.0, y=0.0, basis="b")


def test_relaxation_factor_scales_k_and_is_bounded():
    full = UniformResidual(stress_mpa=100.0, y=1.0, basis="b")
    half = UniformResidual(stress_mpa=100.0, y=1.0, basis="b", relaxation=0.5)
    assert half.k(2.0) == pytest.approx(0.5 * full.k(2.0))
    for bad in (0.0, 1.5, -0.1):
        with pytest.raises(ValueError):
            UniformResidual(stress_mpa=100.0, y=1.0, basis="b", relaxation=bad)


def test_polynomial_reduces_to_uniform_with_g0():
    # sigma(x) = s0 only; K = s0 * G0 * sqrt(pi a): equals uniform with y = G0
    p = PolynomialResidual(
        coefficients_mpa=(80.0,), influence=(1.1,), wall_mm=10.0, basis="b"
    )
    u = UniformResidual(stress_mpa=80.0, y=1.1, basis="b")
    assert p.k(3.0) == pytest.approx(u.k(3.0), rel=1e-12)


def test_polynomial_superposition():
    # K = sqrt(pi a) * sum_i s_i (a/t)^i G_i
    p = PolynomialResidual(
        coefficients_mpa=(50.0, -20.0, 10.0),
        influence=(1.1, 0.7, 0.5),
        wall_mm=8.0,
        basis="user-supplied influence coefficients",
    )
    a = 2.0
    r = a / 8.0
    expected = math.sqrt(math.pi * a / 1000.0) * (
        50.0 * 1.1 + (-20.0) * r * 0.7 + 10.0 * r**2 * 0.5
    )
    assert p.k(a) == pytest.approx(expected, rel=1e-12)


def test_polynomial_requires_matching_influence_coefficients():
    with pytest.raises(ValueError):
        PolynomialResidual(
            coefficients_mpa=(1.0, 2.0), influence=(1.0,), wall_mm=5.0, basis="b"
        )
    p = PolynomialResidual(
        coefficients_mpa=(1.0,), influence=(1.0,), wall_mm=5.0, basis="b"
    )
    with pytest.raises(ValueError):
        p.k(5.0)  # a must lie inside the wall


# --- Kr methods [contract] ------------------------------------------------------------
def test_phi_method_requires_basis():
    with pytest.raises(ValueError):
        kr_api_phi(5.0, 3.0, 100.0, phi=1.1, basis="")


def test_phi_at_least_one():
    with pytest.raises(ValueError):
        kr_api_phi(5.0, 3.0, 100.0, phi=0.9, basis="b")
    assert kr_api_phi(5.0, 3.0, 100.0, phi=1.2, basis="b") == pytest.approx(
        (5.0 + 1.2 * 3.0) / 100.0
    )


def test_rho_non_negative():
    with pytest.raises(ValueError):
        kr_bs7910_rho(5.0, 3.0, 100.0, rho=-0.01, basis="b")
    assert kr_bs7910_rho(5.0, 3.0, 100.0, rho=0.02, basis="b") == pytest.approx(
        (5.0 + 3.0) / 100.0 + 0.02
    )


def test_methods_not_interchangeable():
    with pytest.raises(ValueError):
        kr_total("guess", 5.0, 3.0, 100.0, value=1.0, basis="b")
    api = kr_total("api579_phi", 5.0, 3.0, 100.0, value=1.2, basis="b")
    bs = kr_total("bs7910_rho", 5.0, 3.0, 100.0, value=1.2, basis="b")
    assert api != bs  # the same number means different things in the two methods


def test_kmat_must_be_positive():
    with pytest.raises(ValueError):
        kr_bs7910_rho(5.0, 3.0, 0.0, rho=0.0, basis="b")


# --- screening bounds [invariant + contract] ------------------------------------------
def test_kr_monotone_in_residual():
    krs = [
        kr_bs7910_rho(
            5.0,
            UniformResidual(stress_mpa=s, y=1.12, basis="b").k(2.0),
            100.0,
            rho=0.0,
            basis="b",
        )
        for s in (0.0, 50.0, 100.0, 200.0)
    ]
    assert all(b >= a for a, b in zip(krs, krs[1:]))


def test_screening_bound_labelled():
    bounds = residual_screening_bounds(
        k_primary=7.5,
        kmat=132.0,
        a_mm=2.35,
        y=1.12,
        sigma_y_mpa=127.0,
        sigma_u_mpa=385.0,
        relaxation=0.444,
        basis="screening bounds per owner card B11",
        rho=0.0,
    )
    assert [b.label for b in bounds] == ["relaxed_yield", "yield", "flow"]
    assert all(b.kind == "screening" for b in bounds)
    assert bounds[0].kr < bounds[1].kr < bounds[2].kr
    assert bounds[1].sigma_r_mpa == 127.0
    assert bounds[2].sigma_r_mpa == pytest.approx((127.0 + 385.0) / 2.0)


def test_screening_bounds_rho_has_no_default():
    with pytest.raises(TypeError):
        residual_screening_bounds(
            k_primary=7.5,
            kmat=132.0,
            a_mm=2.35,
            y=1.12,
            sigma_y_mpa=127.0,
            sigma_u_mpa=385.0,
            relaxation=0.444,
            basis="b",
        )
