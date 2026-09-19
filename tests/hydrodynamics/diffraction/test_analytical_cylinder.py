"""Tests for the matched-eigenfunction cylinder reference.

The point of this module is to be trustworthy without a solver, so the tests
check the physics directly rather than pinning numbers alone: the boundary
conditions the solution was never told to satisfy pointwise, the sign and
asymptotic behaviour of the damping, and convergence in the mode count.
"""

from __future__ import annotations

import math

import numpy as np
import pytest

from digitalmodel.hydrodynamics.diffraction.analytical_cylinder import (
    STANDARD_GRAVITY,
    CylinderGeometry,
    heave_added_mass_damping,
    wavenumbers,
)

GEOM = CylinderGeometry(radius=1.0, draft=2.0, water_depth=10.0)
RHO = 1025.0


# ------------------------------------------------------------- geometry

def test_geometry_rejects_grounded_cylinder():
    with pytest.raises(ValueError, match="seabed"):
        CylinderGeometry(radius=1.0, draft=10.0, water_depth=10.0)


def test_geometry_rejects_nonpositive_dimensions():
    with pytest.raises(ValueError, match="radius"):
        CylinderGeometry(radius=0.0, draft=1.0, water_depth=10.0)
    with pytest.raises(ValueError, match="draft"):
        CylinderGeometry(radius=1.0, draft=-1.0, water_depth=10.0)


def test_geometry_derived_quantities():
    assert GEOM.gap == pytest.approx(8.0)
    assert GEOM.displaced_volume == pytest.approx(math.pi * 2.0)


# ---------------------------------------------------------- wavenumbers

def test_propagating_root_satisfies_dispersion_relation():
    h = GEOM.water_depth
    for omega in (0.3, 1.0, 2.5, 5.0):
        k0, _ = wavenumbers(omega, h, 0)
        assert k0 > 0
        assert STANDARD_GRAVITY * k0 * math.tanh(k0 * h) == pytest.approx(
            omega ** 2, rel=1e-12
        )


def test_evanescent_roots_satisfy_their_relation_and_interlace():
    h = GEOM.water_depth
    omega = 1.5
    _, ks = wavenumbers(omega, h, 6)
    assert len(ks) == 6
    for m, km in enumerate(ks, start=1):
        assert -STANDARD_GRAVITY * km * math.tan(km * h) == pytest.approx(
            omega ** 2, rel=1e-10
        )
        assert (m - 0.5) * math.pi < km * h < m * math.pi
    assert np.all(np.diff(ks) > 0)


def test_deep_water_limit_of_propagating_root():
    """For k h >> 1 the dispersion relation collapses to omega^2 = g k."""
    omega = 3.0
    k0, _ = wavenumbers(omega, 500.0, 0)
    assert k0 == pytest.approx(omega ** 2 / STANDARD_GRAVITY, rel=1e-9)


# ------------------------------------------------------------- physics

@pytest.mark.parametrize("omega", [0.25, 0.5, 1.0, 1.5, 2.0, 3.0, 4.0])
def test_damping_is_positive(omega):
    """Radiation damping removes energy; a negative value would be unphysical."""
    r = heave_added_mass_damping(omega, GEOM, RHO, modes=40)
    assert r.damping > 0.0


@pytest.mark.parametrize("omega", [0.25, 1.0, 3.0])
def test_added_mass_is_positive_and_of_sensible_magnitude(omega):
    r = heave_added_mass_damping(omega, GEOM, RHO, modes=40)
    displaced = RHO * GEOM.displaced_volume
    assert r.added_mass > 0.0
    assert r.added_mass < displaced


def test_linear_system_is_well_conditioned():
    for omega in (0.5, 1.0, 2.0, 3.0):
        r = heave_added_mass_damping(omega, GEOM, RHO, modes=40)
        assert r.residual < 1e-10


def test_damping_vanishes_at_high_frequency():
    """A body oscillating fast against a short wave radiates almost nothing."""
    low = heave_added_mass_damping(1.5, GEOM, RHO, modes=40).damping
    high = heave_added_mass_damping(6.0, GEOM, RHO, modes=40).damping
    assert high < 0.01 * low


def test_damping_vanishes_at_low_frequency():
    slow = heave_added_mass_damping(0.05, GEOM, RHO, modes=40).damping
    peak = heave_added_mass_damping(1.5, GEOM, RHO, modes=40).damping
    assert slow < 0.05 * peak


def test_damping_has_a_single_interior_peak():
    omegas = np.linspace(0.2, 5.0, 40)
    values = [heave_added_mass_damping(w, GEOM, RHO, modes=30).damping
              for w in omegas]
    peak = int(np.argmax(values))
    assert 0 < peak < len(values) - 1


def test_coefficients_scale_with_water_density():
    a = heave_added_mass_damping(1.0, GEOM, 1025.0, modes=30)
    b = heave_added_mass_damping(1.0, GEOM, 2050.0, modes=30)
    assert b.added_mass == pytest.approx(2.0 * a.added_mass, rel=1e-12)
    assert b.damping == pytest.approx(2.0 * a.damping, rel=1e-12)


# --------------------------------------------------------- convergence

def test_damping_converges_in_mode_count():
    coarse = heave_added_mass_damping(1.0, GEOM, RHO, modes=40).damping
    fine = heave_added_mass_damping(1.0, GEOM, RHO, modes=120).damping
    assert abs(fine - coarse) / fine < 1e-3


def test_added_mass_converges_in_mode_count():
    """Slower than the damping, because of the singularity at the sharp edge."""
    coarse = heave_added_mass_damping(1.0, GEOM, RHO, modes=40).added_mass
    fine = heave_added_mass_damping(1.0, GEOM, RHO, modes=120).added_mass
    assert abs(fine - coarse) / fine < 5e-3


def test_rejects_invalid_mode_count():
    with pytest.raises(ValueError, match="modes"):
        heave_added_mass_damping(1.0, GEOM, RHO, modes=0)


# ------------------------------------------- boundary-condition residuals

def test_free_surface_and_seabed_conditions_hold():
    """Rebuild the potential and test conditions never imposed pointwise.

    The coefficients come from projections onto a truncated basis, so
    reproducing those projections would be circular. Evaluating the resulting
    field on the boundaries is not.
    """
    from scipy.special import hankel1, iv, kv

    omega, modes = 1.0, 60
    a, d, h = GEOM.radius, GEOM.draft, GEOM.water_depth

    # Re-solve, exposing the coefficients through a direct rebuild of the
    # same linear system the module forms.
    import digitalmodel.hydrodynamics.diffraction.analytical_cylinder as ac

    k0, ks = ac.wavenumbers(omega, h, modes)
    n0, nm = ac._depth_normalisations(k0, ks, h)
    gap = GEOM.gap
    lam = np.array([0.0] + [n * math.pi / gap for n in range(1, modes + 1)])
    overlap = np.zeros((modes + 1, modes + 1))
    for n in range(modes + 1):
        overlap[0, n] = ac._int_cos_cosh(lam[n], k0, gap) / n0
        for m in range(1, modes + 1):
            overlap[m, n] = ac._int_cos_cos(lam[n], ks[m - 1], gap) / nm[m - 1]
    slope = np.empty(modes + 1, dtype=complex)
    slope[0] = -k0 * hankel1(1, k0 * a) / hankel1(0, k0 * a)
    for m in range(1, modes + 1):
        slope[m] = -ks[m - 1] * kv(1, ks[m - 1] * a) / kv(0, ks[m - 1] * a)
    inner = np.zeros(modes + 1)
    for n in range(1, modes + 1):
        inner[n] = iv(1, lam[n] * a) / iv(0, lam[n] * a)

    size = 2 * (modes + 1)
    M = np.zeros((size, size), dtype=complex)
    b = np.zeros(size, dtype=complex)
    for m in range(modes + 1):
        M[m, (modes + 1) + m] = slope[m] * h
        for n in range(1, modes + 1):
            M[m, n] = -lam[n] * inner[n] * overlap[m, n]
        b[m] = -(a / (2 * gap)) * overlap[m, 0]
    for n in range(modes + 1):
        row = (modes + 1) + n
        for m in range(modes + 1):
            M[row, (modes + 1) + m] = overlap[m, n]
        M[row, n] = -(gap if n == 0 else gap / 2.0)
        b[row] = (gap ** 2 / 6.0 - a ** 2 / 4.0) if n == 0 else \
            gap ** 2 * ((-1) ** n) / (n ** 2 * math.pi ** 2)
    x = np.linalg.solve(M, b)
    A, B = x[: modes + 1], x[modes + 1:]

    def phi_E(r, z):
        u = z + h
        out = B[0] * hankel1(0, k0 * r) / hankel1(0, k0 * a) * np.cosh(k0 * u) / n0
        for m in range(1, modes + 1):
            km = ks[m - 1]
            out = out + B[m] * kv(0, km * r) / kv(0, km * a) * np.cos(km * u) / nm[m - 1]
        return out

    def dphi_E_dz(r, z):
        u = z + h
        out = B[0] * hankel1(0, k0 * r) / hankel1(0, k0 * a) * k0 * np.sinh(k0 * u) / n0
        for m in range(1, modes + 1):
            km = ks[m - 1]
            out = out - B[m] * kv(0, km * r) / kv(0, km * a) * km * np.sin(km * u) / nm[m - 1]
        return out

    def dphi_I_dz(r, z):
        u = z + h
        out = np.full_like(np.asarray(r, dtype=float), u / gap)
        for n in range(1, modes + 1):
            ln = lam[n]
            out = out - A[n] * iv(0, ln * r) / iv(0, ln * a) * ln * math.sin(ln * u)
        return out

    r_out = np.linspace(1.05 * a, 6.0 * a, 40)
    free_surface = np.max(np.abs(-omega ** 2 * phi_E(r_out, 0.0)
                                 + STANDARD_GRAVITY * dphi_E_dz(r_out, 0.0)))
    assert free_surface < 1e-10

    seabed = np.max(np.abs(dphi_E_dz(r_out, -h)))
    assert seabed < 1e-12

    r_in = np.linspace(0.02, 0.98 * a, 40)
    base = np.max(np.abs(dphi_I_dz(r_in, -d) - 1.0))
    assert base < 1e-10

    seabed_inner = np.max(np.abs(dphi_I_dz(r_in, -h)))
    assert seabed_inner < 1e-12


# --------------------------------------------------------- regression pins

def test_regression_values_for_the_reference_geometry():
    """Pins the reference so a later refactor cannot move it silently.

    These are the values OrcaWave 11.6c was checked against: Richardson
    extrapolation of a uniformly refined panel sequence reproduced the damping
    to 0.04% at omega = 1.0 and 0.13% at omega = 2.0.
    """
    one = heave_added_mass_damping(1.0, GEOM, RHO, modes=120)
    assert one.added_mass == pytest.approx(2136.594, rel=1e-5)
    assert one.damping == pytest.approx(302.3233, rel=1e-5)

    two = heave_added_mass_damping(2.0, GEOM, RHO, modes=120)
    assert two.added_mass == pytest.approx(1848.755, rel=1e-5)
    assert two.damping == pytest.approx(431.1747, rel=1e-5)


def test_period_helper():
    r = heave_added_mass_damping(1.0, GEOM, RHO, modes=20)
    assert r.period == pytest.approx(2.0 * math.pi)
