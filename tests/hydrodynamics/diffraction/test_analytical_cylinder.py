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

def _fields(s):
    """Rebuild the potential and its derivatives from solved coefficients."""
    from scipy.special import hankel1, ive, kve

    A, Bc = s["interior"], s["exterior"]
    lam, k0, ks = s["lam"], s["k0"], s["k_evanescent"]
    n0s, nm, order = s["n0_scaled"], s["nm"], s["order"]
    a, h, gap = s["radius"], s["water_depth"], s["gap"]

    def z0(z):
        u = z + h
        # cosh(k0 u) / (N0) with both sides scaled by exp(k0 h).
        return 0.5 * (np.exp(k0 * (u - h)) + np.exp(-k0 * (u + h))) / n0s

    def zm(m, z):
        return np.cos(ks[m - 1] * (z + h)) / nm[m - 1]

    def phi_E(r, z):
        out = Bc[0] * hankel1(0, k0 * r) / hankel1(0, k0 * a) * z0(z)
        for m in range(1, order + 1):
            km = ks[m - 1]
            ratio = (kve(0, km * r) / kve(0, km * a)) * np.exp(-km * (r - a))
            out = out + Bc[m] * ratio * zm(m, z)
        return out

    def dphi_E_dr(r, z):
        out = (-Bc[0] * k0 * hankel1(1, k0 * r) / hankel1(0, k0 * a)) * z0(z)
        for m in range(1, order + 1):
            km = ks[m - 1]
            ratio = (kve(1, km * r) / kve(0, km * a)) * np.exp(-km * (r - a))
            out = out - Bc[m] * km * ratio * zm(m, z)
        return out

    def phi_I(r, z):
        u = z + h
        out = (1.0 / (2 * gap)) * (u ** 2 - r ** 2 / 2.0) + A[0]
        for n in range(1, order + 1):
            ln = lam[n]
            ratio = (ive(0, ln * r) / ive(0, ln * a)) * np.exp(ln * (r - a))
            out = out + A[n] * ratio * np.cos(ln * u)
        return out

    def dphi_I_dr(r, z):
        u = z + h
        out = np.full_like(np.asarray(r, dtype=float), -r / (2 * gap))
        for n in range(1, order + 1):
            ln = lam[n]
            ratio = (ive(1, ln * r) / ive(0, ln * a)) * np.exp(ln * (r - a))
            out = out + A[n] * ln * ratio * np.cos(ln * u)
        return out

    return phi_E, dphi_E_dr, phi_I, dphi_I_dr


def _interface_errors(s, zero_coefficients=False):
    """Relative L2 mismatch across the interface, over the gap."""
    import copy

    s = copy.deepcopy(s)
    if zero_coefficients:
        s["interior"] = np.zeros_like(s["interior"])
        s["exterior"] = np.zeros_like(s["exterior"])
    phi_E, dphi_E_dr, phi_I, dphi_I_dr = _fields(s)
    a, h, d = s["radius"], s["water_depth"], s["draft"]
    z = np.linspace(-h, -d, 801)[1:-1]
    dz = z[1] - z[0]

    def l2(v):
        return math.sqrt(float(np.sum(np.abs(v) ** 2) * dz))

    pi_, pe_ = phi_I(a, z), phi_E(a, z)
    return l2(pi_ - pe_) / max(l2(pi_), 1e-30)


def test_interface_potential_continuity_converges():
    """The matching condition itself, on the coefficients the module returns.

    This is the check that distinguishes a correct solution from a wrong one.
    The base, seabed and free-surface conditions below are satisfied by the
    basis functions whatever the coefficients are, so they cannot detect a
    faulty match; interface continuity can, because nothing enforces it
    pointwise.
    """
    import digitalmodel.hydrodynamics.diffraction.analytical_cylinder as ac

    errors = []
    for modes in (20, 40, 80):
        s = ac._solve(1.0, GEOM, STANDARD_GRAVITY, modes)
        errors.append(_interface_errors(s))
    assert errors[0] > errors[1] > errors[2], (
        f"interface mismatch must fall with mode count, got {errors}")
    assert errors[-1] < 2e-2


def test_zeroed_coefficients_fail_the_interface_check():
    """Guards against a test that would pass on a trivial solution."""
    import digitalmodel.hydrodynamics.diffraction.analytical_cylinder as ac

    s = ac._solve(1.0, GEOM, STANDARD_GRAVITY, 40)
    real = _interface_errors(s)
    trivial = _interface_errors(s, zero_coefficients=True)
    assert trivial > 10 * real, (
        f"zeroing the coefficients must break continuity: "
        f"solved {real:.3e}, zeroed {trivial:.3e}")


def test_side_wall_is_impermeable_away_from_the_edge():
    """No radial flow through the cylinder wall, sampled clear of the corner."""
    import digitalmodel.hydrodynamics.diffraction.analytical_cylinder as ac

    s = ac._solve(1.0, GEOM, STANDARD_GRAVITY, 80)
    _, dphi_E_dr, _, _ = _fields(s)
    a, d = s["radius"], s["draft"]
    z = np.linspace(-0.85 * d, -0.05 * d, 40)
    scale = np.max(np.abs(dphi_E_dr(a * 1.0, np.linspace(-s["water_depth"],
                                                         -d, 40))))
    assert np.max(np.abs(dphi_E_dr(a, z))) < 0.35 * scale


def test_shallow_gap_and_deep_water_stay_finite():
    """Arguments that overflow the unscaled Bessel and hyperbolic forms."""
    shallow = heave_added_mass_damping(
        1.0, CylinderGeometry(1.0, 2.0, 2.2), RHO, modes=60)
    assert math.isfinite(shallow.added_mass)
    assert math.isfinite(shallow.damping)
    assert shallow.damping > 0.0
    assert shallow.residual < 1e-8

    deep = heave_added_mass_damping(
        3.0, CylinderGeometry(1.0, 2.0, 500.0), RHO, modes=60)
    assert math.isfinite(deep.added_mass)
    assert math.isfinite(deep.damping)
    assert deep.damping > 0.0


def test_free_surface_and_seabed_conditions_hold():
    """Conditions the basis satisfies by construction, checked for regression.

    These are weak evidence on their own: the vertical eigenfunctions satisfy
    the free-surface and seabed conditions whatever the coefficients are, and
    the particular solution supplies the base velocity, so zeroing every
    coefficient would still pass. They are retained to catch a broken basis,
    not to validate the matching. The matching is tested by
    test_interface_potential_continuity_converges.
    """
    import digitalmodel.hydrodynamics.diffraction.analytical_cylinder as ac

    omega, modes = 1.0, 60
    s = ac._solve(omega, GEOM, STANDARD_GRAVITY, modes)
    phi_E, _, _, _ = _fields(s)
    h, d, a = s["water_depth"], s["draft"], s["radius"]
    k0, ks, n0s, nm, order = (s["k0"], s["k_evanescent"], s["n0_scaled"],
                              s["nm"], s["order"])
    A, Bc = s["interior"], s["exterior"]
    lam, gap = s["lam"], s["gap"]

    from scipy.special import hankel1, ive, kve

    def dphi_E_dz(r, z):
        u = z + h
        dz0 = k0 * 0.5 * (np.exp(k0 * (u - h)) - np.exp(-k0 * (u + h))) / n0s
        out = Bc[0] * hankel1(0, k0 * r) / hankel1(0, k0 * a) * dz0
        for m in range(1, order + 1):
            km = ks[m - 1]
            ratio = (kve(0, km * r) / kve(0, km * a)) * np.exp(-km * (r - a))
            out = out - Bc[m] * ratio * km * np.sin(km * u) / nm[m - 1]
        return out

    def dphi_I_dz(r, z):
        u = z + h
        out = np.full_like(np.asarray(r, dtype=float), u / gap)
        for n in range(1, order + 1):
            ln = lam[n]
            ratio = (ive(0, ln * r) / ive(0, ln * a)) * np.exp(ln * (r - a))
            out = out - A[n] * ratio * ln * math.sin(ln * u)
        return out

    r_out = np.linspace(1.05 * a, 6.0 * a, 40)
    free_surface = np.max(np.abs(-omega ** 2 * phi_E(r_out, 0.0)
                                 + STANDARD_GRAVITY * dphi_E_dz(r_out, 0.0)))
    assert free_surface < 1e-10

    assert np.max(np.abs(dphi_E_dz(r_out, -h))) < 1e-12

    r_in = np.linspace(0.02, 0.98 * a, 40)
    assert np.max(np.abs(dphi_I_dz(r_in, -d) - 1.0)) < 1e-10
    assert np.max(np.abs(dphi_I_dz(r_in, -h))) < 1e-12


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
