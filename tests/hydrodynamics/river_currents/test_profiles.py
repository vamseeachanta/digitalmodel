"""Tests for river/shallow-water current velocity-depth profile models."""

import numpy as np
import pytest

from digitalmodel.hydrodynamics.river_currents import (
    power_law_profile,
    log_law_profile,
    power_law_depth_averaged,
    log_law_depth_averaged,
    river_velocity_profile,
)


# ---------------------------------------------------------------------------
# Power-law profile
# ---------------------------------------------------------------------------

def test_power_law_zero_at_bed():
    """Velocity is exactly zero at the river bed (z' = 0)."""
    assert power_law_profile(0.0, total_depth=10.0, surface_velocity=2.0) == 0.0


def test_power_law_surface_velocity_recovered_at_surface():
    """Velocity equals the surface velocity at z' = total_depth."""
    u = power_law_profile(10.0, total_depth=10.0, surface_velocity=2.0, exponent_m=6.0)
    assert u == pytest.approx(2.0)


def test_power_law_monotonic_increasing():
    """Profile increases monotonically from bed to surface."""
    z = np.linspace(0.0, 12.0, 25)
    u = power_law_profile(z, total_depth=12.0, surface_velocity=1.5, exponent_m=7.0)
    assert np.all(np.diff(u) >= 0.0)


def test_power_law_known_midpoint():
    """Spot value: half-depth with m=6 -> u_s * 0.5**(1/6)."""
    u = power_law_profile(5.0, total_depth=10.0, surface_velocity=2.0, exponent_m=6.0)
    assert u == pytest.approx(2.0 * 0.5 ** (1.0 / 6.0))


def test_power_law_depth_averaged_matches_analytical_integral():
    """Depth-average equals u_s * m/(m+1) and matches a numerical integral."""
    H, u_s, m = 10.0, 2.0, 6.0
    analytic = power_law_depth_averaged(H, u_s, exponent_m=m)
    assert analytic == pytest.approx(u_s * m / (m + 1.0))
    assert analytic == pytest.approx(1.7142857, rel=1e-6)  # hand-verified

    # Numerical confirmation of the closed form.
    z = np.linspace(0.0, H, 20001)
    u = power_law_profile(z, H, u_s, exponent_m=m)
    numeric = np.trapz(u, z) / H
    assert numeric == pytest.approx(analytic, rel=1e-4)


def test_power_law_clamps_outside_range():
    """Heights above the surface clamp to the surface velocity."""
    u = power_law_profile(15.0, total_depth=10.0, surface_velocity=2.0)
    assert u == pytest.approx(2.0)


def test_power_law_invalid_inputs():
    with pytest.raises(ValueError):
        power_law_profile(1.0, total_depth=0.0, surface_velocity=1.0)
    with pytest.raises(ValueError):
        power_law_profile(1.0, total_depth=10.0, surface_velocity=1.0, exponent_m=0.0)


# ---------------------------------------------------------------------------
# Logarithmic (law of the wall) profile
# ---------------------------------------------------------------------------

def test_log_law_zero_at_roughness_height():
    """Velocity is zero at z' = z0 (ln(1) = 0)."""
    u = log_law_profile(0.01, shear_velocity=0.1, roughness_height=0.01)
    assert u == pytest.approx(0.0)


def test_log_law_clamped_below_roughness():
    """Sub-roughness heights clamp to zero (log law not valid there)."""
    u = log_law_profile(0.001, shear_velocity=0.1, roughness_height=0.01)
    assert u == 0.0


def test_log_law_monotonic_increasing():
    z = np.linspace(0.01, 10.0, 50)
    u = log_law_profile(z, shear_velocity=0.1, roughness_height=0.01)
    assert np.all(np.diff(u) >= 0.0)


def test_log_law_known_value():
    """u = (u*/kappa)*ln(z'/z0). At z'=1, z0=0.01: ln(100)=4.60517."""
    u = log_law_profile(1.0, shear_velocity=0.1, roughness_height=0.01, von_karman=0.41)
    assert u == pytest.approx((0.1 / 0.41) * np.log(100.0))


def test_log_law_depth_averaged_matches_analytical_and_numerical():
    """Hand-verified: H=10, z0=0.01, u*=0.1, kappa=0.41 -> ~1.44116 m/s."""
    H, ustar, z0, kappa = 10.0, 0.1, 0.01, 0.41
    analytic = log_law_depth_averaged(H, ustar, z0, von_karman=kappa)
    assert analytic == pytest.approx(1.4411596, rel=1e-6)  # hand-verified

    # Numerical confirmation over [z0, H] (region where log law is positive).
    z = np.linspace(z0, H, 200001)
    u = log_law_profile(z, ustar, z0, von_karman=kappa)
    numeric = np.trapz(u, z) / H
    assert numeric == pytest.approx(analytic, rel=1e-4)


def test_log_law_invalid_inputs():
    with pytest.raises(ValueError):
        log_law_profile(1.0, shear_velocity=0.0, roughness_height=0.01)
    with pytest.raises(ValueError):
        log_law_profile(1.0, shear_velocity=0.1, roughness_height=0.0)
    with pytest.raises(ValueError):
        log_law_depth_averaged(0.005, 0.1, 0.01)  # H <= z0


# ---------------------------------------------------------------------------
# Profile generator
# ---------------------------------------------------------------------------

def test_river_velocity_profile_power_law_endpoints():
    heights, speeds = river_velocity_profile(
        21, total_depth=10.0, model="power_law", surface_velocity=2.0
    )
    assert len(heights) == len(speeds) == 21
    assert heights[0] == 0.0 and heights[-1] == 10.0
    assert speeds[0] == pytest.approx(0.0)
    assert speeds[-1] == pytest.approx(2.0)
    assert np.all(np.diff(speeds) >= 0.0)


def test_river_velocity_profile_log_law():
    heights, speeds = river_velocity_profile(
        50, total_depth=10.0, model="log_law", shear_velocity=0.1, roughness_height=0.01
    )
    assert speeds[0] == 0.0  # bed clamped
    assert np.all(np.diff(speeds) >= 0.0)


def test_river_velocity_profile_invalid():
    with pytest.raises(ValueError):
        river_velocity_profile(1, total_depth=10.0)
    with pytest.raises(ValueError):
        river_velocity_profile(10, total_depth=10.0, model="bogus")
