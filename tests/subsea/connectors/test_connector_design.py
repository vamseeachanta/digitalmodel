"""Tests for subsea jumper connector mechanics (API 17R / ISO 13628-1).

Hand-verified worked example (10-inch tree-to-manifold rigid jumper
connector, X65, 5000 psi):

    D_o = 0.273 m, t = 0.020 m  ->  D_i = 0.233 m
    p   = 34.5e6 Pa (5000 psi), seal diameter = bore D_i
    F_endcap = p * pi/4 * D_i^2
             = 34.5e6 * (pi/4) * 0.233^2 = 1.471e6 N      [Eq. 1]
    U_body   = sigma_vm / (0.67 * 448e6) = 0.653  -> PASS
"""

import math

import pytest

from digitalmodel.subsea.connectors import (
    PipeSection,
    ConnectorMaterial,
    ConnectorType,
    end_cap_force,
    verify_connector,
    thermal_expansion_load,
    bending_from_radius,
    JumperType,
    get_jumper_spec,
)


@pytest.fixture
def section():
    return PipeSection(outer_diameter=0.273, wall_thickness=0.020)


@pytest.fixture
def material():
    return ConnectorMaterial(name="X65", smys=448e6)


# ---------------------------------------------------------------- section ---
def test_section_properties(section):
    assert section.inner_diameter == pytest.approx(0.233)
    assert section.area == pytest.approx(math.pi / 4 * (0.273**2 - 0.233**2))
    assert section.section_modulus == pytest.approx(
        2 * section.moment_of_inertia / 0.273
    )


def test_invalid_section():
    with pytest.raises(ValueError):
        PipeSection(outer_diameter=0.2, wall_thickness=0.2)  # t >= D_o/2


# ----------------------------------------------------------- end-cap (Eq1) --
def test_end_cap_force_matches_formula():
    p = 34.5e6
    d_i = 0.233
    expected = p * math.pi / 4 * d_i**2
    assert end_cap_force(p, d_i) == pytest.approx(expected)
    assert end_cap_force(p, d_i) == pytest.approx(1.471e6, rel=1e-3)


# --------------------------------------------------- within-capacity (PASS) -
def test_within_capacity_passes(section, material):
    r = verify_connector(
        section,
        material,
        internal_pressure=34.5e6,
        applied_axial=200e3,
        applied_moment=80e3,
        preload=8e6,
        seal_seating_force=1.5e6,
        usage_factor=0.67,
        connector_type=ConnectorType.CLAMPED,
    )
    assert r.passed is True
    assert r.utilisation_body == pytest.approx(0.653, abs=0.005)
    assert r.utilisation < 1.0
    assert r.governing_load == "connector_body"
    assert r.f_residual > 0
    # hand-verified end-cap thrust
    assert r.f_endcap == pytest.approx(1.471e6, rel=1e-3)


# ----------------------------------------------------- over-capacity (FAIL) -
def test_over_capacity_fails_with_governing_load(section, material):
    r = verify_connector(
        section,
        material,
        internal_pressure=60e6,
        applied_axial=500e3,
        applied_moment=300e3,
        preload=2e6,
        seal_seating_force=1e6,
        usage_factor=0.67,
        connector_type=ConnectorType.CLAMPED,
    )
    assert r.passed is False
    assert r.utilisation > 1.0
    # preload exhausted -> seal de-energised, infinite utilisation governs
    assert math.isinf(r.utilisation_seal)
    assert r.f_residual < 0
    assert r.governing_load == "seal_preload"
    assert r.utilisation_body > 1.0  # body also over allowable here


# --------------------------------------------------------- boundary (U=1) ---
def test_boundary_utilisation_unity(section, material):
    """Tune usage_factor so the body utilisation sits exactly at 1.0."""
    base = verify_connector(
        section,
        material,
        internal_pressure=34.5e6,
        applied_axial=200e3,
        applied_moment=80e3,
        preload=8e6,
        connector_type=ConnectorType.CLAMPED,
    )
    # sigma_vm is fixed; pick eta so eta*SMYS == sigma_vm -> U_body == 1
    eta = base.sigma_vm / material.smys
    r = verify_connector(
        section,
        material,
        internal_pressure=34.5e6,
        applied_axial=200e3,
        applied_moment=80e3,
        preload=8e6,
        usage_factor=eta,
        connector_type=ConnectorType.CLAMPED,
    )
    assert r.utilisation_body == pytest.approx(1.0, abs=1e-9)
    assert r.passed is True  # <= 1.0 passes at the boundary


def test_welded_skips_seal_check(section, material):
    r = verify_connector(
        section,
        material,
        internal_pressure=34.5e6,
        applied_axial=200e3,
        applied_moment=80e3,
        connector_type=ConnectorType.WELDED,
    )
    assert r.utilisation_seal == 0.0
    assert math.isnan(r.f_residual)
    assert any("Welded" in n for n in r.notes)


# ------------------------------------------------------------ thermal -------
def test_thermal_expansion_load(section):
    t = thermal_expansion_load(
        section, length=10.0, delta_temperature=80.0,
        thermal_expansion_coeff=1.17e-5, youngs_modulus=207e9,
    )
    assert t.thermal_strain == pytest.approx(1.17e-5 * 80.0)
    assert t.free_growth == pytest.approx(1.17e-5 * 80.0 * 10.0)
    assert t.restrained_stress == pytest.approx(207e9 * 1.17e-5 * 80.0)
    # restraint_factor scales the reacted force linearly
    half = thermal_expansion_load(
        section, length=10.0, delta_temperature=80.0, restraint_factor=0.5
    )
    assert half.axial_force == pytest.approx(t.axial_force * 0.5)


def test_thermal_invalid_restraint(section):
    with pytest.raises(ValueError):
        thermal_expansion_load(section, length=1.0, delta_temperature=1.0,
                               restraint_factor=1.5)


# ------------------------------------------------------------ bending -------
def test_bending_from_radius_boundary(section):
    """At R = E (D_o/2)/allow the bending utilisation is exactly 1.0."""
    E = 207e9
    allow = 300e6
    R = E * (section.outer_diameter / 2.0) / allow
    b = bending_from_radius(section, radius=R, youngs_modulus=E,
                            allowable_stress=allow)
    assert b.curvature == pytest.approx(1.0 / R)
    assert b.bending_stress == pytest.approx(allow, rel=1e-9)
    assert b.utilisation == pytest.approx(1.0, abs=1e-9)
    assert b.passed is True


def test_bending_invalid_radius(section):
    with pytest.raises(ValueError):
        bending_from_radius(section, radius=0.0)


# ------------------------------------------------------------ catalog -------
def test_jumper_catalog():
    spec = get_jumper_spec(JumperType.FLEXIBLE)
    assert spec.dynamic is True
    assert "fatigue" in spec.governing_checks
    assert get_jumper_spec(JumperType.RIGID).dynamic is False
