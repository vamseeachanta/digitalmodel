"""Tests for the API 17B / 17J unbonded flexible pipe core.

Hand-verified reference values are documented inline; see
docs solution issue-494.md for the worked combined-stress check.
"""

import math

import pytest

from digitalmodel.subsea.flexible_pipes import (
    BendStiffenerResult,
    FlexiblePipeSection,
    Layer,
    LayerType,
    LoadCase,
    TensileArmour,
    allowable_wire_stress,
    armour_bending_stress,
    armour_tension_stress,
    developed_min_radius,
    required_effective_ei,
    screen_bend_stiffener,
    verify_flexible_pipe,
)

# ─────────────────────────── cross_section ────────────────────────────


def test_layer_mass_per_length_hand_verified():
    # carcass: ID=0.200 m, t=0.005 m, rho=7850 -> mean_D=0.205
    # m = 7850 * pi * 0.205 * 0.005 = 25.278 kg/m
    layer = Layer(LayerType.CARCASS, inner_diameter=0.200, thickness=0.005, density=7850.0)
    assert layer.mean_diameter == pytest.approx(0.205)
    assert layer.outer_diameter == pytest.approx(0.210)
    assert layer.mass_per_length() == pytest.approx(25.278, rel=1e-4)


def test_layer_axial_stiffness_hand_verified():
    # tensile armour: E=207e9, A=2e-3, alpha=35 deg
    # cos35 = 0.819152; cos^3 = 0.549799
    # EA = 207e9 * 2e-3 * 0.549799 = 2.2762e8 N
    layer = Layer(
        LayerType.TENSILE_ARMOUR,
        inner_diameter=0.220,
        thickness=0.006,
        density=7850.0,
        youngs_modulus=207e9,
        metallic_area=2e-3,
        lay_angle_deg=35.0,
    )
    assert layer.axial_stiffness() == pytest.approx(2.2762e8, rel=1e-3)


def test_layer_no_modulus_has_zero_stiffness():
    sheath = Layer(LayerType.PRESSURE_SHEATH, inner_diameter=0.21, thickness=0.006, density=940.0)
    assert sheath.axial_stiffness() == 0.0
    assert sheath.bending_stiffness() == 0.0


def _build_section():
    carcass = Layer(LayerType.CARCASS, 0.200, 0.005, 7850.0, youngs_modulus=200e9,
                    metallic_area=1.2e-3, lay_angle_deg=87.0)
    sheath = Layer(LayerType.PRESSURE_SHEATH, 0.210, 0.006, 940.0)
    parmour = Layer(LayerType.PRESSURE_ARMOUR, 0.222, 0.006, 7850.0, youngs_modulus=207e9,
                    metallic_area=1.8e-3, lay_angle_deg=85.0)
    t1 = Layer(LayerType.TENSILE_ARMOUR, 0.234, 0.005, 7850.0, youngs_modulus=207e9,
               metallic_area=2.0e-3, lay_angle_deg=35.0)
    t2 = Layer(LayerType.TENSILE_ARMOUR, 0.244, 0.005, 7850.0, youngs_modulus=207e9,
               metallic_area=2.0e-3, lay_angle_deg=-35.0)
    outer = Layer(LayerType.OUTER_SHEATH, 0.254, 0.007, 940.0)
    return FlexiblePipeSection([carcass, sheath, parmour, t1, t2, outer], name="6in_riser")


def test_section_geometry_and_armour_area():
    sec = _build_section()
    assert sec.bore_diameter == pytest.approx(0.200)
    assert sec.outer_diameter == pytest.approx(0.268)  # 0.254 + 2*0.007
    # two tensile armour layers, 2e-3 each
    assert sec.tensile_armour_area() == pytest.approx(4.0e-3)


def test_section_rejects_non_contiguous_layers():
    a = Layer(LayerType.CARCASS, 0.200, 0.005, 7850.0)       # OD 0.210
    b = Layer(LayerType.OUTER_SHEATH, 0.215, 0.005, 940.0)   # ID 0.215 (gap)
    with pytest.raises(ValueError, match="not contiguous"):
        FlexiblePipeSection([a, b])


def test_submerged_weight_sign():
    sec = _build_section()
    # empty bore, steel-dominated pipe should be heavier than water
    w_empty = sec.submerged_weight_per_length(content_density=0.0)
    assert w_empty > 0.0
    # heavy bore contents make it heavier still
    w_full = sec.submerged_weight_per_length(content_density=900.0)
    assert w_full > w_empty


def test_layer_validation():
    with pytest.raises(ValueError):
        Layer(LayerType.CARCASS, inner_diameter=-0.1, thickness=0.005, density=7850.0)
    with pytest.raises(ValueError):
        Layer(LayerType.CARCASS, inner_diameter=0.2, thickness=0.0, density=7850.0)


# ─────────────────────────── design_checks_17J ─────────────────────────


def _armour():
    return TensileArmour(
        metallic_area=2.0e-3,
        lay_angle_deg=30.0,
        smys=1400e6,
        mean_radius=0.12,
        wire_thickness=0.006,
        youngs_modulus=207e9,
    )


def test_armour_tension_stress_hand_verified():
    # T=500 kN, A=2e-3, cos30=0.8660254 -> sigma_T = 500000/(2e-3*0.8660254)
    # = 2.8868e8 Pa = 288.68 MPa
    s = armour_tension_stress(_armour(), 500e3)
    assert s == pytest.approx(288.68e6, rel=1e-4)


def test_allowable_wire_stress_factors():
    assert allowable_wire_stress(1400e6, LoadCase.NORMAL) == pytest.approx(938e6)
    assert allowable_wire_stress(1400e6, LoadCase.EXTREME) == pytest.approx(1190e6)
    assert allowable_wire_stress(1400e6, utilisation_factor=0.5) == pytest.approx(700e6)
    with pytest.raises(ValueError):
        allowable_wire_stress(1400e6, utilisation_factor=1.5)


def test_armour_bending_stress_hand_verified():
    # E=207e9, t_w/2=0.003, R=5 -> sigma_b = 207e9*0.003/5 = 1.242e8 Pa
    s = armour_bending_stress(_armour(), 5.0)
    assert s == pytest.approx(124.2e6, rel=1e-4)


def test_verify_within_limit_passes():
    # combined = (288.68 + 124.2) / 938 = 0.4402 ; U_curv = 4/5 = 0.8
    res = verify_flexible_pipe(
        _armour(),
        effective_tension=500e3,
        applied_radius=5.0,
        minimum_bend_radius=4.0,
        load_case=LoadCase.NORMAL,
    )
    assert res.passes
    assert res.u_tension == pytest.approx(0.30776, rel=1e-3)
    assert res.u_combined == pytest.approx(0.44017, rel=1e-3)
    assert res.u_curvature == pytest.approx(0.8)
    assert res.governing == "curvature"
    assert res.max_utilisation == pytest.approx(0.8)


def test_verify_over_tension_fails():
    # T=1500 kN -> sigma_T = 866.0 MPa ; combined with bending 124.2 = 990.2
    # /938 = 1.0556 > 1 -> fail
    res = verify_flexible_pipe(
        _armour(),
        effective_tension=1500e3,
        applied_radius=5.0,
        minimum_bend_radius=4.0,
    )
    assert not res.passes
    assert res.u_combined > 1.0
    assert res.governing == "combined"


def test_verify_mbr_violation_fails():
    # applied radius 3 m < MBR 4 m -> U_curv = 4/3 = 1.333 > 1
    res = verify_flexible_pipe(
        _armour(),
        effective_tension=300e3,
        applied_radius=3.0,
        minimum_bend_radius=4.0,
    )
    assert not res.passes
    assert res.u_curvature == pytest.approx(4.0 / 3.0)
    assert res.governing == "curvature"


def test_verify_boundary_unity():
    # choose tension so U_tension is exactly 1.0 with no curvature:
    # sigma_allow = 0.67*1400 = 938 MPa ; required sigma_T = 938 MPa
    # T = 938e6 * A * cos30 = 938e6 * 2e-3 * 0.8660254 = 1.6247e6 N
    t_unity = 938e6 * 2e-3 * math.cos(math.radians(30.0))
    res = verify_flexible_pipe(_armour(), effective_tension=t_unity)
    assert res.u_tension == pytest.approx(1.0, rel=1e-9)
    assert res.passes  # exactly 1.0 still passes (<= 1.0)


def test_extreme_load_case_relaxes_allowable():
    normal = verify_flexible_pipe(_armour(), 1300e3)
    extreme = verify_flexible_pipe(_armour(), 1300e3, load_case=LoadCase.EXTREME)
    assert extreme.u_tension < normal.u_tension


# ─────────────────────────── bend_stiffener ────────────────────────────


def test_developed_min_radius_hand_verified():
    # EI=5e5, T=2e5, theta=0.1 rad -> R = sqrt(5e5/2e5)/0.1
    # = sqrt(2.5)/0.1 = 1.5811/0.1 = 15.811 m
    r = developed_min_radius(5e5, 2e5, 0.1)
    assert r == pytest.approx(15.8114, rel=1e-4)


def test_required_effective_ei_roundtrip():
    # EI_req to reach R=15.8114 at T=2e5, theta=0.1:
    # = 2e5 * (0.1*15.8114)^2 = 2e5 * 2.5 = 5e5
    ei = required_effective_ei(15.8114, 2e5, 0.1)
    assert ei == pytest.approx(5e5, rel=1e-4)


def test_screen_bend_stiffener_passes():
    res = screen_bend_stiffener(
        bending_stiffness=5e5, effective_tension=2e5, deflection_angle=0.1,
        minimum_bend_radius=4.0,
    )
    assert isinstance(res, BendStiffenerResult)
    assert res.passes
    assert res.r_min == pytest.approx(15.8114, rel=1e-4)
    assert res.u_curvature == pytest.approx(4.0 / 15.8114, rel=1e-4)


def test_screen_bend_stiffener_fails_below_mbr():
    # large rotation -> small radius below MBR
    res = screen_bend_stiffener(
        bending_stiffness=5e5, effective_tension=2e5, deflection_angle=0.5,
        minimum_bend_radius=4.0,
    )
    # R = sqrt(2.5)/0.5 = 3.162 m < 4 m
    assert not res.passes
    assert res.r_min == pytest.approx(3.1623, rel=1e-4)
    assert res.u_curvature > 1.0


def test_bend_stiffener_validation():
    with pytest.raises(ValueError):
        developed_min_radius(0.0, 2e5, 0.1)
    with pytest.raises(ValueError):
        developed_min_radius(5e5, 0.0, 0.1)
    with pytest.raises(ValueError):
        screen_bend_stiffener(5e5, 2e5, 0.1, minimum_bend_radius=0.0)
