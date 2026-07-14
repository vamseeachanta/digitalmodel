# ABOUTME: Golden tests for hull-girder longitudinal strength — IACS UR S11
# ABOUTME: wave bending, combined envelope, yield, and simplified ultimate (S11A).
import math

import pytest

from digitalmodel.naval_architecture.hull_girder_strength import (
    bending_distribution_factor,
    bending_stress_mpa,
    combined_bending_moment,
    compression_flange_ultimate_stress,
    hull_girder_ultimate_moment,
    material_factor_k,
    permissible_stress,
    rule_block_coefficient,
    section_modulus_check,
    shear_distribution_factor_f1,
    shear_distribution_factor_f2,
    ultimate_strength_check,
    wave_bending_moment,
    wave_coefficient,
    wave_shear_force,
)


# --- IACS UR S11 wave coefficient C at rule breakpoints ----------------------
@pytest.mark.parametrize(
    "L, c",
    [
        (90.0, 10.75 - (210.0 / 100.0) ** 1.5),   # 7.7068
        (200.0, 9.75),
        (300.0, 10.75),                            # plateau start
        (350.0, 10.75),                            # plateau end
        (400.0, 10.75 - (50.0 / 150.0) ** 1.5),    # 10.5575
    ],
)
def test_wave_coefficient(L, c):
    assert math.isclose(wave_coefficient(L), c, rel_tol=1e-9)


# --- IACS UR S11 wave bending moment -----------------------------------------
def test_wave_bending_moment_golden():
    w = wave_bending_moment(length_m=200.0, beam_m=32.0, block_coefficient=0.8)
    # C=9.75; base=9.75*200^2*32=12,480,000.
    assert math.isclose(w.wave_coefficient, 9.75, rel_tol=1e-9)
    assert math.isclose(w.hogging_kn_m, 1_896_960.0, rel_tol=1e-9)
    assert math.isclose(w.sagging_kn_m, -2_059_200.0, rel_tol=1e-9)
    # Sagging magnitude exceeds hogging (the (Cb+0.7) factor).
    assert abs(w.sagging_kn_m) > w.hogging_kn_m
    assert w.code_reference == "IACS UR S11 / S11A"


# --- IACS UR S11 rule Cb floor ------------------------------------------------
def test_rule_block_coefficient_floor():
    # S11 2.2.1: Cb not to be taken less than 0.60 in the wave load formulae.
    assert rule_block_coefficient(0.45) == 0.60
    assert rule_block_coefficient(0.82) == 0.82
    with pytest.raises(ValueError, match="positive"):
        rule_block_coefficient(0.0)


def test_wave_bending_moment_applies_cb_floor():
    floored = wave_bending_moment(200.0, 32.0, 0.5)
    at_floor = wave_bending_moment(200.0, 32.0, 0.6)
    assert floored.hogging_kn_m == pytest.approx(at_floor.hogging_kn_m)
    assert floored.sagging_kn_m == pytest.approx(at_floor.sagging_kn_m)


# --- IACS UR S11 bending distribution factor M (Fig. 1) -----------------------
@pytest.mark.parametrize(
    "xi, m",
    [
        (0.0, 0.0),
        (0.2, 0.5),      # linear 0 -> 1 over [0, 0.4L]
        (0.4, 1.0),
        (0.5, 1.0),      # plateau 0.4L - 0.65L
        (0.65, 1.0),
        (0.825, 0.5),    # linear 1 -> 0 over [0.65L, L]
        (1.0, 0.0),
    ],
)
def test_bending_distribution_factor(xi, m):
    assert bending_distribution_factor(xi) == pytest.approx(m)


def test_bending_distribution_factor_rejects_out_of_range():
    with pytest.raises(ValueError, match="x_over_l"):
        bending_distribution_factor(1.2)


# --- IACS UR S11 wave shear force (Figs. 2-3) ---------------------------------
def test_shear_distribution_factors_at_rule_knots():
    cb = 0.8
    aft_hump = 1.59 * cb / (cb + 0.7)    # F1, 0.2L-0.3L
    fwd_hump = 1.73 * cb / (cb + 0.7)    # F2, 0.7L-0.85L
    for xi in (0.2, 0.25, 0.3):
        assert shear_distribution_factor_f1(xi, cb) == pytest.approx(aft_hump)
        assert shear_distribution_factor_f2(xi, cb) == pytest.approx(0.92)
    for xi in (0.4, 0.5, 0.6):
        assert shear_distribution_factor_f1(xi, cb) == pytest.approx(0.7)
        assert shear_distribution_factor_f2(xi, cb) == pytest.approx(0.7)
    for xi in (0.7, 0.775, 0.85):
        assert shear_distribution_factor_f1(xi, cb) == pytest.approx(1.0)
        assert shear_distribution_factor_f2(xi, cb) == pytest.approx(fwd_hump)
    for xi in (0.0, 1.0):
        assert shear_distribution_factor_f1(xi, cb) == pytest.approx(0.0)
        assert shear_distribution_factor_f2(xi, cb) == pytest.approx(0.0)
    # linear between knots (midpoint of the 0.3L-0.4L ramp)
    assert shear_distribution_factor_f1(0.35, cb) == pytest.approx(
        0.5 * (aft_hump + 0.7)
    )


def test_wave_shear_force_golden():
    # L=200 m, B=32 m, Cb=0.8: C=9.75; base = 0.30*C*L*B*(Cb+0.7) = 28 080 kN.
    base = 0.30 * 9.75 * 200.0 * 32.0 * 1.5
    assert math.isclose(base, 28_080.0, rel_tol=1e-12)
    mid = wave_shear_force(200.0, 32.0, 0.8, x_over_l=0.5)
    assert math.isclose(mid.positive_kn, 0.7 * base, rel_tol=1e-9)
    assert math.isclose(mid.negative_kn, -0.7 * base, rel_tol=1e-9)
    aft = wave_shear_force(200.0, 32.0, 0.8, x_over_l=0.25)
    assert math.isclose(aft.positive_kn, (1.59 * 0.8 / 1.5) * base, rel_tol=1e-9)
    assert math.isclose(aft.negative_kn, -0.92 * base, rel_tol=1e-9)
    fwd = wave_shear_force(200.0, 32.0, 0.8, x_over_l=0.775)
    assert math.isclose(fwd.positive_kn, base, rel_tol=1e-9)
    assert math.isclose(fwd.negative_kn, -(1.73 * 0.8 / 1.5) * base, rel_tol=1e-9)
    end = wave_shear_force(200.0, 32.0, 0.8, x_over_l=0.0)
    assert end.positive_kn == 0.0 and end.negative_kn == 0.0
    assert mid.code_reference == "IACS UR S11 / S11A"


# --- combined still-water + wave envelope ------------------------------------
def test_combined_envelope():
    w = wave_bending_moment(200.0, 32.0, 0.8)
    env = combined_bending_moment(
        msw_hogging_kn_m=500_000.0, msw_sagging_kn_m=-400_000.0, wave=w)
    assert math.isclose(env.hogging_kn_m, 500_000.0 + w.hogging_kn_m)
    assert math.isclose(env.sagging_kn_m, -400_000.0 + w.sagging_kn_m)


# --- material factor / permissible stress ------------------------------------
def test_material_factor_and_permissible():
    assert material_factor_k(235) == 1.00
    assert material_factor_k(355) == 0.72        # AH36 / HT36
    assert material_factor_k(390) == 0.68        # EH40 / HT40
    assert material_factor_k(360) == 0.72        # nearest tabulated (355)
    assert math.isclose(permissible_stress(235), 175.0, rel_tol=1e-9)
    assert math.isclose(permissible_stress(355), 175.0 / 0.72, rel_tol=1e-9)


# --- section-modulus (yield) check -------------------------------------------
def test_bending_stress_and_yield_check():
    # M = 2e6 kN·m, Z = 50 m^3 -> sigma = 40 MPa.
    assert math.isclose(bending_stress_mpa(2.0e6, 50.0), 40.0, rel_tol=1e-9)

    chk = section_modulus_check(2.0e6, 50.0, yield_mpa=355.0)
    assert math.isclose(chk.stress_mpa, 40.0, rel_tol=1e-9)
    assert math.isclose(chk.permissible_mpa, 175.0 / 0.72, rel_tol=1e-9)
    assert math.isclose(chk.utilization, 40.0 / (175.0 / 0.72), rel_tol=1e-9)
    assert chk.passes is True


def test_yield_check_fails_when_overstressed():
    chk = section_modulus_check(20.0e6, 50.0, yield_mpa=355.0)  # 400 MPa > 243
    assert chk.passes is False
    assert chk.utilization > 1.0


# --- simplified hull-girder ultimate (S11A format) ---------------------------
def test_ultimate_moment_and_check():
    # sigma_U = 200 MPa, Z = 50 m^3 -> M_U = 200*50*1000 = 1e7 kN·m.
    m_u = hull_girder_ultimate_moment(50.0, 200.0)
    assert math.isclose(m_u, 1.0e7, rel_tol=1e-9)

    chk = ultimate_strength_check(
        msw_kn_m=1.0e6, mwv_kn_m=2.0e6, ultimate_moment_kn_m=m_u)
    # design = 1.0*1e6 + 1.2*2e6 = 3.4e6; factored cap = 1e7/1.1 = 9.09e6.
    assert math.isclose(chk.design_moment_kn_m, 3.4e6, rel_tol=1e-9)
    assert math.isclose(chk.factored_capacity_kn_m, 1.0e7 / 1.1, rel_tol=1e-9)
    assert math.isclose(chk.utilization, 3.4e6 / (1.0e7 / 1.1), rel_tol=1e-9)
    assert chk.passes is True
    assert chk.gamma_w == 1.2


def test_ultimate_check_uses_magnitudes_for_sagging():
    # Negative (sagging) moments enter as magnitudes.
    chk = ultimate_strength_check(-1.0e6, -2.0e6, -1.0e7)
    assert chk.design_moment_kn_m == pytest.approx(3.4e6)


# --- integration: compression-flange ultimate stress via panel solver --------
def test_compression_flange_ultimate_stress_from_panel():
    from digitalmodel.structural.structural_analysis.models import STEEL_AH36
    from digitalmodel.structural.structural_analysis.panel_buckling import (
        StiffenedPanelGeometry,
        StiffenerGeometry,
    )

    panel = StiffenedPanelGeometry(
        plate_length=2400.0, plate_thickness=12.0,
        stiffener=StiffenerGeometry(
            web_height=250.0, web_thickness=10.0, flange_width=90.0,
            flange_thickness=12.0, spacing=800.0, section_type="tee"),
    )
    sigma_u = compression_flange_ultimate_stress(panel, STEEL_AH36)
    assert 0.0 < sigma_u <= STEEL_AH36.yield_strength  # buckling-reduced
    # Feeds the ultimate moment directly.
    assert hull_girder_ultimate_moment(40.0, sigma_u) > 0.0


# --- end-to-end rule worked example ------------------------------------------
def test_worked_example_chain():
    # 200 m / 32 m / Cb 0.8 hull, AH36, Z = 18 m^3, M_sw,sag = -0.6e6 kN·m.
    w = wave_bending_moment(200.0, 32.0, 0.8)
    env = combined_bending_moment(0.5e6, -0.6e6, w)
    # Sagging governs; check yield at the deck and the S11A ultimate.
    yld = section_modulus_check(env.sagging_kn_m, 18.0, 355.0)
    assert yld.stress_mpa > 0
    m_u = hull_girder_ultimate_moment(18.0, 0.85 * 355.0)  # 85% knockdown
    ult = ultimate_strength_check(0.6e6, abs(w.sagging_kn_m), m_u)
    assert ult.ultimate_capacity_kn_m == pytest.approx(0.85 * 355.0 * 18.0 * 1e3)
    assert isinstance(ult.passes, bool)
