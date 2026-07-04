# ABOUTME: Golden tests for hull-girder longitudinal strength — IACS UR S11
# ABOUTME: wave bending, combined envelope, yield, and simplified ultimate (S11A).
import math

import pytest

from digitalmodel.naval_architecture.hull_girder_strength import (
    bending_stress_mpa,
    combined_bending_moment,
    compression_flange_ultimate_stress,
    hull_girder_ultimate_moment,
    material_factor_k,
    permissible_stress,
    section_modulus_check,
    ultimate_strength_check,
    wave_bending_moment,
    wave_coefficient,
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
