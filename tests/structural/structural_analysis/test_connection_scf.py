# ABOUTME: Golden tests for connection & bracket SCF (DNV-RP-C203) vs published
# ABOUTME: misalignment, hot-spot extrapolation, and Efthymiou chord-saddle values.
import math

import pytest

from digitalmodel.structural.structural_analysis.connection_scf import (
    CODE_REFERENCE,
    ConnectionSCFResult,
    assess_connection,
    bracket_toe_scf,
    efthymiou_chord_saddle_axial_scf,
    hot_spot_stress,
    junction_hotspot_stress,
    misalignment_scf,
)


# --- hot-spot stress = SCF x nominal (DNV-RP-C203 Sec. 2.3) -------------------
def test_hot_spot_stress_is_scf_times_nominal():
    assert hot_spot_stress(1.45, 100.0) == pytest.approx(145.0)


def test_hot_spot_stress_rejects_scf_below_one():
    with pytest.raises(ValueError):
        hot_spot_stress(0.9, 100.0)


# --- GOLDEN: axial misalignment SCF (frame-to-girder / girder-to-shell) -------
def test_golden_misalignment_pinned():
    # DNV-RP-C203 App. D: pinned/single butt SCF = 1 + 3 e/t.
    # e=3 mm, t=20 mm -> 1 + 3*3/20 = 1.45 (independently reproduced).
    assert misalignment_scf(20.0, 3.0, restrained=False) == pytest.approx(
        1.45, rel=1e-9
    )


def test_golden_misalignment_restrained():
    # DNV-RP-C203: restrained (cruciform/fixed-ends) SCF = 1 + 6 e/t.
    # e=3 mm, t=20 mm -> 1 + 6*3/20 = 1.90 (independently reproduced).
    assert misalignment_scf(20.0, 3.0, restrained=True) == pytest.approx(1.90, rel=1e-9)


def test_restrained_misalignment_exceeds_pinned():
    pinned = misalignment_scf(20.0, 2.0, restrained=False)
    restrained = misalignment_scf(20.0, 2.0, restrained=True)
    assert bool(restrained > pinned)


# --- GOLDEN: hot-spot stress by linear extrapolation (DNV Eq. 4.1) -----------
def test_golden_hotspot_linear_extrapolation():
    # sigma_hs = 1.67*200 - 0.67*150 = 334 - 100.5 = 233.5 MPa.
    assert junction_hotspot_stress(20.0, 200.0, 150.0) == pytest.approx(233.5)


# --- GOLDEN: Efthymiou chord-saddle SCF, axial T-joint (tau^1.1 + short-chord F1)
def test_golden_efthymiou_chord_saddle_axial():
    # gamma*tau^1.1*(1.11-3(beta-0.52)^2)*sin(theta)^1.6 * F1(beta,gamma,alpha)
    # beta=0.5, gamma=12, tau=0.5, theta=90, alpha=5 (default short chord) ->
    #   12 * 0.5^1.1 * (1.11 - 3*(-0.02)^2) * 1 = 6.207 uncorrected,
    #   F1 = 0.766 -> 6.207 * 0.766 = 4.755 (DNV-RP-C203 App. B short chord).
    scf = efthymiou_chord_saddle_axial_scf(0.5, 12.0, 0.5, 90.0)
    assert scf == pytest.approx(4.755, abs=0.01)
    # hand value to full precision (with F1 short-chord factor at alpha=5)
    f1 = 1.0 - (0.83 * 0.5 - 0.56 * 0.5**2 - 0.02) * 12.0**0.23 * math.exp(
        -0.21 * 12.0 ** (-1.16) * 5.0**2.5
    )
    expected = 12.0 * 0.5**1.1 * (1.11 - 3.0 * (0.5 - 0.52) ** 2) * f1
    assert scf == pytest.approx(expected, rel=1e-12)


def test_long_chord_recovers_uncorrected_saddle():
    # alpha >= 12 -> F1 = 1.0 -> the uncorrected published 6.21 is recovered.
    scf = efthymiou_chord_saddle_axial_scf(0.5, 12.0, 0.5, 90.0, alpha=12.0)
    assert scf == pytest.approx(6.207, abs=0.01)


def test_efthymiou_angle_reduces_saddle_scf():
    # Saddle SCF scales with sin(theta)^1.6 -> a Y-joint (theta<90) is lower.
    s90 = efthymiou_chord_saddle_axial_scf(0.5, 12.0, 0.5, 90.0)
    s45 = efthymiou_chord_saddle_axial_scf(0.5, 12.0, 0.5, 45.0)
    assert bool(s45 < s90)


def test_efthymiou_rejects_nonpositive():
    with pytest.raises(ValueError):
        efthymiou_chord_saddle_axial_scf(0.0, 12.0, 0.5)


# --- soft-toe vs hard-toe bracket --------------------------------------------
def test_soft_toe_lowers_scf_versus_hard_toe():
    hard = bracket_toe_scf(20.0, soft_toe=False)
    soft = bracket_toe_scf(20.0, soft_toe=True)
    assert bool(soft < hard)
    assert bool(hard > 1.0) and bool(soft >= 1.0)


def test_bracket_toe_matches_fillet_toe_formula():
    # hard toe: angle=45 deg, r=1 mm, t=20 mm
    #   1 + 0.27*(pi/4)^0.37*(20/1)^0.25
    th = math.radians(45.0)
    expected = 1.0 + 0.27 * (th**0.37) * (20.0 / 1.0) ** 0.25
    assert bracket_toe_scf(20.0, soft_toe=False) == pytest.approx(
        round(expected, 4), rel=1e-9
    )


# --- connection assessment: detail class + SCF + hot-spot --------------------
def test_assess_connection_returns_class_and_hotspot():
    scf = misalignment_scf(20.0, 3.0, restrained=False)  # 1.45
    res = assess_connection(
        "load-carrying attachment fillet weld at bracket toe",
        nominal_stress_mpa=120.0,
        scf=scf,
        joint_type="attachment",
        transverse_load=True,
    )
    assert isinstance(res, ConnectionSCFResult)
    assert res.code_reference == CODE_REFERENCE
    # load-carrying transverse attachment -> DNV class F1 (Table 2-5)
    assert res.sn_class == "F1"
    assert res.hot_spot_stress_mpa == pytest.approx(1.45 * 120.0)


def test_assess_connection_non_load_carrying_attachment_class_e():
    res = assess_connection(
        "non-load carrying bracket attachment",
        nominal_stress_mpa=100.0,
        scf=1.3,
        joint_type="attachment",
        transverse_load=False,
    )
    assert res.sn_class == "E"


def test_code_reference_constant():
    assert CODE_REFERENCE == "DNV-RP-C203"
