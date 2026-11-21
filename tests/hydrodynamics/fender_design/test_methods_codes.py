"""Energy methods of WG211, BS 6349-4, UFC 4-152-01, ROM 2.0-11, EAU and IS 4651.

Published oracle: Roubos, Mirihagalla, Gaal, Blankers, Groenewegen (2024),
"Comparison of fender dimensions, PIANC WG211 and PIANC WG33", 35th PIANC
World Congress, pp. 973-982, Example 1 (LNG jetty): Table 2 (vessel),
Table 3 (energy by WG211, BS 6349-4 and WG33), Table 4 (fender capacity).
Printed coefficients are two-decimal; tolerances cover that rounding.
"""

from __future__ import annotations

import math

import pytest

from digitalmodel.hydrodynamics import fender_design as fd

# R24B Table 2: LBP 332 m, B 53.8 m, D 12 m, UKC 2.31 m, 126,000 DWT, Mc 178,579 t,
# impact point 33 % LBP
LNG = fd.Ship(displacement_t=178_579, lbp_m=332.0, beam_m=53.8, draft_m=12.0,
              ship_type="gas", dwt_t=126_000)


def _berth(angle, nav="c"):
    return fd.Berth(water_depth_m=12.0 + 2.31, contact_x_from_bow_m=0.33 * 332.0,
                    berthing_angle_deg=angle, navigation=nav, structure="open", fender_deflection_m=1.0)


def test_r24b_wg33_velocities_from_brolsma_by_dwt():
    # Table 3 columns (vi) curve b 0.076 and (vii) curve c 0.115 m/s at 126,000 DWT
    assert fd.brolsma_velocity(126_000, "b") == pytest.approx(0.076, abs=0.001)
    assert fd.brolsma_velocity(126_000, "c") == pytest.approx(0.115, abs=0.001)


def test_r24b_bs6349_velocity_by_displacement_and_vasco_costa():
    # Table 3 column (iv): curve c 0.100 m/s, Cm 1.45 (Vasco Costa), factor 2.00
    r = fd.energy_by_method(LNG, _berth(6.0), "bs6349", citations="off")
    assert r.velocity_m_s.value == pytest.approx(0.100, abs=0.001)
    assert r.cm.value == pytest.approx(1.45, abs=0.005)
    assert r.factor.value == 2.0
    # normal energy with the printed Ce 0.74: 963 kNm (published)
    assert 0.5 * 178_579 * 0.100**2 * 0.74 * 1.45 == pytest.approx(963, rel=0.01)
    # this module's Ce at 6 deg is within 0.02 of the printed 0.74
    assert r.ce.value == pytest.approx(0.74, abs=0.02)
    assert r.design_energy_knm == pytest.approx(1926, rel=0.03)


def test_r24b_pianc_cm_and_wg33_energy():
    r = fd.energy_by_method(LNG, _berth(6.0, "c"), "pianc2002", factor_position="largest", citations="off")
    assert r.cm.value == pytest.approx(1.73, abs=0.005)          # printed 1.73
    assert r.velocity_m_s.value == pytest.approx(0.115, abs=0.001)
    # column (vii): normal 1,520 kNm, Cab 1.25 -> 1,901 kNm (printed coefficients)
    assert 0.5 * 178_579 * 0.115**2 * 0.74 * 1.73 == pytest.approx(1520, rel=0.01)
    assert r.normal_energy_knm == pytest.approx(1520, rel=0.03)
    # the paper applies 1.25 (largest tanker/bulk/cargo); this module's gas-carrier
    # default is 1.5 (SFT p. 22, no PIANC guidance), so the factor is passed here
    assert r.factor.value == 1.5
    r125 = fd.energy_by_method(LNG, _berth(6.0, "c"), "pianc2002", factor=1.25, citations="off")
    assert r125.design_energy_knm == pytest.approx(1901, rel=0.03)


def test_r24b_wg211_characteristic_and_design_energy():
    case = fd.Case(LNG, _berth(4.0), condition="moderate")
    # column (ii): moderate default 0.15 m/s, Ek,c 2,590 kNm, gamma_E 1.80 -> 4,659 kNm
    r = fd.energy_by_method(case, method="wg211", factor=1.80, citations="off")
    assert r.velocity_m_s.value == 0.15
    assert r.cs.value == 1.0 and r.cc.value == 1.0
    assert r.ce.value == pytest.approx(0.75, abs=0.01)           # printed 0.75 at 4 deg
    assert r.normal_energy_knm == pytest.approx(2590, rel=0.01)
    assert r.design_energy_knm == pytest.approx(4659, rel=0.01)
    # column (i): monitored with site velocity 0.100 m/s, gamma_E 1.55 -> 1,151 and 1,783 kNm
    r1 = fd.energy_by_method(case, method="wg211", velocity_m_s=0.100, factor=1.55, citations="off")
    assert r1.normal_energy_knm == pytest.approx(1151, rel=0.01)
    assert r1.design_energy_knm == pytest.approx(1783, rel=0.01)
    assert r.capacity_divisor == 1.10


def test_r24b_wg211_fender_capacity():
    # Table 4 column (i): 1,826 x 1.06 x 0.985 x 1.03 x 1.00 / 1.10 = 1,784 kNm
    assert fd.wg211_fender_capacity(1826, c_ang=1.03, c_t=1.06, c_v=0.985) == pytest.approx(1784, abs=1.5)


def test_wg211_tables():
    assert fd.wg211_gamma_eref("moderate", "high", "B", False) == 1.59
    assert fd.wg211_gamma_eref("unfavourable", "low", "D", True) == 2.14
    assert fd.wg211_gamma_n("A", 100) == pytest.approx(1.00, abs=0.002)
    assert fd.wg211_gamma_n("B", 12) == pytest.approx(0.0605 * math.log(12) + 0.7207)
    assert fd.wg211_gamma_n("C", 500) == pytest.approx(1.10, abs=0.005)    # Table 9 printed 1.10
    v_fav, _ = fd.WG211_VELOCITY["favourable"]
    assert v_fav == pytest.approx(0.125, abs=0.001)                      # derived, see module


def test_wg211_default_factor_from_tables():
    case = fd.Case(LNG, _berth(4.0), condition="moderate", consequence_class="B",
                   cov_m="high", berthings_per_year=12)
    r = fd.energy_by_method(case, method="wg211", citations="off")
    assert r.factor.value == pytest.approx(1.59 * (0.0605 * math.log(12) + 0.7207))


# ---------------------------------------------------------------- UFC 4-152-01

SHIP = fd.Ship(displacement_t=60_000, lbp_m=180.0, beam_m=32.0, draft_m=12.0, ship_type="bulk", dwt_t=48_000)
BERTH = fd.Berth(water_depth_m=14.0, contact_x_from_bow_m=45.0, berthing_angle_deg=5.0,
                 navigation="b", structure="open", fender_deflection_m=1.5)


def test_ufc_velocity_digitized_points():
    lt = 1.0160469088
    assert fd.ufc_velocity(50_000 * lt, "moderate") == pytest.approx(0.35 * 0.3048)
    assert fd.ufc_velocity(10_000 * lt, "favourable") == pytest.approx(0.31 * 0.3048)


def test_ufc_method_hand_check():
    case = fd.Case(SHIP, BERTH, condition="moderate")
    r = fd.energy_by_method(case, method="ufc", citations="off")
    cb = 60_000 / (1.025 * 180 * 32 * 12)           # block coefficient (k only)
    k = (0.19 * cb + 0.11) * 180
    a = 45.0
    ce = k * k / (a * a + k * k)
    # UFC Eq. 5-2 (p. 106) and p. 109: the Cb in F is the berthing coefficient
    # Ce Cg Cd Cc (Cg 0.95 at the quarter point, Cd 1.0, Cc 1.0 open berth)
    cb_berthing = ce * 0.95 * 1.0 * 1.0
    assert cb_berthing < 0.6 < cb                    # the two readings take different F branches
    f = 1.5 * cb_berthing
    cm0 = 1.3 + 1.5 * 12 / 32
    cm1 = f * (12.4 * (12 / 32) ** 0.3 - 50 * 12 / 180)
    cm = cm0 + (cm1 - cm0) * (12 / 14) ** 3.5
    assert r.ce.value == pytest.approx(ce)
    assert r.geometry["cb_berthing"] == pytest.approx(cb_berthing)
    assert r.cm.value == pytest.approx(cm)
    assert r.extra[0].value == 0.95                 # Cg at the quarter point
    v = r.velocity_m_s.value
    assert r.design_energy_knm == pytest.approx(0.5 * 60_000 * v**2 * ce * cm * 0.95 * 1.5)


def test_ufc_berthing_coefficient_regression_from_the_source_text():
    """Independent case worked by hand from UFC 4-152-01 (2017, Change 1 2024),
    5-2.1.1, Eq. 5-2 (p. 106) and e (p. 109); not derived from the module.

    LBP 180 m, B 32 m, T 12 m, d 14 m, block coefficient 0.40, midships
    contact, open berth, 0.10 m/s.
      M = 0.40 x 1.025 x 180 x 32 x 12 = 28,339.2 t
      Ce = 1 (a = 0), Cg = 1.0 (inside the quarter points), Cd = 1.0, Cc = 1.0
      berthing coefficient Cb = Ce Cg Cd Cc = 1.0 >= 0.6, so F = 0.9
      C_M0 = 1.3 + 1.5 (12/32) = 1.8625
      C_m1 = 0.9 [12.4 (0.375)^0.3 - 50 (12/180)] = 0.9 (9.23924 - 3.33333) = 5.31532
      (T/d)^3.5 = (12/14)^3.5 = 0.583009
      Cm = 1.8625 + (5.31532 - 1.8625) 0.583009 = 3.87552
      E = 0.5 x 28,339.2 x 0.10^2 x 3.87552 = 549.15 kNm; x 1.5 = 823.7 kNm
    With the block coefficient in F (F = 0.6) the result would be 604.2 kNm.
    """
    ship = fd.Ship(displacement_t=0.40 * 1.025 * 180 * 32 * 12, lbp_m=180.0, beam_m=32.0, draft_m=12.0,
                   ship_type="bulk", dwt_t=20_000)
    berth = fd.Berth(water_depth_m=14.0, contact_x_from_bow_m=90.0, berthing_angle_deg=0.0,
                     navigation="b", structure="open", fender_deflection_m=1.5)
    r = fd.energy_by_method(fd.Case(ship, berth, condition="moderate"), method="ufc", velocity_m_s=0.10,
                            citations="off")
    assert r.ce.value == pytest.approx(1.0)
    assert r.geometry["cb_berthing"] == pytest.approx(1.0)
    assert r.cm.value == pytest.approx(3.87552, abs=5e-5)
    assert r.normal_energy_knm == pytest.approx(549.15, abs=0.01)
    assert r.design_energy_knm == pytest.approx(823.7, abs=0.05)
    # the F branch below 0.6 (F = 1.5 x 0.4 = 0.6) gives the superseded 604.2 kNm:
    # C_m1 = 0.6 x 5.90591 = 3.54355, Cm = 1.8625 + 1.68105 x 0.583009 = 2.84256
    cm_low = fd.ufc_added_mass(ship, 14.0, 0.4)
    assert cm_low == pytest.approx(2.84256, abs=5e-5)
    assert 0.5 * ship.displacement_t * 0.10**2 * cm_low * 1.5 == pytest.approx(604.2, abs=0.05)


# UFC 4-152-01 5-2.1.1 e (p. 109) prints "Cb = berthing coefficient as defined
# previously" under F, while the 0.6 threshold of F = 1.5 Cb reads as a
# hull-form (block coefficient) limit. Both readings are an explicit option;
# the default follows the printed wording.
def _ufc_source_case(**kw):
    ship = fd.Ship(displacement_t=0.40 * 1.025 * 180 * 32 * 12, lbp_m=180.0, beam_m=32.0, draft_m=12.0,
                   ship_type="bulk", dwt_t=20_000)
    berth = fd.Berth(water_depth_m=14.0, contact_x_from_bow_m=90.0, berthing_angle_deg=0.0,
                     navigation="b", structure="open", fender_deflection_m=1.5)
    return fd.Case(ship, berth, condition="moderate", **kw)


def test_ufc_f_basis_default_is_the_printed_berthing_coefficient():
    assert fd.Case(SHIP, BERTH).ufc_f_basis == "berthing_coefficient"
    assert fd.UFC_F_BASES == ("berthing_coefficient", "block_coefficient")
    r = fd.energy_by_method(_ufc_source_case(), method="ufc", velocity_m_s=0.10, citations="off")
    assert r.geometry["ufc_f_cb"] == pytest.approx(1.0)          # Ce Cg Cd Cc at midships, open berth
    assert r.cm.value == pytest.approx(3.87552, abs=5e-5)        # F 0.9
    assert r.design_energy_knm == pytest.approx(823.7, abs=0.05)
    assert "berthing coefficient" in r.cm.note and "block coefficient" in r.cm.note


def test_ufc_f_basis_block_coefficient_gives_the_hand_value():
    """Same hand case read with the block coefficient in F: Cb 0.40 < 0.6, so
    F = 1.5 x 0.40 = 0.6; C_m1 = 0.6 x 5.90591 = 3.54355;
    Cm = 1.8625 + (3.54355 - 1.8625) x 0.583009 = 2.84256;
    E = 0.5 x 28,339.2 x 0.10^2 x 2.84256 x 1.5 = 604.2 kNm (Ce Cg Cd Cc = 1.0)."""
    r = fd.energy_by_method(_ufc_source_case(ufc_f_basis="block_coefficient"), method="ufc",
                            velocity_m_s=0.10, citations="off")
    assert "block coefficient Cb = 0.400" in r.cm.note
    assert r.geometry["ufc_f_cb"] == pytest.approx(0.40)
    assert r.geometry["cb_berthing"] == pytest.approx(1.0)      # the energy still uses Ce Cg Cd Cc
    assert r.cm.value == pytest.approx(2.84256, abs=5e-5)
    assert r.design_energy_knm == pytest.approx(604.2, abs=0.05)
    assert "F 0.600 from block coefficient" in r.cm.note


def test_ufc_f_basis_block_coefficient_leaves_cm_independent_of_cc():
    case = fd.Case(SHIP, fd.Berth(**{**BERTH.__dict__, "structure": "closed"}), ufc_f_basis="block_coefficient")
    base = fd.energy_by_method(case, method="ufc", citations="off")
    r = fd.energy_by_method(case, method="ufc", cc=1.0, citations="off")
    assert base.cc.value == 0.8
    assert r.cm.value == pytest.approx(base.cm.value, rel=1e-12)
    assert r.design_energy_knm == pytest.approx(base.design_energy_knm / 0.8, rel=1e-12)


def test_ufc_f_basis_refuses_an_unknown_reading():
    with pytest.raises(ValueError, match="ufc_f_basis must be one of"):
        fd.Case(SHIP, BERTH, ufc_f_basis="block")


# ---------------------------------------------------------------- ROM 2.0-11


def test_rom_velocity_and_accidental_factor():
    assert fd.rom_velocity(40_000, "moderate") == pytest.approx(0.15)
    assert fd.rom_velocity(500_000, "favourable") == pytest.approx(0.08)
    assert fd.rom_velocity(1_000, "unfavourable") == pytest.approx(0.60)
    assert fd.rom_accidental_factor(60_000, True, 0.15, 100) == pytest.approx(1.5)
    assert fd.rom_accidental_factor(60_000, True, 0.08, 100) == pytest.approx(1.8)
    assert fd.rom_accidental_factor(100_000, False, 0.15, 400) == pytest.approx(1.25 * 1.25 * 1.15)


def test_rom_method_hand_check():
    case = fd.Case(SHIP, BERTH, condition="moderate")
    r = fd.energy_by_method(case, method="rom", citations="off")
    v = fd.rom_velocity(60_000, "moderate")
    assert r.velocity_m_s.value == pytest.approx(v)
    cm = 1.875 - 0.75 * 2 / 12
    e = 0.5 * 60_000 * v**2 * r.ce.value * cm * 1.0 * 1.0 * 0.95
    assert r.normal_energy_knm == pytest.approx(e)
    assert r.factor.value == pytest.approx(1.5)


# ---------------------------------------------------------------- EAU, IS 4651


def test_eau_velocity():
    assert fd.eau_velocity(50_000, "moderate") == 0.15
    assert fd.eau_velocity(200_000, "unfavourable") == 0.20
    assert fd.eau_velocity(5_000, "favourable") == pytest.approx(0.17)
    with pytest.raises(ValueError):
        fd.eau_velocity(3_000, "moderate")


@pytest.mark.parametrize("l_over_r,angle,printed", [(1.25, 0.0, 0.39), (1.0, 10.0, 0.51), (1.0, 20.0, 0.56)])
def test_is4651_table3_eccentricity(l_over_r, angle, printed):
    lbp = 200.0
    r_g = lbp / 4
    x = lbp / 2 - l_over_r * r_g
    ship = fd.Ship(30_000, lbp, 30.0, 10.0, dwt_t=24_000)
    case = fd.Case(ship, fd.Berth(12.0, x, angle), condition="favourable")
    r = fd.energy_by_method(case, method="is4651", citations="off")
    assert r.ce.value == pytest.approx(printed, abs=0.006)  # printed to 2 decimals
    assert r.velocity_m_s.value == 0.10              # sheltered favourable, <= 100,000 t


def test_all_methods_evaluate_for_generic_case():
    case = fd.Case(SHIP, BERTH, condition="moderate")
    results = fd.compare_methods(case, citations="off")
    assert {r.method for r in results} == set(fd.METHODS)
    for r in results:
        assert all(c.source in fd.SOURCES or c.source == "caller" for c in r.coefficients()), r.method


# ---- first-contact point within the LBP, the same check for every method ----


def _contact_berth(x_from_bow_m):
    # the first contact is on the end nearer the berth: bow nearer (+5 deg) forward, stern nearer (-5 deg) aft
    return fd.Berth(water_depth_m=14.0, contact_x_from_bow_m=x_from_bow_m,
                    berthing_angle_deg=5.0 if x_from_bow_m <= 90.0 else -5.0,
                    navigation="b", structure="open", fender_deflection_m=1.56)


@pytest.mark.parametrize("method", list(fd.METHODS))
@pytest.mark.parametrize("x", [1_800.0, 180.5])
def test_contact_beyond_the_lbp_is_refused_by_every_method(method, x):
    # LBP 180 m. Before the check UFC gave 0.265 kNm and IS 4651 15.655 kNm at
    # 1,800 m from the bow, while the PIANC 2002 family refused the geometry.
    with pytest.raises(ValueError) as exc:
        fd.energy_by_method(fd.Case(SHIP, _contact_berth(x)), method=method, citations="off")
    assert str(exc.value) == (f"first contact {x:g} m from the bow is not on the ship "
                              f"(0 to LBP {SHIP.lbp_m:g} m)")


def test_contact_beyond_the_lbp_gives_one_error_for_all_methods():
    case = fd.Case(SHIP, _contact_berth(1_800.0))
    skipped = fd.compare_methods_skipped(case, citations="off")
    assert set(skipped) == set(fd.METHODS)
    assert set(skipped.values()) == {"first contact 1800 m from the bow is not on the ship (0 to LBP 180 m)"}
    assert fd.compare_methods(case, citations="off") == []
    with pytest.raises(ValueError, match=r"^first contact 1800 m from the bow is not on the ship"):
        fd.eccentricity(SHIP, _contact_berth(1_800.0))


@pytest.mark.parametrize("method", list(fd.METHODS))
@pytest.mark.parametrize("x", [0.0, 180.0])
def test_contact_at_the_ends_of_the_lbp_is_accepted_by_every_method(method, x):
    r = fd.energy_by_method(fd.Case(SHIP, _contact_berth(x)), method=method, citations="off")
    assert r.design_energy_knm > 0.0
