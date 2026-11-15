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
    r = fd.energy_by_method(LNG, _berth(6.0), "bs6349")
    assert r.velocity_m_s.value == pytest.approx(0.100, abs=0.001)
    assert r.cm.value == pytest.approx(1.45, abs=0.005)
    assert r.factor.value == 2.0
    # normal energy with the printed Ce 0.74: 963 kNm (published)
    assert 0.5 * 178_579 * 0.100**2 * 0.74 * 1.45 == pytest.approx(963, rel=0.01)
    # this module's Ce at 6 deg is within 0.02 of the printed 0.74
    assert r.ce.value == pytest.approx(0.74, abs=0.02)
    assert r.design_energy_knm == pytest.approx(1926, rel=0.03)


def test_r24b_pianc_cm_and_wg33_energy():
    r = fd.energy_by_method(LNG, _berth(6.0, "c"), "pianc2002", factor_position="largest")
    assert r.cm.value == pytest.approx(1.73, abs=0.005)          # printed 1.73
    assert r.velocity_m_s.value == pytest.approx(0.115, abs=0.001)
    # column (vii): normal 1,520 kNm, Cab 1.25 -> 1,901 kNm (printed coefficients)
    assert 0.5 * 178_579 * 0.115**2 * 0.74 * 1.73 == pytest.approx(1520, rel=0.01)
    assert r.normal_energy_knm == pytest.approx(1520, rel=0.03)
    # the paper applies 1.25 (largest tanker/bulk/cargo); this module's gas-carrier
    # default is 1.5 (SFT p. 22, no PIANC guidance), so the factor is passed here
    assert r.factor.value == 1.5
    r125 = fd.energy_by_method(LNG, _berth(6.0, "c"), "pianc2002", factor=1.25)
    assert r125.design_energy_knm == pytest.approx(1901, rel=0.03)


def test_r24b_wg211_characteristic_and_design_energy():
    case = fd.Case(LNG, _berth(4.0), condition="moderate")
    # column (ii): moderate default 0.15 m/s, Ek,c 2,590 kNm, gamma_E 1.80 -> 4,659 kNm
    r = fd.energy_by_method(case, method="wg211", factor=1.80)
    assert r.velocity_m_s.value == 0.15
    assert r.cs.value == 1.0 and r.cc.value == 1.0
    assert r.ce.value == pytest.approx(0.75, abs=0.01)           # printed 0.75 at 4 deg
    assert r.normal_energy_knm == pytest.approx(2590, rel=0.01)
    assert r.design_energy_knm == pytest.approx(4659, rel=0.01)
    # column (i): monitored with site velocity 0.100 m/s, gamma_E 1.55 -> 1,151 and 1,783 kNm
    r1 = fd.energy_by_method(case, method="wg211", velocity_m_s=0.100, factor=1.55)
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
    r = fd.energy_by_method(case, method="wg211")
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
    r = fd.energy_by_method(case, method="ufc")
    cb = 60_000 / (1.025 * 180 * 32 * 12)
    k = (0.19 * cb + 0.11) * 180
    a = 45.0
    ce = k * k / (a * a + k * k)
    cm0 = 1.3 + 1.5 * 12 / 32
    cm1 = 0.9 * (12.4 * (12 / 32) ** 0.3 - 50 * 12 / 180)
    cm = cm0 + (cm1 - cm0) * (12 / 14) ** 3.5
    assert r.ce.value == pytest.approx(ce)
    assert r.cm.value == pytest.approx(cm)
    assert r.extra[0].value == 0.95                 # Cg at the quarter point
    v = r.velocity_m_s.value
    assert r.design_energy_knm == pytest.approx(0.5 * 60_000 * v**2 * ce * cm * 0.95 * 1.5)


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
    r = fd.energy_by_method(case, method="rom")
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
    r = fd.energy_by_method(case, method="is4651")
    assert r.ce.value == pytest.approx(printed, abs=0.006)  # printed to 2 decimals
    assert r.velocity_m_s.value == 0.10              # sheltered favourable, <= 100,000 t


def test_all_methods_evaluate_for_generic_case():
    case = fd.Case(SHIP, BERTH, condition="moderate")
    results = fd.compare_methods(case)
    assert {r.method for r in results} == set(fd.METHODS)
    for r in results:
        assert all(c.source in fd.SOURCES or c.source == "caller" for c in r.coefficients()), r.method
