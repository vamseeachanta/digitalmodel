"""Fender design methods: velocity table, coefficients, worked examples, methods.

Published worked examples used as oracles (values as printed, tolerances
cover the printed rounding of intermediates):

* SFT: ShibataFenderTeam Design Manual (English, 2026) p. 19, eccentricity of
  a 100,000 DWT laden tanker at third-point contact, 5 deg.
* TFADM: Trelleborg Fender Application Design Manual MN-FEN-V4.0-EN (2024)
  pp. 39-40, velocity and temperature correction of a cone fender.

Other ship and berth values are generic round numbers.
"""

from __future__ import annotations

import math

import pytest

from digitalmodel.hydrodynamics import fender_design as fd
from digitalmodel.hydrodynamics.berthing_energy import FenderRating

# ---------------------------------------------------------------- velocity


@pytest.mark.parametrize(
    "size,cond,v",
    [(1_000, "a", 0.179), (10_000, "b", 0.192), (50_000, "c", 0.164),
     (100_000, "d", 0.171), (500_000, "e", 0.115), (75_000, "b", 0.094)],
)
def test_brolsma_rows_reproduced(size, cond, v):
    assert fd.brolsma_velocity(size, cond) == pytest.approx(v, abs=1e-12)


def test_brolsma_log_log_interpolation():
    # between 40,000 (0.119) and 50,000 (0.110), condition b, at 48,000
    t = math.log(48_000 / 40_000) / math.log(50_000 / 40_000)
    expect = math.exp(math.log(0.119) + t * (math.log(0.110) - math.log(0.119)))
    assert fd.brolsma_velocity(48_000, "b") == pytest.approx(expect, rel=1e-12)
    assert 0.110 < expect < 0.119


def test_brolsma_does_not_extrapolate():
    with pytest.raises(ValueError, match="outside"):
        fd.brolsma_velocity(600_000, "b")
    with pytest.raises(ValueError):
        fd.brolsma_velocity(50_000, "f")


def test_brolsma_curves_ordered_a_to_e():
    for size in (1_000, 20_000, 100_000, 400_000):
        vs = [fd.brolsma_velocity(size, c) for c in "abcde"]
        assert vs == sorted(vs)


def test_velocity_flags():
    assert fd.velocity_flags(0.12) == []
    assert "caution" in fd.velocity_flags(0.09)[0]
    assert "not recommended" in fd.velocity_flags(0.07)[0]


# ------------------------------------------------- worked example SFT p. 19


def test_sft_p19_third_point_tanker_eccentricity():
    ship = fd.Ship(displacement_t=125_000, lbp_m=236.0, beam_m=43.0, draft_m=15.1, ship_type="tanker")
    berth = fd.Berth(water_depth_m=17.0, contact_x_from_bow_m=236.0 / 3, berthing_angle_deg=5.0)
    g = fd.eccentricity(ship, berth)
    assert g["cb"] == pytest.approx(0.796, abs=0.0006)       # printed 0.796
    # printed 61.7 m from Cb rounded to 0.796; unrounded Cb gives 61.646 m
    assert g["k_m"] == pytest.approx(61.7, abs=0.06)
    assert g["r_m"] == pytest.approx(44.8, abs=0.05)          # printed 44.8 m
    assert g["phi_deg"] == pytest.approx(56.3, abs=0.06)      # printed 56.3 deg
    assert g["ce"] == pytest.approx(0.761, abs=0.002)         # printed 0.761


def test_sft_gamma_formula_matches_contact_geometry():
    # SFT p. 19: gamma = 90 - alpha - asin(B / (2R)) for a contact forward of midships
    ship = fd.Ship(125_000, 236.0, 43.0, 15.1, ship_type="tanker")
    for x, a in ((236 / 3, 5.0), (236 / 4, 0.0), (236 / 4, 10.0)):
        g = fd.eccentricity(ship, fd.Berth(17.0, x, a))
        gamma = 90.0 - a - math.degrees(math.asin(43.0 / (2 * g["r_m"])))
        assert g["phi_deg"] == pytest.approx(gamma, abs=1e-9)


def test_k_squared_numerator_error_overstates_ce():
    # the spreadsheet error (K^2 + K^2 cos^2 phi) against PIANC (K^2 + R^2 cos^2 phi)
    k, r, phi = 50.0, 57.0, 6.0
    wrong = fd.eccentricity_k2_numerator(k, r, phi)
    assert wrong == pytest.approx((2500 + 2500 * math.cos(math.radians(6)) ** 2) / (2500 + 3249))
    right_perpendicular = 2500 / (2500 + 3249)
    assert wrong > 2 * right_perpendicular * 0.9


# -------------------------------------------- worked example TFADM pp. 39-40


def test_tfadm_p39_velocity_and_temperature_correction():
    t = fd.compression_time_s(0.72 * 1000, 0.160)
    assert t == pytest.approx(6.08, abs=0.01)                  # printed 6.1 s
    vf = fd.velocity_factor(t, "sbr")
    assert vf == 1.17                                          # printed 1.17
    assert fd.temperature_factor(10, "sbr") == pytest.approx(1.038)
    assert fd.temperature_factor(40, "sbr") == pytest.approx(0.948)
    # at 20 mm/s the compression time is about 48 s, so VF = 1 (printed)
    t_slow = fd.compression_time_s(720, 0.020)
    assert t_slow == pytest.approx(48.6, abs=0.1)
    vf_energy = fd.velocity_factor(t_slow, "sbr", use="energy")
    assert vf_energy == 1.0                                    # printed "VF=1"
    cone = FenderRating("SCN1000-F2.5-CV", "example", "cone", 1000, None, None, 540.0, 965.0, None, 72.0, "TFADM p. 39")
    p = fd.design_performance(
        cone, energy_tolerance=0.0, reaction_tolerance=0.0,
        velocity_factor_reaction=vf, temperature_factor_reaction=fd.temperature_factor(10, "sbr"),
        velocity_factor_energy=vf_energy, temperature_factor_energy=fd.temperature_factor(40, "sbr"),
    )
    assert p.reaction_kn == pytest.approx(1172, abs=0.5)       # printed 1172 kN (+21.4 %)
    assert p.energy_knm == pytest.approx(512, abs=0.5)         # printed 512 kJ (-5.2 %)
    assert (p.reaction_kn / 965 - 1) * 100 == pytest.approx(21.4, abs=0.05)
    assert (p.energy_knm / 540 - 1) * 100 == pytest.approx(-5.2, abs=0.05)


def test_velocity_factor_beyond_the_table_needs_its_use():
    # 48.65 s (the p. 40 example): the 20 s SBR value 1.08 would overstate energy
    with pytest.raises(ValueError, match="beyond the tabulated 20 s"):
        fd.velocity_factor(48.65, "sbr")
    assert fd.velocity_factor(48.65, "sbr", use="energy") == 1.0
    assert fd.velocity_factor(48.65, "sbr", use="reaction") == 1.08   # 20 s row, upper bound
    assert fd.velocity_factor(48.65, "blend", use="reaction") == 1.03
    # inside the table the use does not change the value
    for t in (1.0, 6.08, 19.9, 20.0):
        assert fd.velocity_factor(t, "sbr", use="energy") == fd.velocity_factor(t, "sbr", use="reaction") \
            == fd.velocity_factor(t, "sbr")
    with pytest.raises(ValueError):
        fd.velocity_factor(5.0, "sbr", use="both")
    with pytest.raises(ValueError):
        fd.velocity_factor(5.0, "epdm")


def test_temperature_factor_interpolates_and_bounds():
    assert fd.temperature_factor(23) == 1.0
    assert fd.temperature_factor(5, "blend") == pytest.approx((1.075 + 1.030) / 2)
    with pytest.raises(ValueError):
        fd.temperature_factor(60)


# ------------------------------------------------------------ coefficients

SHIP = fd.Ship(displacement_t=60_000, lbp_m=180.0, beam_m=32.0, draft_m=12.0,
               ship_type="bulk", dwt_t=48_000)
BERTH = fd.Berth(water_depth_m=14.0, contact_x_from_bow_m=45.0, berthing_angle_deg=0.0,
                 navigation="b", structure="open", fender_deflection_m=1.5)


def test_added_mass_methods():
    assert fd.added_mass_factor(SHIP, 14.0, "pianc2002") == pytest.approx(1.875 - 0.75 * 2 / 12)
    assert fd.added_mass_factor(SHIP, 14.0, "vasco_costa") == pytest.approx(1 + 2 * 12 / 32)
    cb = 60_000 / (1.025 * 180 * 32 * 12)
    assert fd.added_mass_factor(SHIP, 14.0, "ueda") == pytest.approx(1 + math.pi * 12 / (2 * cb * 32))
    assert fd.added_mass_factor(SHIP, 14.0, "longitudinal") == 1.1
    with pytest.raises(ValueError):
        fd.added_mass_factor(SHIP, 14.0, "nope")


@pytest.mark.parametrize(
    "structure,ratio,angle,cm,expect",
    [("open", 0.2, 0, "vasco_costa", 1.0), ("closed", 0.2, 3, "vasco_costa", 0.8),
     ("closed", 0.6, 3, "vasco_costa", 0.9), ("semi_closed", 0.2, 3, "ueda", 0.9),
     ("semi_closed", 0.6, 3, "ueda", 1.0), ("closed", 0.2, 8, "vasco_costa", 1.0),
     ("closed", 0.2, 3, "pianc2002", 1.0)],
)
def test_berth_configuration_factor(structure, ratio, angle, cm, expect):
    assert fd.berth_configuration_factor(structure, ratio, angle, cm) == expect


def test_softness_and_abnormal_and_hull_pressure():
    assert fd.softness_factor(0.20) == 1.0
    assert fd.softness_factor(0.15) == 0.9
    assert fd.abnormal_factor("bulk", "largest") == 1.25
    assert fd.abnormal_factor("bulk", "smallest") == 1.75
    assert fd.abnormal_factor("container", "smallest") == 2.0
    assert fd.hull_pressure_limit("bulk") == [("all sizes", 200.0)]
    with pytest.raises(ValueError):
        fd.abnormal_factor("bulk", "middle")


# ------------------------------------------------------------------ methods


def _hand(v, cm, cc, f):
    cb = 60_000 / (1.025 * 180 * 32 * 12)
    k = (0.19 * cb + 0.11) * 180
    r = math.hypot(90 - 45, 16)
    cos_phi = 16 / r
    ce = (k * k + r * r * cos_phi**2) / (k * k + r * r)
    return 0.5 * 60_000 * v**2 * ce * cm * 1.0 * cc * f, ce


def test_pianc2002_hand_check():
    res = fd.energy_by_method(SHIP, BERTH, "pianc2002", factor_position="largest", citations="off")
    v = fd.brolsma_velocity(48_000, "b")
    e, ce = _hand(v, 1.875 - 0.75 * 2 / 12, 1.0, 1.25)
    assert res.velocity_m_s.value == pytest.approx(v)
    assert res.ce.value == pytest.approx(ce, rel=1e-12)
    assert res.design_energy_knm == pytest.approx(e, rel=1e-12)
    assert res.normal_energy_knm * 1.25 == pytest.approx(res.design_energy_knm)
    assert res.velocity_m_s.source == "pianc2002-velocity"
    assert all(c.source in fd.SOURCES or c.source == "caller" for c in res.coefficients())


def test_bs6349_uses_displacement_and_vasco_costa():
    res = fd.energy_by_method(SHIP, BERTH, "bs6349", citations="off")
    v = fd.brolsma_velocity(60_000, "b")
    e, _ = _hand(v, 1 + 24 / 32, 1.0, 1.75)
    assert res.design_energy_knm == pytest.approx(e, rel=1e-12)


def test_overrides_named_as_caller():
    res = fd.energy_by_method(SHIP, BERTH, "pianc2002", velocity_m_s=0.12, factor=1.5, citations="off")
    assert res.velocity_m_s.source == "caller" and res.factor.source == "caller"
    e, _ = _hand(0.12, 1.75, 1.0, 1.5)
    assert res.design_energy_knm == pytest.approx(e)


def test_grounded_ship_and_missing_dwt_raise():
    with pytest.raises(ValueError, match="keel clearance"):
        fd.energy_by_method(SHIP, fd.Berth(12.0, 45.0), "pianc2002", citations="off")
    no_dwt = fd.Ship(60_000, 180.0, 32.0, 12.0)
    with pytest.raises(ValueError, match="DWT"):
        fd.energy_by_method(no_dwt, BERTH, "pianc2002", citations="off")
    assert "pianc2002" in fd.compare_methods_skipped(no_dwt, BERTH, citations="off")


def test_compare_methods_covers_registered_methods():
    results = fd.compare_methods(SHIP, BERTH, citations="off")
    assert {r.method for r in results} == set(fd.METHODS)
    for r in results:
        assert r.design_energy_knm > 0
        assert r.label


def test_vasco_costa_validity_flag():
    shallow = fd.Berth(12.5, 45.0)
    res = fd.energy_by_method(SHIP, shallow, "bs6349", citations="off")
    assert any("validity" in f for f in res.flags)


def test_standard_sweeps_base_matches_method():
    fender = FenderRating("GEN-PN-5", "generic", "pneumatic", 3000, 5000, 50, 900.0, 1600.0, 150.0, 60.0, "fictional")
    ranges = {
        "velocity_m_s": fd.SweepRange(0.08, 0.20, "test"),
        "water_depth_m": fd.SweepRange(12.6, 18.0, "test"),
        "contact_x_over_lbp": fd.SweepRange(0.2, 0.35, "test"),
        "berthing_angle_deg": fd.SweepRange(0.0, 10.0, "test"),
        "factor": fd.SweepRange(1.25, 1.75, "test"),
    }
    sweeps = fd.standard_sweeps(fd.Case(SHIP, BERTH), "pianc2002", fender, ranges, factor_position="largest")
    base = fd.energy_by_method(SHIP, BERTH, "pianc2002", factor_position="largest", citations="off").design_energy_knm
    for s in sweeps:
        assert s.base_design_energy_knm == pytest.approx(base, rel=1e-9), s.parameter
        assert s.at(s.base_value).design_energy_knm == pytest.approx(base, rel=1e-9)
    depth = next(s for s in sweeps if s.parameter == "water_depth_m")
    # deeper water -> lower Cm -> lower energy
    assert depth.at(depth.range_high).design_energy_knm < depth.at(depth.range_low).design_energy_knm
    vel = next(s for s in sweeps if s.parameter == "velocity_m_s")
    lim = vel.limit_value()
    # utilisation 1 at v where E(v) = 900: E scales with v^2
    assert lim == pytest.approx(vel.base_value * math.sqrt(900 / base), rel=0.01)
