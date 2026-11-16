"""Coefficient bases, figure overlaps, plausibility limits and threshold crossings.

Findings of the client-engineering gate at 2b6bb8a0 (two Claude reviews and one
Codex review), each with a regression:

* Cc and Cs carry the PIANC WG33 (2002) values by default (4.2.7, p. 22;
  4.2.6, p. 21); the vendor tables are options with the vendor source;
* UFC 4-152-01 Figs. 5-3 and 5-4 overlap from 5,000 to 20,000 LT: the larger
  of the two is used there;
* EAU with a velocity override needs no DWT (as the workbook);
* the value at utilisation 1 is solved on the method (closed form for the
  keel-clearance Cm), not interpolated across the Cm break at KC/D 0.1;
* WG211 gamma_n is not extrapolated below one berthing a year; CoV_M defaults
  to the largest factor and the R24B example is reproduced from the tables;
* plausibility of the block coefficient and the berthing angle;
* standard_sweeps refuses fewer than two points;
* neighbouring sizes take the tolerance of their own fender type.
"""

from __future__ import annotations

import dataclasses
import math
import sys
from pathlib import Path

import pytest

from digitalmodel.hydrodynamics import fender_design as fd
from digitalmodel.hydrodynamics.berthing_energy import FenderRating

ROOT = Path(__file__).resolve().parents[3]
EXAMPLE = ROOT / "examples" / "hydrodynamics" / "fender_design"

SHIP = fd.Ship(displacement_t=60_000, lbp_m=180.0, beam_m=32.0, draft_m=12.0, ship_type="bulk", dwt_t=48_000)
BERTH = fd.Berth(water_depth_m=14.0, contact_x_from_bow_m=45.0, berthing_angle_deg=5.0, navigation="b",
                 structure="open", fender_deflection_m=1.56)
CASE = fd.Case(SHIP, BERTH, condition="moderate", cov_m="high")
FENDER = FenderRating("GEN-PN-4", "generic", "pneumatic", 2600, 4500, 80, 700.0, 1450.0, 210.0, 60.0,
                      "fictional test row")
LT = 1.0160469088
FT = 0.3048


# ------------------------------------------------------------- source register


def test_pianc_2002_recorded_as_read_with_pages():
    cc, cs = fd.SOURCES["pianc2002-cc"], fd.SOURCES["pianc2002-cs"]
    assert cc.verification == cs.verification == "full-text"
    assert "p. 22" in cc.clause and "p. 21" in cs.clause
    for sid in ("pianc2002", "pianc2002-cm", "pianc2002-ce", "pianc2002-cc", "pianc2002-cs"):
        s = fd.SOURCES[sid]
        assert s.read_in.startswith(fd.PIANC2002_PRIMARY), sid
        assert s.accessed_on == "2026-11-16", sid
    # the tabulated Brolsma values were read in a manual: still secondary
    assert fd.SOURCES["pianc2002-velocity"].verification == "secondary"
    # no storage location in the public register
    for s in fd.SOURCES.values():
        text = " ".join(str(v) for v in dataclasses.astuple(s))
        assert "/mnt/" not in text and ":\\" not in text and "OCR" not in text, s.source_id


def test_every_source_has_a_date_read():
    for s in fd.SOURCES.values():
        assert len(s.accessed_on) == 10 and s.accessed_on.startswith("2026-11-1"), s.source_id


def test_keel_clearance_pairing_is_not_cited_to_pianc_4_2_7():
    assert "TFADM p. 27" in fd.SWEEP_EXCLUSIONS["pianc2002"]["cc"]
    assert "4.2.7" not in fd.SWEEP_EXCLUSIONS["pianc2002"]["cc"]
    closed = fd.Berth(**{**BERTH.__dict__, "structure": "closed", "berthing_angle_deg": 3.0})
    r = fd.energy_by_method(fd.Case(SHIP, closed), method="pianc2002", citations="off")
    assert (r.cc.value, r.cc.source) == (1.0, "tfadm-2024")
    # a vendor source is named in the coefficient, never cited as a standard
    assert all(c.code_id != "tfadm-2024" for c in fd.result_citations(r.coefficients()))


def test_eau_factor_is_labelled_as_the_pianc_table():
    r = fd.energy_by_method(CASE, method="eau", citations="off")
    assert r.factor.source == "pianc2002"
    assert "applied to EAU" in r.factor.note and "EAU 2012 text was not read" in r.factor.note


def test_cs_basis_and_cc_basis_are_refused_when_unknown():
    with pytest.raises(ValueError, match="cs_basis"):
        fd.Case(SHIP, BERTH, cs_basis="bs6349")
    with pytest.raises(ValueError, match="cc_basis"):
        fd.Case(SHIP, BERTH, cc_basis="trelleborg")


def test_hard_fender_cs_is_pianc_1_by_default_and_0_9_by_the_manuals_option():
    hard = fd.Berth(**{**BERTH.__dict__, "fender_deflection_m": 0.12})
    r = fd.energy_by_method(fd.Case(SHIP, hard), method="pianc2002", citations="off")
    r9 = fd.energy_by_method(fd.Case(SHIP, hard, cs_basis="tfadm-2024"), method="pianc2002", citations="off")
    assert (r.cs.value, r.cs.source) == (1.0, "pianc2002-cs")
    assert (r9.cs.value, r9.cs.source) == (0.9, "tfadm-2024")
    assert r9.design_energy_knm == pytest.approx(0.9 * r.design_energy_knm, rel=1e-12)


# ------------------------------------------------------- UFC figures 5-3 / 5-4


def test_ufc_velocity_takes_the_larger_figure_in_the_overlap():
    # digitized from the WBDG copy at 5,000 / 10,000 / 20,000 LT (sheltered, moderate, exposed), ft/s
    fig53 = {5_000: (0.361, 0.734, 0.985), 10_000: (0.284, 0.574, 0.828), 20_000: (0.237, 0.453, 0.724)}
    fig54 = {5_000: (0.349, 0.655, 0.985), 10_000: (0.309, 0.551, 0.827), 20_000: (0.247, 0.453, 0.691)}
    for lt, a in fig53.items():
        for j, cond in enumerate(("favourable", "moderate", "unfavourable")):
            v = fd.ufc_velocity(lt * LT, cond) / FT
            assert v == pytest.approx(max(a[j], fig54[lt][j]), abs=0.006), (lt, cond)
    # the moderate class no longer drops at the join of the figures (0.77 -> 0.66 ft/s before)
    assert fd.ufc_velocity(5_000 * LT, "moderate") / FT == pytest.approx(0.73)
    assert fd.ufc_velocity(4_000 * LT, "moderate") > fd.ufc_velocity(5_000 * LT, "moderate")
    # velocity falls with size in every class
    for j in (1, 2, 3):
        col = [row[j] for row in fd.UFC_VELOCITY_FT_S]
        assert col == sorted(col, reverse=True)
    r = fd.energy_by_method(CASE, method="ufc", citations="off")
    assert fd.UFC_VELOCITY_RULE in r.velocity_m_s.note


# ----------------------------------------------------------- EAU without DWT


def test_eau_override_needs_no_dwt_and_equals_pianc():
    no_dwt = fd.Case(dataclasses.replace(SHIP, dwt_t=None), BERTH, cov_m="high")
    eau = fd.energy_by_method(no_dwt, method="eau", velocity_m_s=0.08, citations="off")
    pianc = fd.energy_by_method(no_dwt, method="pianc2002", velocity_m_s=0.08, citations="off")
    assert eau.design_energy_knm == pytest.approx(349.899, abs=5e-4)        # workbook value (gate)
    assert eau.design_energy_knm == pytest.approx(pianc.design_energy_knm, rel=1e-12)
    with pytest.raises(ValueError, match="keyed by DWT"):
        fd.energy_by_method(no_dwt, method="eau", citations="off")
    assert "eau" in fd.compare_methods_skipped(no_dwt, citations="off")
    assert "eau" not in fd.compare_methods_skipped(no_dwt, velocity_m_s=0.08, citations="off")


# -------------------------------------------------- value at utilisation 1


def _sweeps(method="pianc2002", case=CASE, **kw):
    ranges = {
        "velocity_m_s": fd.SweepRange(0.0537, 0.172, "test"),
        "displacement_t": fd.SweepRange(45_000, 66_000, "test"),
        "water_depth_m": fd.SweepRange(12.6, 18.0, "test"),
        "contact_x_over_lbp": fd.SweepRange(0.20, 0.35, "test"),
        "berthing_angle_deg": fd.SweepRange(0.0, 10.0, "test"),
        "factor": fd.SweepRange(1.25, 1.75, "test"),
    }
    return {s.parameter: s for s in fd.standard_sweeps(case, method, FENDER, ranges, capacity_knm=700.0,
                                                        citations="off", **kw)}


def test_ukc_depth_for_cm_inverts_the_keel_clearance_rule():
    for cm in (1.51, 1.6, 1.75, 1.79925):
        d = fd.ukc_depth_for_cm(12.0, cm)
        assert fd.added_mass_factor(SHIP, d, "pianc2002") == pytest.approx(cm, rel=1e-12)
    for cm in (1.5, 1.8, 1.9):
        with pytest.raises(ValueError, match="not unique|sloped"):
            fd.ukc_depth_for_cm(12.0, cm)


def test_water_depth_at_utilisation_1_is_solved_not_interpolated_across_the_cm_break():
    # gate P3 at 2b6bb8a0: 13.16 m interpolated between 13.145 and 13.428 m across KC/D 0.1;
    # exact 13.212 m (Cm 1.79925 = 1.75 x 700 / 680.839, KC/D 0.1010)
    sw = _sweeps()["water_depth_m"]
    assert sw.crossing_basis.startswith("closed form")
    assert sw.limit_value() == pytest.approx(13.212, abs=5e-4)
    cm_needed = 1.75 * 700.0 / 680.839
    assert sw.limit_value() == pytest.approx(12.0 * (1.1 + 0.4 * (1.8 - cm_needed) / 0.3), rel=1e-5)
    e = fd.energy_by_method(dataclasses.replace(CASE, berth=fd.Berth(**{**BERTH.__dict__,
                                                                        "water_depth_m": sw.limit_value()})),
                            method="pianc2002", citations="off").design_energy_knm
    assert e == pytest.approx(700.0, rel=1e-9)
    # the old linear reading of the same points stays about 0.05 m low
    plain = dataclasses.replace(sw, crossing=None, crossing_basis="")
    assert plain.limit_value() == pytest.approx(13.16, abs=0.01)
    # the utilisation at 12.6 m is above 1 (1.0004): it must never print as 1.00
    assert sw.at(12.6).utilisation == pytest.approx(1.8 / 1.75 * 680.839 / 700.0, rel=1e-6)
    assert sw.at(12.6).utilisation > 1.0


@pytest.mark.parametrize("method", ["ufc", "rom", "bs6349"])
def test_crossings_of_other_methods_are_roots_of_the_method(method):
    sweeps = _sweeps(method)
    for key, sw in sweeps.items():
        x = sw.limit_value()
        if x is None:
            continue
        assert sw.crossing_basis, key
        # the design energy at the crossing equals the capacity
        lo = min(p.value for p in sw.points)
        hi = max(p.value for p in sw.points)
        assert lo - 1e-9 <= x <= hi + 1e-9
        e = _energy_at(method, key, x)
        assert e == pytest.approx(700.0, rel=1e-8), (method, key, x)


def _energy_at(method, key, x):
    b = BERTH.__dict__
    if key == "velocity_m_s":
        return fd.energy_by_method(CASE, method=method, velocity_m_s=x, citations="off").design_energy_knm
    if key == "displacement_t":
        base = fd.energy_by_method(CASE, method=method, citations="off").design_energy_knm
        return base * x / SHIP.displacement_t
    if key == "factor":
        base = fd.energy_by_method(CASE, method=method, citations="off")
        return base.design_energy_knm * x / base.factor.value
    berth = {"water_depth_m": dict(water_depth_m=x), "contact_x_over_lbp": dict(contact_x_from_bow_m=x * 180.0),
             "berthing_angle_deg": dict(berthing_angle_deg=x)}[key]
    case = dataclasses.replace(CASE, berth=fd.Berth(**{**b, **berth}))
    return fd.energy_by_method(case, method=method, citations="off").design_energy_knm


def test_generic_crossings_match_the_independent_review_values():
    sw = _sweeps()
    assert sw["velocity_m_s"].limit_value() == pytest.approx(0.11315, abs=5e-6)
    assert sw["displacement_t"].limit_value() == pytest.approx(61_689, abs=1)
    assert sw["factor"].limit_value() == pytest.approx(1.7993, abs=5e-5)
    assert sw["contact_x_over_lbp"].limit_value() == pytest.approx(0.25855, abs=5e-5)
    assert sw["berthing_angle_deg"].limit_value() == pytest.approx(7.500, abs=5e-4)


def test_bare_parameter_sweep_still_interpolates():
    sw = fd.parameter_sweep("x", "x", "-", 1.0, [0.5, 1.5], lambda x: 700.0 * x, FENDER,
                            range_low=0.5, range_high=1.5, range_basis="test")
    assert sw.crossing_basis == "" and sw.limit_value() == pytest.approx(1.0)


def test_standard_sweeps_refuses_fewer_than_two_points():
    rng = {"velocity_m_s": fd.SweepRange(0.06, 0.17, "test")}
    for bad in (1, 0, -3, 2.0, True, "25"):
        with pytest.raises(ValueError, match="points must be a whole number of 2 or more"):
            fd.standard_sweeps(CASE, "pianc2002", FENDER, rng, points=bad, citations="off")
    assert len(fd.standard_sweeps(CASE, "pianc2002", FENDER, rng, points=2, citations="off")[0].points) >= 2


# ------------------------------------------------------------------- WG211


def test_wg211_gamma_n_is_not_extrapolated_below_one_call_a_year():
    assert fd.wg211_gamma_n("B", 1.0) == pytest.approx(0.7207)
    for n in (0.9999, 0.22, 1e-6):
        with pytest.raises(ValueError, match="not extrapolated"):
            fd.wg211_gamma_n("B", n)
    rare = dataclasses.replace(CASE, berthings_per_year=0.5)
    assert "wg211" in fd.compare_methods_skipped(rare, citations="off")
    assert fd.energy_by_method(rare, method="rom", citations="off").design_energy_knm > 0.0


def test_cov_m_defaults_to_the_largest_factor():
    assert fd.Case(SHIP, BERTH).cov_m == "low"
    for cond in ("favourable", "moderate", "unfavourable", "monitored"):
        for k in range(8):
            row = {c: fd.WG211_GAMMA_EREF[(cond, c)][k] for c in ("high", "moderate", "low")}
            assert row["low"] == max(row.values()), (cond, k)


# R24B Table 2 vessel (LNG carrier) at the Example 1 jetty
LNG = fd.Ship(displacement_t=178_579, lbp_m=332.0, beam_m=53.8, draft_m=12.0, ship_type="gas", dwt_t=126_000)
LNG_BERTH = fd.Berth(water_depth_m=14.31, contact_x_from_bow_m=0.33 * 332.0, berthing_angle_deg=4.0,
                     navigation="c", structure="open", fender_deflection_m=1.0)


def test_r24b_wg211_factors_come_from_the_tables():
    # printed gamma_E 1.80 (moderate) and 1.55 (monitored, site velocity 0.100 m/s); the tables give
    # gamma_Eref x gamma_n = 1.82 x 0.9993 = 1.819 and 1.57 x 0.9993 = 1.569 (class B, CoV_M low,
    # single contact, 100 calls a year): within 1.5 % of the printed values
    case = fd.Case(LNG, LNG_BERTH, condition="moderate", consequence_class="B", cov_m="low",
                   berthings_per_year=100)
    r = fd.energy_by_method(case, method="wg211", citations="off")
    assert r.factor.source == "pianc-wg211"
    assert r.factor.value == pytest.approx(1.80, rel=0.015)
    assert r.design_energy_knm == pytest.approx(4659, rel=0.015)
    mon = dataclasses.replace(case, condition="monitored")
    r1 = fd.energy_by_method(mon, method="wg211", velocity_m_s=0.100, citations="off")
    assert r1.factor.value == pytest.approx(1.55, rel=0.015)
    assert r1.design_energy_knm == pytest.approx(1783, rel=0.015)


# ----------------------------------------------------------- plausibility


def test_block_coefficient_above_1_is_refused_and_outside_the_range_flagged():
    with pytest.raises(ValueError, match="block coefficient .* above 1"):
        dataclasses.replace(SHIP, displacement_t=600_000)
    small = fd.Case(dataclasses.replace(SHIP, displacement_t=6_000), BERTH)
    r = fd.energy_by_method(small, method="bs6349", citations="off")
    assert any("block coefficient 0.085 outside 0.45 to 0.9" in f for f in r.flags)
    assert not any("block coefficient" in f for f in fd.energy_by_method(CASE, method="bs6349",
                                                                         citations="off").flags)


@pytest.mark.parametrize("angle", [90.0, -90.0, 95.0, 185.0])
def test_berthing_angle_is_below_90_degrees(angle):
    with pytest.raises(ValueError, match="below 90 deg"):
        fd.Berth(**{**BERTH.__dict__, "berthing_angle_deg": angle})


def test_berthing_angle_sign_agrees_with_the_contact_end():
    # gate P3 at 2b6bb8a0: -19.6 deg with the forward quarter-point contact gave 584.0 kNm (-14 %)
    fwd_neg = fd.Case(SHIP, fd.Berth(**{**BERTH.__dict__, "berthing_angle_deg": -19.6}))
    with pytest.raises(ValueError, match="first contact is on the end nearer the berth"):
        fd.energy_by_method(fwd_neg, method="pianc2002", citations="off")
    aft = fd.Case(SHIP, fd.Berth(**{**BERTH.__dict__, "contact_x_from_bow_m": 135.0, "berthing_angle_deg": -5.0}))
    fwd = fd.energy_by_method(CASE, method="pianc2002", citations="off")
    assert fd.energy_by_method(aft, method="pianc2002", citations="off").design_energy_knm == \
        pytest.approx(fwd.design_energy_knm, rel=1e-12)          # mirror image of the generic contact
    mid = fd.Case(SHIP, fd.Berth(**{**BERTH.__dict__, "contact_x_from_bow_m": 90.0, "berthing_angle_deg": -5.0}))
    assert fd.energy_by_method(mid, method="pianc2002", citations="off").design_energy_knm > 0.0


def test_angle_and_contact_sweeps_stay_on_the_berth_side():
    sw = _sweeps()
    assert min(p.value for p in sw["berthing_angle_deg"].points) >= 0.0
    assert max(p.value for p in sw["contact_x_over_lbp"].points) <= 0.5


# ------------------------------------------------- tolerance by fender type


sys.path.insert(0, str(EXAMPLE))


def test_neighbouring_sizes_take_the_tolerance_of_their_type():
    yaml = pytest.importorskip("yaml")  # noqa: F841
    from fender_case import check_type_tolerances, compute, load_case, size_checks, type_tolerances
    res = compute(load_case(EXAMPLE / "generic_case.yaml"), citations="record")
    fm = next(c for c in res.size_checks if c.fender_id == "GEN-FM-6")
    # foam: +/-10 to 15 % (TFADM p. 30; SFT p. 26), upper end assumed: 1,000 x 0.85 = 850 kNm
    assert fm.capacity_knm == pytest.approx(850.0)
    assert fm.utilisation == pytest.approx(680.839 / 850.0, rel=1e-5)
    assert fm.reaction_kn == pytest.approx(1400.0 * 1.15)
    assert fm.hull_pressure_kpa == pytest.approx(180.0 * 1.15)
    assert (fm.meets_reaction, fm.meets_pressure) == (False, False)      # 1,610 kN > 1,600; 207 kPa > 200
    pn = next(c for c in res.size_checks if c.is_base)
    assert pn.capacity_knm == pytest.approx(700.0) and pn.reaction_kn == pytest.approx(1595.0)
    d = res.case["design"]
    assert type_tolerances(d, "foam") == (0.15, 0.15)
    assert type_tolerances(d, "pneumatic") == (0.0, 0.10)
    # a catalogue with another type and no tolerance for it is refused
    bare = {k: v for k, v in d.items() if k != "tolerances_by_type"}
    with pytest.raises(ValueError, match="no tolerances for fender type"):
        check_type_tolerances(res.catalogue, "pneumatic", bare)
    with pytest.raises(ValueError, match="no tolerances for fender type"):
        size_checks(res.catalogue, "GEN-PN-4", 680.0, bare)
    with pytest.raises(ValueError, match="names the base fender's type"):
        check_type_tolerances(res.catalogue, "pneumatic",
                              dict(d, tolerances_by_type={"pneumatic": dict(energy_tolerance=0.0,
                                                                            reaction_tolerance=0.1)}))
    assert math.isfinite(fm.capacity_knm)
