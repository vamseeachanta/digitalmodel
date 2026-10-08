"""W510 (owner decisions 2026-10-07): CR-10 on the significant stress range at the riser joints (test first).

R02: CR-10 is evaluated on the irregular-sea rows (CON-I1) only; the regular-wave rows (CON-R1) are kept as a
screening report on the max - min range and take no part in the verdict.
R03: API RP 16Q (1993) 3.3.2 significant range interpreted as H1/3 of rainflow ranges - owner decision R03,
2026-10-07: the mean of the highest one-third of the rainflow ranges, half cycles weighted 0.5.

Stations: the tension-ring / telescopic-joint section (arc below the first pup) is excluded; the first-pup result
point is a W7 fatigue hot spot, reported and never governing; couplings sit at the section boundaries plus
k x joint length; SAF per station (coupling weld 3.0, joint body 1.245).
"""

from __future__ import annotations

import json
import math

import numpy as np
import pytest

from digitalmodel.drilling_riser.global_model.fatigue_channels import half_cycles
from digitalmodel.drilling_riser.postprocess.checks import KSI_KPA
from digitalmodel.drilling_riser.postprocess.evaluate import STATUSES, evaluate_case, summarise
from digitalmodel.drilling_riser.postprocess.stress_range import SCHEMA as SCHEMA_W501
from digitalmodel.drilling_riser.postprocess.stress_range import (
    SCHEMA_SIG,
    SIGNIFICANT_DEFINITION,
    classify_stations,
    coupling_positions,
    merge_stress_range,
    significant_over_theta,
    significant_range,
    theta_histories,
    turning_points,
)

JOINT = 27.432
COUPLING_MPA = 5.0 * KSI_KPA / 1000.0  # SAF 3.0 -> 15 / 3 = 5 ksi = 34.47 MPa
BODY_MPA = 10.0 * KSI_KPA / 1000.0  # SAF 1.245 <= 1.5 -> 10 ksi = 68.95 MPa
ROW = {"id": "CR-10", "check_key": "dyn_stress_range", "unit": "MPa",
       "limit": {"value_if_saf_le_1_5": 68.948, "saf": {"riser girth weld": 1.245, "riser coupling weld": 3.0}}}
# synthetic line: outer barrel 18.288 m, pups 3.048 + 6.096 + 6.096 m, then 2 slick joints and 3 buoyant joints
SECTIONS = [18.288, 3.048, 6.096, 6.096, 2 * JOINT, 3 * JOINT]
STATIONS = {"sections_m": SECTIONS, "joint_length_m": JOINT, "exclude_below_m": 18.288, "hot_spot_m": 18.796}


# ---------------------------------------------------------------- significant range (R03)


def test_regular_sine_gives_double_amplitude():
    t = np.arange(0.0, 200.0, 0.05)
    a = 37.5
    x = a * np.sin(2.0 * math.pi * t / 10.0)
    assert significant_range(x) == pytest.approx(2.0 * a, rel=1e-3)
    # the peak-to-valley invariant: equal to max - min for a regular wave
    assert significant_range(x) == pytest.approx(x.max() - x.min(), rel=1e-3)


def test_hand_built_sequence_mean_of_the_highest_third_of_rainflow_ranges():
    # reversals 0 8 2 6 1 9 3 5 0: four-point rainflow closes 2-6 (4), 3-5 (2) and 8-1 (7) as full cycles and
    # leaves the residue 0-9-0 as two half cycles of 9. Weight (cycles): 9 x 0.5, 9 x 0.5, 7, 4, 2 -> total 4.
    seq = [0.0, 8.0, 2.0, 6.0, 1.0, 9.0, 3.0, 5.0, 0.0]
    assert sorted(half_cycles(seq), reverse=True) == [9.0, 9.0, 7.0, 7.0, 4.0, 4.0, 2.0, 2.0]
    # highest third = 4/3 cycle: 9 (0.5) + 9 (0.5) + 7 (1/3)
    expect = (9.0 * 0.5 + 9.0 * 0.5 + 7.0 / 3.0) / (4.0 / 3.0)
    assert significant_range(seq) == pytest.approx(expect)
    assert significant_range(seq) <= max(seq) - min(seq)


def test_half_cycles_weigh_half_a_cycle():
    # reversals 0 10 4 6 3: full cycle 4-6 (2), residue 0-10-3 -> half cycles 10 and 7. Total 2 cycles; the highest
    # third (2/3 cycle) is 10 (0.5) + 7 (1/6). Counting the half cycles as whole cycles would give 10.
    seq = [0.0, 10.0, 4.0, 6.0, 3.0]
    assert significant_range(seq) == pytest.approx((10.0 * 0.5 + 7.0 / 6.0) / (2.0 / 3.0))


def test_significant_range_never_exceeds_max_minus_min():
    rng = np.random.default_rng(7)
    for _ in range(25):
        x = np.cumsum(rng.normal(size=rng.integers(5, 400)))
        s = significant_range(x)
        assert 0.0 <= s <= x.max() - x.min() + 1e-12


def test_narrow_band_gaussian_gives_about_four_sigma():
    """Narrow-band Gaussian process: ranges ~ Rayleigh, H1/3 ~ 4.0 sigma. Tolerance fixed at 5 %."""
    rng = np.random.default_rng(1)
    t = np.arange(0.0, 10800.0, 0.1)
    f = np.linspace(0.08, 0.12, 200)
    x = np.zeros_like(t)
    for fi, ph in zip(f, rng.uniform(0.0, 2.0 * math.pi, f.size)):
        x += np.cos(2.0 * math.pi * fi * t + ph)
    assert significant_range(x) / x.std() == pytest.approx(4.0, rel=0.05)


def test_turning_points_keep_the_rainflow_count():
    rng = np.random.default_rng(3)
    x = np.repeat(np.cumsum(rng.normal(size=300)), 3)  # plateaus included
    tp = turning_points(x)
    assert len(tp) < len(x)
    assert sorted(half_cycles(tp)) == pytest.approx(sorted(half_cycles(x)))
    assert significant_range(tp) == pytest.approx(significant_range(x))


def test_theta_reconstruction_is_exact_for_axial_plus_bending():
    rng = np.random.default_rng(5)
    a, b, c = (rng.normal(size=50) for _ in range(3))
    direct = {th: a + b * math.cos(math.radians(th)) + c * math.sin(math.radians(th)) for th in (0.0, 45.0, 90.0,
                                                                                                    180.0, 300.0)}
    rec = theta_histories(direct[0.0], direct[90.0], direct[180.0], thetas=(45.0, 300.0))
    assert rec[45.0] == pytest.approx(direct[45.0])
    assert rec[300.0] == pytest.approx(direct[300.0])


def test_significant_over_theta_takes_the_largest_and_keeps_the_span():
    t = np.arange(0.0, 100.0, 0.05)
    h = {0.0: 10.0 * np.sin(t), 90.0: 30.0 * np.sin(t), 180.0: -10.0 * np.sin(t)}
    sig, th, span = significant_over_theta(h)
    assert th == 90.0 and sig == pytest.approx(60.0, rel=1e-3) and span == pytest.approx(60.0, rel=1e-3)


def test_definition_text_is_marked():
    assert SIGNIFICANT_DEFINITION == ("API RP 16Q (1993) 3.3.2 significant range interpreted as H1/3 of rainflow "
                                      "ranges - owner decision R03, 2026-10-07")


# ---------------------------------------------------------------- stations and SAF


def test_couplings_follow_the_joint_length_from_the_section_boundaries():
    c = coupling_positions(SECTIONS, JOINT)
    b = np.cumsum(SECTIONS)
    expect = [18.288, 21.336, 27.432, 33.528, 33.528 + JOINT, 33.528 + 2 * JOINT,
              b[4] + JOINT, b[4] + 2 * JOINT, b[5]]
    assert c == pytest.approx(expect)
    with pytest.raises(ValueError):
        coupling_positions([18.288, 40.0], JOINT)  # longer than a joint and not a whole number of joints


def test_stations_exclude_the_outer_barrel_and_flag_the_first_pup_hot_spot():
    arcs = [0.508, 9.652, 17.78, 18.796, 19.812, 20.828, 22.352, 30.48, 34.0, 45.0, 60.0, 61.5]
    kinds = classify_stations(arcs, **STATIONS)
    assert kinds[:3] == ["excluded"] * 3
    assert kinds[3] == "hot_spot"
    # 21.336 coupling: nearest result point on each side (20.828, 22.352)
    assert kinds[5] == "coupling" and kinds[6] == "coupling"
    assert kinds[4] == "body"
    # 30.48 / 34.0 straddle the coupling at 33.528; 60.0 / 61.5 the coupling at 33.528 + 27.432 = 60.96
    assert kinds[7] == "coupling" and kinds[8] == "coupling"
    assert kinds[9] == "body" and kinds[10] == "coupling" and kinds[11] == "coupling"


def test_components_below_the_last_joint_are_excluded():
    # adaptor 1.351 m and flex-joint body 1.599 m below the last joint (end of joints at 170.688 m)
    secs = SECTIONS + [1.351, 1.599]
    end = sum(SECTIONS)
    arcs = [18.796, 144.0, 160.0, 170.0, 171.2, 172.9]
    kinds = classify_stations(arcs, sections_m=secs, joint_length_m=JOINT, exclude_below_m=18.288,
                              exclude_above_m=end, hot_spot_m=18.796)
    # 144.0 is the nearest point above the coupling at 143.256 m; 170.0 the nearest below the last joint end
    assert kinds == ["hot_spot", "coupling", "body", "coupling", "excluded", "excluded"]


def test_a_hot_spot_off_the_result_grid_is_refused():
    with pytest.raises(ValueError):
        classify_stations([17.78, 25.0, 40.0], **STATIONS)  # nearest kept point 25.0 m is 6 m from 18.796 m
    r = evaluate_case({None: _doc([17.78, 25.0, 40.0], sig=[1.0, 1.0, 1.0])}, ROW, IRR)
    assert r.status == "NOT_EVALUATED" and "hot spot" in r.reason


# ---------------------------------------------------------------- CR-10 (dyn_stress_range)


def _doc(arcs, sig=None, mx=None):
    w = {"schema": "riser-w5-channels/1", "analysis": "dynamics", "time": {"start_s": 0.0, "end_s": 100.0},
         "points": {}, "range_graphs": {"Riser": {"arc1_m": [0.0, 10.0], "vm_max": [1.0, 2.0]}}}
    doc = {"channels": {"w5": w}}
    line = {"arc_m": list(arcs), "zz_range_max": list(mx if mx is not None else sig),
            "theta_deg": [0.0] * len(arcs)}
    if sig is not None:
        line["zz_range_sig"] = list(sig)
        line["theta_sig_deg"] = [0.0] * len(arcs)
    supp = ({"schema": SCHEMA_SIG, "theta_check_ok": True} if sig is not None else {"schema": SCHEMA_W501})
    merge_stress_range(doc, {**supp, "lines": {"Riser": line}})
    return doc


ARCS = [17.78, 18.796, 19.812, 20.828, 45.0]  # excluded, hot spot, body, coupling, coupling (nearest above 21.336)
IRR = {"stress_line": "Riser", "wave_kind": "irregular", "cr10_stations": STATIONS}
REG = {"stress_line": "Riser", "wave_kind": "regular", "cr10_stations": STATIONS}


def test_saf_applies_per_station_and_the_largest_utilisation_governs():
    # body 50 MPa (U = 0.725), coupling 30 MPa (U = 0.870): the coupling governs at the lower range
    sig = [500e3, 400e3, 50e3, 30e3, 10e3]
    r = evaluate_case({None: _doc(ARCS, sig=sig)}, ROW, IRR)
    assert r.status == "PASS"
    assert r.u == pytest.approx(30.0 / COUPLING_MPA)
    assert r.demand == pytest.approx(30.0) and r.allowable == pytest.approx(COUPLING_MPA)
    assert "20.8" in r.location and "coupling" in r.location
    assert r.detail["by_kind"]["body"]["u"] == pytest.approx(50.0 / BODY_MPA)
    assert r.detail["basis"] == "significant" and r.detail["definition"] == SIGNIFICANT_DEFINITION


def test_hot_spot_is_reported_and_never_governs():
    sig = [900e3, 400e3, 5e3, 5e3, 5e3]
    r = evaluate_case({None: _doc(ARCS, sig=sig)}, ROW, IRR)
    assert r.status == "PASS" and r.u < 0.2
    hs = r.detail["hot_spot"]
    assert hs["arc_m"] == pytest.approx(18.796) and hs["demand"] == pytest.approx(400.0)
    assert hs["u_coupling"] == pytest.approx(400.0 / COUPLING_MPA)
    assert "18.8" not in r.location and "17.8" not in r.location


def test_irregular_rows_use_the_significant_channel_and_fail_on_it():
    sig = [1.0, 1.0, 80e3, 10e3, 10e3]
    mx = [1.0, 1.0, 150e3, 10e3, 10e3]
    r = evaluate_case({None: _doc(ARCS, sig=sig, mx=mx)}, ROW, IRR)
    assert r.status == "FAIL" and r.u == pytest.approx(80.0 / BODY_MPA)


def test_irregular_row_without_the_significant_channel_is_missing():
    doc = _doc(ARCS, mx=[1.0] * 5)
    doc["channels"]["w5"]["range_graphs"]["Riser"].pop("zz_range_sig", None)
    r = evaluate_case({None: doc}, ROW, IRR)
    assert r.status == "NOT_EVALUATED" and r.missing_channel == "range_graphs.Riser.zz_range_sig"


def test_regular_rows_are_screening_and_excluded_from_the_verdict():
    assert "SCREENING" in STATUSES
    mx = [1.0, 1.0, 300e3, 10e3, 10e3]  # 300 MPa max - min on a body: would FAIL
    r_reg = evaluate_case({None: _doc(ARCS, mx=mx)}, ROW, REG)
    assert r_reg.status == "SCREENING" and r_reg.u == pytest.approx(300.0 / BODY_MPA)
    assert r_reg.detail["basis"] == "screening (max - min)"
    sig = [1.0, 1.0, 20e3, 10e3, 10e3]
    seeds = {s: _doc(ARCS, sig=[v * (1 + 0.01 * s) for v in sig]) for s in range(1, 11)}
    r_irr = evaluate_case(seeds, ROW, IRR, seeds_expected=10)
    assert r_irr.status == "PASS"
    s = summarise([("CON-R1-00001", r_reg), ("CON-I1-00001", r_irr)])["CR-10"]
    assert s["status"] == "PASS" and s["counts"]["SCREENING"] == 1
    assert s["governing"]["case_id"] == "CON-I1-00001"


def test_legacy_call_without_wave_kind_is_unchanged():
    # no wave_kind: whole-line max - min against the worst SAF (the W501 path kept for comparison)
    r = evaluate_case({None: _doc(ARCS, mx=[1.0, 20e3, 1.0, 1.0, 1.0])}, ROW, {"stress_line": "Riser"})
    assert r.u == pytest.approx(20.0 / COUPLING_MPA) and "18.8" in r.location


def test_legacy_seeds_still_combine_by_gumbel():
    seeds = {s: _doc(ARCS, mx=[1.0, 20e3 * (1 + 0.01 * s), 1.0, 1.0, 1.0]) for s in range(1, 11)}
    r = evaluate_case(seeds, ROW, {"stress_line": "Riser"}, seeds_expected=10)
    assert r.reason.startswith("Gumbel MPM over 10 seeds")


# ---------------------------------------------------------------- W03: seed mean of the significant range


def test_significant_range_combines_seeds_by_the_mean_at_each_station():
    # station 20.828 (coupling) and 19.812 (body): seed s scales the coupling by (1 + 0.02 s)
    seeds = {s: _doc(ARCS, sig=[1.0, 1.0, 40e3, 20e3 * (1 + 0.02 * s), 10e3]) for s in range(1, 11)}
    r = evaluate_case(seeds, ROW, IRR, seeds_expected=10)
    mean = 20.0 * (1 + 0.02 * 5.5)  # MPa, mean over seeds 1..10
    assert r.demand == pytest.approx(mean) and r.allowable == pytest.approx(COUPLING_MPA)
    assert r.u == pytest.approx(mean / COUPLING_MPA)
    assert r.reason.startswith("seed mean over 10 seeds") and "Gumbel" not in r.reason
    assert r.seed is None and r.stats["n"] == 10 and r.stats["estimator"] == "seed mean"
    assert r.stats["u_max"] == pytest.approx(20.0 * 1.2 / COUPLING_MPA)
    assert r.detail["seed_values"]["10"] == pytest.approx(20.0 * 1.2 / COUPLING_MPA)
    assert r.detail["by_kind"]["body"]["u"] == pytest.approx(40.0 / BODY_MPA)


def test_seed_mean_is_taken_per_station_before_the_maximum():
    # seeds alternate the larger value between two couplings: mean per station (25) < mean of seed maxima (30)
    def sig(s):
        hi, lo = (30e3, 20e3) if s % 2 else (20e3, 30e3)
        return [1.0, 1.0, 1.0, hi, lo]
    seeds = {s: _doc(ARCS, sig=sig(s)) for s in range(1, 11)}
    r = evaluate_case(seeds, ROW, IRR, seeds_expected=10)
    assert r.demand == pytest.approx(25.0)


def test_seed_mean_fails_on_the_mean_not_on_one_seed():
    # nine seeds at U 0.9, one at 1.5: mean 0.96 -> PASS (a Gumbel MPM over seed maxima would exceed 1)
    vals = [0.9] * 9 + [1.5]
    seeds = {s: _doc(ARCS, sig=[1.0, 1.0, 1.0, v * COUPLING_MPA * 1e3, 1.0]) for s, v in enumerate(vals, 1)}
    r = evaluate_case(seeds, ROW, IRR, seeds_expected=10)
    assert r.status == "PASS" and r.u == pytest.approx(0.96)


def test_seed_mean_refuses_different_station_grids():
    seeds = {s: _doc(ARCS, sig=[1.0, 1.0, 1.0, 10e3, 1.0]) for s in range(1, 3)}
    seeds[2] = _doc([17.78, 18.796, 19.812, 20.5, 45.0], sig=[1.0, 1.0, 1.0, 10e3, 1.0])
    r = evaluate_case(seeds, ROW, IRR, seeds_expected=2)
    assert r.status == "NOT_EVALUATED" and "station" in r.reason


def test_seed_mean_applies_without_a_station_map():
    seeds = {s: _doc(ARCS, sig=[1.0, 1.0, 1.0, 10e3 * s, 1.0]) for s in range(1, 5)}
    r = evaluate_case(seeds, ROW, {"stress_line": "Riser", "wave_kind": "irregular"}, seeds_expected=4)
    assert r.demand == pytest.approx(25.0) and r.allowable == pytest.approx(COUPLING_MPA)
    assert r.stats["estimator"] == "seed mean"


def test_seed_mean_reports_the_hot_spot_mean():
    seeds = {s: _doc(ARCS, sig=[1.0, 100e3 + 10e3 * s, 1.0, 1.0, 1.0]) for s in range(1, 5)}
    r = evaluate_case(seeds, ROW, IRR, seeds_expected=4)
    assert r.detail["hot_spot"]["demand"] == pytest.approx(125.0)


# ---------------------------------------------------------------- review r1 (PR #2292) regressions


@pytest.mark.parametrize("bad", [[0.0, 10.0, float("nan"), 0.0], [0.0, float("inf"), 1.0, 0.0]])
def test_non_finite_histories_are_refused(bad):
    with pytest.raises(ValueError):
        turning_points(bad)
    with pytest.raises(ValueError):
        significant_range(bad)


def test_a_station_kind_without_an_allowable_is_not_evaluated():
    row = {**ROW, "limit": {"value_if_saf_le_1_5": 68.948, "saf": {"riser girth weld": 1.245}}}
    r = evaluate_case({None: _doc(ARCS, sig=[1.0, 1.0, 10e3, 60e3, 1.0])}, row, IRR)
    assert r.status == "NOT_EVALUATED" and "riser coupling weld" in r.reason


def test_duplicate_points_on_a_coupling_are_all_coupling_stations():
    # two result points exactly at the 21.336 m coupling (one per side of the section boundary)
    arcs = [18.796, 19.812, 21.336, 21.336, 25.0]
    kinds = classify_stations(arcs, **STATIONS)
    assert kinds[2] == "coupling" and kinds[3] == "coupling"
    r = evaluate_case({None: _doc(arcs, sig=[1.0, 1.0, 10e3, 50e3, 1.0])}, ROW, IRR)
    assert r.status == "FAIL" and r.u == pytest.approx(50.0 / COUPLING_MPA)


def test_a_failed_theta_check_is_refused_at_merge():
    line = {"arc_m": [0.0, 1.0], "zz_range_max": [1.0, 2.0], "zz_range_sig": [0.5, 1.0]}
    doc = {"channels": {"w5": {"range_graphs": {}}}}
    with pytest.raises(ValueError):
        merge_stress_range(doc, {"schema": SCHEMA_SIG, "theta_check_ok": False, "lines": {"Riser": line}})
    assert "Riser" not in doc["channels"]["w5"]["range_graphs"]


def test_a_non_finite_station_value_is_not_evaluated():
    one = evaluate_case({None: _doc(ARCS, sig=[1.0, 1.0, 1.0, float("nan"), 10e3])}, ROW, IRR)
    assert one.status == "NOT_EVALUATED" and "finite" in one.reason
    seeds = {s: _doc(ARCS, sig=[1.0, 1.0, 1.0, float("nan") if s == 2 else 10e3, 10e3]) for s in range(1, 4)}
    r = evaluate_case(seeds, ROW, IRR, seeds_expected=3)
    assert r.status == "NOT_EVALUATED" and "finite" in r.reason


def test_a_seeded_case_cannot_be_screened_as_a_regular_wave():
    seeds = {s: _doc(ARCS, mx=[1.0, 1.0, 300e3, 1.0, 1.0]) for s in range(1, 4)}
    r = evaluate_case(seeds, ROW, REG, seeds_expected=3)
    assert r.status == "NOT_EVALUATED" and "regular" in r.reason


def test_seed_mean_detail_has_no_stale_single_seed_values():
    seeds = {s: _doc(ARCS, sig=[1.0, 1.0, 1.0, 10e3 * s, 1.0]) for s in range(1, 5)}
    r = evaluate_case(seeds, ROW, {"stress_line": "Riser", "wave_kind": "irregular"}, seeds_expected=4)
    assert r.detail["by_detail"]["riser coupling weld"] == pytest.approx(25.0 / COUPLING_MPA)
    assert r.detail["line"] == "Riser"


def test_pup_side_coupling_must_border_the_excluded_section():
    # 21.336 m is a coupling but both its sides are kept: not a pup-side station
    with pytest.raises(ValueError):
        classify_stations([18.796, 20.828, 22.352], **{**STATIONS, "pup_side_couplings_m": [21.336]})


class _FakeOfx:
    """Minimal OrcFxAPI stand-in: ZZ stress at (arc, theta) = a + b cos(theta) + c sin(theta) over time."""

    rpOuter = "outer"

    def __init__(self, arcs, t, bad_45=False):
        self.arcs, self.t, self.bad_45 = arcs, t, bad_45

    def Period(self, n):
        return ("period", n)

    def oeLine(self, **kw):
        return kw

    def TimeHistorySpecification(self, line, var, oe):
        return oe

    def hist(self, arc, theta):
        a = 100.0 + arc
        b = 50.0 * np.sin(0.7 * self.t)
        c = (20.0 + arc) * np.sin(0.31 * self.t + 0.4)
        h = a + b * math.cos(math.radians(theta)) + c * math.sin(math.radians(theta))
        return h + (5.0 if self.bad_45 and theta == 45.0 else 0.0)

    def GetMultipleTimeHistories(self, specs, period):
        return np.column_stack([self.hist(s["ArcLength"], s["Theta"]) for s in specs])  # samples x specs


class _FakeLine:
    def __init__(self, arcs):
        self.arcs = arcs

    def RangeGraph(self, var, period, oe):
        return type("RG", (), {"X": self.arcs})()


def test_extract_significant_against_a_fake_model():
    from digitalmodel.drilling_riser.postprocess.stress_range import extract_significant

    arcs = [0.0, 5.0]
    t = np.arange(0.0, 600.0, 0.1)
    ofx = _FakeOfx(arcs, t)
    doc = extract_significant({"Riser": _FakeLine(arcs)}, ofx, check_every=1)
    assert doc["schema"] == SCHEMA_SIG and doc["theta_check_ok"] is True
    line = doc["lines"]["Riser"]
    for i, arc in enumerate(arcs):
        hs = {th: ofx.hist(arc, th) for th in range(0, 360, 15)}
        expect = max(significant_range(h) for h in hs.values())
        assert line["zz_range_sig"][i] == pytest.approx(expect)
        assert line["zz_range_max"][i] == pytest.approx(max(h.max() - h.min() for h in hs.values()))
    bad = extract_significant({"Riser": _FakeLine(arcs)}, _FakeOfx(arcs, t, bad_45=True), check_every=1)
    assert bad["theta_check_ok"] is False
    unchecked = extract_significant({"Riser": _FakeLine(arcs)}, ofx, check_every=0)
    assert unchecked["theta_check_ok"] is False  # nothing checked is not a pass


# ---------------------------------------------------------------- review r2 (PR #2292) regressions


@pytest.mark.parametrize("pup", [False, True])
def test_duplicates_on_the_exclusion_boundaries_keep_the_excluded_side_out(pup):
    # one result point per side at 18.288 m (barrel | pup) and at the last-joint end (joint | adaptor)
    end = sum(SECTIONS)
    secs = SECTIONS + [1.351, 1.599]
    arcs = [17.78, 18.288, 18.288, 18.796, 19.812, 21.336, 30.0, end, end, end + 1.0]
    st = {"sections_m": secs, "joint_length_m": JOINT, "exclude_below_m": 18.288, "exclude_above_m": end,
          "hot_spot_m": 18.796, **({"pup_side_couplings_m": [18.288]} if pup else {})}
    kinds = classify_stations(arcs, **st)
    assert kinds[0] == "excluded" and kinds[1] == "excluded"  # barrel side
    assert kinds[2] == "coupling"  # pup side of the 18.288 m boundary
    assert kinds[7] == "coupling" and kinds[8] == "excluded" and kinds[9] == "excluded"  # joint | adaptor
    sig = [1.0, 300e3, 10e3, 5e3, 5e3, 5e3, 5e3, 5e3, 300e3, 300e3]
    r = evaluate_case({None: _doc(arcs, sig=sig)}, ROW, {**IRR, "cr10_stations": st})
    assert r.u == pytest.approx(10.0 / COUPLING_MPA)


@pytest.mark.parametrize("flag", ["absent", None])
def test_a_significant_document_without_a_passed_theta_check_is_refused(flag):
    line = {"arc_m": [0.0, 1.0], "zz_range_max": [1.0, 2.0], "zz_range_sig": [0.5, 1.0]}
    supp = {"schema": SCHEMA_SIG, "lines": {"Riser": line}}
    if flag != "absent":
        supp["theta_check_ok"] = flag
    with pytest.raises(ValueError):
        merge_stress_range({"channels": {"w5": {"range_graphs": {}}}}, supp)


def test_a_non_finite_direct_45_channel_fails_extraction():
    from digitalmodel.drilling_riser.postprocess.stress_range import extract_significant

    class NanOfx(_FakeOfx):
        def hist(self, arc, theta):
            h = super().hist(arc, theta)
            return np.full_like(h, np.nan) if theta == 45.0 else h

    arcs = [0.0, 5.0]
    with pytest.raises(ValueError, match="arc"):
        extract_significant({"Riser": _FakeLine(arcs)}, NanOfx(arcs, np.arange(0.0, 60.0, 0.1)), check_every=1)


def test_an_empty_theta_selection_is_refused():
    from digitalmodel.drilling_riser.postprocess.stress_range import extract_significant

    arcs = [0.0]
    with pytest.raises(ValueError):
        extract_significant({"Riser": _FakeLine(arcs)}, _FakeOfx(arcs, np.arange(0.0, 60.0, 0.1)), thetas=[])
    with pytest.raises(ValueError):
        significant_over_theta({})


def test_an_empty_significant_channel_is_not_evaluated():
    ctx = {"stress_line": "Riser", "wave_kind": "irregular"}
    one = evaluate_case({None: _doc([], sig=[])}, ROW, ctx)
    assert one.status in ("NOT_EVALUATED",)
    seeds = {s: _doc([], sig=[]) for s in range(1, 3)}
    assert evaluate_case(seeds, ROW, ctx, seeds_expected=2).status == "NOT_EVALUATED"


def test_a_coupling_far_from_every_result_point_is_refused():
    # coupling at 33.528 m; nearest points 25.0 and 45.0 m are more than the coupling tolerance away
    with pytest.raises(ValueError):
        classify_stations([18.796, 21.336, 25.0, 45.0, 60.96], **{**STATIONS, "coupling_tol_m": 2.0})


def test_a_point_a_rounding_error_off_a_coupling_is_on_it():
    # every coupling up to 33.528 m has a point on it (21.336 one 2e-5 m off), so its neighbours stay body
    kinds = classify_stations([18.796, 20.828, 21.33602, 22.352, 27.432, 30.0, 33.528], **STATIONS)
    assert kinds[2] == "coupling" and kinds[1] == "body" and kinds[3] == "body" and kinds[5] == "body"


def test_seed_mean_refuses_different_allowables_or_hot_spots():
    a = _doc(ARCS, sig=[1.0, 1.0, 1.0, 10e3, 1.0])
    other_row_ctx = {**IRR, "cr10_stations": {**STATIONS, "hot_spot_m": 19.812}}
    from digitalmodel.drilling_riser.postprocess.checks import cr10_station_mean, evaluate_doc
    from digitalmodel.drilling_riser.postprocess.channels import w5

    v1 = evaluate_doc(w5(a), ROW, IRR)
    v2 = evaluate_doc(w5(a), ROW, other_row_ctx)
    from digitalmodel.drilling_riser.postprocess.checks import NotEvaluated

    with pytest.raises(NotEvaluated):
        cr10_station_mean({1: v1, 2: v2})


# ---------------------------------------------------------------- review r3 (PR #2292) regressions


def test_a_significant_document_without_the_significant_channel_is_refused_before_any_change():
    doc = _doc(ARCS, sig=[1.0, 1.0, 1.0, 10e3, 1.0])
    before = json.dumps(doc, sort_keys=True)
    supp = {"schema": SCHEMA_SIG, "theta_check_ok": True,
            "lines": {"Riser": {"arc_m": [0.0, 1.0], "zz_range_max": [1.0, 2.0]}}}
    with pytest.raises(ValueError):
        merge_stress_range(doc, supp)
    assert json.dumps(doc, sort_keys=True) == before


def test_each_kept_side_of_a_coupling_needs_a_point_within_the_tolerance():
    # coupling at 21.336 m: 20.828 m within 3 m below, nearest above 25.0 m is 3.66 m away
    with pytest.raises(ValueError):
        classify_stations([18.796, 20.828, 25.0, 27.432, 33.528], **{**STATIONS, "coupling_tol_m": 3.0})


@pytest.mark.parametrize("arcs", [[18.796, 19.812, float("nan"), 21.336, 45.0],
                                  [18.796, 19.812, 21.336, 20.0, 45.0]])
def test_a_malformed_arc_axis_is_not_evaluated(arcs):
    with pytest.raises(ValueError):
        classify_stations(arcs, **STATIONS)
    for ctx in (IRR, {"stress_line": "Riser", "wave_kind": "irregular"}):
        r = evaluate_case({None: _doc(arcs, sig=[1.0, 1.0, 1.0, 1.0, 1.0])}, ROW, ctx)
        assert r.status == "NOT_EVALUATED", ctx


def test_a_coupling_on_the_exclusion_boundary_needs_its_kept_side_within_the_tolerance():
    end = sum(SECTIONS)
    secs = SECTIONS + [1.351, 1.599]
    st = {"sections_m": secs, "joint_length_m": JOINT, "exclude_below_m": 18.288, "exclude_above_m": end,
          "hot_spot_m": 18.796, "coupling_tol_m": 3.0}
    arcs = [18.796, 21.336, 27.432, 33.528, 60.96, 88.392, 115.824, 143.256, end - 3.5, end + 0.5, end + 2.0]
    with pytest.raises(ValueError):  # the excluded neighbour 0.5 m above must not stand in for the kept side
        classify_stations(arcs, **st)
    ok = arcs[:-3] + [end - 2.0, end + 0.5, end + 2.0]
    kinds = classify_stations(ok, **st)
    assert kinds[-3] == "coupling" and kinds[-2] == "excluded"


def test_a_saf_detail_with_no_station_kind_is_not_evaluated():
    row = {**ROW, "limit": {**ROW["limit"], "saf": {**ROW["limit"]["saf"], "riser flange weld": 5.0}}}
    r = evaluate_case({None: _doc(ARCS, sig=[1.0, 1.0, 1.0, 25e3, 1.0])}, row, IRR)
    assert r.status == "NOT_EVALUATED" and "riser flange weld" in r.reason
    ok = evaluate_case({None: _doc(ARCS, sig=[1.0, 1.0, 1.0, 25e3, 1.0])}, row,
                       {**IRR, "cr10_saf_not_applicable": ["riser flange weld"]})
    assert ok.status == "PASS"


def test_a_hot_spot_on_a_coupling_is_refused():
    with pytest.raises(ValueError):
        classify_stations([18.796, 19.9, 21.336, 25.0, 30.0, 34.0], **{**STATIONS, "hot_spot_m": 21.0})


# ---------------------------------------------------------------- W04: pup-side station of the 18.288 m coupling

PUP = {**STATIONS, "pup_side_couplings_m": [18.288]}


def test_pup_side_station_takes_the_nearest_result_point_above_the_barrel_coupling():
    arcs = [0.508, 17.78, 18.796, 19.812, 20.828, 22.352]
    kinds = classify_stations(arcs, **PUP)
    # barrel side stays excluded; the first pup-side point becomes the coupling station (it is also the hot spot)
    assert kinds == ["excluded", "excluded", "coupling", "body", "coupling", "coupling"]


def test_pup_side_station_must_be_a_coupling_and_on_the_grid():
    with pytest.raises(ValueError):
        classify_stations([18.796, 19.812, 25.0], **{**STATIONS, "pup_side_couplings_m": [19.0]})
    with pytest.raises(ValueError):  # nearest kept point 25.0 m is 6.7 m from the coupling
        classify_stations([17.78, 25.0, 40.0], **{**STATIONS, "hot_spot_m": None, "pup_side_couplings_m": [18.288]})


def test_pup_side_station_governs_with_the_coupling_saf_and_the_hot_spot_is_still_reported():
    sig = [900e3, 76e3, 5e3, 62e3, 5e3]
    r = evaluate_case({None: _doc(ARCS, sig=sig)}, ROW, {**IRR, "cr10_stations": PUP})
    assert r.status == "FAIL" and r.u == pytest.approx(76.0 / COUPLING_MPA)
    assert "18.8" in r.location and "coupling" in r.location
    hs = r.detail["hot_spot"]
    assert hs["arc_m"] == pytest.approx(18.796) and hs["cr10_station"] == "coupling"
    # without the pup-side station the hot spot does not govern (W510 behaviour unchanged)
    r0 = evaluate_case({None: _doc(ARCS, sig=sig)}, ROW, IRR)
    assert r0.u == pytest.approx(62.0 / COUPLING_MPA) and r0.detail["hot_spot"]["cr10_station"] is None
