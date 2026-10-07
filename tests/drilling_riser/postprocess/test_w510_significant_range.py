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

import math

import numpy as np
import pytest

from digitalmodel.drilling_riser.global_model.fatigue_channels import half_cycles
from digitalmodel.drilling_riser.postprocess.checks import KSI_KPA
from digitalmodel.drilling_riser.postprocess.evaluate import STATUSES, evaluate_case, summarise
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
    merge_stress_range(doc, {"schema": SCHEMA_SIG, "lines": {"Riser": line}})
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
