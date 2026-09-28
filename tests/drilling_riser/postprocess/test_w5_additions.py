"""W5 additions (owner decisions W501, W503, W506): the outer-fibre stress-range channel and CR-10, burst and
collapse capacities with CR-23 / CR-25, and the diverter / moonpool clearance angle with CR-21 (test first)."""

from __future__ import annotations

import copy
import math

import pytest

from digitalmodel.drilling_riser.postprocess.checks import KSI_KPA
from digitalmodel.drilling_riser.postprocess.evaluate import evaluate_case
from digitalmodel.drilling_riser.postprocess.pressure import burst_pressure_mpa, collapse_pressure_mpa
from digitalmodel.drilling_riser.postprocess.clearance import limiting_angle_deg
from digitalmodel.drilling_riser.postprocess.stress_range import (
    SCHEMA as SR_SCHEMA,
    merge_stress_range,
    range_over_theta,
    stress_range_limit_ksi,
)

IN = 0.0254
ROW_DSR = {"id": "CR-10", "check_key": "dyn_stress_range", "unit": "MPa",
           "limit": {"value_if_saf_le_1_5": 68.948, "saf": {"riser girth weld": 1.245, "riser coupling weld": 3.0}}}
ROW_BURST = {"id": "CR-23", "check_key": "burst", "unit": "-", "limit": {"factor": 0.6}}
ROW_COLLAPSE = {"id": "CR-25", "check_key": "collapse", "unit": "-", "limit": {"factor": 0.6}}
ROW_CLEAR = {"id": "CR-21", "check_key": "moonpool_clearance", "unit": "m", "limit": {"formula": "clearance > 0"}}
PIPE = {"od_m": 21.0 * IN, "t_min_m": 0.766 * IN, "smys_mpa": 555.0, "smts_mpa": 625.0, "e_mpa": 207000.0,
        "poisson": 0.3}


def _row(te=1000.0, m=500.0, pi=12000.0, po=9500.0, t=None, **kw):
    r = {"te": te, "tw": te, "m": m, "mx": 0.0, "my": m, "pi": pi, "po": po, "sx": 0.0, "sy": 0.0, **kw}
    if t is not None:
        r["t"] = t
    return r


def static_doc(pi_lfj=12000.0, po_lfj=9500.0, pi_top=100.0, po_top=0.0, ufj=1.5):
    return {"channels": {"w5": {
        "schema": "riser-w5-channels/1", "analysis": "statics",
        "points": {
            "ufj": {"line": "InnerBarrel", "static": {**_row(), "ez_angle_deg": ufj}},
            "riser_top": {"line": "Riser", "static": _row(pi=pi_top, po=po_top)},
            "lfj": {"line": "Riser", "static": {**_row(pi=pi_lfj, po=po_lfj), "ez_angle_deg": 0.5}},
        },
        "range_graphs": {"Riser": {"arc1_m": [0.0, 10.0], "vm": [1.0, 2.0]}},
        "stroke": {"static": 0.0},
    }}}


def dyn_doc(ufj_max=3.0, zz=None):
    ext = [{**_row(t=10.0), "driver": "ez_angle_deg", "kind": "max", "ez_angle_deg": ufj_max},
           {**_row(t=11.0), "driver": "ez_angle_deg", "kind": "min", "ez_angle_deg": -0.2}]
    w = {"schema": "riser-w5-channels/1", "analysis": "dynamics", "time": {"start_s": 0.0, "end_s": 100.0},
         "points": {"ufj": {"line": "InnerBarrel", "extremes": ext,
                            "stats": {"ez_angle_deg": {"max": ufj_max, "min": -0.2, "mean": 1.0}}},
                    "riser_top": {"line": "Riser", "extremes": [{**_row(t=5.0), "driver": "te", "kind": "max"}]},
                    "lfj": {"line": "Riser", "extremes": [{**_row(t=6.0), "driver": "te", "kind": "max"}]}},
         "range_graphs": {"Riser": {"arc1_m": [0.0, 10.0], "vm_max": [1.0, 2.0]}}}
    doc = {"channels": {"w5": w}}
    if zz is not None:
        merge_stress_range(doc, {"schema": SR_SCHEMA, "lines": {"Riser": {"arc_m": [0.0, 5.0, 10.0],
                                                                           "zz_range_max": zz,
                                                                           "theta_deg": [0.0, 90.0, 180.0]}}})
    return doc


# ---------------------------------------------------------------- stress range (W501, CR-10)


def test_range_over_theta_takes_the_largest_double_amplitude_at_each_arc():
    envs = {0.0: ([-10.0, -5.0], [30.0, 5.0]), 90.0: ([0.0, -20.0], [50.0, 10.0])}
    rng, th = range_over_theta(envs)
    assert rng == [50.0, 30.0] and th == [90.0, 90.0]
    rng, th = range_over_theta({0.0: ([0.0, 0.0], [40.0, 1.0]), 90.0: ([0.0, 0.0], [10.0, 2.0])})
    assert rng == [40.0, 2.0] and th == [0.0, 90.0]


def test_range_over_theta_rejects_mismatched_grids():
    with pytest.raises(ValueError):
        range_over_theta({0.0: ([0.0, 1.0], [1.0, 2.0]), 90.0: ([0.0], [1.0])})


def test_stress_range_limit_follows_the_saf_rule():
    assert stress_range_limit_ksi(1.0) == 10.0
    assert stress_range_limit_ksi(1.5) == 10.0
    assert stress_range_limit_ksi(1.245) == 10.0
    assert stress_range_limit_ksi(3.0) == pytest.approx(5.0)
    with pytest.raises(ValueError):
        stress_range_limit_ksi(0.0)


def test_merge_puts_the_range_on_its_own_axis_and_leaves_other_channels():
    doc = dyn_doc(zz=[1000.0, 2000.0, 3000.0])
    rg = doc["channels"]["w5"]["range_graphs"]["Riser"]
    assert rg["zz_range_max"] == [1000.0, 2000.0, 3000.0]
    assert rg[rg["axis_of"]["zz_range_max"]] == [0.0, 5.0, 10.0]
    assert rg["vm_max"] == [1.0, 2.0] and rg["arc1_m"] == [0.0, 10.0]
    with pytest.raises(ValueError):
        merge_stress_range(copy.deepcopy(doc), {"schema": "other/1", "lines": {}})


def test_cr10_governed_by_the_detail_with_the_lowest_allowable_range():
    # 20 MPa nominal range: girth weld (SAF 1.245) allows 10 ksi, coupling weld (SAF 3.0) allows 5 ksi
    zz_kpa = 20000.0
    r = evaluate_case({None: dyn_doc(zz=[1.0, zz_kpa, 5.0])}, ROW_DSR, {"stress_line": "Riser"})
    five_ksi_mpa = 5.0 * KSI_KPA / 1000.0
    assert r.status == "PASS"
    assert r.u == pytest.approx(20.0 / five_ksi_mpa)
    assert r.capacity == pytest.approx(five_ksi_mpa) and r.demand == pytest.approx(20.0)
    assert "arc 5.0 m" in r.location and "coupling" in r.location
    assert r.detail["by_detail"]["riser girth weld"] == pytest.approx(20.0 / (10.0 * KSI_KPA / 1000.0))


def test_cr10_static_case_is_not_evaluated_and_dynamic_without_channel_is_missing():
    r = evaluate_case({None: static_doc()}, ROW_DSR, {})
    assert r.status == "NOT_EVALUATED" and r.missing_channel is None and "static" in r.reason
    r = evaluate_case({None: dyn_doc()}, ROW_DSR, {})
    assert r.status == "NOT_EVALUATED" and r.missing_channel == "range_graphs.Riser.zz_range_max"


def test_cr10_without_saf_uses_the_10_ksi_value():
    row = {**ROW_DSR, "limit": {"value_if_saf_le_1_5": 68.948}}
    r = evaluate_case({None: dyn_doc(zz=[0.0, 34474.0, 0.0])}, row, {})
    assert r.u == pytest.approx(34.474 / 68.948)


# ---------------------------------------------------------------- burst and collapse (W503, CR-23 / CR-25)


def test_burst_pressure_api_2rd_5_3_2_hand_value():
    d, t = 21.0 * IN, 0.766 * IN
    expect = 0.45 * (555.0 + 625.0) * math.log(d / (d - 2 * t))
    assert burst_pressure_mpa(d, t, 555.0, 625.0) == pytest.approx(expect)
    assert burst_pressure_mpa(d, t, 555.0, 625.0) == pytest.approx(40.22, abs=0.01)


def test_collapse_pressure_api_2rd_5_3_3_hand_value():
    d, t = 21.0 * IN, 0.766 * IN
    py = 2.0 * 555.0 * t / d
    pel = 2.0 * 207000.0 * (t / d) ** 3 / (1.0 - 0.3 ** 2)
    assert collapse_pressure_mpa(d, t, 555.0) == pytest.approx(py * pel / math.sqrt(py ** 2 + pel ** 2))
    assert collapse_pressure_mpa(d, t, 555.0) == pytest.approx(19.38, abs=0.01)


def test_capacities_reject_bad_geometry():
    with pytest.raises(ValueError):
        burst_pressure_mpa(0.5, 0.25, 555.0, 625.0)
    with pytest.raises(ValueError):
        collapse_pressure_mpa(0.5, 0.0, 555.0)


def test_cr23_burst_takes_the_largest_internal_overpressure_on_the_riser():
    ctx = {"pipe": PIPE, "pressure_points": ["riser_top", "lfj"]}
    r = evaluate_case({None: static_doc(pi_lfj=12000.0, po_lfj=9500.0)}, ROW_BURST, ctx)
    pb = burst_pressure_mpa(PIPE["od_m"], PIPE["t_min_m"], 555.0, 625.0)
    assert r.status == "PASS" and r.location == "lfj"
    assert r.demand == pytest.approx(2.5) and r.capacity == pytest.approx(0.6 * pb)
    assert r.u == pytest.approx(2.5 / (0.6 * pb))


def test_cr25_collapse_with_heavy_contents_has_no_net_external_pressure():
    ctx = {"pipe": PIPE, "pressure_points": ["riser_top", "lfj"]}
    r = evaluate_case({None: static_doc()}, ROW_COLLAPSE, ctx)
    assert r.status == "PASS" and r.u == 0.0 and r.demand < 0.0
    r = evaluate_case({None: static_doc(pi_lfj=1000.0, po_lfj=9500.0)}, ROW_COLLAPSE, ctx)
    pc = collapse_pressure_mpa(PIPE["od_m"], PIPE["t_min_m"], 555.0)
    assert r.u == pytest.approx(8.5 / (0.6 * pc))


def test_burst_on_dynamic_rows_uses_every_coincident_row():
    ctx = {"pipe": PIPE, "pressure_points": ["riser_top", "lfj"]}
    r = evaluate_case({None: dyn_doc()}, ROW_BURST, ctx)
    assert r.demand == pytest.approx(2.5) and r.time_s in (5.0, 6.0)


# ---------------------------------------------------------------- clearance angle (W506, CR-21)


def test_limiting_angle_of_a_cylinder_through_an_opening_below_its_pivot():
    h, d, big = 2.217, 46.0 * IN, 59.0 * IN
    th = limiting_angle_deg(h, d, big)
    x = math.radians(th)
    assert h * math.tan(x) + d / (2.0 * math.cos(x)) == pytest.approx(big / 2.0, abs=1e-9)
    assert 4.0 < th < 4.5
    assert limiting_angle_deg(0.0, d, big) == pytest.approx(math.degrees(math.acos(d / big)))
    assert limiting_angle_deg(h, big, d) == 0.0  # member wider than the opening: no clearance at any angle


def test_cr21_compares_the_largest_ufj_angle_with_the_smallest_limiting_angle():
    ctx = {"clearance_limit_deg": {"diverter housing": 4.2, "moonpool": 8.3}}
    r = evaluate_case({None: dyn_doc(ufj_max=2.1)}, ROW_CLEAR, ctx)
    assert r.status == "PASS" and r.u == pytest.approx(0.5) and "diverter housing" in r.location
    r = evaluate_case({None: dyn_doc(ufj_max=5.0)}, ROW_CLEAR, ctx)
    assert r.status == "FAIL" and r.detail["by_obstruction"]["moonpool"] == pytest.approx(5.0 / 8.3)
    r = evaluate_case({None: static_doc(ufj=-2.1)}, ROW_CLEAR, ctx)
    assert r.u == pytest.approx(0.5)
