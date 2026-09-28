"""Criteria evaluator on the riser-w5-channels/1 schema: demand, capacity, utilisation, governing seed and time."""

from __future__ import annotations

import math

import pytest

from digitalmodel.drilling_riser.postprocess.channels import MissingChannel, point_rows
from digitalmodel.drilling_riser.postprocess.checks import (
    FT_KIP_KNM,
    KIP_KN,
    bore_differential_kpa,
    envelope_min_capacity,
    zero_crossing,
)
from digitalmodel.drilling_riser.postprocess.evaluate import evaluate_case, summarise

G = 9.80665
ROW_FJ_MAX = {"id": "CR-02", "check_key": "fj_max", "location": "upper", "limit": {"value": 4.0}, "unit": "deg"}
ROW_FJ_MEAN = {"id": "CR-01", "check_key": "fj_mean", "location": "upper", "limit": {"value": 2.0}, "unit": "deg"}
ROW_FJ_AVAIL = {"id": "CR-05", "check_key": "fj_max_avail", "location": "upper and lower",
                "limit": {"limit_deg": {"upper": 13.5, "lower": 9.0}}, "unit": "deg"}
ROW_VM = {"id": "CR-07", "check_key": "von_mises", "limit": {"value": 371.85}, "unit": "MPa"}
ROW_TEFF = {"id": "CR-14", "check_key": "t_eff_min", "limit": {"value": 0.0}, "unit": "kN"}
ROW_TMIN = {"id": "CR-11", "check_key": "t_min", "limit": {}, "unit": "kips"}
ROW_TSET = {"id": "CR-13", "check_key": "tension_setting_max", "limit": {"value": 2970.0}, "unit": "kips"}
ROW_TJ = {"id": "CR-17", "check_key": "tj_stroke", "limit": {"usable_m": [0.5, 19.312]}, "unit": "m"}
ROW_TENS = {"id": "CR-18", "check_key": "tensioner_stroke", "limit": {"stroke_m": 15.24}, "unit": "m"}
ROW_COUP = {"id": "CR-27", "check_key": "coupling_rating", "limit": {"rated_kips": 3500.0}, "unit": "kips"}
ROW_TMP = {"id": "CR-19", "check_key": "connector_tmp", "unit": "-", "limit": {"envelopes": {
    "lmrp": {"tension_kips": 1000.0, "bore_pressure_ksi": [0, 5, 10, 15],
             "operational_ft_kips": [5050, 5150, 4300, 2750], "extreme_ft_kips": [6200, 6700, 5550, 3900]}}}}
ROW_COND = {"id": "CR-20", "check_key": "conductor_bending", "unit": "kN m", "limit": {
    "wellhead_system_ft_kips": 5250.0, "conductor_casing_kip_ft": {"conductor_lower": {"My": 4558.6}}}}
ROW_DSR = {"id": "CR-10", "check_key": "dyn_stress_range", "limit": {"value_if_saf_le_1_5": 68.948}, "unit": "MPa"}

CTX = {"t_min_kips": 813.5, "top_tension_kips": 1149.9, "tj_datum_m": 9.855, "connector_level": "operational",
       "connectors": {"stack:A|B": "lmrp"}, "interface_offset_kn": {"stack:A|B": 100.0}, "rho_water_kg_m3": 1025.0,
       "wellhead_point": "stack:datum", "conductor_capacity": ["conductor_lower", "My"]}


def _row(te=1000.0, m=500.0, po=9500.0, t=None, **kw):
    r = {"te": te, "tw": te, "m": m, "mx": 0.0, "my": m, "pi": po, "po": po, "sx": 0.0, "sy": 0.0, **kw}
    if t is not None:
        r["t"] = t
    return r


def static_doc(ufj=1.5, lfj=0.8, vm_kpa=200000.0, te_min=500.0, tw_max=4000.0, stroke=0.3, m_conn=500.0,
               cond_m=3000.0, wh_m=2500.0):
    return {"channels": {"w5": {
        "schema": "riser-w5-channels/1", "analysis": "statics",
        "contents": {"density_kg_m3": 1500.0, "pressure_ref_z_m": 25.0},
        "points": {
            "ufj": {"line": "InnerBarrel", "static": {**_row(), "ez_angle_deg": ufj}},
            "lfj": {"line": "Riser", "static": {**_row(), "ez_angle_deg": lfj}},
            "stack:A|B": {"line": "Stack", "static": {**_row(m=m_conn), "te_above": 1200.0}},
            "stack:datum": {"line": "Stack", "static": _row(m=wh_m)},
        },
        "range_graphs": {
            "Riser": {"arc1_m": [0.0, 10.0, 20.0], "vm": [vm_kpa * 0.5, vm_kpa, vm_kpa * 0.7],
                      "te": [te_min + 10, te_min, te_min + 5], "tw": [tw_max, tw_max - 1, tw_max - 2]},
            "Conductor": {"arc1_m": [0.0, 5.0], "m": [cond_m, cond_m * 0.5]},
        },
        "stroke": {"static": stroke},
    }}}


def dyn_doc(ufj_max=3.0, ufj_mean=1.2, t_ufj=123.4, vm_kpa=250000.0, te_min=400.0, m_conn=800.0, t_conn=55.5,
            s_min=-0.5, s_max=1.0):
    ext = [{**_row(), "driver": "ez_angle_deg", "kind": "max", "t": t_ufj, "ez_angle_deg": ufj_max},
           {**_row(), "driver": "ez_angle_deg", "kind": "min", "t": 1.0, "ez_angle_deg": 0.1}]
    stat = {"ez_angle_deg": {"max": ufj_max, "min": 0.1, "mean": ufj_mean}}
    conn_rows = [{**_row(m=m_conn, t=t_conn), "driver": "m", "kind": "max", "te_above": 1200.0},
                 {**_row(m=100.0, t=2.0), "driver": "m", "kind": "min", "te_above": 1200.0}]
    return {"channels": {"w5": {
        "schema": "riser-w5-channels/1", "analysis": "dynamics",
        "contents": {"density_kg_m3": 1500.0, "pressure_ref_z_m": 25.0},
        "time": {"start_s": 0.0, "end_s": 10800.0},
        "points": {
            "ufj": {"line": "InnerBarrel", "extremes": ext, "stats": stat, "tm_hull": []},
            "lfj": {"line": "Riser", "extremes": ext, "stats": stat, "tm_hull": []},
            "stack:A|B": {"line": "Stack", "extremes": conn_rows, "tm_hull": [_row(m=10.0, t=3.0)], "stats": {}},
            "stack:datum": {"line": "Stack", "extremes": [{**_row(m=2000.0, t=9.0), "driver": "m", "kind": "max"}],
                            "stats": {"m": {"max": 2000.0}}},
        },
        "range_graphs": {
            "Riser": {"arc1_m": [0.0, 10.0], "vm_max": [vm_kpa, 1.0], "te_min": [te_min, te_min + 1],
                      "tw_max": [3000.0, 1.0]},
            "Conductor": {"arc1_m": [0.0, 5.0], "m_max": [2500.0, 1.0]},
        },
        "stroke": {"stats": {"max": s_max, "min": s_min, "mean": 0.2}},
    }}}


# ---------------------------------------------------------------- building blocks


def test_units_and_envelope_capacity_takes_the_lowest_value_over_zero_to_p():
    assert FT_KIP_KNM == pytest.approx(1.3558179483)
    assert KIP_KN == pytest.approx(4.4482216153)
    p, m = [0, 5, 10, 15], [5050, 5150, 4300, 2750]
    assert envelope_min_capacity(1.0, p, m) == 5050
    assert envelope_min_capacity(7.5, p, m) == pytest.approx(4725.0)
    with pytest.raises(ValueError):
        envelope_min_capacity(16.0, p, m)


def test_bore_differential_pressure_from_the_contents_column():
    po = 1025.0 * G * 900.0 / 1000.0  # kPa at 900 m depth
    got = bore_differential_kpa(po, rho_contents=1500.0, z_ref_m=25.0, rho_water=1025.0)
    assert got == pytest.approx((1500.0 * G * 925.0 - 1025.0 * G * 900.0) / 1000.0)


def test_zero_crossing_by_linear_interpolation():
    assert zero_crossing([0.95, 0.90, 0.85, 0.80, 0.75], [300.0, 200.0, 100.0, 20.0, -60.0]) == pytest.approx(0.7875)
    assert zero_crossing([1.0, 0.9], [5.0, 1.0]) is None


def test_point_rows_static_and_dynamic():
    assert point_rows(static_doc()["channels"]["w5"], "stack:A|B")[0]["t"] is None
    rows = point_rows(dyn_doc()["channels"]["w5"], "stack:A|B")
    assert [r["t"] for r in rows] == [55.5, 2.0, 3.0]
    with pytest.raises(MissingChannel):
        point_rows(dyn_doc()["channels"]["w5"], "stack:X|Y")


# ---------------------------------------------------------------- single case (static, regular)


def test_flex_joint_checks_static_and_regular():
    r = evaluate_case({None: static_doc(ufj=3.0)}, ROW_FJ_MAX, CTX)
    assert (r.status, r.u, r.demand, r.capacity, r.location) == ("PASS", 0.75, 3.0, 4.0, "ufj")
    r = evaluate_case({None: dyn_doc(ufj_max=5.0)}, ROW_FJ_MAX, CTX)
    assert (r.status, r.u, r.time_s) == ("FAIL", 1.25, 123.4)
    assert "exceeds" in r.reason and "4" in r.reason
    r = evaluate_case({None: dyn_doc(ufj_mean=1.0)}, ROW_FJ_MEAN, CTX)
    assert r.u == pytest.approx(0.5)
    r = evaluate_case({None: static_doc(ufj=1.35, lfj=4.5)}, ROW_FJ_AVAIL, CTX)
    assert (r.u, r.location) == (pytest.approx(0.5), "lfj")


def test_von_mises_coupling_and_effective_tension():
    r = evaluate_case({None: static_doc(vm_kpa=185925.0)}, ROW_VM, CTX)
    assert (r.u, r.demand, r.location) == (pytest.approx(0.5), pytest.approx(185.925), "Riser arc 10.0 m")
    r = evaluate_case({None: static_doc(tw_max=KIP_KN * 1750.0)}, ROW_COUP, CTX)
    assert r.u == pytest.approx(0.5)
    r = evaluate_case({None: static_doc(te_min=-5.0)}, ROW_TEFF, CTX)
    assert (r.status, r.u, r.demand) == ("FAIL", None, -5.0)
    assert evaluate_case({None: static_doc(te_min=5.0)}, ROW_TEFF, CTX).status == "PASS"


def test_tension_setting_checks_use_the_case_context():
    r = evaluate_case({None: static_doc()}, ROW_TMIN, CTX)
    assert r.u == pytest.approx(813.5 / 1149.9)
    at_min = evaluate_case({None: static_doc()}, ROW_TMIN, {**CTX, "top_tension_kips": 813.5 * (1 + 1e-12)})
    assert at_min.status == "PASS"
    assert evaluate_case({None: static_doc()}, ROW_TSET, CTX).u == pytest.approx(1149.9 / 2970.0)


def test_stroke_checks_from_the_spaced_out_datum():
    r = evaluate_case({None: dyn_doc(s_min=-0.5, s_max=4.7285)}, ROW_TJ, CTX)
    assert r.u == pytest.approx(4.7285 / (19.312 - 9.855))
    r = evaluate_case({None: dyn_doc(s_min=-4.6775, s_max=0.1)}, ROW_TJ, CTX)
    assert r.u == pytest.approx(4.6775 / (9.855 - 0.5))
    r = evaluate_case({None: dyn_doc(s_min=-3.81, s_max=1.0)}, ROW_TENS, CTX)
    assert r.u == pytest.approx(0.5)


def test_connector_tmp_uses_interface_tension_and_coincident_rows():
    doc = dyn_doc(m_conn=5050.0 * FT_KIP_KNM * 0.8)
    r = evaluate_case({None: doc}, ROW_TMP, CTX)
    assert r.u == pytest.approx(0.8)
    assert (r.location, r.time_s) == ("stack:A|B", 55.5)
    assert r.detail["te_interface_kn"] == pytest.approx(1100.0)  # below-side 1,000 kN + W405 offset 100 kN
    assert r.detail["by_location"] == {"stack:A|B": pytest.approx(0.8)}
    ext = evaluate_case({None: doc}, ROW_TMP, {**CTX, "connector_level": "extreme"})
    assert ext.u == pytest.approx(0.8 * 5050 / 6200)


def test_connector_tension_outside_the_chart_is_not_evaluated():
    doc = static_doc()
    doc["channels"]["w5"]["points"]["stack:A|B"]["static"]["te"] = KIP_KN * 1200.0
    r = evaluate_case({None: doc}, ROW_TMP, CTX)
    assert r.status == "NOT_EVALUATED" and "1,000 kips" in r.reason


def test_conductor_bending_takes_the_larger_of_wellhead_and_conductor():
    r = evaluate_case({None: static_doc(cond_m=4558.6 * FT_KIP_KNM * 0.6, wh_m=5250.0 * FT_KIP_KNM * 0.3)},
                      ROW_COND, CTX)
    assert r.u == pytest.approx(0.6) and r.location == "Conductor arc 0.0 m"
    assert r.detail["by_location"]["stack:datum"] == pytest.approx(0.3)


def test_a_missing_channel_is_reported_as_such():
    doc = static_doc()
    del doc["channels"]["w5"]["range_graphs"]["Riser"]["vm"]
    r = evaluate_case({None: doc}, ROW_VM, CTX)
    assert r.status == "NOT_EVALUATED" and r.missing_channel == "range_graphs.Riser.vm"
    r = evaluate_case({None: static_doc()}, ROW_DSR, CTX)
    assert r.missing_channel and r.status == "NOT_EVALUATED"


def test_unknown_check_key_is_not_evaluated_without_failing_the_step():
    r = evaluate_case({None: static_doc()}, {"id": "CR-99", "check_key": "nope", "limit": {}}, CTX)
    assert r.status == "NOT_EVALUATED" and r.missing_channel is None and "no demand function" in r.reason


# ---------------------------------------------------------------- irregular seeds


def test_irregular_seeds_gumbel_on_seed_maxima_and_governing_seed():
    seeds = {s: dyn_doc(ufj_max=v, t_ufj=100.0 + s) for s, v in enumerate([3.0, 3.2, 2.9, 3.6, 3.1, 3.0, 3.3, 3.05,
                                                                          2.95, 3.15], start=1)}
    r = evaluate_case(seeds, ROW_FJ_MAX, CTX, seeds_expected=10)
    assert r.stats["n"] == 10 and r.stats["kind"] == "max"
    assert r.u == pytest.approx(r.stats["mpm"])  # U per seed = angle / 4 deg, fitted on the U maxima
    assert r.demand == pytest.approx(r.u * 4.0)
    assert (r.seed, r.time_s) == (4, 104.0)
    assert r.stats["ci_low"] < r.u < r.stats["ci_high"]


def test_irregular_minimum_effective_tension_is_fitted_on_seed_minima():
    seeds = {s: dyn_doc(te_min=v) for s, v in enumerate([400, 380, 420, 390, 410, 395, 405, 385, 415, 400], 1)}
    r = evaluate_case(seeds, ROW_TEFF, CTX, seeds_expected=10)
    assert r.stats["kind"] == "min" and r.status == "PASS"
    # the mode of a Gumbel minimum lies above the sample mean (left skew) and below the largest seed minimum
    assert r.demand == pytest.approx(r.stats["mpm"]) and 380 < r.demand < 420
    assert r.stats["ci_low"] < r.demand < r.stats["ci_high"]
    assert r.seed == 2


def test_irregular_mean_angle_is_the_mean_of_the_seed_means():
    seeds = {s: dyn_doc(ufj_mean=v) for s, v in enumerate([1.0, 1.2, 1.4, 1.0, 1.2, 1.4, 1.0, 1.2, 1.4, 1.2], 1)}
    r = evaluate_case(seeds, ROW_FJ_MEAN, CTX, seeds_expected=10)
    assert r.demand == pytest.approx(1.2) and r.stats is None


def test_missing_seeds_are_not_evaluated():
    seeds = {s: dyn_doc() for s in range(1, 8)}
    r = evaluate_case(seeds, ROW_FJ_MAX, CTX, seeds_expected=10)
    assert r.status == "NOT_EVALUATED" and "7 of 10 seeds" in r.reason


def test_summary_per_row_governing_case_and_counts():
    a = evaluate_case({None: static_doc(ufj=3.0)}, ROW_FJ_MAX, CTX)
    b = evaluate_case({None: static_doc(ufj=5.0)}, ROW_FJ_MAX, CTX)
    s = summarise([("C-1", a), ("C-2", b)])
    assert s["CR-02"]["counts"] == {"PASS": 1, "FAIL": 1, "NOT_EVALUATED": 0}
    assert s["CR-02"]["governing"]["case_id"] == "C-2"
    assert s["CR-02"]["governing"]["u"] == pytest.approx(1.25)
    assert s["CR-02"]["status"] == "FAIL"
    assert math.isclose(s["CR-02"]["max_u"], 1.25)
