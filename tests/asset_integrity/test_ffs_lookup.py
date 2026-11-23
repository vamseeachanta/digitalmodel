# ABOUTME: Tests for the uniform FFS query router + indexed lookup
# ABOUTME: (ffs_lookup.py): routing, index round-trip, monotonicity, IO.
"""Tests for digitalmodel.asset_integrity.ffs_lookup."""

import json

import numpy as np
import pandas as pd
import pytest

from digitalmodel.asset_integrity.ffs_lookup import (
    barlow_maop_psi,
    build_lookup,
    evaluate_query,
    query_index,
    record_from_assessment,
    signature,
    write_lookup,
)


# --- router ----------------------------------------------------------------
@pytest.mark.parametrize("method, metric_name", [
    ("b31g", "safe_pressure_psi"),
    ("modified_b31g", "safe_pressure_psi"),
    ("rstreng", "safe_pressure_psi"),
    ("dnv_f101", "allowable_pressure_psi"),
])
def test_pipe_router_methods(method, metric_name):
    r = evaluate_query(
        "pipe_corroded_strength", method=method, grade="X52",
        D_in=20.0, t_in=0.5, d_in=0.15, L_in=6.0, maop_psi=900.0,
    )
    assert r["domain"] == "pipe_corroded_strength"
    assert r["metric_name"] == metric_name
    assert r["metric"] > 0
    assert isinstance(r["acceptable"], bool)
    assert r["verdict"] in ("ACCEPTABLE", "UNACCEPTABLE")


def test_plate_router():
    r = evaluate_query(
        "plate_metal_loss", grade="AH36", length_mm=2400.0, width_mm=800.0,
        thickness_mm=16.0, sigma_x_mpa=150.0, metal_loss_mm=2.0,
    )
    assert r["domain"] == "plate_metal_loss"
    assert r["metric_name"] == "utilization"
    assert r["metric"] > 0
    assert "capacity_retained_frac" in r


def test_unknown_domain_and_method_raise():
    with pytest.raises(ValueError):
        evaluate_query("nope", x=1)
    with pytest.raises(ValueError):
        evaluate_query("pipe_corroded_strength", method="bogus", grade="X52",
                       D_in=20.0, t_in=0.5, d_in=0.1, L_in=4.0, maop_psi=900.0)


# --- physics sanity (monotonicity) ----------------------------------------
def test_deeper_defect_lowers_safe_pressure():
    base = dict(method="modified_b31g", grade="X52", D_in=20.0, t_in=0.5,
                L_in=6.0, maop_psi=900.0)
    shallow = evaluate_query("pipe_corroded_strength", d_in=0.10, **base)["metric"]
    deep = evaluate_query("pipe_corroded_strength", d_in=0.30, **base)["metric"]
    assert deep < shallow


def test_more_metal_loss_raises_plate_utilisation():
    base = dict(grade="AH36", length_mm=2400.0, width_mm=800.0,
                thickness_mm=16.0, sigma_x_mpa=150.0)
    low = evaluate_query("plate_metal_loss", metal_loss_mm=1.0, **base)["metric"]
    high = evaluate_query("plate_metal_loss", metal_loss_mm=4.0, **base)["metric"]
    assert high > low


def test_barlow_maop():
    # 0.72 * 2 * 52000 * 0.5 / 20 = 1872 psi
    assert barlow_maop_psi(20.0, 0.5, 52000.0) == pytest.approx(1872.0, rel=1e-6)


# --- index round-trip ------------------------------------------------------
def test_index_round_trip():
    recs = [
        evaluate_query("pipe_corroded_strength", method="modified_b31g",
                       grade="X52", D_in=20.0, t_in=0.5, d_in=d, L_in=6.0,
                       maop_psi=900.0)
        for d in (0.10, 0.20, 0.30)
    ]
    lookup = build_lookup(recs)
    assert lookup["meta"]["n_records"] == 3
    hit = query_index(lookup, "pipe_corroded_strength", method="modified_b31g",
                      grade="X52", D_in=20.0, t_in=0.5, d_in=0.20, L_in=6.0)
    assert hit is not None
    # matches the record's metric
    rec = next(r for r in recs if r["inputs"]["d_in"] == 0.20)
    assert hit["metric"] == pytest.approx(rec["metric"])


def test_query_index_miss_returns_none():
    lookup = build_lookup([evaluate_query(
        "plate_metal_loss", grade="AH36", length_mm=2400.0, width_mm=800.0,
        thickness_mm=16.0, sigma_x_mpa=150.0, metal_loss_mm=2.0)])
    assert query_index(lookup, "plate_metal_loss", grade="EH40",
                       length_mm=2400.0, width_mm=800.0, thickness_mm=16.0,
                       sigma_x_mpa=150.0, metal_loss_mm=2.0) is None


# --- IO --------------------------------------------------------------------
def test_write_lookup(tmp_path):
    recs = [evaluate_query("pipe_corroded_strength", method="b31g", grade="X52",
                           D_in=20.0, t_in=0.5, d_in=0.15, L_in=6.0, maop_psi=900.0)]
    lookup = build_lookup(recs)
    paths = write_lookup(lookup, tmp_path)
    d = json.loads(open(paths["json"]).read())
    assert set(d.keys()) >= {"meta", "lookup", "index"}
    assert len(d["index"]) == 1
    df = pd.read_csv(paths["csv"])
    assert len(df) == 1 and "verdict" in df.columns


# --- coordinator bridge ----------------------------------------------------
def test_record_from_assessment():
    from digitalmodel.asset_integrity.assessment import FFSComponent, assess_component
    comp = FFSComponent(component_id="L-9", design_code="B31.8",
                        nominal_od_in=12.75, nominal_wt_in=0.5,
                        design_pressure_psi=1000.0, smys_psi=52000.0)
    res = assess_component(comp, pd.DataFrame([[0.47] * 5 for _ in range(5)]))
    rec = record_from_assessment(res)
    assert rec["key"] == "component|L-9"
    assert rec["metric_name"] == "rsf"
    assert isinstance(rec["acceptable"], bool)
