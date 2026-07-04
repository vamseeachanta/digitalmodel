# ABOUTME: Tests for the canonical FFS coordinator (ffs_coordinator.py) — the
# ABOUTME: end-to-end chain and unified FFSAssessmentResult data model.
"""Tests for digitalmodel.asset_integrity.assessment.ffs_coordinator."""

import numpy as np
import pandas as pd
import pytest

from digitalmodel.asset_integrity.assessment import (
    FFSComponent,
    FFSAssessmentResult,
    assess_component,
)


def _pipe():
    return FFSComponent(
        component_id="LINE-001",
        design_code="B31.8",
        nominal_od_in=12.75,
        nominal_wt_in=0.500,
        design_pressure_psi=1000.0,
        smys_psi=52_000.0,
        corrosion_rate_in_per_yr=0.005,
        rsf_a=0.90,
    )


def _healthy_grid():
    # 6x6, all near nominal -> general, healthy.
    return pd.DataFrame([[0.48] * 6 for _ in range(6)])


def _local_thin_grid():
    g = np.full((6, 6), 0.50)
    g[2, 3] = 0.30
    g[3, 3] = 0.32
    return g


# --- end-to-end chain ------------------------------------------------------
def test_assess_returns_unified_result():
    res = assess_component(_pipe(), _healthy_grid())
    assert isinstance(res, FFSAssessmentResult)
    assert res.component_id == "LINE-001"
    assert res.assessment_type in ("GML", "LML")
    assert res.t_nominal_in == 0.500
    assert res.t_min_in > 0
    assert res.t_measured_min_in <= res.t_measured_avg_in
    assert 0.0 < res.rsf <= 1.0001
    assert res.verdict in ("ACCEPT", "MONITOR", "RE_RATE", "REPAIR", "REPLACE")
    assert res.sufficiency_status in ("SUFFICIENT", "TAKE_MORE", "ESCALATE")
    # all six stages echoed
    assert res.level1 and res.level2 and res.decision
    assert res.details["router"]["assessment_type"] == res.assessment_type


def test_healthy_component_passes():
    res = assess_component(_pipe(), _healthy_grid())
    assert res.passes is True
    assert res.rsf >= res.rsf_a


def test_local_thin_spot_routes_lml_and_flags_sufficiency():
    # A 2-cell deep spot -> LML, and the sufficiency engine flags under-sampling.
    res = assess_component(_pipe(), _local_thin_grid())
    assert res.assessment_type == "LML"
    assert res.sufficiency_status in ("TAKE_MORE", "ESCALATE")
    assert res.folias_factor >= 1.0


def test_accepts_numpy_dataframe_and_csv(tmp_path):
    arr = np.full((5, 5), 0.47)
    res_np = assess_component(_pipe(), arr)
    res_df = assess_component(_pipe(), pd.DataFrame(arr))
    assert res_np.t_measured_min_in == pytest.approx(res_df.t_measured_min_in)
    # CSV path
    csv = tmp_path / "grid.csv"
    pd.DataFrame(arr).to_csv(csv, index=False, header=False)
    res_csv = assess_component(_pipe(), str(csv))
    assert res_csv.t_measured_min_in == pytest.approx(res_np.t_measured_min_in, rel=1e-6)


def test_mm_units_converted():
    # 12 mm uniform (~0.472 in) should give the same as the inch equivalent.
    mm = pd.DataFrame([[12.0] * 5 for _ in range(5)])
    res = assess_component(_pipe(), mm, input_units="mm")
    assert res.t_measured_min_in == pytest.approx(12.0 / 25.4, rel=1e-3)


def test_force_type_override():
    res = assess_component(_pipe(), _healthy_grid(), force_type="LML")
    assert res.assessment_type == "LML"


def test_rerated_pressure_bounded():
    res = assess_component(_pipe(), _healthy_grid())
    # Healthy -> rsf>=rsf_a -> rerate clamps to design pressure.
    assert res.rerated_pressure_psi == pytest.approx(1000.0, rel=1e-6)


def test_to_dict_is_json_friendly():
    import json
    res = assess_component(_pipe(), _healthy_grid())
    d = res.to_dict()
    json.dumps(d)  # must not raise
    assert d["component_id"] == "LINE-001"
    assert d["passes"] is True
    assert isinstance(d["rsf"], float)


def test_invalid_grid_type_raises():
    with pytest.raises(TypeError):
        assess_component(_pipe(), 42)
