# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""GZ coverage contracts for the intact-criteria gap in issue #3284.

These synthetic linear curves use GZ = heel / 10 solely as arithmetic
fixtures. Their large arms previously passed the intact criteria even
when tabulation stopped before a required integration angle.
"""

import math

import pytest

from digitalmodel.naval_architecture.vessel_stability_screening import (
    GZCurve,
    imo_intact_criteria,
    max_kg_screening,
)
from digitalmodel.vessel_stability_screening.workflow import router


def _curve(end_deg=40.0):
    return GZCurve(
        heel_deg=(0.0, 10.0, end_deg), gz_m=(0.0, 1.0, end_deg / 10.0)
    )


@pytest.mark.parametrize("end_deg", (25.0, 30.0, 35.0, 39.9))
def test_intact_criteria_reject_truncated_curve(end_deg):
    with pytest.raises(ValueError, match="outside tabulated GZ range"):
        imo_intact_criteria(_curve(end_deg), gm0_m=1.0)


@pytest.mark.parametrize("end_deg", (30.0, 34.9))
def test_intact_criteria_require_coverage_to_downflooding_cap(end_deg):
    with pytest.raises(ValueError, match="outside tabulated GZ range"):
        imo_intact_criteria(
            _curve(end_deg), gm0_m=1.0, downflooding_angle_deg=35.0
        )


@pytest.mark.parametrize("cap_deg", (35.0, 40.0))
def test_intact_criteria_accept_exact_governing_endpoint(cap_deg):
    results = imo_intact_criteria(
        _curve(cap_deg), gm0_m=1.0, downflooding_angle_deg=cap_deg
    )
    assert all(result.passed for result in results)
    by_key = {result.key: result for result in results}
    # Integral of heel/10 against radians(heel): radians(cap**2 / 20).
    assert by_key["imo_area_0_40"].value == pytest.approx(
        math.radians(cap_deg**2 / 20.0)
    )


@pytest.mark.parametrize("angle", (-1.0, 40.1, math.nan, math.inf, -math.inf))
def test_gz_lookup_rejects_uncovered_or_nonfinite_angle(angle):
    with pytest.raises(ValueError, match="tabulated GZ range|must be finite"):
        _curve().gz_at(angle)


@pytest.mark.parametrize(
    ("start", "end"),
    (
        (-1.0, 30.0), (0.0, 40.1), (41.0, 42.0), (41.0, 41.0),
        (math.nan, 30.0), (0.0, math.nan), (0.0, math.inf),
        (-math.inf, 30.0),
    ),
)
def test_gz_area_requires_both_endpoints_in_curve(start, end):
    with pytest.raises(ValueError, match="tabulated GZ range|must be finite"):
        _curve().area_m_rad(start, end)


def test_gz_lookup_and_area_preserve_interpolation_and_endpoints():
    curve = _curve()
    assert curve.gz_at(0.0) == 0.0
    assert curve.gz_at(15.0) == 1.5
    assert curve.gz_at(40.0) == 4.0
    assert curve.area_m_rad(0.0, 40.0) == pytest.approx(math.radians(80.0))
    assert curve.area_m_rad(10.0, 30.0) == pytest.approx(math.radians(40.0))
    assert curve.area_m_rad(40.0, 40.0) == 0.0
    with pytest.raises(ValueError, match="to_deg must be >= from_deg"):
        curve.area_m_rad(30.0, 10.0)


def test_max_kg_rejects_truncated_cross_curve():
    with pytest.raises(ValueError, match="outside tabulated GZ range"):
        max_kg_screening([0.0, 10.0, 30.0], [0.0, 1.0, 3.0], 5.0, 5.0)


@pytest.mark.parametrize("source", ("gz_m", "kn_m", "cross_curves"))
def test_workflow_rejects_truncated_curve_before_publishing(tmp_path, source):
    gz = {"heel_deg": [0.0, 10.0, 30.0]}
    if source == "cross_curves":
        gz[source] = {
            "displacements_t": [2.0, 8.0],
            "kn_m": [[0.0, 1.0, 3.0], [0.0, 1.0, 3.0]],
        }
    else:
        gz[source] = [0.0, 1.0, 3.0]
    output_dir = tmp_path / "results"
    settings = {
        "hydrostatics": {"table": [
            {"draft_m": 1.0, "displacement_t": 2.0, "km_m": 5.0},
            {"draft_m": 2.0, "displacement_t": 8.0, "km_m": 5.0},
        ]},
        "loading_condition": {"items": [
            {"name": "solid", "weight_t": 5.0, "vcg_m": 0.0,
             "lcg_m": 0.0, "fsm_t_m": 0.0},
        ]},
        "gz": gz,
        "output_dir": str(output_dir),
    }
    cfg = {"vessel_stability_screening": settings}
    with pytest.raises(ValueError, match="outside tabulated GZ range"):
        router(cfg)
    assert "screening_status" not in cfg
    assert "screening_status" not in settings
    assert not output_dir.exists()
