# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""GZ coverage contracts for the intact-criteria gap in issue #3284.

These synthetic linear curves use GZ = heel / 10 solely as arithmetic
fixtures. Their large arms previously passed the intact criteria even
when tabulation stopped before a required integration angle.
"""

import math

import pytest

from digitalmodel.naval_architecture.damage_stability_screening import (
    _first_down_crossing,
    _positive_net_area_m_rad,
    damage_gz_analysis,
)
from digitalmodel.naval_architecture.vessel_stability_screening import (
    Citation,
    CraneHeelingCase,
    GZCurve,
    LiftingCriteria,
    _net_area_m_rad,
    equilibrium_heel_deg,
    imo_intact_criteria,
    lifting_check,
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
    "angle", (math.nextafter(0.0, -math.inf), math.nextafter(40.0, math.inf))
)
def test_gz_lookup_rejects_even_one_ulp_outside_table(angle):
    with pytest.raises(ValueError, match="outside tabulated GZ range"):
        _curve().gz_at(angle)


def test_lifting_net_area_uses_exact_tabulated_endpoint():
    case = CraneHeelingCase("synthetic", heeling_moment_t_m=0.1)
    area = _net_area_m_rad(_curve(), case, 1.0, 10.7296, 40.0)
    # Integral: pi/180 * (40**2 - 10.7296**2)/20
    # - 0.1 * (sin(40 deg) - sin(10.7296 deg)) = 1.2501371449.
    assert area == pytest.approx(1.2501371449, abs=1e-8)


def test_damage_down_crossing_uses_exact_tabulated_endpoint():
    assert _first_down_crossing(_curve().gz_at, 10.7296, 40.0, 400) is None


def test_damage_positive_area_uses_exact_tabulated_endpoint():
    area = _positive_net_area_m_rad(_curve().gz_at, 10.7296, 40.0)
    # Integral of heel/10: pi/180 * (40**2 - 10.7296**2)/20.
    assert area == pytest.approx(1.2957984833, abs=1e-10)


@pytest.mark.parametrize("steps", (400, 2000))
def test_damage_analysis_accepts_exact_governing_endpoint(steps):
    analysis = damage_gz_analysis(
        _curve(), 1.0, static_heel_deg=10.7296, limit_angle_deg=40.0,
        steps=steps,
    )
    assert analysis.equilibrium_heel_deg == 10.7296
    assert analysis.range_end_deg == 40.0
    # Range: 40 - 10.7296 = 29.2704 deg; area is the hand integral above.
    assert analysis.range_beyond_equilibrium_deg == pytest.approx(29.2704)
    assert analysis.residual_area_m_rad == pytest.approx(1.2957984833, abs=1e-10)


def _lifting_inputs():
    return (
        CraneHeelingCase("synthetic", heeling_moment_t_m=1.092),
        LiftingCriteria(
            max_equilibrium_heel_deg=20.0,
            residual_area_ratio_min=1.0,
            citation=Citation("synthetic", "2026", "arithmetic fixture"),
        ),
    )


def test_lifting_accepts_exact_governing_endpoint_with_hand_areas():
    case, criteria = _lifting_inputs()
    result = lifting_check(_curve(), case, 1.0, criteria)
    # Solve heel/10 = 1.092*cos(heel): heel = 10.7291005951 deg.
    assert equilibrium_heel_deg(_curve(), case, 1.0) == pytest.approx(
        10.7291005951, abs=1e-4
    )
    assert result.equilibrium_heel_deg == pytest.approx(10.7291005951, abs=1e-4)
    assert result.residual_range_to_deg == 40.0
    # Integral: pi/180 * (40**2 - heel**2)/20
    # - 1.092*(sin(40 deg) - sin(heel)); heeling area = 1.092*sin(heel).
    assert result.residual_area_m_rad == pytest.approx(0.79717667, abs=1e-6)
    assert result.heeling_area_m_rad == pytest.approx(0.20329290, abs=2e-6)
    assert result.passed


def test_max_kg_with_lifting_accepts_exact_governing_endpoint():
    case, criteria = _lifting_inputs()
    limits = max_kg_screening(
        [0.0, 10.0, 40.0], [0.0, 1.0, 4.0], 5.0, 1.0,
        lifting_case=case, lifting=criteria,
    )
    by_key = {limit.key: limit for limit in limits}
    # GZ at 20 deg interpolates the corrected 10- and 40-deg ordinates.
    # KG = (2 - 1.092*cos(20 deg))/((2*sin(10 deg) + sin(40 deg))/3).
    assert by_key["lifting_equilibrium_heel"].kg_limit_m == pytest.approx(
        2.9508274828, abs=1e-4
    )
    assert not by_key["lifting_equilibrium_heel"].limited_by_km


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
