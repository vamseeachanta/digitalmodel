# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Constructed benchmark, not a reproduction: note-only drift-off evidence."""
from __future__ import annotations

import copy
import json
import math
from pathlib import Path
from urllib.parse import urlsplit

import pytest

from digitalmodel.drilling_riser import drift_off
from digitalmodel.drilling_riser.envelope import EnvelopeCriteria, RiserSection
from digitalmodel.drilling_riser.telemetry_inputs import DPState
from digitalmodel.subsea.mooring_analysis.models import EnvironmentalConditions

_FIXTURE = Path(__file__).parent / "fixtures" / "drift_off_constructed_benchmark.json"
_CASE_NAMES = ("W2006", "B2005 95% NX", "B2005 99% NX")
_QUANTITIES = {
    "limit_radius", "start_offset", "start_velocity", "disconnect_duration",
    "calibrated_acceleration", "expected_time_to_limit", "expected_lead_time_margin",
    "published_red_offset",
}
_UNITS = {"m", "ft", "s", "m/s", "m/s2", "deg", "kips", "kip-ft",
          "%", "ksi", "in", "not applicable", None}
_EXPECTED_UNITS = {
    "start_offset": "m", "start_velocity": "m/s",
    "disconnect_duration": "s", "calibrated_acceleration": "m/s2",
    "expected_time_to_limit": "s", "expected_lead_time_margin": "s",
}


def _read_fixture():
    return json.loads(_FIXTURE.read_text(encoding="utf-8"))


def _metres(quantity):
    assert quantity["unit"] in {"m", "ft"}
    # Exact conversion used throughout: 1 ft = 0.3048 m.
    return quantity["value"] * (0.3048 if quantity["unit"] == "ft" else 1.0)


def _validate_quantity(quantity, *, allow_zero=False):
    assert {"value", "unit", "locator", "caveat"} <= quantity.keys()
    assert quantity["unit"] in _UNITS
    assert quantity["locator"] and quantity["caveat"]
    value = quantity["value"]
    if quantity["unit"] == "not applicable":
        assert isinstance(value, str) and value.strip()
    else:
        assert type(value) in (float, int) and math.isfinite(value)
        assert value >= 0 if allow_zero else value > 0


def _validate_case(case, sources):
    assert case["source_id"] in sources
    assert _QUANTITIES <= case.keys()
    for key in _QUANTITIES:
        _validate_quantity(case[key], allow_zero=key.startswith("start_"))
        if key in _EXPECTED_UNITS:
            assert case[key]["unit"] == _EXPECTED_UNITS[key]
    for quantity in case["calibration_point"].values():
        _validate_quantity(quantity)
    assert case["calibration_point"]["time"]["unit"] == "s"
    assert _metres(case["calibration_point"]["offset"]) == _metres(case["limit_radius"])
    assert case["calibration_point"]["time"]["value"] == case["expected_time_to_limit"]["value"]
    margin = case["expected_time_to_limit"]["value"] - case["disconnect_duration"]["value"]
    assert case["expected_lead_time_margin"]["value"] == margin
    acceleration = (2 * _metres(case["calibration_point"]["offset"])
                    / case["calibration_point"]["time"]["value"] ** 2)
    # The note prints acceleration to six decimal places, not full precision.
    assert case["calibrated_acceleration"]["value"] == round(acceleration, 6)
    _metres(case["published_red_offset"])


def _validate_recorded_quantities(value, key=""):
    if isinstance(value, dict):
        if "value" in value:
            assert {"value", "unit", "locator", "caveat"} <= value.keys()
            assert value["unit"] in _UNITS and value["locator"] and value["caveat"]
            if value["value"] is None or value["unit"] is None:
                # Unknown depth and an unprinted dimensionless unit stay null.
                assert any(word in value["caveat"].lower()
                           for word in ("not", "redacted", "no unit"))
                if value["value"] is not None:
                    assert type(value["value"]) in (int, float)
                    assert math.isfinite(value["value"]) and value["value"] > 0
            elif "deviation" in key:
                assert type(value["value"]) in (int, float)
                assert math.isfinite(value["value"]) and value["unit"] == "%"
            else:
                _validate_quantity(value, allow_zero=(key.startswith("start_")
                                                       or "direction" in key))
        else:
            for child_key, child in value.items():
                _validate_recorded_quantities(child, child_key)
    elif isinstance(value, list):
        for child in value:
            _validate_recorded_quantities(child, key)


def test_fixture_integrity_constructed_benchmark_not_a_reproduction():
    """Constructed benchmark, not a reproduction: validate evidence and units."""
    fixture = _read_fixture()
    assert fixture["label"].startswith("constructed benchmark, not a reproduction")
    assert fixture["source_of_values"] == "research note on the issue, 2026-10-12"
    sources = {source["id"]: source for source in fixture["sources"]}
    assert len(sources) == len(fixture["sources"]) == 2
    for source in sources.values():
        url = urlsplit(source["stable_url"])
        assert url.scheme in {"http", "https"} and url.hostname
        assert not url.username and not url.password
        assert source["citation"] and source["reuse_terms"]
    assert tuple(case["case"] for case in fixture["cases"]) == _CASE_NAMES
    for case in fixture["cases"]:
        _validate_case(case, sources)
    assert fixture["excluded_rows"] and fixture["modelling_gaps"]
    assert any(gap["id"] == "casing_bending_limit" for gap in fixture["modelling_gaps"])
    assert len([gap for gap in fixture["modelling_gaps"] if "original_gap" in gap]) == 5
    _validate_recorded_quantities(fixture)
    assert fixture["open_parameters"] and fixture["could_not_verify"]


@pytest.mark.parametrize("defect", ["negative", "nonfinite", "boolean", "unit",
                                   "source", "locator", "margin", "acceleration"])
def test_rejects_broken_case_constructed_benchmark_not_a_reproduction(defect):
    """Constructed benchmark, not a reproduction: reject corrupted evidence."""
    fixture = _read_fixture()
    case = copy.deepcopy(fixture["cases"][0])
    if defect in {"negative", "nonfinite", "boolean"}:
        case["limit_radius"]["value"] = {
            "negative": -1, "nonfinite": math.nan, "boolean": True,
        }[defect]
    elif defect == "unit":
        case["start_offset"]["unit"] = "kg"
    elif defect == "source":
        case["source_id"] = "missing"
    elif defect == "locator":
        case["limit_radius"]["locator"] = None
    elif defect == "margin":
        case["expected_lead_time_margin"]["value"] += 1
    else:
        case["calibrated_acceleration"]["value"] *= 2
    with pytest.raises(AssertionError):
        _validate_case(case, {source["id"]: source for source in fixture["sources"]})


def _screen_constructed_case(monkeypatch, case, *, placeholder_scale=1.0):
    # Synthetic placeholders are not source values. Neither patched function
    # reads them; explicit config prevents generic vessel defaults from loading.
    placeholder_mass_kg = 1_000_000.0 * placeholder_scale
    config = {
        "m_eff_low_kg": placeholder_mass_kg,
        "m_eff_high_kg": placeholder_mass_kg,
        "t_eds_s": case["disconnect_duration"]["value"],
        "thrust_efficiency": 0.0,
    }
    monkeypatch.setattr(drift_off, "watch_circle_limit",
                        lambda *args, **kwargs: (_metres(case["limit_radius"]),
                                                  "injected_published_limit"))
    monkeypatch.setattr(drift_off, "environmental_force_n",
                        lambda *args: (case["calibrated_acceleration"]["value"]
                                       * placeholder_mass_kg, {}))
    placeholder_condition = EnvironmentalConditions(
        wave_hs=placeholder_scale, wave_tp=placeholder_scale,
        wave_direction=placeholder_scale, current_speed=placeholder_scale,
        current_direction=placeholder_scale, wind_speed=placeholder_scale,
        wind_direction=placeholder_scale, return_period=placeholder_scale,
        name="synthetic_placeholder",
    )
    placeholder_section = RiserSection(
        outer_diameter_m=placeholder_scale, wall_thickness_m=0.1 * placeholder_scale,
        youngs_modulus_pa=placeholder_scale, smys_pa=placeholder_scale,
    )
    placeholder_criteria = EnvelopeCriteria(
        placeholder_scale, placeholder_scale, placeholder_scale,
        criteria_set="synthetic_placeholder", category="synthetic_placeholder",
    )
    return drift_off.drift_off_screen(
        DPState(thrusters_online=0, total_available_thrust_n=0.0,
                total_commanded_thrust_n=0.0), placeholder_condition,
        section=placeholder_section, water_depth_m=placeholder_scale,
        length_m=placeholder_scale, tension_n=placeholder_scale,
        criteria=placeholder_criteria, config=config,
        x0_m=case["start_offset"]["value"], v0_mps=case["start_velocity"]["value"],
        t_now_s=0.0,
    )


@pytest.mark.parametrize("case_name", _CASE_NAMES)
def test_kinematics_constructed_benchmark_not_a_reproduction(monkeypatch, case_name):
    """Constructed benchmark, not a reproduction: compare published red offsets."""
    case = next(case for case in _read_fixture()["cases"] if case["case"] == case_name)
    result = _screen_constructed_case(monkeypatch, case)
    # Note section 6: 0.5 s guards conversion and patching; calibration sets time.
    assert result.time_to_limit_s == pytest.approx(
        case["expected_time_to_limit"]["value"], abs=0.5)
    # Note section 6: drift-off status; published times have 1 s resolution.
    assert result.status == drift_off.DRIFT_OFF
    assert result.lead_time_margin_s == pytest.approx(
        case["expected_lead_time_margin"]["value"], abs=0.5)
    published_red_m = _metres(case["published_red_offset"])
    # Note section 6: one-sided shortfall tests the module's conservatism claim.
    assert result.point_of_disconnect_m < published_red_m
    # Note section 6: offsets resolve to 0.1 m / 1 ft; 2.9-4.6% observed
    # shortfall motivates 10%. The severe row is excluded independently;
    # its 9.5% shortfall would actually fit 10%, contrary to the note's wording.
    assert result.point_of_disconnect_m == pytest.approx(published_red_m, rel=0.10)


def test_placeholder_invariance_constructed_benchmark_not_a_reproduction(monkeypatch):
    """Constructed benchmark, not a reproduction: vary all unused placeholders."""
    for case in _read_fixture()["cases"]:
        first = _screen_constructed_case(monkeypatch, case, placeholder_scale=1.0)
        varied = _screen_constructed_case(monkeypatch, case, placeholder_scale=2.0)
        assert first.status == varied.status
        for field in ("time_to_limit_s", "lead_time_margin_s", "point_of_disconnect_m"):
            assert getattr(first, field) == getattr(varied, field)
