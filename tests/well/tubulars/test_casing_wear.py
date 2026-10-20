"""Inspection arithmetic and fail-closed worn-casing assessment tests.

All numbers are synthetic inputs, not material properties or standard limits.
No published damaged-casing golden benchmark is claimed.
"""

import copy
import json
import os
from pathlib import Path
import subprocess
import sys

import pytest
import yaml

from digitalmodel.well.tubulars.casing_wear import assess_casing_wear
from digitalmodel.well.tubulars.casing_wear_report import render_report


@pytest.fixture
def inspection():
    return {
        "source_ref": "synthetic-inspection",
        "observed_at": "2026-10-01T12:00:00Z",
        "log_kind": "ultrasonic_wall",
        "damage": "uniform_corrosion",
        "od_mm": 200.0,
        "nominal_wall_mm": 10.0,
        "yield_strength_mpa": 500.0,
        "measurement_uncertainty_mm": 0.2,
        "measured_ovality": 0.01,
        "samples": [
            {"depth_m": 100.0, "azimuth_deg": 0.0, "value_mm": 8.0},
            {"depth_m": 100.0, "azimuth_deg": 90.0, "value_mm": 7.0},
        ],
        "load_cases": [
            {
                "name": "injection",
                "internal_pressure_mpa": 20.0,
                "external_pressure_mpa": 5.0,
                "axial_force_kn": 100.0,
                "temperature_c": 80.0,
                "burst_design_factor": 1.2,
                "collapse_design_factor": 1.3,
            },
            {
                "name": "evacuated",
                "internal_pressure_mpa": 0.0,
                "external_pressure_mpa": 15.0,
                "axial_force_kn": -50.0,
                "temperature_c": 40.0,
                "burst_design_factor": 1.1,
                "collapse_design_factor": 1.4,
            },
        ],
    }


def test_wall_arithmetic_and_load_differentials(inspection):
    """Hand calculation: min(8,7)=7; 7-0.2=6.8 mm.

    Injection: max(20-5,0)=15 MPa burst, max(5-20,0)=0 collapse.
    Evacuated: max(0-15,0)=0 burst, max(15-0,0)=15 MPa collapse.
    These are measurement bookkeeping and pressure differences only.
    """
    result = assess_casing_wear(inspection)
    wall = result["inspection_summary"]
    assert wall["minimum_observed_wall_mm"] == 7.0
    assert wall["lower_wall_bound_mm"] == pytest.approx(6.8)
    assert wall["equivalent_wall_mm"] is None
    assert result["load_cases"][0]["burst_differential_mpa"] == 15.0
    assert result["load_cases"][0]["collapse_differential_mpa"] == 0.0
    assert result["load_cases"][1]["collapse_differential_mpa"] == 15.0
    assert result["inputs"] == inspection


def test_all_capacity_results_remain_unevaluated(inspection):
    result = assess_casing_wear(inspection)
    assert result["status"] == "NOT_EVALUATED"
    assert result["validation"]["api_example_properties"] == "NOT_EVALUATED"
    for case in result["load_cases"]:
        for check in case["checks"]:
            assert check["status"] == "NOT_EVALUATED"
            assert check["value"] is None
            assert check["missing_evidence"]
            assert check["sources"]
    assert {c["id"] for c in result["load_cases"][0]["checks"]} == {
        "wall_equivalence",
        "collapse",
        "burst",
        "combined_loads",
        "design_factors",
    }


@pytest.mark.parametrize("damage", ["uniform_corrosion", "wear_groove"])
def test_damage_description_never_qualifies_wall(inspection, damage):
    inspection["damage"] = damage
    result = assess_casing_wear(inspection)
    assert result["inspection_summary"]["equivalent_wall_mm"] is None
    assert result["status"] == "NOT_EVALUATED"


def test_caliper_does_not_become_wall(inspection):
    inspection["log_kind"] = "caliper_internal_radius"
    inspection["samples"][0]["value_mm"] = 92.0
    inspection["samples"][1]["value_mm"] = 93.0
    result = assess_casing_wear(inspection)
    assert result["inspection_summary"]["minimum_observed_wall_mm"] is None
    assert result["inspection_summary"]["lower_wall_bound_mm"] is None
    assert "outer-surface" in result["inspection_summary"]["limitations"][0]


def test_bound_below_zero_is_preserved_not_clipped(inspection):
    """Hand calculation: 7-8=-1 mm; no positive ligament is established."""
    inspection["measurement_uncertainty_mm"] = 8.0
    result = assess_casing_wear(inspection)
    assert result["inspection_summary"]["lower_wall_bound_mm"] == -1.0
    assert "positive wall" in " ".join(result["inspection_summary"]["limitations"])


@pytest.mark.parametrize(
    "field,value",
    [
        ("od_mm", 0),
        ("od_mm", float("inf")),
        ("nominal_wall_mm", 100),
        ("yield_strength_mpa", float("nan")),
        ("yield_strength_mpa", True),
        ("measurement_uncertainty_mm", -1),
        ("measured_ovality", -0.1),
        ("observed_at", "yesterday"),
        ("observed_at", "2026-10-01T12:00:00"),
        ("source_ref", ""),
        ("samples", []),
        ("load_cases", []),
        ("log_kind", "nominal_wall"),
        ("damage", "crack"),
    ],
)
def test_invalid_inspection_is_rejected(inspection, field, value):
    inspection[field] = value
    with pytest.raises(ValueError):
        assess_casing_wear(inspection)


@pytest.mark.parametrize(
    "field,value",
    [
        ("depth_m", -1),
        ("azimuth_deg", 360),
        ("value_mm", 0),
        ("value_mm", float("nan")),
        ("value_mm", 100),
    ],
)
def test_invalid_sample_is_rejected(inspection, field, value):
    inspection["samples"][0][field] = value
    with pytest.raises(ValueError):
        assess_casing_wear(inspection)


@pytest.mark.parametrize(
    "field,value",
    [
        ("burst_design_factor", 0),
        ("collapse_design_factor", -1),
        ("internal_pressure_mpa", -1),
        ("external_pressure_mpa", float("inf")),
        ("axial_force_kn", float("nan")),
        ("temperature_c", False),
        ("name", ""),
    ],
)
def test_invalid_case_is_rejected(inspection, field, value):
    inspection["load_cases"][0][field] = value
    with pytest.raises(ValueError):
        assess_casing_wear(inspection)


def test_missing_and_unknown_fields_are_rejected(inspection):
    del inspection["measured_ovality"]
    with pytest.raises(ValueError, match="measured_ovality"):
        assess_casing_wear(inspection)
    inspection["measured_ovality"] = 0
    inspection["units"] = "inches"
    with pytest.raises(ValueError, match="units"):
        assess_casing_wear(inspection)


def test_reproducible_detached_result_and_digest(inspection):
    original = copy.deepcopy(inspection)
    first = assess_casing_wear(inspection)
    assert assess_casing_wear(inspection) == first
    assert inspection == original
    inspection["samples"][0]["value_mm"] = 6.0
    second = assess_casing_wear(inspection)
    assert first["inputs"] == original
    assert second["input_sha256"] != first["input_sha256"]


def test_report_preserves_limits_and_escapes_input(inspection):
    inspection["source_ref"] = "<script>example</script>"
    report = render_report(assess_casing_wear(inspection))
    assert "<script>example</script>" not in report
    assert "&lt;script&gt;example&lt;/script&gt;" in report
    assert report.index("Summary") < report.index("Revision history")
    assert report.index("<h2>Revision history") < report.index("<h2>Inspection")
    assert "NOT_EVALUATED" in report
    assert "No operating pressure" in report
    assert "API TR 5C3" in report


def test_workflow_cli_and_registry(inspection, tmp_path):
    root = Path(__file__).resolve().parents[3]
    registry = yaml.safe_load((root / "docs/registry/workflows.yaml").read_text())
    (row,) = [
        r for r in registry["workflows"] if r["id"] == "casing-wear-remaining-capacity"
    ]
    assert row["runtime"] == "standalone"
    assert row["status"] == "experimental"
    input_file = tmp_path / "input.json"
    input_file.write_text(json.dumps(inspection))
    output_dir = tmp_path / "output"
    command = [
        sys.executable,
        "-m",
        row["basename"],
        str(input_file),
        "--output-directory",
        str(output_dir),
    ]
    env = {
        **os.environ,
        "PYTHONPATH": str(root / "src"),
        "PYTHONDONTWRITEBYTECODE": "1",
    }
    completed = subprocess.run(command, env=env, capture_output=True, text=True)
    assert completed.returncode == 0, completed.stderr
    saved = json.loads((output_dir / "assessment.json").read_text())
    assert saved == assess_casing_wear(inspection)
    assert (output_dir / "analysis.html").read_text() == render_report(saved)
    # Existing results cannot be replaced by a rerun.
    repeated = subprocess.run(command, env=env, capture_output=True, text=True)
    assert repeated.returncode != 0
    assert json.loads((output_dir / "assessment.json").read_text()) == saved


def test_documented_worked_example():
    """Independent hand calculation from the documented synthetic inputs.

    min(8,7)-0.2 = 6.8 mm. Burst differential = 20-5 = 15 MPa;
    collapse differential = max(5-20,0) = 0 MPa. These values cannot
    establish equivalent wall or capacity, so each check stays unevaluated.
    """
    root = Path(__file__).resolve().parents[3]
    guide = (root / "docs/domains/well/casing-wear-remaining-capacity.md").read_text()
    example = json.loads(guide.split("```json\n")[1].split("```", 1)[0])
    result = assess_casing_wear(example)
    assert result["inspection_summary"]["lower_wall_bound_mm"] == pytest.approx(6.8)
    assert result["load_cases"][0]["burst_differential_mpa"] == 15.0
    assert result["load_cases"][0]["collapse_differential_mpa"] == 0.0
    assert result["status"] == "NOT_EVALUATED"


def test_duplicate_case_name_rejected(inspection):
    inspection["load_cases"][1]["name"] = "injection"
    with pytest.raises(ValueError, match="unique"):
        assess_casing_wear(inspection)


def test_nominal_ratings_cannot_qualify_damaged_casing(inspection, monkeypatch):
    from digitalmodel.well.tubulars import casing as catalog_entry
    import importlib

    nominal = importlib.import_module(catalog_entry.__module__)

    def forbidden(*args, **kwargs):
        pytest.fail("Nominal ratings cannot establish damaged-casing capacity")

    monkeypatch.setattr(nominal, "collapse_pressure", forbidden)
    monkeypatch.setattr(nominal, "internal_yield_pressure", forbidden)
    result = assess_casing_wear(inspection)
    for case in result["load_cases"]:
        for check in case["checks"]:
            assert check["status"] == "NOT_EVALUATED"
            assert set(check["sources"]) <= result["sources"].keys()
