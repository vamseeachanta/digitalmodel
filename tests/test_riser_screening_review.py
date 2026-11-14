"""Report regressions from the second-provider review of the F201 workflow."""

import copy
import json
import math
from pathlib import Path

import pytest
import yaml

from digitalmodel.subsea.riser.riser_case_screening import run_riser_screening


def reject_nonfinite_constant(value):
    raise ValueError(f"Non-finite JSON number: {value}")


@pytest.mark.parametrize("diameter,thickness,smys,smts,pressure", [
    (0.5334, 0.022225, 552e6, 621e6, 200e6),
    (0.254, 0.008, 448e6, 531e6, 100e6),
])
def test_out_of_domain_report_is_strict_json_and_keeps_failed_verdict(
    tmp_path, diameter, thickness, smys, smts, pressure,
):
    cfg = {"riser_screening": {
        "code": "DNV-ST-F201", "output_dir": str(tmp_path), "label": "domain",
        "geometry": {"outer_diameter": diameter, "wall_thickness": thickness,
                     "corrosion_allowance": 0.003},
        "material": {"grade": "screening", "smys": smys, "smts": smts},
        "load_cases": [
            {"name": "within_domain", "internal_pressure": 1e6},
            {"name": "outside_domain", "internal_pressure": pressure},
        ],
    }}
    result = run_riser_screening(copy.deepcopy(cfg))["riser_screening"]["result"]
    report_file = tmp_path / "domain_results.json"
    before = report_file.read_bytes()
    saved = json.loads(before, parse_constant=reject_nonfinite_constant)
    assert saved["overall_status"] == "FAIL"
    assert saved["screening_status"] == "fail"
    assert saved["governing_case"] == "outside_domain"
    assert saved["governing_check"] == "combined"
    assert saved["max_utilisation"] == "Infinity"
    failed, finite = saved["cases"]
    assert failed["name"] == "outside_domain"
    assert failed["status"] == "FAIL"
    assert failed["checks"]["combined"] == "Infinity"
    assert failed["max_utilisation"] == "Infinity"
    combined = failed["details"]["combined"]
    assert combined["criterion_domain_exceeded"] is True
    assert combined["utilisation"] == "Infinity"
    assert combined["limit"] == 1.0
    assert failed["checks"]["pressure_containment"] > 1.0
    assert isinstance(finite["checks"]["combined"], float)
    assert finite["status"] == "PASS"
    # Numerical consumers retain the failure sentinel and ranking in memory.
    assert math.isinf(result["max_utilisation"])
    assert math.isinf(result["cases"][0]["checks"]["combined"])
    assert finite == result["cases"][1]
    note = (tmp_path / "domain_calcnote.md").read_text()
    assert "**Overall status:** FAIL" in note
    assert '"Infinity"' in note
    run_riser_screening(copy.deepcopy(cfg))
    assert report_file.read_bytes() == before


def test_f201_example_describes_its_screening_inputs():
    source = (Path(__file__).resolve().parents[1]
              / "examples/workflows/riser-screening-f201/input.yml")
    text = source.read_text()
    cfg = yaml.safe_load(text)["riser_screening"]
    assert "~1200 m water depth class" in text
    assert "~3000 m" not in text
    assert "rounded ksi" in text
    assert "not certified API 5L grade minima" in text
    assert cfg["material"]["grade"] == "X80-class screening (rounded ksi)"
    # Correct the description without changing the registered numerical case.
    assert cfg["material"]["smys"] == 552e6
    assert cfg["material"]["smts"] == 621e6
    assert cfg["load_cases"][0]["external_pressure"] == 12e6
