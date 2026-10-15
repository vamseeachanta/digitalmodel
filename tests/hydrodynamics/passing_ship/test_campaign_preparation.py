# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Exercise force preparation without a response solver or numerical plot data."""

import copy
from datetime import datetime
import hashlib
import json
from pathlib import Path
import subprocess
import sys

import pytest

ROOT = Path(__file__).resolve().parents[3]
DATA = ROOT / "docs/domains/passing_ship/preparation"


def api():
    from digitalmodel.hydrodynamics.passing_ship import preparation
    return preparation


def spec():
    from digitalmodel.hydrodynamics.passing_ship.input_schemas import PassingShipSpec
    return PassingShipSpec.model_validate(json.loads((DATA / "model.json").read_text()))


def test_model_and_all_matrix_entries_validate():
    model = spec()
    prepared = api().prepare_campaign(model, json.loads((DATA / "matrix.json").read_text()))
    assert len(prepared) == 18
    assert len({p.name for p in prepared}) == 18
    assert len(model.moored_vessel.mooring_lines) == 4


@pytest.mark.parametrize("field,value", [("water_depth", 1), ("angle_deg", 90), ("current_speed", 1)])
def test_unsupported_geometry_is_rejected(field, value):
    model = spec()
    if field == "angle_deg":
        model.passing_ship.track.angle_deg = value
    else:
        setattr(model.environment, field, value)
    with pytest.raises(ValueError):
        api().validate_model(model)


def test_duplicate_lines_rejected():
    model = spec()
    model.moored_vessel.mooring_lines.append(model.moored_vessel.mooring_lines[0])
    with pytest.raises(ValueError, match="unique"):
        api().validate_model(model)


def test_executor_honours_overrides_flags_and_wind_inputs():
    from digitalmodel.hydrodynamics.passing_ship.input_schemas import AnalysisCase
    model = spec()
    model.analysis_cases = [
        AnalysisCase(name="base"),
        AnalysisCase(name="faster", passing_speed_knots=model.passing_ship.track.speed_knots * 2),
        AnalysisCase(name="closer", separation_m=30),
        AnalysisCase(name="off", include_passing_ship=False),
        AnalysisCase(name="wind", include_passing_ship=False, include_wind=True),
    ]
    results = api().execute_cases(model, staggers_m=[-50, 0, 50], wind_loads={"wind": [1, 2, 3]})
    assert [r["name"] for r in results] == [c.name for c in model.analysis_cases]
    for component in ("surge_n", "sway_n", "yaw_nm"):
        assert results[1][component] == pytest.approx([v * 4 for v in results[0][component]])
        assert results[3][component] == [0, 0, 0]
    assert abs(results[2]["sway_n"][1]) > abs(results[0]["sway_n"][1])
    assert results[4]["sway_n"] == [2, 2, 2]
    assert results[0]["response_status"] == "not computed; response adapter required"
    assert model.passing_ship.track.separation_m == 40


def test_wind_request_without_loads_refused():
    model = spec()
    model.analysis_cases[0].include_wind = True
    with pytest.raises(ValueError, match="wind"):
        api().execute_cases(model, staggers_m=[0])


@pytest.mark.parametrize("speed", [0, float("nan"), float("inf")])
def test_invalid_matrix_speed_refused(speed):
    matrix = json.loads((DATA / "matrix.json").read_text())
    matrix["speed_knots"] = [speed]
    with pytest.raises(ValueError):
        api().prepare_campaign(spec(), matrix)


def test_pending_published_comparisons_refuse_qualification():
    records = json.loads((DATA / "benchmark_candidates.json").read_text())
    assert len(records["cases"]) == 6
    for record in records["cases"]:
        with pytest.raises(ValueError, match="tabulated"):
            api().compare_benchmark(record, [0, 0, 0])


def test_benchmark_tolerance_is_enforced():
    record = {"reference": [100, 0, 100], "relative_tolerance": 0.05, "absolute_tolerance": [1, 1, 1]}
    assert api().compare_benchmark(record, [104, 0, 100])["passed"]
    assert not api().compare_benchmark(record, [106, 0, 100])["passed"]
    bad = copy.deepcopy(record)
    bad["reference"][0] = float("nan")
    with pytest.raises(ValueError):
        api().compare_benchmark(bad, [104, 0, 100])


def test_launch_is_refused_even_with_a_digest():
    with pytest.raises(RuntimeError, match="qualified execution adapter"):
        api().launch_solver("0" * 64)


def test_no_licensed_interface_imported_in_fresh_process():
    code = """
import sys
from digitalmodel.hydrodynamics.passing_ship.preparation import validate_model
assert not any(n.lower().startswith(('orcfxapi', 'orcawave', 'ansys')) for n in sys.modules)
"""
    subprocess.run([sys.executable, "-c", code], check=True, capture_output=True, text=True)


def test_builder_reproducible_manifest_and_revision(tmp_path):
    from digitalmodel.hydrodynamics.passing_ship.preparation_report import build
    first = tmp_path / "first"
    second = tmp_path / "second"
    build(DATA, first)
    build(DATA, second)
    for path in first.iterdir():
        assert path.read_bytes() == (second / path.name).read_bytes()
    manifest = json.loads((first / "manifest.template.json").read_text())
    assert manifest["launch_allowed"] is False
    for name, digest in manifest["inputs"].items():
        assert hashlib.sha256((ROOT / name).read_bytes()).hexdigest() == digest
        assert manifest["input_provenance"][name]["sha256"] == digest
        assert manifest["input_provenance"][name]["source_url"]
        assert manifest["input_provenance"][name]["record_count"] > 0
    receipt = manifest["execution"]
    assert receipt["criteria_sha256"] == hashlib.sha256((DATA / "criteria.json").read_bytes()).hexdigest()
    assert datetime.fromisoformat(receipt["recorded_at_utc"]) < datetime.fromisoformat(receipt["first_run_started_at_utc"])
    assert (first / "manifest.sha256").read_text().split()[0] == hashlib.sha256((first / "manifest.template.json").read_bytes()).hexdigest()
    assert manifest["outputs"]["report.html"] == hashlib.sha256((first / "report.html").read_bytes()).hexdigest()
    page = (first / "report.html").read_text()
    assert page.index("Summary") < page.index("Revision history") < page.index("2. Scope")
    revisions = json.loads((DATA / "revisions.json").read_text())
    assert revisions["revision"] == revisions["history"][-1]["revision"]
    assert f'Revision: {revisions["history"][-1]["revision"]}' in page
    assert page.count('<section id="section-') == 10
    assert 'src=' not in page
    assert '<link' not in page
    assert manifest["qualified_benchmarks"] == 0
