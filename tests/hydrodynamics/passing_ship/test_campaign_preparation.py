# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Exercise force preparation without a response solver or numerical plot data."""

import copy
from datetime import datetime
import hashlib
import json
import math
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
    prepared = api().prepare_campaign(model, json.loads((DATA / "matrix.json").read_text()),
                                      mooring_layout=json.loads((DATA / "model.json").read_text())["mooring_layout"])
    assert len(prepared) == 18
    assert len({p.name for p in prepared}) == 18
    assert len(model.moored_vessel.mooring_lines) == 4
    record = json.loads((DATA / "model.json").read_text())
    from digitalmodel.marine_ops.marine_analysis.catenary.simplified import SimplifiedCatenarySolver
    solver = SimplifiedCatenarySolver()
    for case in prepared:
        for line in case.moored_vessel.mooring_lines:
            sized = solver.solve_from_force(600000, record["mooring_layout"]["submerged_weight_n_m"],
                                            case.environment.water_depth)
            assert line.pretension == 600
            assert line.length == pytest.approx(sized.arc_length)
            reach = math.hypot(line.anchor_x - line.fairlead_x, line.anchor_y - line.fairlead_y)
            assert reach == pytest.approx(sized.horizontal_distance)
            assert line.length - reach < case.environment.water_depth
            assert abs(line.anchor_y) < 30 - case.passing_ship.dimensions.beam / 2


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


def test_candidate_tolerances_cannot_override_frozen_criteria():
    record = {"reference": [1, 0, 1], "relative_tolerance": 1,
              "absolute_tolerance": [100, 100, 100]}
    assert not api().compare_benchmark(record, [1.06, 0, 1])["passed"]
    criteria = json.loads((DATA / "criteria.json").read_text())
    assert criteria["quantities"] == ["Cx", "Cy", "Cm"]
    assert criteria["definition"]["reference_series"] == "model tests"
    assert all(value.startswith("positive maximum") for value in criteria["definition"]["peak_sign"].values())
    assert criteria["definition"]["stagger_window"]["x_over_L_min"] == "not established"
    basis = criteria["absolute_floor_basis"]
    force_scale = 0.5 * basis["rho_kg_m3"] * basis["draft_m"] * basis["length_m"] * basis["speed_ms"] ** 2
    assert criteria["absolute_tolerance"] == pytest.approx([
        basis["force_floor_n"] / force_scale, basis["force_floor_n"] / force_scale,
        basis["moment_floor_nm"] / (force_scale * basis["length_m"])])
    assert api().compare_benchmark(record, [1.04, 0, 1])["passed"]
    assert api().compare_benchmark(record, [1, criteria["absolute_tolerance"][1], 1])["passed"]
    assert not api().compare_benchmark(record, [1, 2 * criteria["absolute_tolerance"][1], 1])["passed"]
    candidates = json.loads((DATA / "benchmark_candidates.json").read_text())
    assert all("relative_tolerance" not in row and "absolute_tolerance" not in row
               for row in candidates["cases"])
    receipt = json.loads((DATA / "execution.json").read_text())
    assert receipt["criteria_revision"] == "3"
    assert receipt["first_benchmark_comparison_started_at_utc"] is None
    assert receipt["benchmark_comparison_status"] == "not_run"
    assert receipt["previous_receipts"][0]["criteria_sha256"] != receipt["criteria_sha256"]


@pytest.mark.parametrize("extra_depths", [1, 2])
def test_excess_line_length_at_or_above_depth_refused(extra_depths):
    model = spec()
    line = model.moored_vessel.mooring_lines[0]
    reach = math.hypot(line.anchor_x - line.fairlead_x, line.anchor_y - line.fairlead_y)
    line.length = reach + extra_depths * model.environment.water_depth
    with pytest.raises(ValueError, match="excess line length"):
        api().validate_model(model)


@pytest.mark.parametrize("case_override", [False, True])
def test_track_over_line_footprint_refused(case_override):
    model = spec()
    separation = 30 if case_override else model.passing_ship.track.separation_m
    if case_override:
        model.passing_ship.track.separation_m = 60
        model.analysis_cases[0].separation_m = separation
    line = model.moored_vessel.mooring_lines[0]
    line.anchor_y = separation + model.passing_ship.dimensions.beam
    reach = math.hypot(line.anchor_x - line.fairlead_x, line.anchor_y - line.fairlead_y)
    line.length = math.hypot(reach, model.environment.water_depth)
    with pytest.raises(ValueError, match="track over a line footprint"):
        api().validate_model(model)


def test_track_over_anchor_refused():
    model = spec()
    line = model.moored_vessel.mooring_lines[0]
    line.anchor_y = model.passing_ship.track.separation_m - model.passing_ship.dimensions.beam / 2
    reach = math.hypot(line.anchor_x - line.fairlead_x, line.anchor_y - line.fairlead_y)
    line.length = math.hypot(reach, model.environment.water_depth)
    with pytest.raises(ValueError, match="track over an anchor"):
        api().validate_model(model)


def assert_committed_outputs(fresh, committed):
    """Compare every delivered byte, including the manifest and its digest."""
    for name in ("report.html", "force_samples.json", "campaign.json",
                 "manifest.template.json", "manifest.sha256"):
        assert (fresh / name).read_bytes() == (committed / name).read_bytes(), name


def test_fresh_build_matches_committed_outputs(tmp_path):
    from digitalmodel.hydrodynamics.passing_ship.preparation_report import build
    build(DATA, tmp_path)
    assert_committed_outputs(tmp_path, DATA)
