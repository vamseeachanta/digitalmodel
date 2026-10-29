# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Exercise source-bound analytical checks without commercial bindings."""

import copy
import hashlib
import importlib.util
import json
import math
import shlex
import sys
from pathlib import Path
from types import SimpleNamespace

import pytest


ROOT = Path(__file__).resolve().parents[2]
SPEC = importlib.util.spec_from_file_location(
    "analytical_riser_regression",
    ROOT / "scripts/risers/analytical_riser_regression.py",
)
regression = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(regression)


@pytest.fixture
def sources(tmp_path):
    """Copy only pinned inputs to isolated scratch for negative probes."""
    for pin in regression.PINS.values():
        path = tmp_path / pin["path"]
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_bytes((ROOT / pin["path"]).read_bytes())
    return tmp_path


@pytest.fixture
def cases():
    blobs = regression.verify_sources(ROOT)
    return json.loads(blobs["FIXTURE"])["cases"]


@pytest.fixture
def triangle():
    """A vertex cable with H=4, V=3, T=5, w=1 and drop=1 in SI."""
    case = {"id": "synthetic", "type": "SCR", "values": {
        "water_depth": 1.0, "hang_off_elevation": 0.0,
        "angle": math.degrees(math.atan2(4, 3)),
    }}
    result = {"horizontal_N": 4.0, "vertical_N": 3.0, "tension_N": 5.0,
              "segments": [{"length_m": 3.0, "horizontal_m": 4 * math.asinh(0.75),
                            "radius_m": 4.0, "weight_N_per_m": 1.0}]}
    return case, result


def test_pinned_sources_and_nested_manifest(sources):
    blobs = regression.verify_sources(sources)
    assert len(blobs) == len(regression.PINS)


@pytest.mark.parametrize("kind", ["missing", "digest"])
def test_source_failure_precedes_replay(sources, kind):
    path = sources / regression.PINS["FIXTURE"]["path"]
    if kind == "missing":
        path.rename(path.with_suffix(".missing"))
    else:
        path.write_text("not valid JSON", encoding="utf-8", newline="\n")
    with pytest.raises(regression.EvidenceError, match="FIXTURE"):
        regression.verify_sources(sources)


def test_nested_manifest_disagreement_even_with_matching_outer_digest(sources, monkeypatch):
    pins = copy.deepcopy(regression.PINS)
    path = sources / pins["MANIFEST"]["path"]
    document = json.loads(path.read_text(encoding="utf-8"))
    document["files"][0]["sha256"] = "0" * 64
    path.write_text(json.dumps(document), encoding="utf-8", newline="\n")
    pins["MANIFEST"].update(sha256=hashlib.sha256(path.read_bytes()).hexdigest(),
                            bytes=path.stat().st_size)
    monkeypatch.setattr(regression, "PINS", pins)
    with pytest.raises(regression.EvidenceError, match="nested manifest"):
        regression.verify_sources(sources)


@pytest.mark.parametrize("mutation", ["field", "nan", "inf", "units", "duplicate",
                                    "prepared", "native", "family", "bool"])
def test_reject_malformed_or_unqualified_cases(cases, mutation):
    if mutation == "field":
        del cases[0]["values"]["angle"]
    elif mutation in {"nan", "inf", "bool"}:
        cases[0]["values"]["angle"] = {"nan": math.nan, "inf": math.inf, "bool": True}[mutation]
    elif mutation == "units":
        cases[0]["provenance"]["angle"]["units"] = "radians"
    elif mutation == "duplicate":
        cases[1]["id"] = cases[0]["id"]
    elif mutation == "family":
        cases[0]["type"] = "SLWR"
    else:
        cases[0]["qualification"] = mutation
    with pytest.raises(regression.EvidenceError):
        regression.validate_cases({"cases": cases})


def test_exact_family_membership(cases):
    regression.validate_cases({"cases": cases})
    assert [(c["id"], c["type"]) for c in cases] == [
        ("scr-shallow", "SCR"), ("scr-deep", "SCR"),
        ("slwr-shallow", "SLWR"), ("slwr-deep", "SLWR")]


def test_force_and_angle_units():
    assert regression.newtons_to_kn(2500) == 2.5
    assert regression.angle_from_vertical(4, 3) == pytest.approx(53.13010235415598)


@pytest.fixture
def wave_triangle(triangle):
    """Five arcs with rational heights and continuous interface forces."""
    case, result = triangle
    case["type"] = "SLWR"
    case["values"].update(water_depth=2.0, sag_elevation=1.0, hog_elevation=1.5)
    # cosh(asinh(3/4))=5/4; cosh(asinh(5/12))=13/12.
    # R*(cosh-1) gives drops [1, 1/3, 1/6, 1/2, 1] m.
    # Signed depth=2 m; sag=2-1=1 m; hog=1+1/3+1/6=1.5 m.
    # Rising V=1*(5/3)=2*(5/6)=5/3 N; falling V=2*(3/2)=1*3=3 N.
    radii, slopes, weights = [4, 4, 2, 2, 4], [3/4, 5/12, 5/12, 3/4, 3/4], [1, 1, -2, -2, 1]
    result["segments"] = [
        {"length_m": radius*slope, "horizontal_m": radius*math.asinh(slope),
         "radius_m": radius, "weight_N_per_m": weight}
        for radius, slope, weight in zip(radii, slopes, weights, strict=True)]
    return case, result, weights


def test_compensating_segment_lengths_cannot_pass(wave_triangle):
    case, result, weights = wave_triangle
    assert regression.aggregate(regression.check_geometry(case, result, weights)) == "PASS"
    result["segments"][0]["length_m"] += 1
    result["segments"][1]["length_m"] -= 1
    checks = regression.check_geometry(case, result, weights)
    assert checks["global_force"]["status"] == "PASS"
    assert checks["segment_1_arc"]["status"] == "FAIL"
    assert checks["segment_2_arc"]["status"] == "FAIL"
    assert regression.aggregate(checks) == "FAIL"


@pytest.mark.parametrize("index", range(5))
def test_each_segment_arc_is_checked(wave_triangle, index):
    case, result, weights = wave_triangle
    result["segments"][index]["length_m"] += 0.01
    checks = regression.check_geometry(case, result, weights)
    assert checks[f"segment_{index+1}_arc"]["status"] == "FAIL"


@pytest.mark.parametrize("elevation", ["sag_elevation", "hog_elevation"])
def test_stale_geometry_rejected_after_requested_elevation_changes(wave_triangle, elevation):
    case, result, weights = wave_triangle
    case["values"][elevation] += 0.01
    checks = regression.check_geometry(case, result, weights)
    assert checks["depth"]["status"] == "PASS"
    assert checks[elevation]["status"] == "FAIL"
    assert regression.aggregate(checks) == "FAIL"


def test_reconstructed_elevations_and_interfaces_match_hand_values(wave_triangle):
    case, result, weights = wave_triangle
    checks = regression.check_geometry(case, result, weights)
    # Hand calculations appear in wave_triangle, independently of solver output.
    assert checks["sag_elevation"]["actual"] == pytest.approx(1.0)
    assert checks["hog_elevation"]["actual"] == pytest.approx(1.5)
    assert checks["rising_interface"]["actual"] == pytest.approx(5/3)
    assert checks["falling_interface"]["actual"] == pytest.approx(3.0)
    assert regression.aggregate(checks) == "PASS"


@pytest.mark.parametrize(("index", "criterion"), [(1, "rising_interface"), (3, "falling_interface")])
def test_each_interface_rejects_discontinuous_geometry(wave_triangle, index, criterion):
    case, result, weights = wave_triangle
    segment = result["segments"][index]
    segment["horizontal_m"] += 0.01
    segment["length_m"] = segment["radius_m"]*math.sinh(segment["horizontal_m"]/segment["radius_m"])
    checks = regression.check_geometry(case, result, weights)
    assert checks[f"segment_{index+1}_arc"]["status"] == "PASS"
    assert checks[criterion]["status"] == "FAIL"


@pytest.mark.parametrize("path", [
    "scripts/risers/analytical_riser_regression.py",
    "docs/audits/analytical-riser-regression.json",
    "docs/audits/analytical-riser-regression.html",
    "tests/subsea/test_analytical_riser_regression.py",
])
def test_regression_changes_select_covered_ci_domain(path):
    spec = importlib.util.spec_from_file_location(
        "riser_domain_detector", ROOT / "scripts/ci/detect_touched_domains.py")
    detector = importlib.util.module_from_spec(spec)
    sys.modules[spec.name] = detector
    spec.loader.exec_module(detector)
    domains = detector.parse_domains(ROOT / "tests/DOMAINS.md")
    assert {d.name for d in detector.touched_domains([path], domains)} == {"subsea"}


def test_existing_subsea_gate_collects_regression_suite():
    import yaml

    gates = yaml.safe_load((ROOT / ".claude/quality-gates.yaml").read_text(encoding="utf-8"))["gates"]
    gate = gates["tests-subsea"]
    path = Path(__file__).relative_to(ROOT).as_posix()
    assert any(path.startswith(root) for root in gate["test_roots"])
    assert "tests/subsea/" in shlex.split(gate["command"])


def test_output_junction_is_rejected_before_writes(replay, tmp_path, monkeypatch):
    monkeypatch.setattr(Path, "is_junction", lambda path: path.name.endswith(".json"), raising=False)
    with pytest.raises(regression.EvidenceError, match="symlinks|junctions"):
        regression.write_reports(replay, ROOT, tmp_path / "output", metadata_only=True)
    assert not (tmp_path / "output").exists()


def test_relative_denominator_and_missing_zero_rule():
    assert regression.metric(105, 100, 0.05, "1", relative=True)["status"] == "PASS"
    assert regression.metric(100, 105, 0.0477, "1", relative=True)["status"] == "PASS"
    assert regression.metric(1, 0, 0.05, "1", relative=True)["status"] == "BLOCKED-IF-MISSING"
    assert regression.metric(0, 0, 0.05, "1", relative=True)["status"] == "BLOCKED-IF-MISSING"
    assert regression.metric(0.1, 0, 0.05, "m", relative=True,
                             zero_absolute=0.2)["status"] == "PASS"


@pytest.mark.parametrize("criterion", ["depth", "angle", "global_force", "tension"])
def test_each_assumed_criterion_can_fail_independently(triangle, criterion):
    case, result = triangle
    baseline = regression.check_geometry(case, result, [1.0])
    assert regression.aggregate(baseline) == "PASS"
    if criterion == "depth":
        case["values"]["water_depth"] += 2e-6
    elif criterion == "angle":
        case["values"]["angle"] += 2e-9
    elif criterion == "global_force":
        result["segments"][0]["length_m"] += 1e-9
    else:
        result["tension_N"] += 1e-9
    checks = regression.check_geometry(case, result, [1.0])
    assert checks[criterion]["status"] == "FAIL"
    assert all(v["status"] == "PASS" for k, v in checks.items() if k != criterion)


def test_consistently_scaled_solver_weights_and_forces_fail(triangle):
    case, result = triangle
    for key in ("horizontal_N", "vertical_N", "tension_N"):
        result[key] *= 2
    result["segments"][0]["weight_N_per_m"] *= 2
    checks = regression.check_geometry(case, result, [1.0])
    assert checks["global_force"]["status"] == "FAIL"
    assert checks["segment_weights"]["status"] == "FAIL"


@pytest.mark.parametrize("key,increment", [("top_tension_kN", 6.0),
                                          ("top_angle_deg_from_vertical", 1.1),
                                          ("touchdown_horizontal_distance_from_top_m", 6.0)])
def test_each_closed_comparison_band(key, increment):
    expected = {"top_tension_kN": 100.0, "top_angle_deg_from_vertical": 10.0,
                "touchdown_horizontal_distance_from_top_m": 100.0}
    actual = dict(expected)
    actual[key] += increment
    checks = regression.check_closed(actual, expected)
    assert checks[key]["status"] == "FAIL"
    assert sum(v["status"] == "FAIL" for v in checks.values()) == 1


def test_aggregate_never_trusts_pass_flag():
    assert regression.aggregate({"overall_pass": True, "depth": {"status": "FAIL"}}) == "FAIL"
    assert regression.aggregate({}) == "BLOCKED-IF-MISSING"


def test_nonfinite_result_cannot_pass(triangle):
    case, result = triangle
    result["horizontal_N"] = math.nan
    with pytest.raises(regression.EvidenceError):
        regression.check_geometry(case, result, [1.0])


@pytest.mark.parametrize("escape", ["repo", "outside", "traversal", "symlink", "file_symlink"])
def test_full_payload_rejects_destination_before_writes(tmp_path, monkeypatch, escape):
    root, scratch = tmp_path / "repo", tmp_path / "scratch"
    root.mkdir()
    scratch.mkdir()
    monkeypatch.setenv("TMPDIR", str(scratch))
    destination = {"repo": root, "outside": tmp_path / "outside",
                   "traversal": scratch / ".." / "outside"}.get(escape, scratch / "link")
    if escape == "symlink":
        destination.symlink_to(root, target_is_directory=True)
    if escape == "file_symlink":
        destination.mkdir()
        (destination / "analytical-riser-regression-readback.json").symlink_to(root / "sentinel")
    sentinel = root / "sentinel"
    sentinel.write_text("preserve", encoding="utf-8", newline="\n")
    with pytest.raises(regression.EvidenceError):
        regression.write_reports({}, root, destination, metadata_only=False)
    assert sentinel.read_text(encoding="utf-8") == "preserve"
    assert sorted(p.name for p in root.iterdir()) == ["sentinel"]


@pytest.fixture(scope="module")
def replay():
    return regression.run(ROOT)


def test_real_replay_counts_and_source_preservation(replay):
    assert replay["counts"] == {"assumed_attempted": 4, "assumed_completed": 4,
                                "assumed_passed": 4, "comparisons_attempted": 2,
                                "comparisons_completed": 2, "comparisons_passed": 2,
                                "native_runs": 0, "qualified_results": 0}
    assert replay["status"] == "PASS"
    assert len(replay["blockers"]) == 3
    assert all(b["status"] == "BLOCKED-IF-MISSING" for b in replay["blockers"])
    regression.verify_sources(ROOT)


def test_metadata_whitelist_and_saved_readback(replay, tmp_path):
    metadata = regression.metadata(replay)

    def keys(value):
        if isinstance(value, dict):
            return set(value).union(*(keys(item) for item in value.values()))
        if isinstance(value, list):
            return set().union(*(keys(item) for item in value))
        return set()

    assert keys(metadata).isdisjoint({"values", "residual", "horizontal_N", "expected",
                                    "inputs", "readback", "mapping_defaults"})
    regression.write_reports(replay, ROOT, tmp_path, metadata_only=True)
    document = json.loads((tmp_path / "analytical-riser-regression.json").read_text(encoding="utf-8"))
    assert document == metadata
    html = (tmp_path / "analytical-riser-regression.html").read_text(encoding="utf-8")
    assert html.index("Summary") < html.index("Revision history") < html.index("Criteria")
    assert "BLOCKED-IF-MISSING" in html
    assert not (tmp_path / "analytical-riser-regression-readback.json").exists()


def test_deterministic_replay(replay):
    assert regression.metadata(regression.run(ROOT)) == regression.metadata(replay)


def test_full_readback_retains_results_only_in_scratch(replay, tmp_path, monkeypatch):
    monkeypatch.setenv("TMPDIR", str(tmp_path))
    regression.write_reports(replay, ROOT, tmp_path / "full", metadata_only=False)
    saved = json.loads((tmp_path / "full/analytical-riser-regression-readback.json").read_text(encoding="utf-8"))
    assert saved == replay
    assert all("readback" in row and "inputs" in row for row in saved["assumed"] + saved["comparisons"])


def test_missing_interface_cannot_admit_unexecuted_cases(monkeypatch):
    monkeypatch.setattr(regression, "load_interface", lambda *args: object())
    with pytest.raises(regression.EvidenceError, match="interface missing"):
        regression.run(ROOT)


def test_failed_comparison_remains_failed(monkeypatch):
    real_check = regression.check_closed

    def altered_check(actual, expected):
        actual = dict(actual)
        actual["top_tension_kN"] *= 2
        return real_check(actual, expected)

    monkeypatch.setattr(regression, "check_closed", altered_check)
    replay = regression.run(ROOT)
    assert replay["status"] == "FAIL"
    assert replay["counts"]["comparisons_completed"] == 2
    assert replay["counts"]["comparisons_passed"] == 0


def test_committed_metadata_matches_generator(replay):
    public = regression.metadata(replay)
    rebuild = "Rebuild: uv run --no-project python scripts/risers/analytical_riser_regression.py --root . --metadata-only --out-dir docs/audits"
    assert json.loads((ROOT / "docs/audits/analytical-riser-regression.json").read_text(encoding="utf-8")) == public, rebuild
    assert (ROOT / "docs/audits/analytical-riser-regression.html").read_text(encoding="utf-8") == regression.render_html(public), rebuild


def test_source_history_identity_survives_deliverable_commit(monkeypatch, tmp_path):
    calls = []
    head, source_commit = "a" * 40, "c" * 40
    for key in ("GIT_DIR", "GIT_WORK_TREE", "GIT_COMMON_DIR"):
        monkeypatch.setenv(key, str(tmp_path / key.lower()))

    def git_read(command, **options):
        calls.append(command)
        assert not {"GIT_DIR", "GIT_WORK_TREE", "GIT_COMMON_DIR"} & set(options["env"])
        return SimpleNamespace(stdout=(source_commit if "log" in command else head) + "\n")

    monkeypatch.setattr(regression.subprocess, "run", git_read)
    before = regression.source_revision(ROOT)
    head = "b" * 40
    after = regression.source_revision(ROOT)
    assert before == after == source_commit
    assert all(pin["path"] in calls[0] for pin in regression.PINS.values())
    assert "scripts/risers/analytical_riser_regression.py" not in calls[0]
