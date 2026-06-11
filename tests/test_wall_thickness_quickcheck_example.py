# ABOUTME: Tests the wall-thickness quick-check example (deckhand#170/#227).
# ABOUTME: Exercises sweep cache, arrestor branch selection, and offline HTML artifact.
"""Tests for the wall-thickness quick-check example.

Verifies the Deckhand "quick calc" EXECUTE artifact at
``examples/structural/wall_thickness_quickcheck/quick_check.py``:

- ``--from-cache`` runs with no engine call and no network (offline determinism).
- The committed cache fixture matches a fresh ``--compute`` run (the engine and
  the cached numbers agree — the fixture is not stale).
- The single stable command prints a PASS summary and an ``ARTIFACT:`` line
  pointing at a self-contained Plotly HTML report that is actually written.
"""

import importlib.util
import json
import subprocess
import sys
from pathlib import Path

import pytest

REPO_ROOT = Path(__file__).resolve().parents[1]
EXAMPLE_DIR = REPO_ROOT / "examples" / "structural" / "wall_thickness_quickcheck"
SCRIPT = EXAMPLE_DIR / "quick_check.py"
CACHE = EXAMPLE_DIR / "data" / "quickcheck_cache.json"


def _load_module():
    spec = importlib.util.spec_from_file_location("wt_quickcheck", SCRIPT)
    module = importlib.util.module_from_spec(spec)
    # Register before exec so the @dataclass decorator can resolve annotations.
    sys.modules["wt_quickcheck"] = module
    spec.loader.exec_module(module)
    return module


# ---------------------------------------------------------------------------
# Fixture / file presence
# ---------------------------------------------------------------------------

def test_example_files_exist():
    assert SCRIPT.exists(), f"missing example script: {SCRIPT}"
    assert CACHE.exists(), f"missing committed cache fixture: {CACHE}"


def test_cache_fixture_is_valid_json_with_two_codes():
    payload = json.loads(CACHE.read_text(encoding="utf-8"))
    assert payload["schema_version"] == 3
    assert payload["case"]["codes"] == ["DNV-ST-F101", "API-RP-1111"]
    assert payload["case"]["pressure_basis"] == "design"
    assert payload["case"]["buckle_arrestors"] == "with"
    assert payload["sweep"]
    assert payload["standard_walls"]
    assert {"with_arrestors", "without_arrestors"} <= set(payload["selection"])


@pytest.mark.unit
def test_headline_rule_selects_smallest_passing_standard_wall():
    module = _load_module()
    payload = module.run_from_cache()

    # With the corrected DNV collapse solver (see
    # tests/test_wall_thickness_collapse_solver.py) and per-check load
    # premises, the collapse-governed minimum is 14.91 mm; SCH 60 (14.27)
    # just misses and SCH 80 (17.475 mm / 0.688 in) is the first passing
    # standard wall. DNV collapse governs (U≈0.63).
    with_arrestors = payload["selection"]["with_arrestors"]
    selected_wall = with_arrestors["selected_standard_wall_mm"]
    assert selected_wall == pytest.approx(17.475, abs=0.01)
    assert with_arrestors["selected_standard_wall_in"] == pytest.approx(0.688, abs=0.001)
    assert with_arrestors["governing_check"] == "DNV-ST-F101 collapse"

    all_standard_walls = sorted(w["wall_mm"] for w in payload["standard_walls"])
    smaller_standard_walls = [wall for wall in all_standard_walls if wall < selected_wall]
    assert smaller_standard_walls
    for wall in smaller_standard_walls:
        point = module.find_sweep_point(payload, wall)
        assert not module.point_passes(point, include_propagation=False)

    point = module.find_sweep_point(payload, selected_wall)
    assert module.point_passes(point, include_propagation=False)
    assert not module.point_passes(point, include_propagation=True)


@pytest.mark.unit
def test_without_arrestors_branch_remains_visible_and_propagation_governed():
    payload = _load_module().run_from_cache()
    without_arrestors = payload["selection"]["without_arrestors"]

    assert without_arrestors["selected_standard_wall_mm"] == pytest.approx(28.575, abs=0.001)
    assert without_arrestors["selected_standard_wall_in"] == pytest.approx(1.125, abs=0.001)
    assert "propagation" in without_arrestors["governing_check"]
    assert 25.0 <= without_arrestors["nonstandard_minimum_wall_mm"] <= 26.0


@pytest.mark.unit
def test_buckle_arrestor_sizing_uses_selected_pipe_and_crossover_pressure():
    payload = _load_module().run_from_cache()
    sizing = payload["buckle_arrestor_sizing"]

    assert sizing["pipe_wall_mm"] == pytest.approx(
        payload["selection"]["with_arrestors"]["selected_standard_wall_mm"], abs=0.001
    )
    assert sizing["arrestor_wall_mm"] > sizing["pipe_wall_mm"]
    assert sizing["arrestor_length_m"] > 0.6
    assert sizing["governing_code"] == "DNV-ST-F101"
    assert sizing["crossover_pressure_mpa"] >= payload["external_pressure_mpa"]


# ---------------------------------------------------------------------------
# --from-cache mode: offline, deterministic, end-to-end via the stable command
# ---------------------------------------------------------------------------

@pytest.mark.unit
def test_from_cache_runs_offline_and_writes_artifact(tmp_path):
    """The stable allowlist command runs with no network and emits the report."""
    out_html = tmp_path / "report.html"
    result = subprocess.run(
        [
            sys.executable,
            str(SCRIPT),
            "--from-cache",
            "--output",
            str(out_html),
        ],
        cwd=str(REPO_ROOT),
        env={"PYTHONPATH": str(REPO_ROOT / "src")},
        capture_output=True,
        text=True,
        timeout=120,
    )
    assert result.returncode == 0, result.stderr
    stdout = result.stdout

    # Short text summary on stdout.
    assert "Pipeline Wall Thickness" in stdout
    assert "DNV-ST-F101" in stdout
    assert "API-RP-1111" in stdout
    assert "Headline wall" in stdout
    assert "Without arrestors" in stdout

    # Artifact line points at a real, self-contained HTML report.
    artifact_lines = [ln for ln in stdout.splitlines() if ln.startswith("ARTIFACT: ")]
    assert len(artifact_lines) == 1
    artifact_path = Path(artifact_lines[0].split("ARTIFACT: ", 1)[1].strip())
    assert artifact_path == out_html.resolve()
    assert out_html.exists()

    html = out_html.read_text(encoding="utf-8")
    assert "<!DOCTYPE html>" in html
    assert "X65" in html  # inputs echoed for traceability
    assert "Utilization Sweep" in html
    assert "Selected Wall Utilizations" in html
    assert "Propagation Buckling Branch" in html
    assert "Buckle Arrestor Sizing" in html
    # Inline Plotly: self-contained and large enough to carry plotly.js.
    assert out_html.stat().st_size > 1_000_000
    # Offline: no external resource fetches.
    for token in ("src=\"http", "<script src", "rel=\"stylesheet\" href=\"http"):
        assert token not in html


# ---------------------------------------------------------------------------
# Determinism: committed cache matches a fresh engine compute
# ---------------------------------------------------------------------------

@pytest.mark.unit
def test_cache_matches_fresh_compute():
    """The fixture is not stale: --compute reproduces the cached numbers."""
    module = _load_module()
    fresh = module.run_compute()
    cached = module.run_from_cache()

    assert fresh["selection"] == cached["selection"]
    assert fresh["sweep"] == cached["sweep"]
    assert fresh["buckle_arrestor_sizing"] == cached["buckle_arrestor_sizing"]


@pytest.mark.unit
def test_from_cache_is_deterministic():
    """Two cache loads yield identical numbers."""
    module = _load_module()
    a = module.run_from_cache()
    b = module.run_from_cache()
    assert a == b
