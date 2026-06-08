# ABOUTME: Tests the wall-thickness multi-code quick-check example (deckhand#170).
# ABOUTME: Exercises --from-cache mode offline: stable command, deterministic output, HTML artifact.
"""Tests for the wall-thickness quick-check example.

Verifies the Deckhand "quick calc" EXECUTE artifact at
``examples/structural/wall_thickness_quickcheck/quick_check.py``:

- ``--from-cache`` runs with no engine call and no network (offline determinism).
- The committed cache fixture matches a fresh ``--compute`` run (the engine and
  the cached numbers agree — the fixture is not stale).
- The single stable command prints a PASS summary and an ``ARTIFACT:`` line
  pointing at a self-contained HTML report that is actually written.
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
    assert payload["case"]["codes"] == ["DNV-ST-F101", "API-RP-1111"]
    assert len(payload["results"]) == 2
    for r in payload["results"]:
        assert r["is_safe"] is True
        assert 0.0 < r["max_utilisation"] < 1.0


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
    assert "Overall verdict : PASS" in stdout

    # Artifact line points at a real, self-contained HTML report.
    artifact_lines = [ln for ln in stdout.splitlines() if ln.startswith("ARTIFACT: ")]
    assert len(artifact_lines) == 1
    artifact_path = Path(artifact_lines[0].split("ARTIFACT: ", 1)[1].strip())
    assert artifact_path == out_html.resolve()
    assert out_html.exists()

    html = out_html.read_text(encoding="utf-8")
    assert "<!DOCTYPE html>" in html
    assert "X65" in html  # inputs echoed for traceability
    # Offline: no external resource fetches (only the SVG xmlns namespace URI).
    for token in ("src=\"http", "href=\"http", "cdn", "<script src"):
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

    assert len(fresh) == len(cached) == 2
    for f, c in zip(fresh, cached):
        assert f.code_label == c.code_label
        assert f.governing_check == c.governing_check
        assert f.is_safe == c.is_safe
        assert f.max_utilisation == pytest.approx(c.max_utilisation, abs=1e-6)


@pytest.mark.unit
def test_from_cache_is_deterministic():
    """Two cache loads yield identical numbers."""
    module = _load_module()
    a = module.run_from_cache()
    b = module.run_from_cache()
    assert [r.max_utilisation for r in a] == [r.max_utilisation for r in b]
