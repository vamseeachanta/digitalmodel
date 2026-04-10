#!/usr/bin/env python3
"""GTM Demo Suite — Validation Tests.

Validates all 5 GTM demos produce correct outputs.
Run from the digitalmodel root:
    PYTHONPATH=examples/demos/gtm:src uv run python examples/demos/gtm/tests/test_gtm_demos.py

Or with pytest:
    PYTHONPATH=examples/demos/gtm:src uv run pytest examples/demos/gtm/tests/test_gtm_demos.py -v

Ref: GH #2093
"""
import json
import os
import subprocess
import sys
import time
from pathlib import Path

# Use pytest if available, otherwise standalone
try:
    import pytest

    USE_PYTEST = True
except ImportError:
    USE_PYTEST = False

# ---------------------------------------------------------------------------
# Constants
# ---------------------------------------------------------------------------

# Resolve GTM demo root relative to this file
GTM_ROOT = Path(__file__).resolve().parent.parent
RESULTS_DIR = GTM_ROOT / "results"
OUTPUT_DIR = GTM_ROOT / "output"

DEMOS = [
    {
        "id": "01",
        "slug": "freespan",
        "script": "demo_01_dnv_freespan_viv.py",
        "results_file": "demo_01_freespan_results.json",
        "report_file": "demo_01_freespan_report.html",
        "expected_cases": 680,
        "data_key": "cases",  # top-level key holding the case array
    },
    {
        "id": "02",
        "slug": "wall_thickness",
        "script": "demo_02_wall_thickness_multicode.py",
        "results_file": "demo_02_wall_thickness_results.json",
        "report_file": "demo_02_wall_thickness_report.html",
        "expected_cases": 72,
        "data_key": "results",
    },
    {
        "id": "03",
        "slug": "mudmat_installation",
        "script": "demo_03_deepwater_mudmat_installation.py",
        "results_file": "demo_03_mudmat_installation_results.json",
        "report_file": "demo_03_mudmat_installation_report.html",
        "expected_cases": 180,
        "data_key": "cases",
    },
    {
        "id": "04",
        "slug": "shallow_pipelay",
        "script": "demo_04_shallow_water_pipelay.py",
        "results_file": "demo_04_shallow_pipelay_results.json",
        "report_file": "demo_04_shallow_pipelay_report.html",
        "expected_cases": 60,
        "data_key": "cases",
    },
    {
        "id": "05",
        "slug": "jumper_installation",
        "script": "demo_05_deepwater_rigid_jumper_installation.py",
        "results_file": "demo_05_jumper_installation_results.json",
        "report_file": "demo_05_jumper_installation_report.html",
        "expected_cases": 300,
        "data_key": "results",
    },
]

# Minimum chart-container divs expected per report
MIN_CHART_CONTAINERS = 5


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------


def _load_json(path: Path) -> dict:
    """Load and return parsed JSON from *path*."""
    with open(path) as fh:
        return json.load(fh)


def _read_text(path: Path) -> str:
    """Read and return full text of *path*."""
    return path.read_text(encoding="utf-8")


def _demo_ids():
    """Yield (demo_dict, id_string) for parametrize."""
    return [(d, f"demo_{d['id']}_{d['slug']}") for d in DEMOS]


# ---------------------------------------------------------------------------
# Test 1: Results JSON exists and is valid
# ---------------------------------------------------------------------------

if USE_PYTEST:

    @pytest.mark.parametrize("demo", DEMOS, ids=[d["slug"] for d in DEMOS])
    def test_results_json_exists_and_valid(demo):
        """Results JSON exists, parses, has metadata, and correct case count."""
        path = RESULTS_DIR / demo["results_file"]
        assert path.exists(), f"Missing results file: {path}"

        data = _load_json(path)

        # Has metadata key
        assert "metadata" in data, f"No 'metadata' key in {demo['results_file']}"
        meta = data["metadata"]
        assert isinstance(meta, dict), "metadata is not a dict"

        # Case count in metadata
        assert "total_cases" in meta, "metadata missing 'total_cases'"
        assert meta["total_cases"] == demo["expected_cases"], (
            f"Expected {demo['expected_cases']} cases, got {meta['total_cases']}"
        )

        # Data array length matches
        data_key = demo["data_key"]
        assert data_key in data, f"Missing top-level key '{data_key}'"
        assert len(data[data_key]) == demo["expected_cases"], (
            f"Data array '{data_key}' has {len(data[data_key])} items, "
            f"expected {demo['expected_cases']}"
        )


# ---------------------------------------------------------------------------
# Test 2: HTML report exists and has charts
# ---------------------------------------------------------------------------

if USE_PYTEST:

    @pytest.mark.parametrize("demo", DEMOS, ids=[d["slug"] for d in DEMOS])
    def test_html_report_exists_and_has_charts(demo):
        """HTML report exists with chart containers, Plotly, and branding."""
        path = OUTPUT_DIR / demo["report_file"]
        assert path.exists(), f"Missing report: {path}"

        html = _read_text(path)

        # Chart containers
        chart_count = html.count('<div class="chart-container">')
        assert chart_count >= MIN_CHART_CONTAINERS, (
            f"Expected >= {MIN_CHART_CONTAINERS} chart-container divs, "
            f"found {chart_count}"
        )

        # Plotly CDN script
        assert "plotly" in html.lower(), "Plotly CDN script tag not found"

        # Branding elements
        assert "digitalmodel" in html.lower(), (
            "Missing 'digitalmodel' branding in report"
        )
        assert "report-footer" in html, "Missing report-footer element"


# ---------------------------------------------------------------------------
# Test 3: --from-cache smoke test
# ---------------------------------------------------------------------------

if USE_PYTEST:

    @pytest.mark.parametrize("demo", DEMOS, ids=[d["slug"] for d in DEMOS])
    def test_from_cache_smoke(demo):
        """Run demo with --from-cache and verify exit 0 + report regenerated."""
        script_path = GTM_ROOT / demo["script"]
        report_path = OUTPUT_DIR / demo["report_file"]

        assert script_path.exists(), f"Missing script: {script_path}"

        # Record mtime before run (if report exists)
        mtime_before = report_path.stat().st_mtime if report_path.exists() else 0

        # Brief pause to ensure filesystem mtime granularity
        time.sleep(0.05)

        result = subprocess.run(
            [sys.executable, str(script_path), "--from-cache"],
            capture_output=True,
            text=True,
            timeout=120,
            cwd=str(GTM_ROOT),
        )

        assert result.returncode == 0, (
            f"--from-cache failed (rc={result.returncode}):\n"
            f"STDOUT: {result.stdout[-500:]}\n"
            f"STDERR: {result.stderr[-500:]}"
        )

        # Report should be regenerated (mtime updated)
        assert report_path.exists(), "Report not generated after --from-cache"
        mtime_after = report_path.stat().st_mtime
        assert mtime_after > mtime_before, (
            "Report mtime not updated — report was not regenerated"
        )

        # No import errors in stderr
        for pattern in ["ImportError", "ModuleNotFoundError"]:
            assert pattern not in result.stderr, (
                f"Import error detected in stderr: {result.stderr[-300:]}"
            )


# ---------------------------------------------------------------------------
# Test 4: JSON schema validation
# ---------------------------------------------------------------------------

if USE_PYTEST:

    @pytest.mark.parametrize("demo", DEMOS, ids=[d["slug"] for d in DEMOS])
    def test_json_schema(demo):
        """Validate required schema fields in each results JSON."""
        path = RESULTS_DIR / demo["results_file"]
        data = _load_json(path)

        meta = data["metadata"]

        # metadata.demo must be a non-empty string
        assert "demo" in meta, "metadata missing 'demo' field"
        assert isinstance(meta["demo"], str) and len(meta["demo"]) > 0, (
            "metadata.demo must be a non-empty string"
        )

        # metadata.total_cases must be a positive integer
        assert "total_cases" in meta, "metadata missing 'total_cases'"
        assert isinstance(meta["total_cases"], int) and meta["total_cases"] > 0, (
            "metadata.total_cases must be a positive integer"
        )

        # Data array must exist and be a list
        data_key = demo["data_key"]
        assert data_key in data, f"Missing top-level '{data_key}' array"
        assert isinstance(data[data_key], list), f"'{data_key}' is not a list"
        assert len(data[data_key]) > 0, f"'{data_key}' array is empty"

        # Summary must exist
        assert "summary" in data, "Missing 'summary' key"


# ===========================================================================
# Standalone runner (no pytest required)
# ===========================================================================


def _run_standalone():
    """Run all checks without pytest, printing pass/fail to stdout."""
    passed = 0
    failed = 0
    errors = []

    def check(name: str, condition: bool, msg: str = ""):
        nonlocal passed, failed
        if condition:
            passed += 1
            print(f"  PASS  {name}")
        else:
            failed += 1
            detail = f" — {msg}" if msg else ""
            errors.append(f"{name}{detail}")
            print(f"  FAIL  {name}{detail}")

    for demo in DEMOS:
        tag = f"demo_{demo['id']}_{demo['slug']}"
        print(f"\n{'='*60}")
        print(f"  {tag}")
        print(f"{'='*60}")

        # --- Test 1: Results JSON ---
        rpath = RESULTS_DIR / demo["results_file"]
        check(f"{tag}/json_exists", rpath.exists(), f"missing {rpath}")
        if rpath.exists():
            try:
                data = _load_json(rpath)
                check(f"{tag}/json_parses", True)
            except Exception as exc:
                check(f"{tag}/json_parses", False, str(exc))
                continue

            meta = data.get("metadata", {})
            check(f"{tag}/has_metadata", isinstance(meta, dict))
            check(
                f"{tag}/case_count",
                meta.get("total_cases") == demo["expected_cases"],
                f"expected {demo['expected_cases']}, got {meta.get('total_cases')}",
            )
            dk = demo["data_key"]
            check(
                f"{tag}/data_array_length",
                len(data.get(dk, [])) == demo["expected_cases"],
            )

        # --- Test 2: HTML report ---
        hpath = OUTPUT_DIR / demo["report_file"]
        check(f"{tag}/html_exists", hpath.exists(), f"missing {hpath}")
        if hpath.exists():
            html = _read_text(hpath)
            cc = html.count('<div class="chart-container">')
            check(f"{tag}/chart_containers", cc >= MIN_CHART_CONTAINERS, f"found {cc}")
            check(f"{tag}/plotly_cdn", "plotly" in html.lower())
            check(f"{tag}/branding", "digitalmodel" in html.lower())
            check(f"{tag}/report_footer", "report-footer" in html)

        # --- Test 3: --from-cache smoke ---
        script = GTM_ROOT / demo["script"]
        if script.exists() and rpath.exists():
            mtime_before = hpath.stat().st_mtime if hpath.exists() else 0
            time.sleep(0.05)
            result = subprocess.run(
                [sys.executable, str(script), "--from-cache"],
                capture_output=True,
                text=True,
                timeout=120,
                cwd=str(GTM_ROOT),
            )
            check(f"{tag}/from_cache_rc0", result.returncode == 0, result.stderr[-200:])
            if hpath.exists():
                check(
                    f"{tag}/report_regenerated",
                    hpath.stat().st_mtime > mtime_before,
                )
            check(
                f"{tag}/no_import_errors",
                "ImportError" not in result.stderr
                and "ModuleNotFoundError" not in result.stderr,
            )

        # --- Test 4: JSON schema ---
        if rpath.exists():
            data = _load_json(rpath)
            meta = data.get("metadata", {})
            check(
                f"{tag}/schema_demo_field",
                isinstance(meta.get("demo"), str) and len(meta.get("demo", "")) > 0,
            )
            check(
                f"{tag}/schema_total_cases",
                isinstance(meta.get("total_cases"), int)
                and meta.get("total_cases", 0) > 0,
            )
            check(f"{tag}/schema_summary", "summary" in data)

    # --- Summary ---
    total = passed + failed
    print(f"\n{'='*60}")
    print(f"  Results: {passed}/{total} passed, {failed} failed")
    print(f"{'='*60}")
    if errors:
        print("\nFailures:")
        for e in errors:
            print(f"  - {e}")
    return 1 if failed else 0


if __name__ == "__main__":
    if USE_PYTEST:
        sys.exit(pytest.main([__file__, "-v"] + sys.argv[1:]))
    else:
        sys.exit(_run_standalone())
