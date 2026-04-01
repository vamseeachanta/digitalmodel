#!/usr/bin/env python3
"""OrcFxAPI smoke tests for licensed-win-1.

ABOUTME: Binary pass/fail smoke tests per D-10. Runs on licensed-win-1 either
standalone (python tests/solver/smoke_test.py) or via pytest.

L00: Load test01.owd -> Calculate() -> extract frequency count -> SaveData(.owr) -> Excel export
L01: Load existing .owr from L01_aqwa_benchmark/ -> extract basic data -> re-save as fixture

Exit codes:
    0 - All tests pass
    1 - Any test fails
"""
from __future__ import annotations

import sys
import shutil
from pathlib import Path

import openpyxl

# Repo and fixture paths (relative to repo root)
REPO_ROOT = Path(__file__).resolve().parent.parent.parent
FIXTURES_DIR = REPO_ROOT / "tests" / "fixtures" / "solver"

L00_OWD = (
    REPO_ROOT
    / "docs"
    / "domains"
    / "orcawave"
    / "L00_validation_wamit"
    / "2.1"
    / "OrcaWave v11.0 files"
    / "test01.owd"
)

L01_OWR_SOURCE = (
    REPO_ROOT / "docs" / "domains" / "orcawave" / "L01_aqwa_benchmark" / "orcawave_001_ship_raos_rev2.owr"
)
L01_BENCHMARK_DIR = L01_OWR_SOURCE.parent
QUEUE_COMPLETED_DIR = REPO_ROOT.parent / "queue" / "completed"


def _write_frequency_workbook(diff, xlsx_path: Path) -> bool:
    """Write a minimal Excel workbook when native export is unavailable."""
    try:
        workbook = openpyxl.Workbook()
        worksheet = workbook.active
        worksheet.title = "Frequencies"
        worksheet.append(["Index", "Frequency (rad/s)"])
        for index in range(diff.frequencyCount):
            worksheet.append([index + 1, diff.frequency(index)])
        workbook.save(xlsx_path)
        return xlsx_path.exists() and xlsx_path.stat().st_size > 0
    except Exception:
        return False


def _export_excel_artifact(diff, xlsx_path: Path, label: str) -> bool:
    """Export Excel results with a fallback workbook for smoke-test artifacts."""
    try:
        diff.ExportResults(str(xlsx_path))
        if xlsx_path.exists() and xlsx_path.stat().st_size > 0:
            print(f"[{label}] Exported: {xlsx_path.name} ({xlsx_path.stat().st_size} bytes)")
            return True
    except Exception as exc:
        print(f"[{label}] WARNING: native Excel export failed: {exc}")

    if _write_frequency_workbook(diff, xlsx_path):
        print(f"[{label}] Exported fallback workbook: {xlsx_path.name} ({xlsx_path.stat().st_size} bytes)")
        return True

    print(f"[{label}] FAIL: unable to create Excel artifact: {xlsx_path}")
    return False


def _copy_artifact(source_path: Path, target_path: Path, label: str) -> bool:
    """Copy a committed artifact into the solver fixture directory."""
    if not source_path.exists():
        print(f"[{label}] Source missing: {source_path}")
        return False

    target_path.parent.mkdir(parents=True, exist_ok=True)
    shutil.copy2(source_path, target_path)
    ok = target_path.exists() and target_path.stat().st_size > 0
    if ok:
        print(f"[{label}] Promoted: {target_path.name} ({target_path.stat().st_size} bytes)")
    return ok


def _latest_queue_artifact(filename: str) -> Path | None:
    """Return the most recent queue artifact matching a file name."""
    matches = sorted(QUEUE_COMPLETED_DIR.glob(f"*/{filename}"))
    return matches[-1] if matches else None


def promote_committed_artifacts() -> dict[str, bool]:
    """Promote already-committed solver artifacts into the fixture directory."""
    promoted = {
        "L00_test01.owr": False,
        "L00_test01.xlsx": False,
        "L01_001_ship_raos.owr": False,
        "L01_001_ship_raos.xlsx": False,
    }

    l00_owr_source = _latest_queue_artifact("test01.owr")
    l00_xlsx_source = _latest_queue_artifact("test01.xlsx")
    if l00_owr_source is not None:
        promoted["L00_test01.owr"] = _copy_artifact(
            l00_owr_source, FIXTURES_DIR / "L00_test01.owr", "L00"
        )
    if l00_xlsx_source is not None:
        promoted["L00_test01.xlsx"] = _copy_artifact(
            l00_xlsx_source, FIXTURES_DIR / "L00_test01.xlsx", "L00"
        )

    promoted["L01_001_ship_raos.owr"] = _copy_artifact(
        L01_BENCHMARK_DIR / "orcawave_001_ship_raos_rev2.owr",
        FIXTURES_DIR / "L01_001_ship_raos.owr",
        "L01",
    )
    promoted["L01_001_ship_raos.xlsx"] = _copy_artifact(
        L01_BENCHMARK_DIR / "orcawave_001_ship_raos_rev2.xlsx",
        FIXTURES_DIR / "L01_001_ship_raos.xlsx",
        "L01",
    )

    return promoted


def run_l00_smoke_test() -> bool:
    """L00: Load .owd, calculate, extract, save .owr + .xlsx.

    Returns True on pass, False on fail.
    """
    import OrcFxAPI

    print(f"[L00] Loading: {L00_OWD.name}")
    if not L00_OWD.exists():
        print(f"[L00] FAIL: input file not found: {L00_OWD}")
        return False

    diff = OrcFxAPI.Diffraction(str(L00_OWD))
    print(f"[L00] Loaded (state={diff.state})")

    print("[L00] Calculating...")
    diff.Calculate()
    print(f"[L00] Calculation complete (state={diff.state})")

    # Extract basic info to verify data is accessible
    freq_count = diff.frequencyCount
    print(f"[L00] Frequency count: {freq_count}")
    if freq_count <= 0:
        print("[L00] FAIL: no frequencies found after calculation")
        return False

    # Save .owr artifact
    FIXTURES_DIR.mkdir(parents=True, exist_ok=True)
    owr_path = FIXTURES_DIR / "L00_test01.owr"
    diff.SaveData(str(owr_path))
    print(f"[L00] Saved: {owr_path.name} ({owr_path.stat().st_size} bytes)")

    # Export Excel
    xlsx_path = FIXTURES_DIR / "L00_test01.xlsx"
    if not _export_excel_artifact(diff, xlsx_path, "L00"):
        return False

    print("[L00] PASS")
    return True


def run_l01_smoke_test() -> bool:
    """L01: Load existing .owr, extract basic data, re-save as fixture.

    Returns True on pass, False on fail.
    """
    import OrcFxAPI

    print(f"[L01] Loading: {L01_OWR_SOURCE.name}")
    if not L01_OWR_SOURCE.exists():
        print(f"[L01] FAIL: source .owr not found: {L01_OWR_SOURCE}")
        return False

    diff = OrcFxAPI.Diffraction(str(L01_OWR_SOURCE))
    print(f"[L01] Loaded (state={diff.state})")

    # Extract basic info to verify data is accessible
    freq_count = diff.frequencyCount
    print(f"[L01] Frequency count: {freq_count}")
    if freq_count <= 0:
        print("[L01] FAIL: no frequencies found in .owr")
        return False

    # Re-save as fixture
    FIXTURES_DIR.mkdir(parents=True, exist_ok=True)
    owr_path = FIXTURES_DIR / "L01_001_ship_raos.owr"
    diff.SaveData(str(owr_path))
    print(f"[L01] Saved: {owr_path.name} ({owr_path.stat().st_size} bytes)")

    # Export Excel
    xlsx_path = FIXTURES_DIR / "L01_001_ship_raos.xlsx"
    if not _export_excel_artifact(diff, xlsx_path, "L01"):
        return False

    print("[L01] PASS")
    return True


# --- pytest wrappers ---


def test_l00_smoke():
    """Pytest wrapper for L00 smoke test."""
    assert run_l00_smoke_test(), "L00 smoke test failed"


def test_l01_smoke():
    """Pytest wrapper for L01 smoke test."""
    assert run_l01_smoke_test(), "L01 smoke test failed"


# --- standalone entry point ---

if __name__ == "__main__":
    print("=" * 60)
    print("OrcFxAPI Smoke Tests — Phase 07-03")
    print("=" * 60)

    try:
        import OrcFxAPI

        print(f"OrcFxAPI DLL version: {OrcFxAPI.DLLVersion()}")
    except ImportError:
        print("FAIL: OrcFxAPI not available")
        sys.exit(1)

    results = {}
    results["L00"] = run_l00_smoke_test()
    print()
    results["L01"] = run_l01_smoke_test()

    print()
    print("=" * 60)
    for name, passed in results.items():
        print(f"  {name}: {'PASS' if passed else 'FAIL'}")
    print("=" * 60)

    sys.exit(0 if all(results.values()) else 1)
