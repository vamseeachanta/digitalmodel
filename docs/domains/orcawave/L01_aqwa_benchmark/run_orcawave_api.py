#!/usr/bin/env python3
"""OrcFxAPI-based runner for OrcaWave diffraction analysis.

Drives the .yml -> .owr -> .xlsx pipeline entirely via OrcFxAPI (no subprocess).

Usage (module):
    from run_orcawave_api import run_orcawave_from_yml
    run_orcawave_from_yml("input.yml", "output.owr", "output.xlsx")

Usage (CLI):
    python run_orcawave_api.py orcawave_001_ship_raos_rev2.yml
    python run_orcawave_api.py input.yml output.owr output.xlsx
"""
from __future__ import annotations

import sys
import tempfile
from pathlib import Path

# Keys unsupported in OrcFxAPI 11.6 that must be stripped before loading
_UNSUPPORTED_KEYS = {"PanelAngleWarningLevel"}


def _write_filtered_yml(yml_path: Path) -> Path:
    """Write a copy of yml_path with unsupported keys removed, in the same directory.

    Returns the path to the filtered file (caller is responsible for cleanup).
    The temp file is placed in the same directory as the source so relative
    mesh paths (e.g. BodyMeshFileName) resolve correctly.
    """
    lines = yml_path.read_text(encoding="utf-8").splitlines(keepends=True)
    filtered = [
        line
        for line in lines
        if not any(line.strip().startswith(f"{key}:") for key in _UNSUPPORTED_KEYS)
    ]

    # Write next to source so relative paths still resolve
    tmp = tempfile.NamedTemporaryFile(
        mode="w",
        encoding="utf-8",
        suffix=".yml",
        dir=yml_path.parent,
        delete=False,
        prefix="_ow_filtered_",
    )
    tmp.writelines(filtered)
    tmp.close()
    return Path(tmp.name)


def run_orcawave_from_yml(
    yml_path: str | Path,
    owr_path: str | Path,
    xlsx_path: str | Path | None = None,
) -> bool:
    """Load yml, calculate, save .owr, optionally export .xlsx.

    Returns True on success, False on failure.
    """
    import OrcFxAPI

    yml_path = Path(yml_path).resolve()
    owr_path = Path(owr_path)
    if xlsx_path is not None:
        xlsx_path = Path(xlsx_path)

    print(f"[OW] Input YML : {yml_path}")
    print(f"[OW] Output OWR: {owr_path}")
    if xlsx_path:
        print(f"[OW] Output XLSX: {xlsx_path}")

    if not yml_path.exists():
        print(f"[OW] FAIL: input file not found: {yml_path}")
        return False

    # Strip unsupported keys before loading
    filtered_yml = _write_filtered_yml(yml_path)
    try:
        print(f"[OW] Loading (filtered) YML...")
        diff = OrcFxAPI.Diffraction(str(filtered_yml))
        print(f"[OW] Loaded (state={diff.state})")
    finally:
        filtered_yml.unlink(missing_ok=True)

    print("[OW] Calculating...")
    diff.Calculate()
    print(f"[OW] Calculation complete (state={diff.state})")

    freq_count = len(diff.frequencies)
    heading_count = len(diff.headings)
    print(f"[OW] Frequencies: {freq_count}, Headings: {heading_count}")
    if freq_count <= 0:
        print("[OW] FAIL: no frequencies in result")
        return False

    owr_path.parent.mkdir(parents=True, exist_ok=True)
    diff.SaveResults(str(owr_path))
    print(f"[OW] Saved: {owr_path.name} ({owr_path.stat().st_size:,} bytes)")

    if xlsx_path is not None:
        xlsx_path.parent.mkdir(parents=True, exist_ok=True)
        try:
            diff.SaveResultsSpreadsheet(str(xlsx_path))
            print(f"[OW] Exported: {xlsx_path.name} ({xlsx_path.stat().st_size:,} bytes)")
        except Exception as e:
            print(f"[OW] WARNING: Excel export failed (non-fatal): {e}")

    print("[OW] Done.")
    return True


# --- CLI entry point ---

if __name__ == "__main__":
    args = sys.argv[1:]
    if not args:
        print("Usage: python run_orcawave_api.py <input.yml> [output.owr] [output.xlsx]")
        sys.exit(1)

    yml = Path(args[0])
    owr = Path(args[1]) if len(args) > 1 else yml.with_suffix(".owr")
    xlsx = Path(args[2]) if len(args) > 2 else None

    success = run_orcawave_from_yml(yml, owr, xlsx)
    sys.exit(0 if success else 1)
