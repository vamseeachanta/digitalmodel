#!/usr/bin/env python3
"""Generate YAML companions for OrcaWave .owd files in L00_validation_wamit.

For each .owd file, loads the project via OrcFxAPI.Diffraction and calls
SaveData() to export OrcaWave's native YAML format alongside the binary file.
No Calculate() is required — SaveData() exports input configuration only.

Usage:
    uv run python scripts/benchmark/generate_owd_ymls.py          # all cases
    uv run python scripts/benchmark/generate_owd_ymls.py --case 2.1
    uv run python scripts/benchmark/generate_owd_ymls.py --force  # overwrite existing
"""
from __future__ import annotations

import argparse
import io
import sys
from pathlib import Path

# Fix Windows console encoding
if sys.platform == "win32":
    sys.stdout = io.TextIOWrapper(sys.stdout.buffer, encoding="utf-8")
    sys.stderr = io.TextIOWrapper(sys.stderr.buffer, encoding="utf-8")

REPO_ROOT = Path(__file__).parent.parent.parent
L00_DIR = REPO_ROOT / "docs" / "modules" / "orcawave" / "L00_validation_wamit"


def find_owd_files(case_filter: str | None) -> list[Path]:
    """Return all .owd paths under L00_DIR, optionally filtered by case folder."""
    all_owds = sorted(L00_DIR.rglob("*.owd"))
    if case_filter is None:
        return all_owds

    # Match by top-level case directory name (e.g. "2.1" matches .../2.1/...)
    # The case folder is the first component after L00_DIR
    filtered = []
    for p in all_owds:
        try:
            rel = p.relative_to(L00_DIR)
        except ValueError:
            continue
        if rel.parts[0] == case_filter:
            filtered.append(p)
    return filtered


def generate_yml(owd_path: Path, force: bool) -> str:
    """Load .owd and save YAML companion. Returns one of: created / skipped / error."""
    yml_path = owd_path.with_suffix(".yml")

    if yml_path.exists() and not force:
        return "skipped"

    try:
        import OrcFxAPI  # noqa: PLC0415 — deferred import (not available everywhere)
    except ImportError:
        return "error: OrcFxAPI not installed"

    try:
        diff = OrcFxAPI.Diffraction()
        diff.LoadData(str(owd_path.resolve()))
        diff.SaveData(str(yml_path.resolve()))
        return "created"
    except Exception as exc:  # noqa: BLE001
        return f"error: {exc}"


def main() -> None:
    parser = argparse.ArgumentParser(
        description="Generate YAML companions for .owd files in L00_validation_wamit"
    )
    parser.add_argument(
        "--case",
        metavar="CASE_ID",
        help="Process only this case folder (e.g. 2.1, 2.8)",
    )
    parser.add_argument(
        "--force",
        action="store_true",
        help="Overwrite existing .yml files",
    )
    args = parser.parse_args()

    owd_files = find_owd_files(args.case)

    if not owd_files:
        target = f"case {args.case}" if args.case else "L00_validation_wamit"
        print(f"No .owd files found under {target}")
        sys.exit(1)

    counts = {"created": 0, "skipped": 0, "error": 0}

    for owd_path in owd_files:
        rel = owd_path.relative_to(L00_DIR)
        status = generate_yml(owd_path, force=args.force)

        tag = status.split(":")[0]  # "created", "skipped", "error"
        counts[tag] = counts.get(tag, 0) + 1

        icon = {"created": "✓", "skipped": "–", "error": "✗"}.get(tag, "?")
        print(f"  {icon} {rel}  [{status}]")

    print(
        f"\nDone: {counts['created']} created,"
        f" {counts['skipped']} skipped,"
        f" {counts.get('error', 0)} errors"
    )
    if counts.get("error", 0):
        sys.exit(1)


if __name__ == "__main__":
    main()
