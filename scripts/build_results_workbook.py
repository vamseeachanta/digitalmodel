#!/usr/bin/env python3
"""Build a multi-tab .xlsx from a licensed-run output directory.

Mirrors the layout OrcaFlex / OrcaWave post-processing spreadsheets use, but
driven by whatever the digitalmodel workflow actually wrote to its results dir:

  - Index           : every tab + a one-line description
  - Run Metadata    : run_id / scope / workflow / state / sha / host / timestamps
                      (from the licensed-run result JSON, if given)
  - <each CSV>       : one tab per CSV (strength Summary, per-variable RangeGraphs,
                      OrcaWave RAOs / AddedMass / Damping / Excitation, …)
  - Validation       : flattened issues if a *validation*.json is present
  - <other JSON>     : flattened key/value tab
  - Artifacts (local): .owr/.owd/.sim/.gdf and other heavy files — path + size
                      only (NEVER embedded; they stay on the licensed host)

Usage:
  python build_postproc_workbook.py <results_dir> [more_dirs ...] \
      [--result-json <licensed-run result.json>] [--out report.xlsx]

Generic by design: unknown CSV/JSON still land as tabs, so no result is lost.
"""

from __future__ import annotations

import argparse
import json
import re
from pathlib import Path

import pandas as pd

HEAVY_SUFFIXES = {".owr", ".owd", ".sim", ".gdf", ".dat", ".h5", ".hdf5",
                  ".mat", ".png", ".jpg", ".zip", ".owc", ".bin"}
MAX_TEXT_BYTES = 5_000_000
_BAD_SHEET = re.compile(r"[\[\]:*?/\\]")
_used_names: set[str] = set()


def _sheet_name(raw: str) -> str:
    """Excel sheet names: <=31 chars, no []:*?/\\, unique."""
    name = _BAD_SHEET.sub("-", str(raw)).strip() or "sheet"
    name = name[:31]
    base, i = name, 1
    while name.lower() in _used_names:
        suffix = f"~{i}"
        name = base[: 31 - len(suffix)] + suffix
        i += 1
    _used_names.add(name.lower())
    return name


def _is_validation(name: str, obj) -> bool:
    n = name.lower()
    if "valid" in n:
        return True
    if isinstance(obj, dict) and any(k in obj for k in
                                     ("overall_status", "schema_validation", "issues")):
        return True
    return False


def _flatten(obj, prefix="") -> list[tuple[str, str]]:
    rows = []
    if isinstance(obj, dict):
        for k, v in obj.items():
            rows += _flatten(v, f"{prefix}{k}.")
    elif isinstance(obj, list):
        for idx, v in enumerate(obj):
            rows += _flatten(v, f"{prefix}{idx}.")
    else:
        rows.append((prefix.rstrip("."), str(obj)))
    return rows


def _validation_rows(obj) -> pd.DataFrame:
    """Turn a validation report into (section, item, detail) rows."""
    rows = []
    status = obj.get("overall_status") or obj.get("status") if isinstance(obj, dict) else None
    if status:
        rows.append({"section": "OVERALL", "item": "status", "detail": status})
    if isinstance(obj, dict):
        for section, body in obj.items():
            if section in ("overall_status", "status"):
                continue
            if isinstance(body, dict):
                for item, detail in body.items():
                    if isinstance(detail, list):
                        for d in detail:
                            rows.append({"section": section, "item": item, "detail": str(d)})
                    else:
                        rows.append({"section": section, "item": item, "detail": str(detail)})
            elif isinstance(body, list):
                for d in body:
                    rows.append({"section": section, "item": "", "detail": str(d)})
    return pd.DataFrame(rows) if rows else pd.DataFrame([{"detail": json.dumps(obj)[:500]}])


def collect(results_dirs: list[Path], result_json: Path | None):
    """Return ordered list of (sheet_name, dataframe, description)."""
    tabs: list[tuple[str, pd.DataFrame, str]] = []

    # 1) Run Metadata
    if result_json and result_json.is_file():
        meta = json.loads(result_json.read_text())
        flat = {**{k: v for k, v in meta.items() if not isinstance(v, (dict, list))},
                **{f"audit.{k}": v for k, v in (meta.get("audit") or {}).items()}}
        df = pd.DataFrame(sorted(flat.items()), columns=["field", "value"])
        tabs.append((_sheet_name("Run Metadata"), df, "licensed-run result metadata"))

    # 2) Heavy artifacts (path + size only — never embedded)
    heavy = []
    for root in results_dirs:
        for f in sorted(root.rglob("*")):
            if f.is_file() and f.suffix.lower() in HEAVY_SUFFIXES:
                heavy.append({"file": str(f), "suffix": f.suffix.lower(),
                              "bytes": f.stat().st_size})
    if heavy:
        tabs.append((_sheet_name("Artifacts (local)"), pd.DataFrame(heavy),
                     "heavy outputs kept on the licensed host (not embedded)"))

    # 3) CSVs -> one tab each
    for root in results_dirs:
        for f in sorted(root.rglob("*.csv")):
            if f.stat().st_size > MAX_TEXT_BYTES:
                continue
            try:
                df = pd.read_csv(f)
            except Exception as exc:
                df = pd.DataFrame([{"error": f"could not read CSV: {exc}"}])
            tabs.append((_sheet_name(f.stem), df, f"CSV {f.relative_to(root)}"))

    # 4) JSONs -> validation or flattened
    for root in results_dirs:
        for f in sorted(root.rglob("*.json")):
            if f.stat().st_size > MAX_TEXT_BYTES:
                continue
            try:
                obj = json.loads(f.read_text())
            except Exception as exc:
                tabs.append((_sheet_name(f.stem),
                             pd.DataFrame([{"error": f"bad JSON: {exc}"}]),
                             f"JSON {f.relative_to(root)}"))
                continue
            if _is_validation(f.name, obj):
                tabs.append((_sheet_name("Validation"), _validation_rows(obj),
                             f"validation report {f.relative_to(root)}"))
            else:
                df = pd.DataFrame(_flatten(obj), columns=["key", "value"])
                tabs.append((_sheet_name(f.stem), df, f"JSON {f.relative_to(root)}"))
    return tabs


def main(argv=None):
    ap = argparse.ArgumentParser(description=__doc__,
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("results_dirs", nargs="+")
    ap.add_argument("--result-json", default=None)
    ap.add_argument("--out", default="licensed_run_report.xlsx")
    args = ap.parse_args(argv)

    dirs = [Path(d) for d in args.results_dirs]
    missing = [str(d) for d in dirs if not d.is_dir()]
    if missing:
        raise SystemExit(f"results dir(s) not found: {', '.join(missing)}")

    tabs = collect(dirs, Path(args.result_json) if args.result_json else None)
    if not tabs:
        raise SystemExit("no CSV/JSON/heavy outputs found to report")

    index = pd.DataFrame([{"tab": n, "description": d} for n, _, d in tabs])
    out = Path(args.out)
    with pd.ExcelWriter(out, engine="openpyxl") as xl:
        index.to_excel(xl, sheet_name="Index", index=False)
        for name, df, _ in tabs:
            df.to_excel(xl, sheet_name=name, index=False)
    print(f"wrote {out}  ({len(tabs)+1} tabs: Index + {len(tabs)})")
    for n, df, _ in tabs:
        print(f"   - {n:<31} {df.shape[0]}x{df.shape[1]}")


if __name__ == "__main__":
    raise SystemExit(main())
