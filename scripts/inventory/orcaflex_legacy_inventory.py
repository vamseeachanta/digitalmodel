#!/usr/bin/env python
"""Enumerate all legacy OrcFxAPI scripts and output a triage table.

Scans docs/domains/orcaflex/ for Python files that import OrcFxAPI,
classifies each by type and domain, assigns a migration decision,
and writes a Markdown inventory table.

Usage:
    python scripts/inventory/orcaflex_legacy_inventory.py
"""
import pathlib
import sys

# Root of digitalmodel repository
REPO_ROOT = pathlib.Path(__file__).resolve().parent.parent.parent
SCAN_ROOT = REPO_ROOT / "docs" / "domains" / "orcaflex"
OUTPUT_PATH = SCAN_ROOT / "LEGACY_SCRIPT_INVENTORY.md"

# -----------------------------------------------------------------------
# Classification helpers
# -----------------------------------------------------------------------


def _imports_orcfxapi(src: str) -> bool:
    return "import OrcFxAPI" in src or "from OrcFxAPI" in src


def _report_type(src: str, filepath: pathlib.Path) -> str:
    src_lower = src.lower()
    if "html" in src_lower and ("plotly" in src_lower or "chart" in src_lower):
        return "html_report"
    if "matplotlib" in src_lower or (
        "plotly" in src_lower and "html" not in src_lower
    ):
        return "chart"
    if "excel" in src_lower or ("csv" in src_lower and "write" in src_lower):
        return "csv_export"
    if "CreateObject" in src or ("Model(" in src and "SaveData" in src):
        return "preproc"
    if "TimeHistory" in src or "RangeGraph" in src or "StaticResult" in src:
        return "postproc"
    return "misc"


def _domain(filepath: pathlib.Path) -> str:
    parts = filepath.parts
    try:
        idx = next(i for i, p in enumerate(parts) if p == "orcaflex")
    except StopIteration:
        return "unknown"
    if idx + 1 < len(parts):
        return parts[idx + 1]
    return "orcaflex"


def _migration(filepath: pathlib.Path, report_type: str, src: str) -> str:
    """Assign migration decision based on path and content heuristics."""
    path_str = str(filepath)

    # Vendor examples -- keep as-is
    if (
        "/raw/K01/" in path_str
        or "/raw/K02/" in path_str
        or "\\raw\\K01\\" in path_str
        or "\\raw\\K02\\" in path_str
    ):
        return "out-of-scope"

    # WRK-315 pipeline scripts -- already migrating
    if "24in_pipeline" in path_str:
        return "migrating (WRK-315)"

    # Riser production charts/csv -- superseded by Phase 4 renderers
    if "compression-study" in path_str:
        if report_type in ("chart", "csv_export"):
            return "archive"

    # Drilling preprocessing -- one-time setup scripts
    if "/drilling/" in path_str or "\\drilling\\" in path_str:
        return "archive"

    # GOM SCR design study -- active parametric workflow
    if "gom_scr" in path_str:
        return "review"

    # Mooring RAO scripts -- RAO comparison not in Phase 4
    if "rao-check" in path_str:
        return "review"

    # HTML report scripts not yet tracked
    if report_type == "html_report":
        return "review"

    return "archive"


def _phase4_gap(filepath: pathlib.Path, report_type: str, src: str) -> str:
    """Identify Phase 4 feature gap blocking migration."""
    if "matplotlib" in src.lower():
        return "matplotlib renderer"
    if (
        "excel" in src.lower()
        or "openpyxl" in src.lower()
        or "xlsxwriter" in src.lower()
    ):
        return "Excel export"
    if "CreateObject" in src:
        return "parametric model gen"
    if "TimeHistory" in src and "RAO" in src.upper():
        return "RAO comparison charts"
    if "RangeGraph" in src and "csv" in src.lower():
        return "RangeGraph CSV export (added WRK-315)"
    if "html" in src.lower() and "plotly" in src.lower():
        return "multi-case HTML report"
    return "\u2014"


def classify(filepath: pathlib.Path) -> dict | None:
    try:
        src = filepath.read_text(errors="replace")
    except Exception:
        return None

    lines = src.splitlines()
    rt = _report_type(src, filepath)
    dom = _domain(filepath)
    mig = _migration(filepath, rt, src)
    gap = _phase4_gap(filepath, rt, src)

    try:
        rel = filepath.relative_to(SCAN_ROOT)
    except ValueError:
        rel = filepath

    return {
        "file": str(rel).replace("\\", "/"),
        "lines": len(lines),
        "type": rt,
        "domain": dom,
        "migration": mig,
        "phase4_gap": gap,
        "imports_orcfxapi": _imports_orcfxapi(src),
    }


# -----------------------------------------------------------------------
# Systemic gap analysis
# -----------------------------------------------------------------------


def systemic_gaps(rows: list) -> dict:
    """Return gap -> count mapping for gaps appearing in >= 2 scripts."""
    gap_counts: dict = {}
    for row in rows:
        g = row["phase4_gap"]
        if g and g != "\u2014":
            gap_counts[g] = gap_counts.get(g, 0) + 1
    return {
        g: c
        for g, c in sorted(gap_counts.items(), key=lambda x: -x[1])
        if c >= 2
    }


# -----------------------------------------------------------------------
# Main
# -----------------------------------------------------------------------


def main() -> None:
    if not SCAN_ROOT.exists():
        print(f"ERROR: scan root not found: {SCAN_ROOT}", file=sys.stderr)
        sys.exit(1)

    py_files = sorted(SCAN_ROOT.rglob("*.py"))
    py_files = [
        f
        for f in py_files
        if ".venv" not in f.parts and "__pycache__" not in f.parts
    ]

    rows = []
    for f in py_files:
        result = classify(f)
        if result is not None:
            rows.append(result)

    orcfx_rows = [r for r in rows if r["imports_orcfxapi"]]
    other_rows = [r for r in rows if not r["imports_orcfxapi"]]

    gaps = systemic_gaps(orcfx_rows)

    # -------------------------------------------------------------------
    # Build Markdown output
    # -------------------------------------------------------------------
    lines_out = [
        "# Legacy OrcFxAPI Script Inventory",
        "",
        "> Generated: 2026-02-23 | Scanned: `docs/domains/orcaflex/`"
        " | WRK-316",
        "",
        f"Total Python files scanned: **{len(rows)}**  ",
        f"Files importing OrcFxAPI: **{len(orcfx_rows)}**  ",
        f"Other Python files (no OrcFxAPI): **{len(other_rows)}**",
        "",
        "## OrcFxAPI Script Triage Table",
        "",
        "| File | Lines | Type | Domain | Migration | Phase4 Gap |",
        "|------|-------|------|--------|-----------|------------|",
    ]

    for r in orcfx_rows:
        lines_out.append(
            f"| `{r['file']}` | {r['lines']} | {r['type']}"
            f" | {r['domain']}"
            f" | {r['migration']} | {r['phase4_gap']} |"
        )

    lines_out += [
        "",
        "## Non-OrcFxAPI Python Files",
        "",
        "| File | Lines | Type | Domain |",
        "|------|-------|------|--------|",
    ]

    for r in other_rows:
        lines_out.append(
            f"| `{r['file']}` | {r['lines']} | {r['type']}"
            f" | {r['domain']} |"
        )

    lines_out += [
        "",
        "## Systemic Phase 4 Gaps",
        "",
        "Features present in **>= 2 legacy scripts** that Phase 4 does"
        " not yet support:",
        "",
        "| Gap | Script Count | Recommended Action |",
        "|-----|-------------|-------------------|",
    ]

    gap_actions = {
        "matplotlib renderer": (
            "Add matplotlib-based chart renderer to Phase 4"
            " (new WRK item)"
        ),
        "Excel export": (
            "Add Excel output capability to Phase 4 extractors"
            " (new WRK item)"
        ),
        "parametric model gen": (
            "Out of Phase 4 scope -- keep as standalone scripts"
        ),
        "RAO comparison charts": (
            "Add RAO chart module to Phase 4 (new WRK item)"
        ),
        "RangeGraph CSV export (added WRK-315)": (
            "Completed via WRK-315 (export_rangegraph_csvs)"
        ),
        "multi-case HTML report": (
            "Phase 4 single-case design -- keep domain scripts"
            " for multi-case"
        ),
    }

    for gap, count in gaps.items():
        action = gap_actions.get(
            gap,
            "Evaluate -- create WRK item if >= 3 scripts affected",
        )
        lines_out.append(f"| {gap} | {count} | {action} |")

    lines_out += [
        "",
        "## Migration Decision Key",
        "",
        "| Decision | Meaning |",
        "|----------|---------|",
        "| `archive` | No active use; superseded by Phase 4."
        " Move to `_archive/`. |",
        "| `review` | Active or complex workflow -- needs manual"
        " evaluation before archiving. |",
        "| `migrating (WRK-315)` | Pipeline installation scripts being"
        " migrated in WRK-315. |",
        "| `out-of-scope` | Vendor/example code -- keep as reference,"
        " do not migrate. |",
        "",
        "## Top Migration Candidates",
        "",
        "| Priority | Script(s) | Effort | Rationale |",
        "|----------|-----------|--------|-----------|",
        "| 1 | `pipeline/installation/...` (3 scripts) | Medium"
        " | WRK-315 -- actively migrating |",
        "| 2 | `risers/production/compression-study/code/charts/`"
        " (10 scripts) | High"
        " | Matplotlib -- need Phase 4 matplotlib renderer |",
        "| 3 | `mooring/semi/rao-check/` (2 scripts) | Medium"
        " | RAO comparison -- new Phase 4 module needed |",
        "",
        "## Notes",
        "",
        "- K01/K02 raw examples are Orcina vendor tutorial scripts"
        " and are **out of scope** for migration.",
        "- Production riser chart scripts all use `matplotlib` --"
        " a systemic Phase 4 gap.",
        "- GOM SCR design study uses parametric model generation --"
        " outside Phase 4 scope.",
        "- Drilling preprocessing scripts are one-time setup scripts"
        " -- archive without migration.",
    ]

    output = "\n".join(lines_out) + "\n"
    OUTPUT_PATH.write_text(output, encoding="utf-8")
    print(f"Inventory written to: {OUTPUT_PATH}")
    print(f"  OrcFxAPI scripts: {len(orcfx_rows)}")
    print(f"  Other Python files: {len(other_rows)}")
    print(f"  Systemic gaps: {len(gaps)}")


if __name__ == "__main__":
    main()
