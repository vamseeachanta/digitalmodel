#!/usr/bin/env python
"""Audit and classify the OrcaFlex spec library.

Walks docs/domains/orcaflex/ for spec.yml files, scores quality,
infers structural category, and produces YAML + HTML reports.

Usage:
    uv run python scripts/audit_spec_library.py
    uv run python scripts/audit_spec_library.py --html audit_report.html
    uv run python scripts/audit_spec_library.py --root docs/domains/orcaflex/library
"""
from __future__ import annotations

import argparse
import html as html_mod
import re
import sys
from pathlib import Path

import yaml

try:
    from digitalmodel.solvers.orcaflex.modular_generator.schema import ProjectInputSpec

    HAS_SCHEMA = True
except ImportError:
    HAS_SCHEMA = False

LIBRARY_ROOT = Path("docs/domains/orcaflex")
OUTPUT_DIR = Path("docs/domains/orcaflex/library/templates")

# ── prefix-to-category mapping for model_library dirs ──────────────────────
_PREFIX_CATEGORY = {
    "a": "riser",
    "b": "drilling",
    "c": "mooring",
    "d": "installation",
    "e": "installation",
    "f": "installation",
    "g": "installation",
    "h": "heavy_lift",
    "i": "seismic",
    "j": "subsea",
    "k": "wind_turbine",
    "l": "vessel",
    "m": "pipeline",
    "z": "other",
}


# ── quality helpers ─────────────────────────────────────────────────────────

def _compute_quality_score(
    schema_valid: bool,
    has_comments: bool,
    meaningful_name: bool,
    meaningful_description: bool,
    has_raw_properties: bool,
    line_count: int,
) -> int:
    score = 0
    if schema_valid:
        score += 30
    if has_comments:
        score += 20
    if meaningful_name:
        score += 15
    if meaningful_description:
        score += 10
    if not has_raw_properties:
        score += 15
    if line_count < 1000:
        score += 10
    return max(0, min(100, score))


def _is_meaningful_description(desc: str) -> bool:
    if not desc:
        return False
    if desc.lower().startswith("extracted from"):
        return False
    if desc.lower() == "generic":
        return False
    if len(desc) < 10:
        return False
    return True


def _is_meaningful_name(name: str) -> bool:
    if len(name) < 3:
        return False
    if name.lower().startswith("temp"):
        return False
    if name.strip().isdigit():
        return False
    return True


# ── category inference ──────────────────────────────────────────────────────

def _infer_category(spec_path: Path, data: dict) -> str:
    parts = [p.lower() for p in spec_path.parts]
    joined = "/".join(parts)

    if "riser" in data or "tier2_fast" in parts:
        return "riser"
    if "pipeline" in data or "pipeline" in parts:
        return "pipeline"
    if "jumper" in parts or "/jumper" in joined:
        return "jumper"
    if ("installation" in parts or any(p.startswith("installation") for p in parts)) and "model_library" not in parts:
        return "installation"
    if "mooring" in parts or any(p.startswith("mooring") for p in parts):
        return "mooring"
    if "training" in parts:
        return "training"

    # model_library prefix matching
    for part in parts:
        m = re.match(r"^([a-z])\d{2}_", part)
        if m:
            prefix = m.group(1)
            if prefix in _PREFIX_CATEGORY:
                return _PREFIX_CATEGORY[prefix]

    # fallback: use metadata.structure field
    structure = data.get("metadata", {}).get("structure", "").lower()
    _STRUCTURE_CATEGORY = {
        "riser": "riser", "pipeline": "pipeline", "mooring": "mooring",
        "installation": "installation", "vessel": "vessel",
    }
    if structure in _STRUCTURE_CATEGORY:
        return _STRUCTURE_CATEGORY[structure]

    return "other"


# ── single-spec audit ──────────────────────────────────────────────────────

def _audit_single_spec(spec_path: Path) -> dict:
    text = spec_path.read_text(encoding="utf-8")
    lines = text.splitlines()
    line_count = len(lines)
    has_comments = any(ln.lstrip().startswith("#") for ln in lines)

    try:
        data = yaml.safe_load(text) or {}
    except yaml.YAMLError:
        data = {}

    schema_valid = None
    if HAS_SCHEMA:
        try:
            ProjectInputSpec(**data)
            schema_valid = True
        except Exception:
            schema_valid = False

    metadata = data.get("metadata", {})
    name = metadata.get("name", spec_path.parent.name)
    description = metadata.get("description", "")
    structure = metadata.get("structure", "")
    operation = metadata.get("operation", "")

    env = data.get("environment", {})
    has_raw_properties = "raw_properties" in env

    m_name = _is_meaningful_name(name)
    m_desc = _is_meaningful_description(description)

    quality_score = _compute_quality_score(
        schema_valid=schema_valid if schema_valid is not None else False,
        has_comments=has_comments,
        meaningful_name=m_name,
        meaningful_description=m_desc,
        has_raw_properties=has_raw_properties,
        line_count=line_count,
    )

    category = _infer_category(spec_path, data)

    if "riser" in data:
        model_type = "riser"
    elif "pipeline" in data:
        model_type = "pipeline"
    elif "generic" in data:
        model_type = "generic"
    else:
        model_type = "unknown"

    return {
        "path": str(spec_path.as_posix()),
        "name": name,
        "description": description,
        "structure": structure,
        "operation": operation,
        "model_type": model_type,
        "category": category,
        "quality_score": quality_score,
        "schema_valid": schema_valid,
        "line_count": line_count,
        "has_comments": has_comments,
        "has_raw_properties": has_raw_properties,
    }


# ── discovery ───────────────────────────────────────────────────────────────

def _find_all_specs(root: Path) -> list[Path]:
    return sorted(root.rglob("spec.yml"))


# ── main audit ──────────────────────────────────────────────────────────────

def audit_library(root: Path = LIBRARY_ROOT) -> dict:
    """Audit every spec.yml under *root* and return structured results."""
    specs = _find_all_specs(root)
    results = [_audit_single_spec(p) for p in specs]

    by_category: dict[str, list[dict]] = {}
    for r in results:
        by_category.setdefault(r["category"], []).append(r)

    total = len(results)
    avg_quality = round(sum(r["quality_score"] for r in results) / total, 1) if total else 0.0
    schema_valid_count = sum(1 for r in results if r["schema_valid"] is True)
    by_category_counts = {k: len(v) for k, v in sorted(by_category.items())}

    summary = {
        "total": total,
        "by_category": by_category_counts,
        "avg_quality": avg_quality,
        "schema_valid_count": schema_valid_count,
    }

    return {"summary": summary, "specs": results, "by_category": by_category}


# ── output writers ──────────────────────────────────────────────────────────

def write_audit_yaml(results: dict, output_path: Path):
    """Write audit results to YAML."""
    output_path.parent.mkdir(parents=True, exist_ok=True)
    with open(output_path, "w", encoding="utf-8") as fh:
        yaml.dump(results, fh, default_flow_style=False, sort_keys=False)
    print(f"YAML written to {output_path}")


def write_audit_html(results: dict, output_path: Path):
    """Write a single-page HTML audit report."""
    summary = results["summary"]
    by_category = results["by_category"]

    def _score_color(score: int) -> str:
        if score >= 70:
            return "#2d6a2d"
        if score >= 40:
            return "#8a6d00"
        return "#a32020"

    def _score_bg(score: int) -> str:
        if score >= 70:
            return "#e6f5e6"
        if score >= 40:
            return "#fff8e1"
        return "#fde8e8"

    rows_summary = ""
    for cat, count in sorted(summary["by_category"].items()):
        rows_summary += f"<tr><td>{html_mod.escape(cat)}</td><td>{count}</td></tr>\n"

    category_sections = ""
    for cat in sorted(by_category.keys()):
        specs = by_category[cat]
        table_rows = ""
        for i, s in enumerate(specs):
            bg = "#f9f9f9" if i % 2 == 0 else "#ffffff"
            sc = s["quality_score"]
            sc_style = f'color:{_score_color(sc)};background:{_score_bg(sc)};font-weight:bold;padding:2px 8px;border-radius:3px'
            valid_txt = {True: "yes", False: "no", None: "n/a"}.get(s["schema_valid"], "n/a")
            table_rows += (
                f'<tr style="background:{bg}">'
                f'<td style="font-family:Consolas,Cascadia Code,monospace;font-size:0.85em">{html_mod.escape(s["path"])}</td>'
                f"<td>{html_mod.escape(s['name'])}</td>"
                f"<td>{html_mod.escape(s['model_type'])}</td>"
                f'<td style="{sc_style}">{sc}</td>'
                f"<td>{valid_txt}</td>"
                f"<td>{s['line_count']}</td>"
                f"</tr>\n"
            )
        category_sections += f"""
        <h2 id="{html_mod.escape(cat)}">{html_mod.escape(cat)} ({len(specs)})</h2>
        <table>
            <thead><tr>
                <th>Path</th><th>Name</th><th>Type</th>
                <th>Quality</th><th>Schema</th><th>Lines</th>
            </tr></thead>
            <tbody>
                {table_rows}
            </tbody>
        </table>
        """

    page = f"""<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="utf-8">
<title>OrcaFlex Spec Library Audit</title>
<style>
    body {{ font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif;
           margin: 2em; color: #222; }}
    h1 {{ border-bottom: 2px solid #333; padding-bottom: 0.3em; }}
    h2 {{ margin-top: 2em; color: #444; }}
    table {{ border-collapse: collapse; width: 100%; margin: 1em 0; }}
    th {{ background: #333; color: #fff; text-align: left; padding: 8px 12px; }}
    td {{ padding: 6px 12px; border-bottom: 1px solid #ddd; }}
    tr:hover {{ background: #eef3ff !important; }}
    .summary-grid {{ display: grid; grid-template-columns: repeat(auto-fit, minmax(180px, 1fr));
                     gap: 1em; margin: 1em 0; }}
    .summary-card {{ background: #f4f4f4; padding: 1em; border-radius: 6px; text-align: center; }}
    .summary-card .value {{ font-size: 2em; font-weight: bold; font-family: Consolas, Cascadia Code, monospace; }}
    .summary-card .label {{ color: #666; font-size: 0.9em; }}
</style>
</head>
<body>
<h1>OrcaFlex Spec Library Audit</h1>

<div class="summary-grid">
    <div class="summary-card"><div class="value">{summary['total']}</div><div class="label">Total Specs</div></div>
    <div class="summary-card"><div class="value">{summary['avg_quality']}</div><div class="label">Avg Quality</div></div>
    <div class="summary-card"><div class="value">{summary['schema_valid_count']}</div><div class="label">Schema Valid</div></div>
    <div class="summary-card"><div class="value">{len(by_category)}</div><div class="label">Categories</div></div>
</div>

<h2>Category Summary</h2>
<table>
    <thead><tr><th>Category</th><th>Count</th></tr></thead>
    <tbody>{rows_summary}</tbody>
</table>

{category_sections}

</body>
</html>"""

    output_path.parent.mkdir(parents=True, exist_ok=True)
    with open(output_path, "w", encoding="utf-8") as fh:
        fh.write(page)
    print(f"HTML written to {output_path}")


# ── CLI ─────────────────────────────────────────────────────────────────────

def main():
    """Run the spec library audit from the command line."""
    parser = argparse.ArgumentParser(
        description="Audit and classify the OrcaFlex spec library."
    )
    parser.add_argument(
        "--root",
        type=Path,
        default=LIBRARY_ROOT,
        help=f"Library root directory (default: {LIBRARY_ROOT})",
    )
    parser.add_argument(
        "--output",
        type=Path,
        default=None,
        help=f"YAML output path (default: {OUTPUT_DIR / 'audit_results.yaml'})",
    )
    parser.add_argument(
        "--html",
        type=Path,
        default=None,
        help="Optional HTML report output path",
    )
    args = parser.parse_args()

    output_path = args.output or (OUTPUT_DIR / "audit_results.yaml")

    results = audit_library(args.root)
    summary = results["summary"]

    print(f"Audited {summary['total']} specs")
    print(f"  Avg quality: {summary['avg_quality']}")
    print(f"  Schema valid: {summary['schema_valid_count']}")
    print(f"  Categories: {summary['by_category']}")

    write_audit_yaml(results, output_path)

    if args.html:
        write_audit_html(results, args.html)


if __name__ == "__main__":
    main()
