# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Build a standalone riser reference-data report using the public loader.

Usage: PYTHONPATH=src python scripts/reports/build_riser_database_report.py
Only the HTML report and its change-panel sidecar are written. Reference tables,
examples and citation stores are read-only; no analysis solver is imported.
"""

from __future__ import annotations

import argparse
import hashlib
import html
import json
import re
from dataclasses import asdict
from pathlib import Path
from string import Template

import yaml

from digitalmodel.riser_database import RiserConfigRow, RiserDatabase, RiserDatabaseError

ROOT = Path(__file__).resolve().parents[2]
REPORT_DIR = Path("docs/reports/modules/riserdatabase")
REPORT_NAME = "riser-database-analysis.html"
TEMPLATE = Path("scripts/reports/templates/riser_database_report.html")
SCRIPT = Path("scripts/reports/assets/riser_database_review.js")
STYLE = Path("src/digitalmodel/reporting/assets/calc_report_format.html")
EXTRA_STYLE = Path("src/digitalmodel/reporting/assets/standard_report.css")
EXAMPLE_ID = "riser_drilling"
CHANGES = {
    "document": "Riser reference database analysis",
    "revision": "0",
    "date": "2026-10-22",
    "changes": [
        {"section": "worked-example", "description":
         "First issue: demonstrate retrieval of public drilling-riser geometry against its source."},
        {"section": "limitations", "description":
         "Separate reference-data checks from unestablished physical operating limits."},
        {"section": "recommendations", "description":
         "Identify the data and calculation basis needed before a connection decision."},
    ],
}


def source_digests(root: Path, example_path: str) -> dict:
    """Fingerprint only public evidence, resolving paths against the intended root."""
    paths = [
        Path(example_path), Path("scripts/riser_database/sources.yml"),
        Path("docs/riser_database.md"), Path("scripts/riser_database/build_tables.py"),
        Path("scripts/reports/build_riser_database_report.py"), TEMPLATE, SCRIPT,
        STYLE, EXTRA_STYLE,
    ]
    for folder, pattern in (
        ("src/digitalmodel/riser_database", "*.py"),
        ("data/riser_database", "*"),
        ("tests/riser_database", "test_*.py"),
    ):
        paths += [p.relative_to(root) for p in sorted((root / folder).glob(pattern)) if p.is_file()]
    return {
        p.as_posix(): hashlib.sha256((root / p).read_bytes()).hexdigest()
        for p in sorted(set(paths))
    }


def public_source_values(root: Path, row: RiserConfigRow, conv: dict) -> dict:
    """Read the public example independently and reject disagreement with the row."""
    source = yaml.safe_load((root / row.source_path).read_text(encoding="utf-8"))
    expected = {
        "water_depth_m": source["geometry"]["water_depth"],
        "od_mm": source["material"]["outer_diameter"] * conv["m_to_mm"],
        "wt_mm": source["material"]["wall_thickness"] * conv["m_to_mm"],
    }
    if any(getattr(row, key) != value for key, value in expected.items()):
        raise ValueError("Public example differs from the returned catalogue geometry")
    return expected


def collect_evidence(root: Path) -> dict:
    """Call the module and compare its example with an independently read source."""
    db = RiserDatabase.load(root=root / "data/riser_database")
    row = db.config(EXAMPLE_ID)
    if row.water_depth_m is None or row.od_mm is None or row.wt_mm is None:
        raise ValueError("Public example geometry must be complete")
    conv = yaml.safe_load((root / "scripts/riser_database/sources.yml").read_text())
    conv = conv["unit_conversions"]
    expected = public_source_values(root, row, conv)
    counts = {
        "config_catalog": len(db.configs),
        "standards_crosswalk": len(db.crosswalk_rows),
        "material_sn_scf_dff": len(db.materials),
        "riser_stackup": len(db.stackups),
        "rig_riser_interface": len(db.rigs),
    }
    manifest = yaml.safe_load((root / "data/riser_database/manifest.yaml").read_text())
    unknown_rejected = False
    try:
        db.config("not-a-catalogue-key")
    except RiserDatabaseError as error:
        unknown_rejected = "unknown key" in error.reason
    numbers = {
        "depth_ft": row.water_depth_m / conv["ft_to_m"],
        "od_in": row.od_mm / conv["in_to_mm"],
        "wall_in": row.wt_mm / conv["in_to_mm"],
        **{f"difference_{key}": 100 * (getattr(row, key) - value) / value
           for key, value in expected.items()},
        **{f"count_{key}": value for key, value in counts.items()},
        "target_difference": 0,
    }
    numbers["largest_difference"] = max(
        abs(numbers[f"difference_{key}"]) for key in expected
    )
    return {
        "example": asdict(row), "source_values": expected, "conversions": conv,
        "physical_response_status": "NOT_EVALUATED",
        "numbers": numbers, "counts": counts,
        "qa": {
            "Public example geometry agrees with the returned record": True,
            "Unknown configuration is rejected": unknown_rejected,
            "Absent catalogue depth is preserved as missing": db.config(
                "pipe_py_21OD_0875in_Drilling_Riser").water_depth_m is None,
            "Loaded row counts agree with the manifest": all(
                counts[key] == manifest["tables"][key]["rows"] for key in counts),
        },
        "source_digests": source_digests(root, row.source_path),
    }


def number(model: dict, key: str, precision: str, tag: str = "span") -> str:
    """Bind each displayed quantity to the unrounded module-derived evidence."""
    value = format(model["numbers"][key], precision)
    return f'<{tag} data-number="{key}" data-format="{precision}">{value}</{tag}>'


def agreement_chart(model: dict) -> str:
    """Plot source agreement; the zero target is a data check, not a design limit."""
    labels = {"water_depth_m": "Water depth", "od_mm": "Outer diameter", "wt_mm": "Wall thickness"}
    points = []
    for index, (key, label) in enumerate(labels.items()):
        value = model["numbers"][f"difference_{key}"]
        x = 135 + index * 205
        tip = (f"Case: public drilling-riser example; Component: {label}; "
               f"Demand: {value:.1f} % difference from source; Target: 0.0 %; "
               "Operating limit: not established")
        points.append(
            f'<g data-measure="{key}" tabindex="0" role="img" aria-label="{tip}">'
            f'<title>{tip}</title><circle cx="{x}" cy="95" r="8" fill="var(--good)"/>'
            f'<text x="{x}" y="135" text-anchor="middle">{label}</text></g>'
        )
    return ('<svg id="agreement-chart" viewBox="0 0 680 170" role="img" '
            'aria-label="Percentage difference from the public source by geometry quantity">'
            '<text x="20" y="25">Difference from source (%)</text>'
            '<line data-target="0" x1="65" x2="635" y1="95" y2="95" '
            'stroke="var(--ro)" stroke-width="2"/>'
            '<text x="65" y="75">Target: 0.0 %</text>' + "".join(points) + '</svg>')


def source_records(model: dict) -> str:
    """Keep implementation locators and the callable example in section 9.2."""
    records = [
        ("Public example", model["example"]["source_path"]),
        ("Loader and typed records", "src/digitalmodel/riser_database/loader.py"),
        ("Citation getters", "src/digitalmodel/riser_database/getters.py"),
        ("Public interface", "src/digitalmodel/riser_database/__init__.py"),
        ("Seed sources and conversion definitions", "scripts/riser_database/sources.yml"),
        ("Table generation", "scripts/riser_database/build_tables.py"),
        ("Database documentation", "docs/riser_database.md"),
        ("Database manifest", "data/riser_database/manifest.yaml"),
        ("Report generator", "scripts/reports/build_riser_database_report.py"),
        ("Report regression checks", "tests/riser_database/test_module_report.py"),
        ("Loader checks", "tests/riser_database/test_loader.py"),
        ("Reference-consumer checks", "tests/riser_database/test_material_refs.py"),
        ("Citation and edition checks", "tests/riser_database/test_citations.py"),
    ]
    return "".join(
        '<tr><td>' + title + '</td><td><a data-repo-source href="../../../../' + path + '">'
        + html.escape(path) + '</a></td></tr>' for title, path in records
    )


def build_report(root: Path = ROOT, output_dir: Path | None = None) -> Path:
    """Render deterministic report bytes with no external runtime dependencies."""
    root = root.resolve()
    output_dir = output_dir or root / REPORT_DIR
    model = collect_evidence(root)
    style_match = re.search(r"<style>.*?</style>", (root / STYLE).read_text(), re.S)
    if style_match is None:
        raise ValueError("The repository report stylesheet is missing")
    style = style_match.group()
    style += "<style>" + (root / EXTRA_STYLE).read_text() + "</style>"
    inventory_names = {
        "config_catalog": "Configuration catalogue",
        "standards_crosswalk": "Standards crosswalk",
        "material_sn_scf_dff": "Fatigue reference mapping",
        "riser_stackup": "Stack-up reference handles",
        "rig_riser_interface": "Rig-interface reference handles",
    }
    inventory = "".join(
        f'<tr><td>{label}</td><td>{number(model, "count_" + key, "d")}</td>'
        '<td>Database manifest and loaded records</td></tr>'
        for key, label in inventory_names.items()
    )
    qa = "".join(f'<tr><td>{html.escape(label)}</td><td>{"✓ Pass." if passed else "✕ Fail."}</td>'
                 '<td>Direct database lookup and public source comparison</td></tr>'
                 for label, passed in model["qa"].items())
    if not all(model["qa"].values()):
        raise ValueError("Reference-data pre-flight criteria are not met")
    page = Template((root / TEMPLATE).read_text(encoding="utf-8")).substitute(
        style=style, script=(root / SCRIPT).read_text(encoding="utf-8"),
        model_json=json.dumps(model, sort_keys=True, separators=(",", ":")).replace("<", "\\u003c"),
        changes_json=json.dumps(CHANGES, sort_keys=True).replace("<", "\\u003c"),
        depth=number(model, "depth_ft", ",.0f"),
        od=number(model, "od_in", ".2f"), wall=number(model, "wall_in", ".3f"),
        schematic_depth=number(model, "depth_ft", ",.0f", "tspan"),
        schematic_od=number(model, "od_in", ".2f", "tspan"),
        schematic_wall=number(model, "wall_in", ".3f", "tspan"),
        difference=number(model, "largest_difference", ".1f"),
        chart=agreement_chart(model), inventory=inventory, qa=qa,
        records=source_records(model),
    )
    output_dir.mkdir(parents=True, exist_ok=True)
    path = output_dir / REPORT_NAME
    path.write_text(page, encoding="utf-8")
    (output_dir / "changes.json").write_text(json.dumps(CHANGES, indent=2) + "\n", encoding="utf-8")
    return path


def main() -> None:
    """Accept an explicit output directory for isolated rebuild checks."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output-dir", type=Path)
    args = parser.parse_args()
    path = build_report(output_dir=args.output_dir)
    print(f"Built {path.name} and changes.json")


if __name__ == "__main__":
    main()
