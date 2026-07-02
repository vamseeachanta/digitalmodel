#!/usr/bin/env python
"""Deterministic generator for data/riser_database/ (#1245).

Reads the externalized seed spec (sources.yml: file lists, filename rules,
unit conversions, reference-table rows) and writes the three public CSVs plus
a provenance manifest. Regeneration is deterministic — stable sort, %g float
formatting, no timestamps — so ``git diff --exit-code data/riser_database``
after a rebuild verifies the committed tables match the seeds.

Run as a file (the workspace deny-list blocks ``python3 -c``):

    .venv/bin/python scripts/riser_database/build_tables.py
"""
from __future__ import annotations

import csv
import hashlib
import re
import sys
from pathlib import Path

import yaml

REPO_ROOT = Path(__file__).resolve().parents[2]
SPEC_PATH = Path(__file__).with_name("sources.yml")
OUT_DIR = REPO_ROOT / "data" / "riser_database"

CONFIG_COLUMNS = [
    "config_id",
    "riser_type",
    "source_path",
    "water_depth_m",
    "od_mm",
    "wt_mm",
    "material_grade",
    "analysis_kind",
    "notes",
]
CROSSWALK_COLUMNS = [
    "code_id",
    "family_key",
    "publisher",
    "section",
    "topic",
    "wiki_path",
    "registry_revision",
    "note",
]
MATERIAL_COLUMNS = [
    "ref_id",
    "sn_curve_name",
    "environment",
    "scf_ref",
    "dff_ref",
    "code_id",
    "wiki_path",
    "note",
]


def _num(value) -> str:
    """Deterministic numeric cell: %g, empty when unknown (never guessed)."""
    return "" if value is None else f"{float(value):g}"


def _riser_input_row(rel: str, conv: dict) -> dict:
    doc = yaml.safe_load((REPO_ROOT / rel).read_text())
    geometry = doc.get("geometry") or {}
    material = doc.get("material") or {}
    analysis = doc.get("analysis") or {}
    kind = "statics" if analysis.get("run_statics") else ""
    if analysis.get("run_dynamics"):
        kind = (kind + "+dynamics").lstrip("+")
    od = material.get("outer_diameter")
    wt = material.get("wall_thickness")
    return {
        "config_id": Path(rel).stem,
        "riser_type": str(doc.get("riser_type", "")),
        "source_path": rel,
        "water_depth_m": _num(geometry.get("water_depth")),
        "od_mm": _num(None if od is None else od * conv["m_to_mm"]),
        "wt_mm": _num(None if wt is None else wt * conv["m_to_mm"]),
        "material_grade": str(material.get("grade", "")),
        "analysis_kind": kind,
        "notes": "seeded from the unified riser input schema examples",
    }


def _pipe_row(rel: str, rules: dict, conv: dict) -> dict:
    doc = yaml.safe_load((REPO_ROOT / rel).read_text())
    outer = doc.get("Outer_Pipe") or {}
    geometry = outer.get("Geometry") or {}
    material = outer.get("Material") or {}
    name = Path(rel).stem
    depth_match = re.search(rules["depth_token"], name)
    depth_m = (
        float(depth_match.group(1)) * conv["ft_to_m"] if depth_match else None
    )
    position = next(
        (t for t in rules["position_tokens"] if t.lower() in name.lower()), ""
    )
    od = geometry.get("Nominal_OD")
    wt = geometry.get("Design_WT")
    return {
        "config_id": name,
        "riser_type": "drilling" if position.lower() == "drilling" else "pipe_section",
        "source_path": rel,
        "water_depth_m": _num(depth_m),
        "od_mm": _num(None if od is None else od * conv["in_to_mm"]),
        "wt_mm": _num(None if wt is None else wt * conv["in_to_mm"]),
        "material_grade": str(material.get("Material_Grade", "")),
        "analysis_kind": f"pipe_design:{position.lower()}" if position else "pipe_design",
        "notes": "seeded from the pipe capacity configs",
    }


def _write_csv(path: Path, columns: list[str], rows: list[dict]) -> dict:
    with path.open("w", newline="") as fh:
        writer = csv.DictWriter(fh, fieldnames=columns, lineterminator="\n")
        writer.writeheader()
        for row in rows:
            writer.writerow({col: str(row.get(col, "")) for col in columns})
    return {
        "file": path.name,
        "sha256": hashlib.sha256(path.read_bytes()).hexdigest(),
        "rows": len(rows),
    }


def main() -> int:
    spec = yaml.safe_load(SPEC_PATH.read_text())
    conv = spec["unit_conversions"]
    rules = spec["filename_rules"]

    config_rows = [_riser_input_row(rel, conv) for rel in spec["riser_input_files"]]
    config_rows += [_pipe_row(rel, rules, conv) for rel in spec["config_pipe_files"]]
    config_rows.sort(key=lambda r: r["config_id"])

    crosswalk_rows = sorted(spec["crosswalk_rows"], key=lambda r: r["code_id"])
    material_rows = sorted(spec["material_rows"], key=lambda r: r["ref_id"])

    OUT_DIR.mkdir(parents=True, exist_ok=True)
    tables = {
        "config_catalog": _write_csv(
            OUT_DIR / "config_catalog.csv", CONFIG_COLUMNS, config_rows
        ),
        "standards_crosswalk": _write_csv(
            OUT_DIR / "standards_crosswalk.csv", CROSSWALK_COLUMNS, crosswalk_rows
        ),
        "material_sn_scf_dff": _write_csv(
            OUT_DIR / "material_sn_scf_dff.csv", MATERIAL_COLUMNS, material_rows
        ),
    }
    manifest = {
        "generated_by": "scripts/riser_database/build_tables.py",
        "sources_spec": "scripts/riser_database/sources.yml",
        "route": "public",
        "issue": 1245,
        "tables": tables,
    }
    (OUT_DIR / "manifest.yaml").write_text(
        yaml.safe_dump(manifest, sort_keys=True, default_flow_style=False)
    )
    for name, meta in tables.items():
        print(f"{name}: {meta['rows']} rows sha256={meta['sha256'][:12]}…")
    return 0


if __name__ == "__main__":
    sys.exit(main())
