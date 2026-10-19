"""Deterministic, offline Hugging Face bundles from committed explorer sidecars.

Run as ``python -m scripts.hf.capability_bundle --help``. No upload, credentials,
solver execution, or deploy hook is involved. Engineering readiness is separate
from faithful projection and remains NOT_EVALUATED.
"""

from __future__ import annotations

import argparse
import hashlib
import json
import math
import re
from pathlib import Path

from scripts.hf import build_capability_datasets as builder

REPO = Path(__file__).resolve().parents[2]

# Sources and units: the named generators' build_study methods and DTO fields.
# These projections copy existing observations; no engineering constants are added.
SOURCES = {
    "wall_thickness": (
        "structural/wall-thickness-explorer.json",
        "scripts/capabilities/build_wall_thickness_explorer.py",
        "Rounded wall sweep; check columns vary by standard.",
    ),
    "wall_thickness_tension_moment": (
        "structural/wall-thickness-3d.json",
        "scripts/capabilities/build_wall_thickness_3d.py",
        "Generated full sweep required; not retained in Git.",
    ),
    "cathodic_protection": (
        "structural/cathodic-protection-explorer.json",
        "scripts/capabilities/build_cathodic_protection_explorer.py",
        "Synthetic structure/climate/coating sweep; rounded outputs.",
    ),
    "galvanic_compatibility": (
        "corrosion/galvanic-compatibility-explorer.json",
        "scripts/corrosion/build_galvanic_explorer.py",
        "Ordered material-pair screening; source verdicts retained.",
    ),
    "ipr": (
        "structural/ipr-explorer.json",
        "scripts/capabilities/build_ipr_explorer.py",
        "Synthetic saturated inflow curves; rounded flow rates; no measured wells.",
    ),
    "anchor_holding": (
        "structural/anchor-holding-explorer.json",
        "scripts/capabilities/build_anchor_holding_explorer.py",
        "Synthetic soil/anchor sweep; no site qualification or soil survey.",
    ),
    "ship_resistance": (
        "structural/ship-resistance-explorer.json",
        "scripts/capabilities/build_ship_resistance_explorer.py",
        "Generic hull forms; rounded calm-water screening, no sea margin.",
    ),
    "viv": (
        "structural/viv-explorer.json",
        "scripts/capabilities/build_viv_explorer.py",
        "Synthetic member screening; source status is not a fatigue qualification.",
    ),
    "pore_pressure": (
        "structural/pore-pressure-explorer.json",
        "scripts/capabilities/build_pore_pressure_explorer.py",
        "Synthetic sonic trend; no calibrated log or drilling approval.",
    ),
    "field_economics": (
        "structural/field-economics-explorer.json",
        "scripts/capabilities/build_field_economics_explorer.py",
        "Synthetic pre-tax sensitivity; source rounding and null payback retained; "
        "source field-summary metadata differs from generator reservoir input.",
    ),
}


def _json_bytes(value: object) -> bytes:
    return (
        json.dumps(value, sort_keys=True, ensure_ascii=False, allow_nan=False) + "\n"
    ).encode()


def _sha(data: bytes) -> str:
    return hashlib.sha256(data).hexdigest()


def _check_locators(value: object) -> None:
    """Fail before staging; scientific values and retained sources are not edited."""
    if isinstance(value, dict):
        for key, item in value.items():
            _check_locators(key)
            _check_locators(item)
    elif isinstance(value, list):
        for item in value:
            _check_locators(item)
    elif isinstance(value, str):
        patterns = (
            r"/(?:home|mnt)/",
            r"(?i)[a-z]:[\\/]+Users[\\/]",
            r"[\w.+-]+@[\w.-]+\.[a-zA-Z]{2,}",
        )
        if any(re.search(pattern, value) for pattern in patterns):
            raise ValueError("Source contains a restricted locator")


def _rows(rows: list[dict]) -> tuple[list[dict], list[str]]:
    if not rows:
        raise ValueError("Empty tables cannot establish a column contract")
    columns = sorted({key for row in rows for key in row})
    for row in rows:
        for value in row.values():
            if value is not None and not isinstance(value, (str, int, float, bool)):
                raise ValueError("Nested cells are not tidy observations")
            if isinstance(value, float) and not math.isfinite(value):
                raise ValueError("Non-finite observation")
    return [{column: row.get(column) for column in columns} for row in rows], columns


def _table_files(name, project):
    source, generator, limits = SOURCES[name]
    source_path = REPO / "docs/api" / source
    source_bytes = source_path.read_bytes()
    document = json.loads(source_bytes)
    _check_locators(document)
    rows, columns = _rows(project(document))
    table_path = f"{name}/train.jsonl"
    data = b"".join(_json_bytes(row) for row in rows)
    files = {table_path: data, f"sources/{name}.json": source_bytes}
    metadata = {
        "source": source_path.relative_to(REPO).as_posix(),
        "source_sha256": _sha(source_bytes),
        "generator": generator,
        "generator_sha256": _sha((REPO / generator).read_bytes()),
        "row_count": len(rows),
        "columns": columns,
        "data_sha256": _sha(data),
        "engineering_readiness": "NOT_EVALUATED",
        "data_limits": limits,
    }
    return files, metadata


def bundle_files(dataset: str, *, license_id: str) -> dict[str, bytes]:
    """Return a complete expected file set, before any destination is mutated.

    The license is a required maintainer input, not inferred from source access.
    JSON front matter is YAML-compatible (YAML 1.2.2, 2021, section 1.2 YAML History).
    Config and split layout follows the website docs/capabilities-registry.md.
    """
    if not re.fullmatch(r"[a-z][a-z0-9.-]*", license_id):
        raise ValueError("A license identifier is required")
    files: dict[str, bytes] = {}
    tables = {}
    configs = []
    for name, project in builder.DATASETS[dataset].items():
        table_files, tables[name] = _table_files(name, project)
        files.update(table_files)
        table_path = f"{name}/train.jsonl"
        configs.append(
            {
                "config_name": name,
                "data_files": [{"split": "train", "path": table_path}],
            }
        )
    manifest = {
        "schema_version": 1,
        "dataset": dataset,
        "license": license_id,
        "projection_sha256": _sha(Path(builder.__file__).read_bytes()),
        "packager_sha256": _sha(Path(__file__).read_bytes()),
        "tables": tables,
    }
    files["manifest.json"] = _json_bytes(manifest)
    files["README.md"] = _card(dataset, tables, configs, license_id)
    return files


def _card(dataset, tables, configs, license_id):
    front = json.dumps({"license": license_id, "configs": configs}, indent=2)
    sections = [
        f"---\n{front}\n---\n\n# {dataset}\n",
        "Projection of committed explorer observations. No model is solved here.\n",
        "## Provenance and limits\n",
        "Engineering readiness: NOT_EVALUATED. Dataset export does not establish "
        "engineering qualification. Source and output digests are in manifest.json; "
        "source sidecars are retained under sources/.\n",
    ]
    for name, table in tables.items():
        sections.append(
            f"### {name}\n\n{table['data_limits']}\n\n"
            f"Source: `{table['source']}`; generator: `{table['generator']}`.\n\n"
            f"Rows: {table['row_count']}. Columns: {', '.join(table['columns'])}.\n"
        )
    return "\n".join(sections).encode()


def write_bundle(dataset: str, destination: Path, *, license_id: str) -> None:
    """Write a new directory only; refuse to overwrite existing data."""
    files = bundle_files(dataset, license_id=license_id)
    destination.mkdir(parents=True, exist_ok=False)
    for name, data in files.items():
        path = destination / name
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_bytes(data)
        if path.read_bytes() != data:
            raise ValueError(f"Readback mismatch: {name}")


def verify_bundle(dataset: str, destination: Path, *, license_id: str) -> None:
    """Compare every byte and expected path without modifying the bundle."""
    expected = bundle_files(dataset, license_id=license_id)
    actual = {
        p.relative_to(destination).as_posix(): p.read_bytes()
        for p in destination.rglob("*")
        if p.is_file()
    }
    if expected != actual:
        raise ValueError(
            "Bundle differs from current sources or contains unexpected files"
        )


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--dataset", required=True, choices=sorted(builder.DATASETS))
    parser.add_argument("--out", required=True, type=Path, help="new bundle directory")
    parser.add_argument("--license", required=True, dest="license_id")
    parser.add_argument(
        "--verify", action="store_true", help="read-only byte comparison"
    )
    args = parser.parse_args()
    action = verify_bundle if args.verify else write_bundle
    action(args.dataset, args.out, license_id=args.license_id)
    print(
        f"{args.dataset}: {'verified' if args.verify else 'prepared'} (not published)"
    )


if __name__ == "__main__":
    main()
