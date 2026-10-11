"""Populate the bounded public benchmark fixture database (issue 1736).

Inputs remain the repository spreadsheets; no hull scales or solver qualification
are inferred. General RAODatabase and RaoRegistry storage contracts are unchanged.
"""

import hashlib
import json
import os
from datetime import date
from pathlib import Path
import subprocess

import numpy as np

from digitalmodel.hydrodynamics.hull_library.rao_database import RAODatabase
from digitalmodel.hydrodynamics.hull_library.rao_extractor import (
    XlsxFormatDetector,
    populate_database_from_xlsx,
)
from digitalmodel.hydrodynamics.hull_library.rao_fixture_comparison import (
    FIELDS,
    compare_box_exports,
)

FIXTURE_STEMS = (
    "test01_unit_box", "ellipsoid", "L00_test01",
    "L01_001_ship_raos", "L02_OC4_semi_sub",
)
BOX_PAIR = {"test01_unit_box", "L00_test01"}
EXPECTED_IDS = set(FIXTURE_STEMS) - {"L00_test01"}
EXPORTER_SOURCE = "workspace-hub:scripts/solver/process-queue.py::_export_orcawave_xlsx"
BASIS = {
    "frequencies": "rad/s", "directions": "deg", "phases": "deg",
    "translations": "m/m", "rotations": "rad/m",
    "unit_evidence": "L00_test01.xlsx:Displacement RAOs, row 9; raw displacementRAOs exporter",
    "phase_evidence": "native degree phases; pipeline degrees(angle(displacementRAOs))",
    "coordinate_reference_qualification": "unverified; benchmark fixture comparison only",
    "exporter_source": EXPORTER_SOURCE,
}


def _same_content(left, right) -> bool:
    return (left.hull_params == right.hull_params
            and left.metadata.get("basis") == right.metadata.get("basis")
            and all(np.array_equal(getattr(left.rao_data, field),
                                   getattr(right.rao_data, field)) for field in FIELDS))


def _merge_sources(retained, incoming) -> None:
    sources = retained.metadata.setdefault("sources", [])
    for source in incoming.metadata.get("sources", []):
        old = next((item for item in sources if item["path"] == source["path"]), None)
        if old is not None and old != source:
            raise ValueError("Changed provenance for an occupied source path")
        if old is None:
            sources.append(source)


def store_fixture_entry(db: RAODatabase, entry, box_comparison=None):
    """Collapse exact content; permit only the separately verified box alias."""
    if any(not np.isfinite(getattr(entry.rao_data, field)).all() for field in FIELDS):
        raise ValueError("RAO arrays must be finite")
    for retained in db.query({}):
        exact = _same_content(retained, entry)
        box_alias = (box_comparison is not None and box_comparison["equivalent"]
                     and {retained.variation_id, entry.variation_id} == BOX_PAIR
                     and retained.hull_params == entry.hull_params
                     and retained.metadata.get("basis") == entry.metadata.get("basis"))
        if retained.variation_id == entry.variation_id and not exact:
            raise ValueError("Different data for an occupied variation ID")
        if exact or box_alias:
            _merge_sources(retained, entry)
            if box_alias:
                retained.metadata["box_comparison"] = box_comparison
            return retained
    db.store(entry)
    return entry


def _source_provenance(root: Path, path: Path, retrieved_on: str) -> dict:
    relative = path.resolve().relative_to(root).as_posix()
    # Fixture root resolution must not inherit another checkout's Git binding.
    env = {key: value for key, value in os.environ.items()
           if key not in {"GIT_DIR", "GIT_WORK_TREE", "GIT_COMMON_DIR"}}
    revision = subprocess.run(
        ["git", "log", "-1", "--format=%H", "--", relative], cwd=root,
        env=env, check=True, capture_output=True, text=True,
    ).stdout.strip()
    if not revision:
        raise ValueError("Fixture has no source revision")
    raw = path.read_bytes()
    committed = subprocess.run(
        ["git", "show", f"{revision}:{relative}"], cwd=root,
        env=env, check=True, capture_output=True,
    ).stdout
    if raw != committed:
        raise ValueError("Fixture differs from its cited source revision")
    fmt = XlsxFormatDetector.detect(path)
    return {
        "path": relative, "sha256": hashlib.sha256(raw).hexdigest(),
        "revision": revision, "retrieved_on": retrieved_on, "format": fmt,
        "worksheet": "RAOs" if fmt == "pipeline" else "Displacement RAOs",
        "table": "all displacement RAO rows and headings",
        # Resolve from data/manifest on the repository's blob view.
        "url": f"../../{revision}/{relative}",
    }


def populate_fixture_database(db: RAODatabase, repo_root: Path, retrieved_on: str):
    """Extract every named fixture, preserving aliases and repeat-run provenance."""
    root = Path(repo_root).resolve()
    date.fromisoformat(retrieved_on)
    previous = {source["path"]: source for entry in db.query({})
                for source in entry.metadata.get("sources", [])}
    for stem in FIXTURE_STEMS:
        path = root / "tests/fixtures/solver" / f"{stem}.xlsx"
        source = _source_provenance(root, path, retrieved_on)
        old = previous.get(source["path"])
        if old is not None and old["sha256"] == source["sha256"]:
            source["retrieved_on"] = old["retrieved_on"]
        entry = populate_database_from_xlsx(
            RAODatabase(), path, stem, {},
            metadata={"source_file": source["path"], "solver": "OrcaWave",
                      "basis": BASIS.copy(), "sources": [source]},
        )
        comparison = None
        if stem == "L00_test01":
            comparison = compare_box_exports(
                db.get_by_id("test01_unit_box").rao_data, entry.rao_data,
                root / "tests/fixtures/solver/test01_unit_box.xlsx", path,
            )
            if not comparison["equivalent"]:
                raise ValueError("Box alias comparison failed at source export precision")
        store_fixture_entry(db, entry, comparison)
    if {entry.variation_id for entry in db.query({})} != EXPECTED_IDS:
        raise ValueError("Unexpected fixture entry set; unrelated data cannot be replaced")
    return db


def _verify_readback(expected: RAODatabase, path: Path) -> None:
    restored = RAODatabase()
    restored.load_from_disk(path)
    if {entry.variation_id for entry in restored.query({})} != EXPECTED_IDS:
        raise ValueError("Parquet readback entry set differs")
    for entry in expected.query({}):
        saved = restored.get_by_id(entry.variation_id)
        if (not _same_content(entry, saved) or entry.metadata != saved.metadata
                or entry.rao_data.vessel_name != saved.rao_data.vessel_name):
            raise ValueError("Parquet readback content differs")


def regenerate_fixture_database(root: Path, output: Path, manifest: Path, retrieved_on: str):
    """Regenerate through repo population code; preserve divergent existing data."""
    output, manifest = Path(output), Path(manifest)
    if output.resolve() == manifest.resolve():
        raise ValueError("Database and manifest paths must differ")
    db = RAODatabase()
    if output.exists():
        db.load_from_disk(output)
        if {entry.variation_id for entry in db.query({})} - EXPECTED_IDS:
            raise ValueError("Existing database contains unrelated entries")
    populate_fixture_database(db, root, retrieved_on)
    source_date = db.get_by_id("test01_unit_box").metadata["sources"][0]["retrieved_on"]
    db.save_to_disk(output)
    _verify_readback(db, output)
    payload = {
        "owner": "digitalmodel", "dataset": "rao_database", "basis": BASIS,
        "readiness": "benchmark fixtures; engineering coordinate/reference qualification unverified",
        "reuse": "Existing in-repository fixtures; repository LICENSE applies; no new upstream rights claimed",
        "regenerate": "PYTHONPATH=src uv run --no-sources python scripts/populate_rao_database.py "
                      f"--retrieved-on {source_date}",
        "comparison_criterion": "Named box pair only: per-source half decimal export unit (10**(order - observed significant digits + 1)/2) plus binary spacing; circular phase residual includes full-turn spacing. Generic deduplication requires exact arrays and matching basis.",
        "database_sha256": hashlib.sha256(output.read_bytes()).hexdigest(),
        "database_bytes": output.stat().st_size,
        "entries": [{"variation_id": entry.variation_id,
                     "frequency_count": len(entry.rao_data.frequencies),
                     "heading_count": len(entry.rao_data.directions),
                     "sources": entry.metadata["sources"],
                     **({"box_comparison": entry.metadata["box_comparison"]}
                        if "box_comparison" in entry.metadata else {})}
                    for entry in db.query({})],
    }
    manifest.parent.mkdir(parents=True, exist_ok=True)
    manifest.write_text(json.dumps(payload, indent=2) + "\n", encoding="utf-8")
    if json.loads(manifest.read_text(encoding="utf-8")) != payload:
        raise ValueError("Manifest readback differs")
    return db
