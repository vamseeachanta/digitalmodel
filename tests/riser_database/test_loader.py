"""Suite 1 (#1245): RiserDatabase loader — typed rows, bounded reads, fail-closed.

Escalation contract mirrors ``Atlas.predict``: an unknown key raises with a
reason — never a fuzzy match, never a silent default.
"""
from __future__ import annotations

import hashlib
import shutil
from pathlib import Path

import pytest
import yaml

from digitalmodel.riser_database import loader as rdb_loader
from digitalmodel.riser_database.loader import (
    DEFAULT_DB_ROOT,
    MaterialSnScfDffRow,
    RiserConfigRow,
    RiserDatabase,
    RiserDatabaseError,
    StandardsCrosswalkRow,
)

REPO_ROOT = Path(__file__).resolve().parents[2]
SOURCES_SPEC = REPO_ROOT / "scripts" / "riser_database" / "sources.yml"


def _copy_db(tmp_path: Path) -> Path:
    dst = tmp_path / "riser_database"
    shutil.copytree(DEFAULT_DB_ROOT, dst)
    return dst


def _repair_manifest(root: Path, table: str) -> None:
    """Recompute a table's sha256/rows in manifest.yaml after a deliberate edit,
    so tests can isolate a single defect (e.g. wrong columns with a valid hash)."""
    manifest_path = root / "manifest.yaml"
    manifest = yaml.safe_load(manifest_path.read_text())
    table_file = root / manifest["tables"][table]["file"]
    manifest["tables"][table]["sha256"] = hashlib.sha256(
        table_file.read_bytes()
    ).hexdigest()
    manifest["tables"][table]["rows"] = max(
        0, len(table_file.read_text().splitlines()) - 1
    )
    manifest_path.write_text(yaml.safe_dump(manifest, sort_keys=True))


# -- happy path ----------------------------------------------------------------


def test_load_happy_path_typed_rows():
    db = RiserDatabase.load()
    assert all(isinstance(r, RiserConfigRow) for r in db.configs)
    assert all(isinstance(r, StandardsCrosswalkRow) for r in db.crosswalk_rows)
    assert all(isinstance(r, MaterialSnScfDffRow) for r in db.materials)


def test_row_counts_pinned():
    db = RiserDatabase.load()
    # 5 examples/riser_input + 22 config/pipe riser files (explicit list in sources.yml)
    assert len(db.configs) == 27
    assert len(db.crosswalk_rows) == 4
    assert len(db.materials) >= 2


def test_config_row_values_scr():
    row = RiserDatabase.load().config("riser_scr")
    assert row.riser_type == "SCR"
    assert row.water_depth_m == 1500.0
    assert row.od_mm == pytest.approx(273.1)
    assert row.material_grade == "X65"
    assert row.source_path == "examples/riser_input/riser_scr.yml"


def test_config_row_pipe_filename_derivation():
    # depth/position live only in the FILENAME (ft -> m x 0.3048); OD/WT are
    # implicitly inches in the pipe schema (x 25.4 -> mm).
    row = RiserDatabase.load().config("pipe_py_21OD_0875in_5000ft_upper_riser")
    assert row.water_depth_m == pytest.approx(1524.0)
    assert row.od_mm == pytest.approx(533.4)
    assert row.wt_mm == pytest.approx(22.225)
    assert "upper" in row.analysis_kind


def test_config_row_drilling_variant_has_no_depth():
    # No depth token in the filename -> empty cell -> None. Never guessed.
    row = RiserDatabase.load().config("pipe_py_21OD_0875in_Drilling_Riser")
    assert row.water_depth_m is None


def test_every_seed_file_has_a_row():
    spec = yaml.safe_load(SOURCES_SPEC.read_text())
    db = RiserDatabase.load()
    ids = {r.config_id for r in db.configs}
    missing = [
        rel
        for rel in (spec["riser_input_files"] + spec["config_pipe_files"])
        if Path(rel).stem not in ids
    ]
    assert not missing, f"seed files without a config_catalog row: {missing}"


# -- escalation: unknown key raises, never defaults ------------------------------


@pytest.mark.parametrize("accessor", ["config", "crosswalk", "material"])
def test_unknown_key_escalates(accessor):
    db = RiserDatabase.load()
    with pytest.raises(RiserDatabaseError) as exc:
        getattr(db, accessor)("no-such-key")
    assert "no-such-key" in str(exc.value)
    assert "unknown" in exc.value.reason


# -- fail-closed loading ---------------------------------------------------------


def test_missing_root_fail_closed(tmp_path):
    with pytest.raises(RiserDatabaseError) as exc:
        RiserDatabase.load(root=tmp_path / "nope")
    assert "missing" in exc.value.reason


def test_missing_table_fail_closed(tmp_path):
    root = _copy_db(tmp_path)
    (root / "config_catalog.csv").unlink()
    with pytest.raises(RiserDatabaseError) as exc:
        RiserDatabase.load(root=root)
    assert exc.value.table == "config_catalog"


def test_fingerprint_mismatch_fail_closed(tmp_path):
    root = _copy_db(tmp_path)
    path = root / "standards_crosswalk.csv"
    path.write_text(path.read_text() + "\n")
    with pytest.raises(RiserDatabaseError) as exc:
        RiserDatabase.load(root=root)
    assert "fingerprint" in exc.value.reason


def test_wrong_columns_fail_closed(tmp_path):
    root = _copy_db(tmp_path)
    path = root / "material_sn_scf_dff.csv"
    lines = path.read_text().splitlines()
    lines[0] = lines[0] + ",smuggled_value"
    lines[1:] = [line + ",42.0" for line in lines[1:]]
    path.write_text("\n".join(lines) + "\n")
    _repair_manifest(root, "material_sn_scf_dff")
    with pytest.raises(RiserDatabaseError) as exc:
        RiserDatabase.load(root=root)
    assert "column" in exc.value.reason


def test_duplicate_key_fail_closed(tmp_path):
    root = _copy_db(tmp_path)
    path = root / "standards_crosswalk.csv"
    lines = path.read_text().splitlines()
    path.write_text("\n".join(lines + [lines[1]]) + "\n")
    _repair_manifest(root, "standards_crosswalk")
    with pytest.raises(RiserDatabaseError) as exc:
        RiserDatabase.load(root=root)
    assert "duplicate" in exc.value.reason


def test_oversized_table_fail_closed(tmp_path, monkeypatch):
    root = _copy_db(tmp_path)
    monkeypatch.setattr(rdb_loader, "MAX_TABLE_BYTES", 16)
    with pytest.raises(RiserDatabaseError) as exc:
        RiserDatabase.load(root=root)
    assert "size" in exc.value.reason


def test_iter_public_rows_covers_all_tables():
    db = RiserDatabase.load()
    tables = {table for table, _key, _flat in db.iter_public_rows()}
    assert tables == {"config_catalog", "standards_crosswalk", "material_sn_scf_dff"}
    n_rows = sum(1 for _ in db.iter_public_rows())
    assert n_rows == len(db.configs) + len(db.crosswalk_rows) + len(db.materials)
