# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Suite 2 (#1245): schema allowlists + content-shape tripwires.

The public tables carry clause IDENTIFIERS and references only. A new
value-bearing column is a test failure by construction (exact column-set
match), and free-text fields are shape-constrained so licensed values cannot
ride past the column allowlist ([R2-B4]).
"""

from __future__ import annotations

import csv
import hashlib
import re
import shutil
from dataclasses import asdict
from pathlib import Path

import pytest
import yaml

from digitalmodel.riser_database.loader import (
    DEFAULT_DB_ROOT,
    RiserDatabase,
    RiserDatabaseError,
)

CONFIG_CATALOG_COLUMNS = [
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
STANDARDS_CROSSWALK_COLUMNS = [
    "code_id",
    "family_key",
    "publisher",
    "section",
    "topic",
    "wiki_path",
    "registry_revision",
    "note",
]
MATERIAL_SN_SCF_DFF_COLUMNS = [
    "ref_id",
    "sn_curve_name",
    "environment",
    "scf_ref",
    "dff_ref",
    "code_id",
    "wiki_path",
    "note",
]

# Clause locator, not clause content: short identifier like "Sec.5" / "Sec.3.3".
SECTION_RE = re.compile(r"^$|^§?[0-9A-Za-z][0-9A-Za-z .\-()]{0,30}$")
FREE_TEXT_FIELDS = {"note", "notes", "topic"}
FREE_TEXT_MAX_LEN = 160
FREE_TEXT_MAX_NUMERIC_TOKENS = 3
_NUMERIC_TOKEN_RE = re.compile(r"\d+(?:\.\d+)?")


def _read(table_file: str) -> tuple[list[str], list[dict]]:
    """Check the public rows returned after fingerprint and header validation."""
    accessor = {
        "config_catalog.csv": "configs",
        "standards_crosswalk.csv": "crosswalk_rows",
        "material_sn_scf_dff.csv": "materials",
    }[table_file]
    rows = [asdict(row) for row in getattr(RiserDatabase.load(), accessor)]
    assert rows, f"{table_file}: schema checks require nonempty loaded rows"
    # Independent CSV decoding cross-checks the package's complete row output.
    with (DEFAULT_DB_ROOT / table_file).open(newline="", encoding="utf-8") as fh:
        reader = csv.DictReader(fh)
        header = list(reader.fieldnames)
        independent = list(reader)
    assert header == list(rows[0])
    assert len(independent) == len(rows)
    for raw, loaded in zip(independent, rows):
        for column, value in loaded.items():
            expected = raw[column]
            if value is None:
                assert expected == ""
            elif isinstance(value, float):
                assert value == float(expected)
            else:
                assert value == expected
    return header, rows


def test_loaded_public_table_counts():
    """Assert package output against the public manifest at revision 6476218e.

    Source: data/riser_database/manifest.yaml, tables.*.rows. Independent
    inventory: configurations = five input files + twenty-two pipe files +
    one lift reference = twenty-eight; the remaining CSVs contain five
    crosswalk, three material, eighty-four stack-up and ten rig records
    (excluding each header). These are inventory counts, not design limits.
    """
    db = RiserDatabase.load()
    # Public manifest, revision 6476218e, tables.*.rows (see inventory above).
    assert {
        "config_catalog": len(db.configs),
        "standards_crosswalk": len(db.crosswalk_rows),
        "material_sn_scf_dff": len(db.materials),
        "riser_stackup": len(db.stackups),
        "rig_riser_interface": len(db.rigs),
    } == {
        "config_catalog": 28,
        "standards_crosswalk": 5,
        "material_sn_scf_dff": 3,
        "riser_stackup": 84,
        "rig_riser_interface": 10,
    }


@pytest.mark.parametrize(
    "table",
    [
        "config_catalog",
        "standards_crosswalk",
        "material_sn_scf_dff",
        "riser_stackup",
        "rig_riser_interface",
    ],
)
def test_loaded_schema_rejects_added_column(tmp_path, table):
    """The loader's exact-header contract rejects a re-fingerprinted table.

    Source: loader.py::_load_table, expected_columns contract. A synthetic
    extra column is outside every declared row schema, so the independent
    expected outcome is a column-mismatch error for the modified table.
    Updating only its digest isolates schema validation from integrity checks.
    """
    root = tmp_path / "riser_database"
    shutil.copytree(DEFAULT_DB_ROOT, root)
    table_path = root / f"{table}.csv"
    with table_path.open(newline="") as fh:
        records = list(csv.reader(fh))
    with table_path.open("w", newline="") as fh:
        csv.writer(fh).writerows([*record, "unexpected_column"] for record in records)
    manifest_path = root / "manifest.yaml"
    manifest = yaml.safe_load(manifest_path.read_text())
    manifest["tables"][table]["sha256"] = hashlib.sha256(
        table_path.read_bytes()
    ).hexdigest()
    manifest_path.write_text(yaml.safe_dump(manifest))

    with pytest.raises(RiserDatabaseError) as exc:
        RiserDatabase.load(root=root)
    assert exc.value.table == table
    assert "column mismatch" in exc.value.reason


def test_config_catalog_columns_exact():
    header, _ = _read("config_catalog.csv")
    assert header == CONFIG_CATALOG_COLUMNS


def test_standards_crosswalk_columns_exact():
    header, _ = _read("standards_crosswalk.csv")
    assert header == STANDARDS_CROSSWALK_COLUMNS


def test_material_sn_scf_dff_columns_exact():
    header, _ = _read("material_sn_scf_dff.csv")
    assert header == MATERIAL_SN_SCF_DFF_COLUMNS


def test_wiki_paths_are_canonical_relative():
    for table in ("standards_crosswalk.csv", "material_sn_scf_dff.csv"):
        _, rows = _read(table)
        for row in rows:
            wiki_path = row["wiki_path"]
            assert wiki_path.startswith("wikis/"), (table, wiki_path)
            assert "knowledge/" not in wiki_path, (table, wiki_path)
            assert "http" not in wiki_path, (table, wiki_path)
            assert not Path(wiki_path).is_absolute(), (table, wiki_path)


def test_section_is_a_locator_not_content():
    _, rows = _read("standards_crosswalk.csv")
    for row in rows:
        assert SECTION_RE.match(row["section"]), row["section"]


def test_free_text_shape_constraints():
    violations = []
    for table in (
        "config_catalog.csv",
        "standards_crosswalk.csv",
        "material_sn_scf_dff.csv",
    ):
        _, rows = _read(table)
        for row in rows:
            for field in FREE_TEXT_FIELDS & set(row):
                text = row[field]
                if len(text) > FREE_TEXT_MAX_LEN:
                    violations.append((table, field, "too long"))
                if len(_NUMERIC_TOKEN_RE.findall(text)) > FREE_TEXT_MAX_NUMERIC_TOKENS:
                    violations.append((table, field, f"numeric-dense: {text!r}"))
    assert not violations, violations


def test_no_numeric_value_columns_in_reference_tables():
    """The reference tables must not carry standards-derived numeric VALUES:
    every non-identifier cell must not parse as a bare number.

    ``registry_revision`` is exempt: revision strings are identifiers, and
    edition years ("2010") — or even "3e-2025" — happen to parse as floats.
    """
    for table in ("standards_crosswalk.csv", "material_sn_scf_dff.csv"):
        _, rows = _read(table)
        for row in rows:
            for field, value in row.items():
                if field == "registry_revision":
                    continue
                try:
                    float(value)
                except ValueError:
                    continue
                raise AssertionError(
                    f"{table}:{field} carries a bare numeric value {value!r} — "
                    "values belong in CitedValue getters, not public tables"
                )
