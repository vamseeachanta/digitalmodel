"""Suite 2 (#1245): schema allowlists + content-shape tripwires.

The public tables carry clause IDENTIFIERS and references only. A new
value-bearing column is a test failure by construction (exact column-set
match), and free-text fields are shape-constrained so licensed values cannot
ride past the column allowlist ([R2-B4]).
"""
from __future__ import annotations

import csv
import re
from pathlib import Path

from digitalmodel.riser_database.loader import DEFAULT_DB_ROOT

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
    with (DEFAULT_DB_ROOT / table_file).open(newline="") as fh:
        reader = csv.DictReader(fh)
        return list(reader.fieldnames or []), list(reader)


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
