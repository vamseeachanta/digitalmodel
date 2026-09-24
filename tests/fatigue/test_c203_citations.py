"""DNV-RP-C203 citation correctness (#2161).

The S-N library implements the 2021 amendment of the 2019 edition. The
table that holds each environment's S-N curves depends on the edition:

* 2011: Table 2-1 air, Table 2-2 seawater with CP, Table 2-3 free corrosion.
* 2019 / 2021: Table 2-1 air, Table 2-2 seawater with CP, Table 2-3 tubular
  joints, Table 2-4 free corrosion.

Weld detail classification is in Appendix A (Tables A-1 to A-10) in both
layouts, not in the Section 2 S-N tables.

These tests pin labels only; no numeric value is asserted or changed here.
"""

from __future__ import annotations

import re
from pathlib import Path

import pytest

from digitalmodel.fatigue import c203_editions as ce
from digitalmodel.fatigue import sn_curves, weld_classification
from digitalmodel.fatigue.sn_library import get_catalog

SRC = Path(__file__).resolve().parents[2] / "src" / "digitalmodel"


# -- the single edition / table map ------------------------------------------


def test_implemented_edition_is_2021():
    assert ce.DNV_RP_C203_IMPLEMENTED_EDITION == "2021"


@pytest.mark.parametrize(
    ("edition", "environment", "table"),
    [
        ("2011", "air", "Table 2-1"),
        ("2011", "seawater_cp", "Table 2-2"),
        ("2011", "free_corrosion", "Table 2-3"),
        ("2019", "air", "Table 2-1"),
        ("2019", "seawater_cp", "Table 2-2"),
        ("2019", "free_corrosion", "Table 2-4"),
        ("2021", "air", "Table 2-1"),
        ("2021", "seawater_cp", "Table 2-2"),
        ("2021", "free_corrosion", "Table 2-4"),
    ],
)
def test_sn_table_by_edition(edition, environment, table):
    assert ce.c203_sn_table(environment, edition) == table


def test_sn_table_defaults_to_implemented_edition():
    assert ce.c203_sn_table("free_corrosion") == "Table 2-4"


def test_tubular_joint_table_2019_layout():
    assert ce.c203_sn_table("tubular_joint", "2021") == "Table 2-3"
    assert ce.c203_sn_table("tubular_joint", "2019") == "Table 2-3"


def test_unknown_edition_or_environment_fails_closed():
    with pytest.raises(KeyError):
        ce.c203_sn_table("air", "2016")
    with pytest.raises(KeyError):
        ce.c203_sn_table("seabed", "2021")
    with pytest.raises(KeyError):
        # 2011 tubular-joint table is not mapped (not established here)
        ce.c203_sn_table("tubular_joint", "2011")


def test_detail_classification_is_appendix_a():
    assert ce.DNV_RP_C203_DETAIL_CLASSIFICATION == "Appendix A (Tables A-1 to A-10)"


# -- the S-N library cites the map -------------------------------------------


def _dnv_records():
    return [c for c in get_catalog().curves if c.standard == "DNV-RP-C203"]


def test_sn_library_dnv_records_state_implemented_edition():
    records = _dnv_records()
    assert records
    assert {r.standard_edition for r in records} == {"2021"}


@pytest.mark.parametrize("environment", ["air", "seawater_cp", "free_corrosion"])
def test_sn_library_notes_cite_edition_table(environment):
    records = [r for r in _dnv_records() if r.environment == environment]
    assert len(records) == 14
    expected = ce.c203_sn_table(environment)
    for r in records:
        assert f"DNV-RP-C203 {expected}" in r.note, r.note


def test_sn_library_free_corrosion_not_cited_as_table_2_2():
    for r in _dnv_records():
        if r.environment == "free_corrosion":
            assert "Table 2-2" not in r.note


def test_sn_curves_docstring_cites_table_2_4_for_free_corrosion():
    doc = sn_curves.__doc__
    assert "Table 2-4" in doc
    assert "free corrosion per Table 2-2" not in doc


# -- weld detail classification ----------------------------------------------


def test_weld_classification_docstring_points_to_appendix_a():
    doc = weld_classification.__doc__
    assert "Appendix A (Tables A-1 to A-10)" in doc
    assert "2-1 through 2-8" not in doc
    assert "2-1 to 2-8" not in doc


def test_weld_classification_rules_cite_appendix_a_or_tubular_table():
    tables = {rule["table"] for rule in weld_classification._RULES}
    allowed = {
        ce.DNV_RP_C203_DETAIL_CLASSIFICATION,
        f"{ce.c203_sn_table('tubular_joint')} (T-curve)",
    }
    assert tables <= allowed, tables


def test_weld_classification_result_table_label():
    result = weld_classification.classify_weld_detail(
        weld_classification.WeldDetail(
            description="fillet weld toe", joint_type="fillet"
        )
    )
    assert result.dnv_table.startswith("Appendix A")


# -- structural design-code report ---------------------------------------------


def test_design_code_report_cites_edition_tables():
    from digitalmodel.structural.fatigue.design_code_report import _SUPPORTED_STANDARDS

    refs = " | ".join(_SUPPORTED_STANDARDS["DNV-RP-C203"]["references"])
    assert "2016" not in refs
    assert (
        "Table 2-1 / Table 2-2 / Table 2-4 (air / seawater+CP / free corrosion)" in refs
    )
    assert "Table 2-1 to Table 2-3" not in refs


# -- source-level scans --------------------------------------------------------

_FREE_CORR_2_2 = re.compile(
    r"free[ -_]?corrosion.*Table 2-2|Table 2-2.*free[ -_]?corrosion", re.I
)
_C203_2016 = re.compile(r"C203\s*\(2016\)")


def _py_files(*subdirs: str):
    for sub in subdirs:
        yield from (SRC / sub).rglob("*.py")


def test_no_free_corrosion_cited_as_table_2_2_in_fatigue_sources():
    hits = []
    for path in _py_files("fatigue", "structural/fatigue", "riser_fatigue"):
        lines = path.read_text(encoding="utf-8").splitlines()
        for i, line in enumerate(lines, 1):
            # a comment heading followed by the body on the next line
            window = line + " " + (lines[i] if i < len(lines) else "")
            # a line may name Table 2-2 (seawater with CP) next to the correct
            # free-corrosion table; only a free-corrosion mention without it is wrong
            names_fc_table = "Table 2-4" in window or "Table 2-3" in window
            if (
                "Table 2-2" in line
                and _FREE_CORR_2_2.search(window)
                and not names_fc_table
            ):
                hits.append(f"{path.relative_to(SRC)}:{i}")
    assert not hits, hits


def test_no_c203_2016_edition_label_in_src():
    hits = []
    for path in SRC.rglob("*.py"):
        for i, line in enumerate(path.read_text(encoding="utf-8").splitlines(), 1):
            if _C203_2016.search(line):
                hits.append(f"{path.relative_to(SRC)}:{i}")
    assert not hits, hits


# -- review r1 (PR #2164) ------------------------------------------------------


def test_c203_label_fails_closed_for_unmapped_edition():
    assert ce.c203_label("2011") == "DNV-RP-C203 (2011)"
    with pytest.raises(KeyError):
        ce.c203_label("2099")


def test_free_corrosion_notes_disclose_unverified_values():
    fc = [
        r
        for r in get_catalog().curves
        if r.standard == "DNV-RP-C203" and r.environment == "free_corrosion"
    ]
    assert fc
    for r in fc:
        assert "values not verified" in r.note and "#2165" in r.note, r.note
