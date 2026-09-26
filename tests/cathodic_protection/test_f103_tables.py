"""Tests for the cited DNV-RP-F103 table lookups (issue #2207).

Every table value is asserted against the fixture CSVs copied verbatim from
the llm-wiki datasets (see ``tests/fixtures/test_vectors/cathodic_protection/
datasets/PROVENANCE.md``). The Annex 1 CSVs print ``a x100`` / ``b x100``;
the tests divide the CSV cell by 100 exactly as the module does.
"""

from __future__ import annotations

import csv
from pathlib import Path
import warnings

import pytest

from digitalmodel.cathodic_protection import f103_tables as tbl
from digitalmodel.cathodic_protection.f103_tables import (
    Exposure,
    FieldJointCoating,
    FluidTemperatureBand,
    LinepipeCoating,
)
from digitalmodel.citations import CitationResolutionError, validate_citation
from digitalmodel.citations.resolver import resolve_wiki_path

FIXTURE_DIR = (
    Path(__file__).resolve().parents[1]
    / "fixtures"
    / "test_vectors"
    / "cathodic_protection"
    / "datasets"
    / "dnv-rp-f103"
    / "2010"
)
_PREFIX = "dnv-rp-f103-2010-table-"

EDITION = "2010"
SCALE = 100.0

# Representative temperature inside each Table 5-1 band.
_TEMPERATURE_IN_BAND = {
    FluidTemperatureBand.LE_50: 20.0,
    FluidTemperatureBand.GT_50_80: 65.0,
    FluidTemperatureBand.GT_80_120: 100.0,
    FluidTemperatureBand.GT_120: 130.0,
}


# ---------------------------------------------------------------------------
# CSV parsing helpers
# ---------------------------------------------------------------------------


def _read_table(number: int) -> list[list[str]]:
    path = FIXTURE_DIR / f"{_PREFIX}{number:03d}.csv"
    with path.open(encoding="utf-8", newline="") as handle:
        return [[cell.strip() for cell in row] for row in csv.reader(handle)]


def _band_from_header(cell: str) -> FluidTemperatureBand:
    """``≤ 50`` -> LE_50, ``>50 - 80`` -> GT_50_80, ..."""
    return FluidTemperatureBand(cell.replace("≤", "<=").replace(" ", ""))


def _parse_table_5_1() -> dict[tuple[Exposure, FluidTemperatureBand], float]:
    rows = _read_table(1)
    bands = {
        idx: _band_from_header(cell) for idx, cell in enumerate(rows[2]) if idx > 0
    }
    return {
        (Exposure(row[0].rstrip("*)")), band): float(row[col])
        for row in rows[3:]
        for col, band in bands.items()
    }


def _data_rows(rows: list[list[str]]) -> list[list[str]]:
    """Rows after the header, excluding the trailing ``Note:`` row."""
    return [row for row in rows[2:] if not row[0].startswith("Note")]


def _parse_table_a1() -> dict[int, dict[str, object]]:
    """Table A.1 keyed by DNV-RP-F106 CDS number."""
    out: dict[int, dict[str, object]] = {}
    for row in _data_rows(_read_table(2)):
        cds = int(row[1].replace("No.", "").strip())
        out[cds] = {
            "concrete": row[2] == "yes",
            "max_temp": float(row[3]),
            "a_x100": float(row[4]),
            "b_x100": float(row[5]),
        }
    return out


def _parse_table_a2() -> dict[str, dict[str, object]]:
    """Table A.2 keyed by FJC system code (``none``, ``1A``, ``2A``...)."""
    out: dict[str, dict[str, object]] = {}
    for row in _data_rows(_read_table(3)):
        code = row[0].split()[0]
        key = code.lower() if code == "None" else code
        out[key] = {
            "max_temp": float(row[2]) if row[2] else None,
            "a_x100": float(row[4]),
            "b_x100": float(row[5]),
        }
    return out


@pytest.fixture(scope="module")
def table_5_1() -> dict[tuple[Exposure, FluidTemperatureBand], float]:
    return _parse_table_5_1()


@pytest.fixture(scope="module")
def table_a1() -> dict[int, dict[str, object]]:
    return _parse_table_a1()


@pytest.fixture(scope="module")
def table_a2() -> dict[str, dict[str, object]]:
    return _parse_table_a2()


# ---------------------------------------------------------------------------
# Fixture sanity
# ---------------------------------------------------------------------------


def test_fixture_csvs_present():
    for number in range(1, 4):
        assert (FIXTURE_DIR / f"{_PREFIX}{number:03d}.csv").is_file()


def test_table_5_1_parses_all_cells(table_5_1):
    assert len(table_5_1) == 8


def test_table_a1_covers_every_linepipe_coating(table_a1):
    assert set(table_a1) == {coating.cds_number for coating in LinepipeCoating}


def test_table_a2_covers_every_field_joint_coating(table_a2):
    assert set(table_a2) == {fjc.value for fjc in FieldJointCoating}


# ---------------------------------------------------------------------------
# Table 5-1
# ---------------------------------------------------------------------------


@pytest.mark.parametrize("exposure", list(Exposure))
@pytest.mark.parametrize("band", list(FluidTemperatureBand))
def test_mean_current_density_matches_table_5_1(table_5_1, exposure, band):
    result = tbl.mean_current_density(
        exposure, _TEMPERATURE_IN_BAND[band], edition=EDITION
    )
    assert result.value == table_5_1[(exposure, band)]
    assert result.units == "A/m2"
    assert result.citation.section == "Table 5-1"
    assert band.value in result.citation.note


def test_non_buried_le_50_spot_value():
    """Hand-derived: non-buried, <= 50 °C, 0.050 A/m2."""
    result = tbl.mean_current_density(Exposure.NON_BURIED, 40.0, edition=EDITION)
    assert result.value == pytest.approx(0.050)


@pytest.mark.parametrize(
    ("fluid_temp_c", "expected"),
    [
        (-5.0, FluidTemperatureBand.LE_50),
        (50.0, FluidTemperatureBand.LE_50),  # header "<= 50"
        (50.01, FluidTemperatureBand.GT_50_80),
        (80.0, FluidTemperatureBand.GT_50_80),  # header ">50 - 80"
        (80.01, FluidTemperatureBand.GT_80_120),
        (120.0, FluidTemperatureBand.GT_80_120),  # header ">80 - 120"
        (120.01, FluidTemperatureBand.GT_120),
    ],
)
def test_fluid_temperature_band_boundaries(fluid_temp_c, expected):
    assert tbl.fluid_temperature_band(fluid_temp_c) is expected


# ---------------------------------------------------------------------------
# Table A.1
# ---------------------------------------------------------------------------


@pytest.mark.parametrize("coating", list(LinepipeCoating))
def test_linepipe_coating_constants_match_table_a1(table_a1, coating):
    row = table_a1[coating.cds_number]
    a, b = tbl.linepipe_coating_constants(coating, edition=EDITION)
    assert a.value == row["a_x100"] / SCALE
    assert b.value == row["b_x100"] / SCALE
    assert coating.max_temperature_c == row["max_temp"]
    assert a.units == "dimensionless"
    assert b.units == "1/yr"
    assert a.citation.section == "Table A.1"
    assert b.citation.section == "Table A.1"
    assert f"CDS No. {coating.cds_number}" in a.citation.note


def test_fbe_spot_values():
    """Hand-derived: single/dual layer FBE a = 0.010, b = 0.0003."""
    a, b = tbl.linepipe_coating_constants(LinepipeCoating.FBE, edition=EDITION)
    assert a.value == pytest.approx(0.010)
    assert b.value == pytest.approx(0.0003)
    assert LinepipeCoating.FBE.cds_number == 1
    assert LinepipeCoating.FBE.max_temperature_c == 90.0


# ---------------------------------------------------------------------------
# Table A.2
# ---------------------------------------------------------------------------


@pytest.mark.parametrize("fjc", list(FieldJointCoating))
def test_field_joint_coating_constants_match_table_a2(table_a2, fjc):
    row = table_a2[fjc.value]
    a, b = tbl.field_joint_coating_constants(fjc, edition=EDITION)
    assert a.value == row["a_x100"] / SCALE
    assert b.value == row["b_x100"] / SCALE
    assert fjc.max_temperature_c == row["max_temp"]
    assert a.citation.section == "Table A.2"
    assert b.citation.section == "Table A.2"
    assert f"FJC {fjc.value}" in b.citation.note


def test_no_field_joint_coating_has_no_max_temperature():
    assert FieldJointCoating.NONE.max_temperature_c is None


# ---------------------------------------------------------------------------
# Bracelet utilisation (deferred to B401)
# ---------------------------------------------------------------------------


def test_bracelet_utilisation_factor_defers_to_b401_table_10_8():
    result = tbl.bracelet_utilisation_factor(edition=EDITION)
    assert result.value == pytest.approx(0.80)
    assert result.units == "dimensionless"
    assert result.citation.code_id == "dnv-rp-b401"
    assert result.citation.revision == "2011"
    assert result.citation.section == "Table 10-8"
    assert "defers to DNV-RP-B401 Table 10-8" in result.citation.note
    assert "f103_edition=2010" in result.citation.note
    assert "f103_provenance=verified-2010-tables" in result.citation.note
    assert "edition=2010" in result.citation.note


def test_bracelet_utilisation_factor_2016_maps_to_b401_2017():
    result = tbl.bracelet_utilisation_factor(edition="2016")
    assert result.value == pytest.approx(0.80)
    assert "f103_provenance=inherited-2010-unverified" in result.citation.note
    assert "edition=2017" in result.citation.note


# ---------------------------------------------------------------------------
# Editions and provenance
# ---------------------------------------------------------------------------


@pytest.mark.parametrize(
    ("edition", "expected"),
    [
        ("2010", "verified-2010-tables"),
        ("dnv-rp-f103-2010", "verified-2010-tables"),
        ("2016", "inherited-2010-unverified"),
        ("DNVGL-RP-F103-2016", "inherited-2010-unverified"),
    ],
)
def test_edition_provenance(edition, expected):
    assert tbl.edition_provenance(edition) == expected


def test_edition_provenance_rejects_unknown_edition():
    with pytest.raises(ValueError, match="Unsupported DNV-RP-F103 edition"):
        tbl.edition_provenance("2003")


def test_edition_none_warns_and_defaults_to_2010():
    with pytest.warns(UserWarning, match="defaulting to DNV-RP-F103 2010"):
        result = tbl.mean_current_density(Exposure.BURIED, 25.0)
    assert "edition=2010" in result.citation.note
    assert "provenance=verified-2010-tables" in result.citation.note


def test_values_identical_across_editions(table_5_1):
    for edition in ("2010", "2016"):
        result = tbl.mean_current_density(Exposure.BURIED, 130.0, edition=edition)
        assert result.value == table_5_1[(Exposure.BURIED, FluidTemperatureBand.GT_120)]
        assert f"provenance={tbl.edition_provenance(edition)}" in result.citation.note


# ---------------------------------------------------------------------------
# Citations
# ---------------------------------------------------------------------------


def _all_f103_results() -> list:
    a1, b1 = tbl.linepipe_coating_constants(
        LinepipeCoating.MULTI_LAYER_FBE_PP, edition=EDITION
    )
    a2, b2 = tbl.field_joint_coating_constants(
        FieldJointCoating.FJC_3D_FBE_PP, edition=EDITION
    )
    return [
        tbl.mean_current_density(Exposure.NON_BURIED, 90.0, edition=EDITION),
        a1,
        b1,
        a2,
        b2,
    ]


def test_every_result_carries_an_f103_citation():
    with warnings.catch_warnings():
        warnings.simplefilter("error")
        results = _all_f103_results()
    for result in results:
        citation = result.citation
        assert citation.code_id == "dnv-rp-f103"
        assert citation.publisher == "DNV"
        assert citation.revision == "2010"
        assert citation.wiki_path == tbl.F103_WIKI_PATH
        assert "provenance=verified-2010-tables" in citation.note
        assert result.units


def test_citations_validate_against_live_wiki():
    try:
        page = resolve_wiki_path(tbl.F103_WIKI_PATH)
    except CitationResolutionError as exc:
        pytest.skip(f"wiki not resolvable: {exc.reason.splitlines()[0]}")
    if not page.is_file():
        pytest.skip(f"wiki page missing: {page}")
    for result in _all_f103_results():
        validate_citation(result.citation)
    validate_citation(tbl.bracelet_utilisation_factor(edition=EDITION).citation)
