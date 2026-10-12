"""Tests for the cited DNV-RP-B401 table lookups (issues #2207, #2208).

Every table value is asserted against the fixture CSVs under
``tests/fixtures/test_vectors/cathodic_protection/datasets/dnv-rp-b401/``:
the 2005/2010 files are verbatim copies of the llm-wiki datasets, the
2017-06 and 2021-05 files are hand transcriptions of the licensed prints (see
``PROVENANCE.md`` there). Citations of the 2017 and 2021 editions are
validated against the vendored wiki fixture pages; the 2010 page is validated
against the live wiki when the resolver can find it.
"""

from __future__ import annotations

import csv
from functools import lru_cache
from pathlib import Path
import warnings

import pytest

from digitalmodel.cathodic_protection import b401_tables as tbl
from digitalmodel.cathodic_protection._edition import Edition
from digitalmodel.cathodic_protection.b401_tables import (
    AnodeEnvironment,
    AnodeMaterial,
    AnodeShape,
    Climate,
    DepthBand,
    DesignPhase,
    PaintCategory,
)
from digitalmodel.citations import CitationResolutionError, validate_citation
from digitalmodel.citations.resolver import resolve_wiki_path

DATASETS = (
    Path(__file__).resolve().parents[1]
    / "fixtures"
    / "test_vectors"
    / "cathodic_protection"
    / "datasets"
    / "dnv-rp-b401"
)
CITATION_FIXTURES = Path(__file__).resolve().parents[1] / "citations" / "fixtures"

# Per edition: fixture directory, file prefix, {table number: file id}, table
# label prefix. 2005 shares the 2010/2011 fixtures (identical tables).
_2010 = (
    "2005-with-2008-amendments",
    "dnv-rp-b401-2005-with-2008-amendments-table-",
    {1: "001", 2: "002", 3: "003", 4: "004", 6: "005", 8: "008"},
    "Table 10-",
)
EDITION_FIXTURES: dict[str, tuple[str, str, dict[int, str], str]] = {
    "2005": _2010,
    "2010": _2010,
    "2017": (
        "2017-06",
        "dnv-rp-b401-2017-06-table-",
        {n: f"a-{n}" for n in (1, 2, 3, 4, 6, 8)},
        "Table A-",
    ),
    "2021": (
        "2021-05",
        "dnv-rp-b401-2021-05-table-",
        {n: f"8-{n}" for n in (1, 2, 3, 4, 6, 8)},
        "Table 8-",
    ),
}
EDITIONS: list[Edition] = ["2005", "2010", "2017", "2021"]
AMBIENT_ANODE_EDITIONS: list[Edition] = ["2005", "2010", "2017"]
EXPECTED = {
    "2005": ("2011", tbl.B401_WIKI_PATH, "verified-2005-tables"),
    "2010": ("2011", tbl.B401_WIKI_PATH, "verified-2010-tables"),
    "2017": ("2017-06", tbl.B401_WIKI_PATH_2017, "verified-2017-tables"),
    "2021": ("2021-05", tbl.B401_WIKI_PATH_2021, "verified-2021-tables"),
}

# Edition whose tables the original fixtures hold; used so lookups do not warn.
EDITION: Edition = "2010"

_CLIMATE_BY_HEADER = {
    "tropical": Climate.TROPICAL,
    "sub-tropical": Climate.SUBTROPICAL,
    "temperate": Climate.TEMPERATE,
    "arctic": Climate.ARCTIC,
}
_MATERIAL_BY_LABEL = {
    "Al-based": AnodeMaterial.ALUMINIUM,
    "Zn-based": AnodeMaterial.ZINC,
    "Al-Zn-In": AnodeMaterial.ALUMINIUM,
    "Zn": AnodeMaterial.ZINC,
}
_ENVIRONMENT_BY_LABEL = {
    "seawater": AnodeEnvironment.SEAWATER,
    "sediments": AnodeEnvironment.SEDIMENT,
}
_SHAPE_BY_LABEL = {
    "Long slender stand-off": AnodeShape.LONG_SLENDER_STANDOFF,
    "Short slender stand-off": AnodeShape.SHORT_SLENDER_STANDOFF,
    "Long flush mounted": AnodeShape.LONG_FLUSH,
    "Long flush-mounted": AnodeShape.LONG_FLUSH,
    "Short flush-mounted, bracelet": AnodeShape.SHORT_FLUSH_BRACELET,
    "Short flush-mounted, bracelet and other types": AnodeShape.SHORT_FLUSH_BRACELET,
}
# Table 8-6 temperature row label -> representative temperature (°C).
_TEMPERATURE_OF_ROW = {"≤30": 30.0, "60": 60.0, "80": 80.0, "> 30 to 50": 50.0}


# ---------------------------------------------------------------------------
# CSV parsing helpers
# ---------------------------------------------------------------------------


def _fixture_path(edition: str, number: int) -> Path:
    directory, prefix, ids, _ = EDITION_FIXTURES[edition]
    return DATASETS / directory / f"{prefix}{ids[number]}.csv"


def _read_table(edition: str, number: int) -> list[list[str]]:
    """Read a fixture table as rows of stripped cells."""
    with _fixture_path(edition, number).open(encoding="utf-8", newline="") as handle:
        return [[cell.strip() for cell in row] for row in csv.reader(handle)]


def _data_rows(rows: list[list[str]]) -> list[list[str]]:
    """Drop footnote rows (``1) ...``) that some 2021 tables end with."""
    return [row for row in rows if not row[0].startswith("1)")]


def _number(cell: str) -> float:
    """Parse a numeric CSV cell, tolerating ``b = 0.10`` and ``2,000``."""
    text = cell.split("=")[-1].replace(",", "").strip()
    return float(text)


def _climate_from_header(cell: str) -> Climate:
    """Map a header cell such as ``‘Sub-Tropical’\\n(12- 20 °C)`` to Climate."""
    name = cell.splitlines()[0].strip("‘’'\"").strip().lower()
    return _CLIMATE_BY_HEADER[name]


def _climate_columns(header: list[str]) -> dict[int, Climate]:
    """Return {column index: Climate} for the non-empty climate header cells."""
    return {
        idx: _climate_from_header(cell)
        for idx, cell in enumerate(header)
        if idx > 0 and cell
    }


@lru_cache(maxsize=None)
def table_1(edition: str) -> dict[tuple[Climate, DepthBand, DesignPhase], float]:
    """Table 10-1 / A-1 / 8-1: paired initial/final columns per climate."""
    rows = _read_table(edition, 1)
    climates = _climate_columns(rows[1])
    phase_labels = rows[2]
    out: dict[tuple[Climate, DepthBand, DesignPhase], float] = {}
    for row in _data_rows(rows[3:]):
        band = DepthBand(row[0])
        for col, climate in climates.items():
            for offset in (0, 1):
                phase = DesignPhase(phase_labels[col + offset])
                out[(climate, band, phase)] = _number(row[col + offset])
    return out


@lru_cache(maxsize=None)
def single_value_table(edition: str, number: int) -> dict[tuple[Climate, str], float]:
    """Tables x-2 / x-3: one value per climate column, row label in col 0."""
    rows = _read_table(edition, number)
    climates = _climate_columns(rows[1])
    return {
        (climate, row[0]): _number(row[col])
        for row in _data_rows(rows[2:])
        for col, climate in climates.items()
    }


@lru_cache(maxsize=None)
def table_4(
    edition: str,
) -> tuple[dict[PaintCategory, float], dict[tuple[PaintCategory, str], float]]:
    """Table x-4: ``a`` in the category headers, ``b`` per depth row."""
    rows = _read_table(edition, 4)
    categories: dict[int, PaintCategory] = {}
    a_values: dict[PaintCategory, float] = {}
    for idx, cell in enumerate(rows[2]):
        if idx == 0 or not cell:
            continue
        category = PaintCategory(cell.splitlines()[0].strip())
        categories[idx] = category
        a_values[category] = _number(cell.splitlines()[1].strip("()"))
    b_values = {
        (category, row[0]): _number(row[col])
        for row in rows[3:]
        for col, category in categories.items()
    }
    return a_values, b_values


@lru_cache(maxsize=None)
def table_6_ambient(
    edition: str,
) -> dict[tuple[AnodeMaterial, AnodeEnvironment], tuple[float, float]]:
    """Table 10-6 / A-6: (capacity, potential); material carried over blanks."""
    rows = _read_table(edition, 6)
    out: dict[tuple[AnodeMaterial, AnodeEnvironment], tuple[float, float]] = {}
    material: AnodeMaterial | None = None
    for row in rows[2:]:
        if row[0]:
            material = _MATERIAL_BY_LABEL[row[0]]
        assert material is not None
        environment = _ENVIRONMENT_BY_LABEL[row[1]]
        out[(material, environment)] = (_number(row[2]), _number(row[3]))
    return out


@lru_cache(maxsize=None)
def table_8_6() -> dict[tuple[AnodeMaterial, AnodeEnvironment, str], tuple[float, float]]:
    """Table 8-6 (2021): (potential, capacity) per material, exposure and row.

    The material and the Zn seawater cell are carried forward over blank
    cells (the print sets them once across the rows they cover).
    """
    rows = _read_table("2021", 6)
    out: dict[tuple[AnodeMaterial, AnodeEnvironment, str], tuple[float, float]] = {}
    material: AnodeMaterial | None = None
    seawater: tuple[float, float] | None = None
    for row in _data_rows(rows[2:]):
        if row[0]:
            material = _MATERIAL_BY_LABEL[row[0].splitlines()[0]]
            seawater = None
        assert material is not None
        label = row[1]
        if row[2]:
            seawater = (_number(row[2]), _number(row[3]))
        assert seawater is not None
        out[(material, AnodeEnvironment.SEAWATER, label)] = seawater
        out[(material, AnodeEnvironment.SEDIMENT, label)] = (
            _number(row[4]),
            _number(row[5]),
        )
    return out


@lru_cache(maxsize=None)
def table_8(edition: str) -> dict[AnodeShape, float]:
    """Table x-8: first header line of each row names the shape."""
    rows = _read_table(edition, 8)
    return {
        _SHAPE_BY_LABEL[row[0].splitlines()[0].strip()]: _number(row[1])
        for row in rows[2:]
    }


def _categories(edition: str) -> list[PaintCategory]:
    return list(PaintCategory) if edition == "2021" else [
        PaintCategory.I, PaintCategory.II, PaintCategory.III
    ]


# ---------------------------------------------------------------------------
# Fixture sanity
# ---------------------------------------------------------------------------


@pytest.mark.parametrize("edition", EDITIONS)
def test_fixture_csvs_present(edition):
    for number in (1, 2, 3, 4, 6, 8):
        assert _fixture_path(edition, number).is_file()
    assert (DATASETS.parent / "PROVENANCE.md").is_file()


@pytest.mark.parametrize("edition", EDITIONS)
def test_tables_parse_all_cells(edition):
    """4 climates x 4 depth bands x 2 phases; 16 mean cells; 12 reinforcement."""
    assert len(table_1(edition)) == 32
    assert len(single_value_table(edition, 2)) == 16
    assert len(single_value_table(edition, 3)) == 12
    assert {row for _, row in single_value_table(edition, 3)} == {"0-30", ">30-100", ">100"}
    a_values, b_values = table_4(edition)
    assert set(a_values) == set(_categories(edition))
    assert len(b_values) == 2 * len(a_values)
    assert set(table_8(edition)) == set(AnodeShape)


def test_table_8_6_parses_all_cells():
    rows = table_8_6()
    assert len(rows) == 10
    assert rows[(AnodeMaterial.ZINC, AnodeEnvironment.SEAWATER, "> 30 to 50")] == (-1.030, 780.0)


@pytest.mark.parametrize("edition", EDITIONS)
def test_tables_identical_across_editions(edition):
    """Current densities, Cat I-III and utilisation factors are identical."""
    assert table_1(edition) == table_1("2010")
    assert single_value_table(edition, 2) == single_value_table("2010", 2)
    assert single_value_table(edition, 3) == single_value_table("2010", 3)
    a_values, b_values = table_4(edition)
    a_2010, b_2010 = table_4("2010")
    assert {k: v for k, v in a_values.items() if k in a_2010} == a_2010
    assert {k: v for k, v in b_values.items() if k in b_2010} == b_2010
    assert table_8(edition) == table_8("2010")


# ---------------------------------------------------------------------------
# Tables x-1 / x-2: design current densities
# ---------------------------------------------------------------------------


@pytest.mark.parametrize("edition", EDITIONS)
@pytest.mark.parametrize("climate", list(Climate))
@pytest.mark.parametrize("band", list(DepthBand))
@pytest.mark.parametrize("phase", [DesignPhase.INITIAL, DesignPhase.FINAL])
def test_design_current_density_matches_table_1(edition, climate, band, phase):
    result = tbl.design_current_density(climate, band, phase, edition=edition)
    assert result.value == table_1(edition)[(climate, band, phase)]
    assert result.units == "A/m2"
    assert result.citation.section == EDITION_FIXTURES[edition][3] + "1"


@pytest.mark.parametrize("edition", EDITIONS)
@pytest.mark.parametrize("climate", list(Climate))
@pytest.mark.parametrize("band", list(DepthBand))
def test_design_current_density_mean_matches_table_2(edition, climate, band):
    result = tbl.design_current_density(climate, band, DesignPhase.MEAN, edition=edition)
    assert result.value == single_value_table(edition, 2)[(climate, band.value)]
    assert result.citation.section == EDITION_FIXTURES[edition][3] + "2"


@pytest.mark.parametrize(
    ("phase", "expected"),
    [
        (DesignPhase.INITIAL, 0.200),
        (DesignPhase.MEAN, 0.100),
        (DesignPhase.FINAL, 0.130),
    ],
)
def test_temperate_0_30_spot_values(phase, expected):
    """Hand-derived spot values from the 2026-09-24 review (temperate, 0-30 m)."""
    result = tbl.design_current_density(
        Climate.TEMPERATE, DepthBand.M0_30, phase, edition=EDITION
    )
    assert result.value == pytest.approx(expected)


# ---------------------------------------------------------------------------
# Table x-3: reinforcement
# ---------------------------------------------------------------------------


@pytest.mark.parametrize("edition", EDITIONS)
@pytest.mark.parametrize("climate", list(Climate))
@pytest.mark.parametrize("band", list(DepthBand))
def test_reinforcement_current_density_matches_table_3(edition, climate, band):
    result = tbl.reinforcement_current_density(climate, band, edition=edition)
    row = {
        DepthBand.M0_30: "0-30",
        DepthBand.M30_100: ">30-100",
        DepthBand.M100_300: ">100",
        DepthBand.M300_PLUS: ">100",
    }[band]
    assert result.value == single_value_table(edition, 3)[(climate, row)]
    assert result.citation.section == EDITION_FIXTURES[edition][3] + "3"
    assert f"row {row} m" in result.citation.note


# ---------------------------------------------------------------------------
# Table x-4: coating breakdown constants
# ---------------------------------------------------------------------------


@pytest.mark.parametrize("edition", EDITIONS)
@pytest.mark.parametrize("category", list(PaintCategory))
@pytest.mark.parametrize("band", list(DepthBand))
def test_coating_breakdown_constants_match_table_4(edition, category, band):
    if category not in _categories(edition):
        with pytest.raises(ValueError, match="category IV is defined only in DNV-RP-B401 \\(May 2021\\)"):
            tbl.coating_breakdown_constants(category, band, edition=edition)
        return
    a_values, b_values = table_4(edition)
    a, b = tbl.coating_breakdown_constants(category, band, edition=edition)
    row = "0-30" if band is DepthBand.M0_30 else ">30"
    assert a.value == a_values[category]
    assert b.value == b_values[(category, row)]
    assert a.units == "dimensionless"
    assert b.units == "1/yr"
    assert a.citation.section == EDITION_FIXTURES[edition][3] + "4"
    assert b.citation.section == EDITION_FIXTURES[edition][3] + "4"


def test_category_iii_0_30_spot_values():
    """Hand-derived: Cat III, 0-30 m, a = 0.02, b = 0.012."""
    a, b = tbl.coating_breakdown_constants(
        PaintCategory.III, DepthBand.M0_30, edition=EDITION
    )
    assert a.value == pytest.approx(0.02)
    assert b.value == pytest.approx(0.012)


def test_category_iv_2021_spot_values():
    """Table 8-4 column IV: a = 0.02, b = 0.008 (0-30 m) / 0.005 (>30 m)."""
    a, b = tbl.coating_breakdown_constants(PaintCategory.IV, DepthBand.M0_30, edition="2021")
    assert (a.value, b.value) == (0.02, 0.008)
    _, b_deep = tbl.coating_breakdown_constants(
        PaintCategory.IV, DepthBand.M100_300, edition="2021"
    )
    assert b_deep.value == 0.005
    assert a.citation.section == "Table 8-4"


# ---------------------------------------------------------------------------
# Table x-6: anode material parameters
# ---------------------------------------------------------------------------


@pytest.mark.parametrize("edition", AMBIENT_ANODE_EDITIONS)
@pytest.mark.parametrize("material", list(AnodeMaterial))
@pytest.mark.parametrize("environment", list(AnodeEnvironment))
def test_anode_capacity_and_potential_match_ambient_table_6(edition, material, environment):
    capacity, potential = table_6_ambient(edition)[(material, environment)]
    cap = tbl.anode_capacity(material, environment, edition=edition)
    pot = tbl.anode_closed_circuit_potential(material, environment, edition=edition)
    assert cap.value == capacity
    assert pot.value == potential
    assert cap.units == "Ah/kg"
    assert pot.units.startswith("V")
    assert cap.citation.section == EDITION_FIXTURES[edition][3] + "6"
    assert pot.citation.section == EDITION_FIXTURES[edition][3] + "6"
    assert "seawater ambient temperature" in cap.citation.note


@pytest.mark.parametrize("edition", AMBIENT_ANODE_EDITIONS)
def test_ambient_editions_reject_anode_temperature_above_30(edition):
    with pytest.raises(ValueError, match="seawater ambient temperature .*Use edition '2021'"):
        tbl.anode_capacity(
            AnodeMaterial.ALUMINIUM,
            AnodeEnvironment.SEAWATER,
            edition=edition,
            anode_surface_temperature_c=30.01,
        )
    with pytest.raises(ValueError, match="no row for an anode surface temperature of 60"):
        tbl.design_driving_voltage(
            AnodeMaterial.ALUMINIUM, edition=edition, anode_surface_temperature_c=60.0
        )


@pytest.mark.parametrize(
    ("material", "environment", "row"),
    [
        (material, environment, row)
        for material, rows in (
            (AnodeMaterial.ALUMINIUM, ("≤30", "60", "80")),
            (AnodeMaterial.ZINC, ("≤30", "> 30 to 50")),
        )
        for environment in AnodeEnvironment
        for row in rows
    ],
)
def test_anode_values_2021_match_table_8_6(material, environment, row):
    potential, capacity = table_8_6()[(material, environment, row)]
    temperature = _TEMPERATURE_OF_ROW[row]
    cap = tbl.anode_capacity(material, environment, "2021", temperature)
    pot = tbl.anode_closed_circuit_potential(material, environment, "2021", temperature)
    assert cap.value == capacity
    assert pot.value == potential
    assert cap.citation.section == "Table 8-6"
    assert f"row {row} °C" in cap.citation.note
    assert f"row {row} °C" in pot.citation.note


@pytest.mark.parametrize(
    ("material", "environment", "temperature", "row"),
    [
        (AnodeMaterial.ALUMINIUM, AnodeEnvironment.SEAWATER, 30.0, "≤30"),
        (AnodeMaterial.ALUMINIUM, AnodeEnvironment.SEAWATER, 30.01, "60"),
        (AnodeMaterial.ALUMINIUM, AnodeEnvironment.SEDIMENT, 45.0, "60"),
        (AnodeMaterial.ALUMINIUM, AnodeEnvironment.SEDIMENT, 60.0, "60"),
        (AnodeMaterial.ALUMINIUM, AnodeEnvironment.SEAWATER, 61.0, "80"),
        (AnodeMaterial.ZINC, AnodeEnvironment.SEAWATER, 31.0, "> 30 to 50"),
        (AnodeMaterial.ZINC, AnodeEnvironment.SEDIMENT, 50.0, "> 30 to 50"),
    ],
)
def test_table_8_6_rows_are_selected_stepwise(material, environment, temperature, row):
    """The first row at or above the requested temperature applies (no interpolation)."""
    selected = tbl.anode_temperature_row(material, environment, temperature)
    assert selected.row_label == row
    assert selected.max_temperature_c >= temperature


@pytest.mark.parametrize(
    ("material", "temperature"),
    [(AnodeMaterial.ALUMINIUM, 80.01), (AnodeMaterial.ZINC, 50.01)],
)
def test_table_8_6_rejects_temperature_above_last_row(material, temperature):
    with pytest.raises(ValueError, match="table ends at"):
        tbl.anode_capacity(material, AnodeEnvironment.SEDIMENT, "2021", temperature)


def test_zinc_seawater_2021_holds_for_both_temperature_rows():
    """The Zn seawater cell (-1.030 V, 780 Ah/kg) spans the <=30 and >30-50 rows."""
    for temperature in (10.0, 30.0, 40.0, 50.0):
        cap = tbl.anode_capacity(AnodeMaterial.ZINC, AnodeEnvironment.SEAWATER, "2021", temperature)
        pot = tbl.anode_closed_circuit_potential(
            AnodeMaterial.ZINC, AnodeEnvironment.SEAWATER, "2021", temperature
        )
        assert (cap.value, pot.value) == (780.0, -1.030)


def test_aluminium_seawater_spot_values():
    """Hand-derived: Al-based in seawater, 2000 Ah/kg and -1.05 V (every edition)."""
    for edition in EDITIONS:
        cap = tbl.anode_capacity(AnodeMaterial.ALUMINIUM, AnodeEnvironment.SEAWATER, edition)
        pot = tbl.anode_closed_circuit_potential(
            AnodeMaterial.ALUMINIUM, AnodeEnvironment.SEAWATER, edition
        )
        assert cap.value == pytest.approx(2000.0)
        assert pot.value == pytest.approx(-1.05)


# ---------------------------------------------------------------------------
# Table x-8: utilisation factors
# ---------------------------------------------------------------------------


@pytest.mark.parametrize("edition", EDITIONS)
@pytest.mark.parametrize("shape", list(AnodeShape))
def test_utilisation_factor_matches_table_8(edition, shape):
    result = tbl.utilisation_factor(shape, edition=edition)
    assert result.value == table_8(edition)[shape]
    assert result.units == "dimensionless"
    assert result.citation.section == EDITION_FIXTURES[edition][3] + "8"


# ---------------------------------------------------------------------------
# Clause-derived values
# ---------------------------------------------------------------------------


@pytest.mark.parametrize(
    ("edition", "section"),
    [
        ("2005", "Sec. 5 (structure-to-electrolyte potential criteria)"),
        ("2010", "Sec. 5 (structure-to-electrolyte potential criteria)"),
        ("2017", "[5.4] (design protective potential)"),
        ("2021", "[2.4.2] (design protective potential)"),
    ],
)
def test_protection_potential(edition, section):
    result = tbl.protection_potential(edition=edition)
    assert result.value == pytest.approx(-0.80)
    assert result.units.startswith("V")
    assert result.citation.section == section


@pytest.mark.parametrize("edition", EDITIONS)
@pytest.mark.parametrize(
    ("material", "expected"),
    [(AnodeMaterial.ALUMINIUM, 0.25), (AnodeMaterial.ZINC, {"2021": 0.23, "other": 0.20})],
)
def test_design_driving_voltage_seawater(edition, material, expected):
    """E_c - E_a with E_c = -0.80 V; Zn is -1.00 V up to 2017 and -1.030 V in 2021."""
    if isinstance(expected, dict):
        expected = expected.get(edition, expected["other"])
    result = tbl.design_driving_voltage(material, edition=edition)
    assert result.value == pytest.approx(expected)
    assert result.units == "V"
    assert result.citation.section.startswith(EDITION_FIXTURES[edition][3] + "6 with ")


def test_design_driving_voltage_sediment_uses_sediment_potential():
    result = tbl.design_driving_voltage(
        AnodeMaterial.ALUMINIUM, edition=EDITION, environment=AnodeEnvironment.SEDIMENT
    )
    assert result.value == pytest.approx(0.15)
    hot = tbl.design_driving_voltage(
        AnodeMaterial.ALUMINIUM,
        edition="2021",
        environment=AnodeEnvironment.SEDIMENT,
        anode_surface_temperature_c=80.0,
    )
    assert hot.value == pytest.approx(0.20)


@pytest.mark.parametrize(
    ("edition", "section"),
    [
        ("2005", "Sec. 6.3 (buried surfaces)"),
        ("2010", "Sec. 6.3 (buried surfaces)"),
        ("2017", "[6.3.8] (buried surfaces)"),
        ("2021", "[3.3.8] (buried surfaces)"),
    ],
)
def test_buried_current_density(edition, section):
    result = tbl.buried_current_density(edition=edition)
    assert result.value == pytest.approx(0.020)
    assert result.units == "A/m2"
    assert result.citation.section == section


# ---------------------------------------------------------------------------
# Helpers: climate and depth band boundaries
# ---------------------------------------------------------------------------


@pytest.mark.parametrize(
    ("temperature_c", "expected"),
    [
        (25.0, Climate.TROPICAL),
        (20.01, Climate.TROPICAL),
        (20.0, Climate.SUBTROPICAL),  # header "12-20 °C" is inclusive
        (12.0, Climate.SUBTROPICAL),  # header "12-20 °C" is inclusive
        (11.99, Climate.TEMPERATE),
        (11.5, Climate.TEMPERATE),  # gap between "7-11" (10-1) and "7-12" (10-2)
        (7.0, Climate.TEMPERATE),  # header "7-11/12 °C" is inclusive
        (6.99, Climate.ARCTIC),  # header "< 7 °C"
        (-1.5, Climate.ARCTIC),
    ],
)
def test_climate_from_temperature_boundaries(temperature_c, expected):
    """Convention: >20 tropical; 12-20 sub-tropical; 7-<12 temperate; <7 arctic."""
    assert tbl.climate_from_temperature(temperature_c) is expected


@pytest.mark.parametrize(
    ("depth_m", "expected"),
    [
        (0.0, DepthBand.M0_30),
        (30.0, DepthBand.M0_30),  # row "0-30" inclusive
        (30.01, DepthBand.M30_100),  # row ">30-100"
        (100.0, DepthBand.M30_100),
        (100.01, DepthBand.M100_300),  # row ">100-300"
        (300.0, DepthBand.M100_300),
        (300.01, DepthBand.M300_PLUS),  # row ">300"
        (1500.0, DepthBand.M300_PLUS),
    ],
)
def test_depth_band_boundaries(depth_m, expected):
    assert tbl.depth_band(depth_m) is expected


def test_depth_band_rejects_negative_depth():
    with pytest.raises(ValueError, match="non-negative"):
        tbl.depth_band(-0.1)


def test_climate_and_depth_helpers_feed_the_lookup():
    """Temperate 15 m initial via the helpers equals the Table 10-1 cell."""
    climate = tbl.climate_from_temperature(9.0)
    band = tbl.depth_band(15.0)
    result = tbl.design_current_density(climate, band, DesignPhase.INITIAL, edition=EDITION)
    assert result.value == table_1(EDITION)[
        (Climate.TEMPERATE, DepthBand.M0_30, DesignPhase.INITIAL)
    ]


# ---------------------------------------------------------------------------
# Editions and provenance
# ---------------------------------------------------------------------------


@pytest.mark.parametrize(
    ("edition", "expected"),
    [
        ("2005", "verified-2005-tables"),
        ("2005-2008", "verified-2005-tables"),
        ("2010", "verified-2010-tables"),
        ("2011", "verified-2010-tables"),
        ("dnv-rp-b401-2011", "verified-2010-tables"),
        ("2017", "verified-2017-tables"),
        ("DNVGL-RP-B401-2017", "verified-2017-tables"),
        ("2021", "verified-2021-tables"),
        ("2021-05", "verified-2021-tables"),
    ],
)
def test_edition_provenance(edition, expected):
    assert tbl.edition_provenance(edition) == expected
    assert "inherited" not in expected


def test_edition_provenance_rejects_unknown_edition():
    with pytest.raises(ValueError, match="Unsupported DNV-RP-B401 edition"):
        tbl.edition_provenance("1993")


def test_edition_none_warns_and_defaults_to_verified_2021():
    with pytest.warns(UserWarning, match="defaulting to DNV-RP-B401 2021"):
        result = tbl.utilisation_factor(AnodeShape.LONG_SLENDER_STANDOFF)
    assert "provenance=verified-2021-tables" in result.citation.note
    assert "edition=2021" in result.citation.note
    assert result.citation.section == "Table 8-8"


@pytest.mark.parametrize("edition", EDITIONS)
def test_table_label_follows_edition_numbering(edition):
    assert tbl.table_label(edition, 6) == EDITION_FIXTURES[edition][3] + "6"


# ---------------------------------------------------------------------------
# Citations
# ---------------------------------------------------------------------------


def _all_results(edition: Edition) -> list:
    """One CitedValue from every public lookup."""
    a, b = tbl.coating_breakdown_constants(PaintCategory.I, DepthBand.M30_100, edition=edition)
    return [
        tbl.design_current_density(
            Climate.TROPICAL, DepthBand.M300_PLUS, DesignPhase.MEAN, edition=edition
        ),
        tbl.reinforcement_current_density(Climate.ARCTIC, DepthBand.M100_300, edition=edition),
        a,
        b,
        tbl.anode_capacity(AnodeMaterial.ZINC, AnodeEnvironment.SEDIMENT, edition=edition),
        tbl.anode_closed_circuit_potential(
            AnodeMaterial.ZINC, AnodeEnvironment.SEAWATER, edition=edition
        ),
        tbl.utilisation_factor(AnodeShape.SHORT_FLUSH_BRACELET, edition=edition),
        tbl.protection_potential(edition=edition),
        tbl.design_driving_voltage(AnodeMaterial.ALUMINIUM, edition=edition),
        tbl.buried_current_density(edition=edition),
    ]


@pytest.mark.parametrize("edition", EDITIONS)
def test_every_result_carries_the_edition_citation(edition):
    revision, wiki_path, provenance = EXPECTED[edition]
    with warnings.catch_warnings():
        warnings.simplefilter("error")
        results = _all_results(edition)
    for result in results:
        citation = result.citation
        assert citation.code_id == "dnv-rp-b401"
        assert citation.publisher == "DNV"
        assert citation.revision == revision
        assert citation.wiki_path == wiki_path
        assert citation.section
        assert f"provenance={provenance}" in citation.note
        assert f"edition={edition}" in citation.note
        assert result.units
        assert tbl.citation_label(citation) == f"dnv-rp-b401 {revision} {citation.section}"


@pytest.mark.parametrize("edition", ["2017", "2021"])
def test_citations_validate_against_vendored_fixture_pages(edition):
    """Fail-closed resolution against tests/citations/fixtures succeeds."""
    for result in _all_results(edition):
        validate_citation(result.citation, repo_root=CITATION_FIXTURES)


def test_citations_validate_against_live_wiki():
    """Fail-closed resolution succeeds when the 2010/2011 wiki page is reachable."""
    try:
        page = resolve_wiki_path(tbl.B401_WIKI_PATH)
    except CitationResolutionError as exc:
        pytest.skip(f"wiki not resolvable: {exc.reason.splitlines()[0]}")
    if not page.is_file():
        pytest.skip(f"wiki page missing: {page}")
    for result in _all_results(EDITION):
        validate_citation(result.citation)
