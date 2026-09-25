"""Tests for the cited DNV-RP-B401 table lookups (issue #2207).

Every table value is asserted against the fixture CSVs copied verbatim from
the llm-wiki datasets (see ``tests/fixtures/test_vectors/cathodic_protection/
datasets/PROVENANCE.md``). Citations are validated against the live wiki when
the resolver can find it; otherwise that test skips with the resolver reason.
"""

from __future__ import annotations

import csv
from pathlib import Path
import warnings

import pytest

from digitalmodel.cathodic_protection import b401_tables as tbl
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

FIXTURE_DIR = (
    Path(__file__).resolve().parents[1]
    / "fixtures"
    / "test_vectors"
    / "cathodic_protection"
    / "datasets"
    / "dnv-rp-b401"
    / "2005-with-2008-amendments"
)
_PREFIX = "dnv-rp-b401-2005-with-2008-amendments-table-"

# Edition whose tables the fixtures hold; used so lookups do not warn.
EDITION = "2010"

_CLIMATE_BY_HEADER = {
    "tropical": Climate.TROPICAL,
    "sub-tropical": Climate.SUBTROPICAL,
    "temperate": Climate.TEMPERATE,
    "arctic": Climate.ARCTIC,
}
_MATERIAL_BY_LABEL = {
    "Al-based": AnodeMaterial.ALUMINIUM,
    "Zn-based": AnodeMaterial.ZINC,
}
_ENVIRONMENT_BY_LABEL = {
    "seawater": AnodeEnvironment.SEAWATER,
    "sediments": AnodeEnvironment.SEDIMENT,
}
_SHAPE_BY_LABEL = {
    "Long slender stand-off": AnodeShape.LONG_SLENDER_STANDOFF,
    "Short slender stand-off": AnodeShape.SHORT_SLENDER_STANDOFF,
    "Long flush mounted": AnodeShape.LONG_FLUSH,
    "Short flush-mounted, bracelet": AnodeShape.SHORT_FLUSH_BRACELET,
}


# ---------------------------------------------------------------------------
# CSV parsing helpers
# ---------------------------------------------------------------------------


def _read_table(number: int) -> list[list[str]]:
    """Read fixture ``table-00<number>.csv`` as rows of stripped cells."""
    path = FIXTURE_DIR / f"{_PREFIX}{number:03d}.csv"
    with path.open(encoding="utf-8", newline="") as handle:
        return [[cell.strip() for cell in row] for row in csv.reader(handle)]


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


def _parse_table_10_1() -> dict[tuple[Climate, DepthBand, DesignPhase], float]:
    """Table 10-1: paired initial/final columns under each climate header."""
    rows = _read_table(1)
    climates = _climate_columns(rows[1])
    phase_labels = rows[2]
    out: dict[tuple[Climate, DepthBand, DesignPhase], float] = {}
    for row in rows[3:]:
        band = DepthBand(row[0])
        for col, climate in climates.items():
            for offset in (0, 1):
                phase = DesignPhase(phase_labels[col + offset])
                out[(climate, band, phase)] = _number(row[col + offset])
    return out


def _parse_single_value_table(number: int) -> dict[tuple[Climate, str], float]:
    """Tables 10-2 / 10-3: one value per climate column, row label in col 0."""
    rows = _read_table(number)
    climates = _climate_columns(rows[1])
    return {
        (climate, row[0]): _number(row[col])
        for row in rows[2:]
        for col, climate in climates.items()
    }


def _parse_table_10_4() -> (
    tuple[dict[PaintCategory, float], dict[tuple[PaintCategory, str], float]]
):
    """Table 10-4: ``a`` in the category headers, ``b`` per depth row."""
    rows = _read_table(4)
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


def _parse_table_10_6() -> (
    dict[tuple[AnodeMaterial, AnodeEnvironment], tuple[float, float]]
):
    """Table 10-6 (fixture 005): material carried forward over blank cells."""
    rows = _read_table(5)
    out: dict[tuple[AnodeMaterial, AnodeEnvironment], tuple[float, float]] = {}
    material: AnodeMaterial | None = None
    for row in rows[2:]:
        if row[0]:
            material = _MATERIAL_BY_LABEL[row[0]]
        assert material is not None
        environment = _ENVIRONMENT_BY_LABEL[row[1]]
        out[(material, environment)] = (_number(row[2]), _number(row[3]))
    return out


def _parse_table_10_8() -> dict[AnodeShape, float]:
    """Table 10-8 (fixture 008): first header line of each row names the shape."""
    rows = _read_table(8)
    return {
        _SHAPE_BY_LABEL[row[0].splitlines()[0].strip()]: _number(row[1])
        for row in rows[2:]
    }


@pytest.fixture(scope="module")
def table_10_1() -> dict[tuple[Climate, DepthBand, DesignPhase], float]:
    return _parse_table_10_1()


@pytest.fixture(scope="module")
def table_10_2() -> dict[tuple[Climate, str], float]:
    return _parse_single_value_table(2)


@pytest.fixture(scope="module")
def table_10_3() -> dict[tuple[Climate, str], float]:
    return _parse_single_value_table(3)


@pytest.fixture(scope="module")
def table_10_4() -> (
    tuple[dict[PaintCategory, float], dict[tuple[PaintCategory, str], float]]
):
    return _parse_table_10_4()


@pytest.fixture(scope="module")
def table_10_6() -> dict[tuple[AnodeMaterial, AnodeEnvironment], tuple[float, float]]:
    return _parse_table_10_6()


@pytest.fixture(scope="module")
def table_10_8() -> dict[AnodeShape, float]:
    return _parse_table_10_8()


# ---------------------------------------------------------------------------
# Fixture sanity
# ---------------------------------------------------------------------------


def test_fixture_csvs_present():
    """All eight B401 CSVs are copied alongside PROVENANCE.md."""
    for number in range(1, 9):
        assert (FIXTURE_DIR / f"{_PREFIX}{number:03d}.csv").is_file()
    assert (FIXTURE_DIR.parents[1] / "PROVENANCE.md").is_file()


def test_table_10_1_parses_all_cells(table_10_1):
    """4 climates x 4 depth bands x 2 phases."""
    assert len(table_10_1) == 32


def test_table_10_2_parses_all_cells(table_10_2):
    assert len(table_10_2) == 16


def test_table_10_3_parses_all_cells(table_10_3):
    assert len(table_10_3) == 12
    assert {row for _, row in table_10_3} == {"0-30", ">30-100", ">100"}


# ---------------------------------------------------------------------------
# Table 10-1 / 10-2: design current densities
# ---------------------------------------------------------------------------


@pytest.mark.parametrize("climate", list(Climate))
@pytest.mark.parametrize("band", list(DepthBand))
@pytest.mark.parametrize("phase", [DesignPhase.INITIAL, DesignPhase.FINAL])
def test_design_current_density_matches_table_10_1(table_10_1, climate, band, phase):
    result = tbl.design_current_density(climate, band, phase, edition=EDITION)
    assert result.value == table_10_1[(climate, band, phase)]
    assert result.units == "A/m2"
    assert result.citation.section == "Table 10-1"


@pytest.mark.parametrize("climate", list(Climate))
@pytest.mark.parametrize("band", list(DepthBand))
def test_design_current_density_mean_matches_table_10_2(table_10_2, climate, band):
    result = tbl.design_current_density(
        climate, band, DesignPhase.MEAN, edition=EDITION
    )
    assert result.value == table_10_2[(climate, band.value)]
    assert result.citation.section == "Table 10-2"


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
# Table 10-3: reinforcement
# ---------------------------------------------------------------------------


@pytest.mark.parametrize("climate", list(Climate))
@pytest.mark.parametrize("band", list(DepthBand))
def test_reinforcement_current_density_matches_table_10_3(table_10_3, climate, band):
    result = tbl.reinforcement_current_density(climate, band, edition=EDITION)
    row = {
        DepthBand.M0_30: "0-30",
        DepthBand.M30_100: ">30-100",
        DepthBand.M100_300: ">100",
        DepthBand.M300_PLUS: ">100",
    }[band]
    assert result.value == table_10_3[(climate, row)]
    assert result.citation.section == "Table 10-3"
    assert f"row {row} m" in result.citation.note


# ---------------------------------------------------------------------------
# Table 10-4: coating breakdown constants
# ---------------------------------------------------------------------------


@pytest.mark.parametrize("category", list(PaintCategory))
@pytest.mark.parametrize("band", list(DepthBand))
def test_coating_breakdown_constants_match_table_10_4(table_10_4, category, band):
    a_values, b_values = table_10_4
    a, b = tbl.coating_breakdown_constants(category, band, edition=EDITION)
    row = "0-30" if band is DepthBand.M0_30 else ">30"
    assert a.value == a_values[category]
    assert b.value == b_values[(category, row)]
    assert a.units == "dimensionless"
    assert b.units == "1/yr"
    assert a.citation.section == "Table 10-4"
    assert b.citation.section == "Table 10-4"


def test_category_iii_0_30_spot_values():
    """Hand-derived: Cat III, 0-30 m, a = 0.02, b = 0.012."""
    a, b = tbl.coating_breakdown_constants(
        PaintCategory.III, DepthBand.M0_30, edition=EDITION
    )
    assert a.value == pytest.approx(0.02)
    assert b.value == pytest.approx(0.012)


# ---------------------------------------------------------------------------
# Table 10-6: anode material parameters
# ---------------------------------------------------------------------------


@pytest.mark.parametrize("material", list(AnodeMaterial))
@pytest.mark.parametrize("environment", list(AnodeEnvironment))
def test_anode_capacity_and_potential_match_table_10_6(
    table_10_6, material, environment
):
    capacity, potential = table_10_6[(material, environment)]
    cap = tbl.anode_capacity(material, environment, edition=EDITION)
    pot = tbl.anode_closed_circuit_potential(material, environment, edition=EDITION)
    assert cap.value == capacity
    assert pot.value == potential
    assert cap.units == "Ah/kg"
    assert pot.units.startswith("V")
    assert cap.citation.section == "Table 10-6"
    assert pot.citation.section == "Table 10-6"


def test_aluminium_seawater_spot_values():
    """Hand-derived: Al-based in seawater, 2000 Ah/kg and -1.05 V."""
    cap = tbl.anode_capacity(
        AnodeMaterial.ALUMINIUM, AnodeEnvironment.SEAWATER, edition=EDITION
    )
    pot = tbl.anode_closed_circuit_potential(
        AnodeMaterial.ALUMINIUM, AnodeEnvironment.SEAWATER, edition=EDITION
    )
    assert cap.value == pytest.approx(2000.0)
    assert pot.value == pytest.approx(-1.05)


# ---------------------------------------------------------------------------
# Table 10-8: utilisation factors
# ---------------------------------------------------------------------------


def test_table_10_8_parses_all_rows(table_10_8):
    assert set(table_10_8) == set(AnodeShape)


@pytest.mark.parametrize("shape", list(AnodeShape))
def test_utilisation_factor_matches_table_10_8(table_10_8, shape):
    result = tbl.utilisation_factor(shape, edition=EDITION)
    assert result.value == table_10_8[shape]
    assert result.units == "dimensionless"
    assert result.citation.section == "Table 10-8"


# ---------------------------------------------------------------------------
# Clause-derived values
# ---------------------------------------------------------------------------


def test_protection_potential():
    result = tbl.protection_potential(edition=EDITION)
    assert result.value == pytest.approx(-0.80)
    assert result.units.startswith("V")
    assert result.citation.section.startswith("Sec. 5")


@pytest.mark.parametrize(
    ("material", "expected"),
    [(AnodeMaterial.ALUMINIUM, 0.25), (AnodeMaterial.ZINC, 0.20)],
)
def test_design_driving_voltage_seawater(material, expected):
    """E_c - E_a with E_c = -0.80 V and Table 10-6 seawater potentials."""
    result = tbl.design_driving_voltage(material, edition=EDITION)
    assert result.value == pytest.approx(expected)
    assert result.units == "V"
    assert "Table 10-6" in result.citation.section


def test_design_driving_voltage_sediment_uses_sediment_potential():
    result = tbl.design_driving_voltage(
        AnodeMaterial.ALUMINIUM, edition=EDITION, environment=AnodeEnvironment.SEDIMENT
    )
    assert result.value == pytest.approx(0.15)


def test_buried_current_density():
    result = tbl.buried_current_density(edition=EDITION)
    assert result.value == pytest.approx(0.020)
    assert result.units == "A/m2"
    assert result.citation.section.startswith("Sec. 6.3")


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


def test_climate_and_depth_helpers_feed_the_lookup(table_10_1):
    """Temperate 15 m initial via the helpers equals the Table 10-1 cell."""
    climate = tbl.climate_from_temperature(9.0)
    band = tbl.depth_band(15.0)
    result = tbl.design_current_density(
        climate, band, DesignPhase.INITIAL, edition=EDITION
    )
    assert (
        result.value
        == table_10_1[(Climate.TEMPERATE, DepthBand.M0_30, DesignPhase.INITIAL)]
    )


# ---------------------------------------------------------------------------
# Editions and provenance
# ---------------------------------------------------------------------------


@pytest.mark.parametrize(
    ("edition", "expected"),
    [
        ("2005", "verified-2011-tables"),
        ("2005-2008", "verified-2011-tables"),
        ("2010", "verified-2011-tables"),
        ("2011", "verified-2011-tables"),
        ("dnv-rp-b401-2011", "verified-2011-tables"),
        ("2017", "inherited-2011-unverified"),
        ("2021", "inherited-2011-unverified"),
    ],
)
def test_edition_provenance(edition, expected):
    assert tbl.edition_provenance(edition) == expected


def test_edition_provenance_rejects_unknown_edition():
    with pytest.raises(ValueError, match="Unsupported DNV-RP-B401 edition"):
        tbl.edition_provenance("1993")


def test_edition_none_warns_and_reports_inherited_provenance():
    """Default edition (2021) keeps the None warning and is flagged unverified."""
    with pytest.warns(UserWarning, match="defaulting to DNV-RP-B401 2021"):
        result = tbl.utilisation_factor(AnodeShape.LONG_SLENDER_STANDOFF)
    assert "provenance=inherited-2011-unverified" in result.citation.note
    assert "edition=2021" in result.citation.note


@pytest.mark.parametrize("edition", ["2005", "2010", "2017", "2021"])
def test_values_identical_across_editions(table_10_1, edition):
    """All editions return the 2010/2011 numbers; only the provenance differs."""
    result = tbl.design_current_density(
        Climate.ARCTIC, DepthBand.M0_30, DesignPhase.INITIAL, edition=edition
    )
    expected = table_10_1[(Climate.ARCTIC, DepthBand.M0_30, DesignPhase.INITIAL)]
    assert result.value == expected
    assert f"provenance={tbl.edition_provenance(edition)}" in result.citation.note


# ---------------------------------------------------------------------------
# Citations
# ---------------------------------------------------------------------------


def _all_results() -> list:
    """One CitedValue from every public lookup."""
    a, b = tbl.coating_breakdown_constants(
        PaintCategory.I, DepthBand.M30_100, edition=EDITION
    )
    return [
        tbl.design_current_density(
            Climate.TROPICAL, DepthBand.M300_PLUS, DesignPhase.MEAN, edition=EDITION
        ),
        tbl.reinforcement_current_density(
            Climate.ARCTIC, DepthBand.M100_300, edition=EDITION
        ),
        a,
        b,
        tbl.anode_capacity(
            AnodeMaterial.ZINC, AnodeEnvironment.SEDIMENT, edition=EDITION
        ),
        tbl.anode_closed_circuit_potential(
            AnodeMaterial.ZINC, AnodeEnvironment.SEAWATER, edition=EDITION
        ),
        tbl.utilisation_factor(AnodeShape.SHORT_FLUSH_BRACELET, edition=EDITION),
        tbl.protection_potential(edition=EDITION),
        tbl.design_driving_voltage(AnodeMaterial.ALUMINIUM, edition=EDITION),
        tbl.buried_current_density(edition=EDITION),
    ]


def test_every_result_carries_a_b401_citation():
    with warnings.catch_warnings():
        warnings.simplefilter("error")
        results = _all_results()
    for result in results:
        citation = result.citation
        assert citation.code_id == "dnv-rp-b401"
        assert citation.publisher == "DNV"
        assert citation.revision == "2011"
        assert citation.wiki_path == tbl.B401_WIKI_PATH
        assert citation.section
        assert "provenance=verified-2011-tables" in citation.note
        assert result.units


def test_citations_validate_against_live_wiki():
    """Fail-closed resolution succeeds when the wiki page is reachable."""
    try:
        page = resolve_wiki_path(tbl.B401_WIKI_PATH)
    except CitationResolutionError as exc:
        pytest.skip(f"wiki not resolvable: {exc.reason.splitlines()[0]}")
    if not page.is_file():
        pytest.skip(f"wiki page missing: {page}")
    for result in _all_results():
        validate_citation(result.citation)
