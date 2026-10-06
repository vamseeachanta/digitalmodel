"""Tests for the cited DNV-RP-F103 table lookups (issues #2207, #2208).

Every table value is asserted against the fixture CSVs under
``tests/fixtures/test_vectors/cathodic_protection/datasets/dnv-rp-f103/``:
the 2010 files are verbatim copies of the llm-wiki datasets (Annex 1 prints
``a x100`` / ``b x100``, divided by 100 here exactly as the module does), the
2019-09 files are hand transcriptions of the licensed print (``a`` and ``b``
printed directly). See ``PROVENANCE.md`` there.
"""

from __future__ import annotations

import csv
from functools import lru_cache
from pathlib import Path
import warnings

import pytest

from digitalmodel.cathodic_protection import f103_tables as tbl
from digitalmodel.cathodic_protection._edition import F103Edition
from digitalmodel.cathodic_protection.b401_tables import (
    AnodeEnvironment,
    AnodeMaterial,
)
from digitalmodel.cathodic_protection.f103_tables import (
    Exposure,
    FieldJointCoating,
    FieldJointCoating2019,
    FluidTemperatureBand,
    LinepipeCoating,
)
from digitalmodel.citations import CitationResolutionError, validate_citation
from digitalmodel.citations.resolver import resolve_wiki_path

DATASETS = (
    Path(__file__).resolve().parents[1]
    / "fixtures"
    / "test_vectors"
    / "cathodic_protection"
    / "datasets"
    / "dnv-rp-f103"
)
CITATION_FIXTURES = Path(__file__).resolve().parents[1] / "citations" / "fixtures"

FILES = {
    "2010": ("2010", "dnv-rp-f103-2010-table-", {"density": "001", "a1": "002", "a2": "003"}),
    "2019": (
        "2019-09",
        "dnv-rp-f103-2019-09-table-",
        {"density": "6-2", "anode": "6-3", "a1": "a-1", "a2": "a-2"},
    ),
}
EDITIONS: list[F103Edition] = ["2010", "2019"]
EXPECTED = {
    "2010": ("2010", tbl.F103_WIKI_PATH, "verified-2010-tables", "Table 5-1", "Table A.1", "Table A.2"),
    "2019": ("2019-09", tbl.F103_WIKI_PATH_2019, "verified-2019-tables", "Table 6-2", "Table A-1", "Table A-2"),
}

EDITION: F103Edition = "2010"
SCALE = 100.0

# Representative temperature inside each band.
_TEMPERATURE_IN_BAND = {
    FluidTemperatureBand.LE_25: 10.0,
    FluidTemperatureBand.GT_25_50: 40.0,
    FluidTemperatureBand.LE_50: 20.0,
    FluidTemperatureBand.GT_50_80: 65.0,
    FluidTemperatureBand.GT_80_120: 100.0,
    FluidTemperatureBand.GT_120: 130.0,
}
_EXPOSURE_BY_LABEL = {"non-buried": Exposure.NON_BURIED, "buried": Exposure.BURIED}
_COATING_2019_BY_NAME = {
    "Glass fibre reinforced asphalt enamel": LinepipeCoating.GFR_ASPHALT_ENAMEL,
    "FBE": LinepipeCoating.FBE,
    "3-layer FBE/PE": LinepipeCoating.THREE_LAYER_FBE_PE,
    "3-layer FBE/PP": LinepipeCoating.THREE_LAYER_FBE_PP,
    "FBE/PP thermally insulating coating": LinepipeCoating.FBE_PP_THERMAL_INSULATION,
    "FBE/PU thermally insulating coating": LinepipeCoating.FBE_PU_THERMAL_INSULATION,
    "Polychloroprene": LinepipeCoating.POLYCHLOROPRENE,
}
# (first token of the FJC type cell, first token of the infill cell) -> member
_FJC_2019_BY_TOKENS = {
    ("none", "4E(1)"): FieldJointCoating2019.NONE_4E1_PU,
    ("1D", "4E(2)"): FieldJointCoating2019.FJC_1D_2A_MASTIC,
    ("2B(1)", "none"): FieldJointCoating2019.FJC_2B1_HSS_PE,
    ("2C(1)", "none"): FieldJointCoating2019.FJC_2C1_HSS_PP,
    ("3A", "none"): FieldJointCoating2019.FJC_3A_FBE,
    ("3A", "4E(2)"): FieldJointCoating2019.FJC_3A_FBE_4E2_INFILL,
    ("2B(2)", "none"): FieldJointCoating2019.FJC_2B2_FBE_PE_HSS,
    ("5D(1)", "none"): FieldJointCoating2019.FJC_5D1_5E_FBE_PE,
    ("2C(2)", "none"): FieldJointCoating2019.FJC_2C2_FBE_PP_HSS,
    ("5A/B/C(1)", "none"): FieldJointCoating2019.FJC_5ABC1_FBE_PP,
    ("NA", "5C(1)"): FieldJointCoating2019.FJC_5C1_PE_ON_FBE,
    ("NA", "5C(2)"): FieldJointCoating2019.FJC_5C2_PP_ON_FBE,
    ("8A", "none"): FieldJointCoating2019.FJC_8A_POLYCHLOROPRENE,
}
_MATERIAL_BY_LABEL = {"Al-Zn-In": AnodeMaterial.ALUMINIUM, "Zn": AnodeMaterial.ZINC}
_TEMPERATURE_OF_ROW = {"≤30": 30.0, "60": 60.0, "80": 80.0, ">30to50": 50.0}
_ROW_LABEL_IN_MODULE = {"≤30": "≤30", "60": "60", "80": "80", ">30to50": "> 30 to 50"}


# ---------------------------------------------------------------------------
# CSV parsing helpers
# ---------------------------------------------------------------------------


def _fixture_path(edition: str, key: str) -> Path:
    directory, prefix, ids = FILES[edition]
    return DATASETS / directory / f"{prefix}{ids[key]}.csv"


def _read_table(edition: str, key: str) -> list[list[str]]:
    with _fixture_path(edition, key).open(encoding="utf-8", newline="") as handle:
        return [[cell.strip() for cell in row] for row in csv.reader(handle)]


def _data_rows(rows: list[list[str]]) -> list[list[str]]:
    """Rows after the header, excluding footnote / ``Note:`` rows."""
    return [
        row
        for row in rows
        if row[0] and not row[0].startswith(("Note", "*", "1)")) or (row and not row[0] and any(row[1:]))
    ]


def _band_from_header(cell: str) -> FluidTemperatureBand:
    """``≤ 50`` -> LE_50, ``>50 - 80`` -> GT_50_80, ``> 25 - 50`` -> GT_25_50 ..."""
    return FluidTemperatureBand(cell.replace("≤", "<=").replace(" ", ""))


@lru_cache(maxsize=None)
def density_table(edition: str) -> dict[tuple[Exposure, FluidTemperatureBand], float]:
    """Table 5-1 (2010) / Table 6-2 (2019)."""
    rows = _read_table(edition, "density")
    bands = {idx: _band_from_header(cell) for idx, cell in enumerate(rows[2]) if idx > 0}
    return {
        (_EXPOSURE_BY_LABEL[row[0].rstrip("*)").lower()], band): float(row[col])
        for row in _data_rows(rows[3:])
        for col, band in bands.items()
    }


@lru_cache(maxsize=None)
def table_a1_2010() -> dict[int, dict[str, object]]:
    """Table A.1 keyed by DNV-RP-F106 CDS number (printed x100)."""
    out: dict[int, dict[str, object]] = {}
    for row in _data_rows(_read_table("2010", "a1")[2:]):
        cds = int(row[1].replace("No.", "").strip())
        out[cds] = {
            "concrete": row[2] == "yes",
            "max_temp": float(row[3]),
            "a_x100": float(row[4]),
            "b_x100": float(row[5]),
        }
    return out


@lru_cache(maxsize=None)
def table_a1_2019() -> dict[tuple[LinepipeCoating, bool], dict[str, object]]:
    """Table A-1 keyed by (coating, concrete weight coating); a and b direct."""
    out: dict[tuple[LinepipeCoating, bool], dict[str, object]] = {}
    for row in _data_rows(_read_table("2019", "a1")[2:]):
        coating = _COATING_2019_BY_NAME[row[0]]
        out[(coating, row[2] == "yes")] = {
            "cds": " ".join(row[1].split()),
            "max_temp": float(row[3]),
            "a": float(row[4]),
            "b": float(row[5]),
        }
    return out


@lru_cache(maxsize=None)
def table_a2_2010() -> dict[str, dict[str, object]]:
    """Table A.2 keyed by FJC system code (``none``, ``1A``, ``2A``...)."""
    out: dict[str, dict[str, object]] = {}
    for row in _data_rows(_read_table("2010", "a2")[2:]):
        code = row[0].split()[0]
        key = code.lower() if code == "None" else code
        out[key] = {
            "max_temp": float(row[2]) if row[2] else None,
            "a_x100": float(row[4]),
            "b_x100": float(row[5]),
        }
    return out


@lru_cache(maxsize=None)
def table_a2_2019() -> dict[FieldJointCoating2019, dict[str, object]]:
    """Table A-2 keyed by the FieldJointCoating2019 member; a and b direct."""
    out: dict[FieldJointCoating2019, dict[str, object]] = {}
    for row in _data_rows(_read_table("2019", "a2")[2:]):
        key = _FJC_2019_BY_TOKENS[(row[0].split()[0], row[1].split()[0])]
        out[key] = {"max_temp": float(row[2]), "a": float(row[4]), "b": float(row[5])}
    return out


@lru_cache(maxsize=None)
def table_6_3() -> dict[tuple[AnodeMaterial, AnodeEnvironment, str], tuple[float, float]]:
    """Table 6-3: (potential, capacity); material and Zn seawater carried over blanks."""
    rows = _read_table("2019", "anode")
    out: dict[tuple[AnodeMaterial, AnodeEnvironment, str], tuple[float, float]] = {}
    material: AnodeMaterial | None = None
    seawater: tuple[float, float] | None = None
    for row in _data_rows(rows[2:]):
        if row[0]:
            material = _MATERIAL_BY_LABEL[row[0].splitlines()[0]]
            seawater = None
        assert material is not None
        label = row[1].replace(" ", "")
        if row[2]:
            seawater = (float(row[2]), float(row[3]))
        assert seawater is not None
        out[(material, AnodeEnvironment.SEAWATER, label)] = seawater
        out[(material, AnodeEnvironment.SEDIMENT, label)] = (float(row[4]), float(row[5]))
    return out


# ---------------------------------------------------------------------------
# Fixture sanity
# ---------------------------------------------------------------------------


@pytest.mark.parametrize("edition", EDITIONS)
def test_fixture_csvs_present(edition):
    for key in FILES[edition][2]:
        assert _fixture_path(edition, key).is_file()


def test_density_tables_parse_all_cells():
    assert len(density_table("2010")) == 8
    assert len(density_table("2019")) == 10


def test_table_a1_2010_covers_every_2010_linepipe_coating():
    coatings_2010 = [c for c in LinepipeCoating if c not in (
        LinepipeCoating.FBE_PP_THERMAL_INSULATION, LinepipeCoating.FBE_PU_THERMAL_INSULATION
    )]
    assert set(table_a1_2010()) == {coating.cds_number for coating in coatings_2010}


def test_table_a1_2019_rows():
    rows = table_a1_2019()
    assert len(rows) == 10
    assert rows[(LinepipeCoating.FBE, True)]["b"] == 0.0003
    assert rows[(LinepipeCoating.FBE, False)]["b"] == 0.0010
    assert LinepipeCoating.GFR_COAL_TAR_ENAMEL not in {c for c, _ in rows}
    assert LinepipeCoating.MULTI_LAYER_FBE_PP not in {c for c, _ in rows}


def test_table_a2_covers_every_field_joint_coating():
    assert set(table_a2_2010()) == {fjc.value for fjc in FieldJointCoating}
    assert set(table_a2_2019()) == set(FieldJointCoating2019)


@pytest.mark.parametrize(("tokens", "member"), list(_FJC_2019_BY_TOKENS.items()))
def test_resolve_2019_field_joint_id_with_infill_matches_table_a2(tokens, member):
    # Issue #2256: every printed (FJC id, infill) pair of Table A-2 resolves to its row;
    # the "NA" rows are addressed by their infill id (5C(1) / 5C(2)).
    fjc_id, infill = tokens
    if fjc_id == "NA":
        fjc_id = infill
    assert tbl.resolve_field_joint_coating_2019(fjc_id, infill) is member
    assert infill in tbl.field_joint_infill_choices_2019(member)


@pytest.mark.parametrize("member", list(FieldJointCoating2019))
def test_resolve_2019_field_joint_amended_name(member):
    amended = tbl._TABLE_A2_2019[member].amended_2021_id
    infill = "none" if amended == "17A" else None
    assert tbl.resolve_field_joint_coating_2019(amended.upper(), infill) is member


def test_resolve_2019_field_joint_3a_requires_infill():
    with pytest.raises(ValueError, match=r"splits this row by infill.*\['none', '4E\(2\)'\]"):
        tbl.resolve_field_joint_coating_2019("3A")
    assert tbl.resolve_field_joint_coating_2019("3A+4E(2)") is FieldJointCoating2019.FJC_3A_FBE_4E2_INFILL


def test_resolve_2019_field_joint_unknown_id_lists_valid_ids():
    with pytest.raises(ValueError, match=r"valid DNVGL-RP-F102 \(2011\) ids: \['none', '1D/2A'"):
        tbl.resolve_field_joint_coating_2019("3B")


def test_2010_field_joint_id_under_2019_still_raises_clearly():
    with pytest.raises(ValueError, match=r"FieldJointCoating.FJC_3A_FBE is a DNV-RP-F103 \(2010\)"):
        tbl.field_joint_coating_constants(FieldJointCoating.FJC_3A_FBE, "2019")


def test_table_6_3_equals_b401_2021_table_8_6():
    from tests.cathodic_protection.test_b401_tables import table_8_6

    b401 = {(m, e, _ROW_LABEL_IN_MODULE[r]): v for (m, e, r), v in table_6_3().items()}
    assert b401 == table_8_6()


# ---------------------------------------------------------------------------
# Table 5-1 / 6-2
# ---------------------------------------------------------------------------


@pytest.mark.parametrize("edition", EDITIONS)
@pytest.mark.parametrize("exposure", list(Exposure))
@pytest.mark.parametrize("band", list(FluidTemperatureBand))
def test_mean_current_density_matches_density_table(edition, exposure, band):
    if band not in tbl.temperature_bands(edition):
        return
    result = tbl.mean_current_density(exposure, _TEMPERATURE_IN_BAND[band], edition=edition)
    assert result.value == density_table(edition)[(exposure, band)]
    assert result.units == "A/m2"
    assert result.citation.section == EXPECTED[edition][3]
    assert band.value in result.citation.note


def test_temperature_bands_per_edition():
    assert tbl.temperature_bands("2010") == (
        FluidTemperatureBand.LE_50,
        FluidTemperatureBand.GT_50_80,
        FluidTemperatureBand.GT_80_120,
        FluidTemperatureBand.GT_120,
    )
    assert tbl.temperature_bands("2019") == (
        FluidTemperatureBand.LE_25,
        FluidTemperatureBand.GT_25_50,
        FluidTemperatureBand.GT_50_80,
        FluidTemperatureBand.GT_80_120,
        FluidTemperatureBand.GT_120,
    )


def test_non_buried_le_50_spot_value():
    """Hand-derived: non-buried, <= 50 °C, 0.050 A/m2 (2010); 0.060 at 40 °C (2019)."""
    assert tbl.mean_current_density(Exposure.NON_BURIED, 40.0, edition="2010").value == 0.050
    assert tbl.mean_current_density(Exposure.NON_BURIED, 40.0, edition="2019").value == 0.060


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
def test_fluid_temperature_band_boundaries_2010(fluid_temp_c, expected):
    assert tbl.fluid_temperature_band(fluid_temp_c, "2010") is expected


@pytest.mark.parametrize(
    ("fluid_temp_c", "expected"),
    [
        (-5.0, FluidTemperatureBand.LE_25),
        (25.0, FluidTemperatureBand.LE_25),  # header "<= 25"
        (25.01, FluidTemperatureBand.GT_25_50),
        (50.0, FluidTemperatureBand.GT_25_50),  # header "> 25 - 50"
        (50.01, FluidTemperatureBand.GT_50_80),
        (80.0, FluidTemperatureBand.GT_50_80),
        (120.0, FluidTemperatureBand.GT_80_120),
        (120.01, FluidTemperatureBand.GT_120),
    ],
)
def test_fluid_temperature_band_boundaries_2019(fluid_temp_c, expected):
    assert tbl.fluid_temperature_band(fluid_temp_c, "2019") is expected


# ---------------------------------------------------------------------------
# Table A.1 (2010) / A-1 (2019)
# ---------------------------------------------------------------------------


@pytest.mark.parametrize("coating", [c for c in LinepipeCoating if "thermally" not in c.value])
def test_linepipe_coating_constants_match_table_a1_2010(coating):
    row = table_a1_2010()[coating.cds_number]
    a, b = tbl.linepipe_coating_constants(coating, edition=EDITION)
    assert a.value == row["a_x100"] / SCALE
    assert b.value == row["b_x100"] / SCALE
    assert coating.max_temperature_c == row["max_temp"]
    assert a.units == "dimensionless"
    assert b.units == "1/yr"
    assert a.citation.section == "Table A.1"
    assert b.citation.section == "Table A.1"
    assert f"CDS No. {coating.cds_number}" in a.citation.note


@pytest.mark.parametrize(("coating", "concrete"), list(table_a1_2019()))
def test_linepipe_coating_constants_match_table_a1_2019(coating, concrete):
    row = table_a1_2019()[(coating, concrete)]
    a, b = tbl.linepipe_coating_constants(coating, "2019", concrete_weight_coating=concrete)
    assert a.value == row["a"]
    assert b.value == row["b"]
    assert a.citation.section == "Table A-1"
    assert b.citation.section == "Table A-1"
    assert f"CDS {row['cds']}" in a.citation.note
    assert f"concrete weight coating {'yes' if concrete else 'no'}" in a.citation.note
    printed = tbl.linepipe_coating_row(coating, "2019", concrete)
    assert printed.max_temperature_c == row["max_temp"]


def test_fbe_spot_values():
    """Hand-derived: FBE a = 0.010, b = 0.0003 (2010); a = 0.030, b = 0.0003 / 0.0010 (2019)."""
    a, b = tbl.linepipe_coating_constants(LinepipeCoating.FBE, edition=EDITION)
    assert (a.value, b.value) == pytest.approx((0.010, 0.0003))
    assert LinepipeCoating.FBE.cds_number == 1
    assert LinepipeCoating.FBE.max_temperature_c == 90.0
    a, b = tbl.linepipe_coating_constants(LinepipeCoating.FBE, "2019", concrete_weight_coating=True)
    assert (a.value, b.value) == pytest.approx((0.030, 0.0003))
    a, b = tbl.linepipe_coating_constants(LinepipeCoating.FBE, "2019")
    assert (a.value, b.value) == pytest.approx((0.030, 0.0010))


def test_single_flag_rows_2019_ignore_the_requested_concrete_flag():
    """Asphalt enamel is printed with concrete only; polychloroprene without only."""
    with_flag = tbl.linepipe_coating_constants(
        LinepipeCoating.GFR_ASPHALT_ENAMEL, "2019", concrete_weight_coating=False
    )
    assert (with_flag[0].value, with_flag[1].value) == (0.01, 0.0003)
    assert "concrete weight coating yes" in with_flag[0].citation.note
    neo = tbl.linepipe_coating_constants(
        LinepipeCoating.POLYCHLOROPRENE, "2019", concrete_weight_coating=True
    )
    assert (neo[0].value, neo[1].value) == (0.010, 0.001)


@pytest.mark.parametrize(
    "coating", [LinepipeCoating.GFR_COAL_TAR_ENAMEL, LinepipeCoating.MULTI_LAYER_FBE_PP]
)
def test_coatings_absent_from_2019_table_raise(coating):
    with pytest.raises(ValueError, match="not tabulated in DNVGL-RP-F103 \\(2019\\) Table A-1"):
        tbl.linepipe_coating_constants(coating, "2019")


@pytest.mark.parametrize(
    "coating",
    [LinepipeCoating.FBE_PP_THERMAL_INSULATION, LinepipeCoating.FBE_PU_THERMAL_INSULATION],
)
def test_2019_only_coatings_raise_under_2010(coating):
    with pytest.raises(ValueError, match="no row in DNV-RP-F103 \\(October 2010\\) Table A.1"):
        tbl.linepipe_coating_constants(coating, "2010")
    with pytest.raises(ValueError, match="no row in DNV-RP-F103 \\(October 2010\\)"):
        _ = coating.cds_number


# ---------------------------------------------------------------------------
# Table A.2 (2010) / A-2 (2019)
# ---------------------------------------------------------------------------


@pytest.mark.parametrize("fjc", list(FieldJointCoating))
def test_field_joint_coating_constants_match_table_a2_2010(fjc):
    row = table_a2_2010()[fjc.value]
    a, b = tbl.field_joint_coating_constants(fjc, edition=EDITION)
    assert a.value == row["a_x100"] / SCALE
    assert b.value == row["b_x100"] / SCALE
    assert fjc.max_temperature_c == row["max_temp"]
    assert a.citation.section == "Table A.2"
    assert b.citation.section == "Table A.2"
    assert f"FJC {fjc.value}" in b.citation.note


@pytest.mark.parametrize("fjc", list(FieldJointCoating2019))
def test_field_joint_coating_constants_match_table_a2_2019(fjc):
    row = table_a2_2019()[fjc]
    a, b = tbl.field_joint_coating_constants(fjc, edition="2019")
    assert a.value == row["a"]
    assert b.value == row["b"]
    assert fjc.max_temperature_c == row["max_temp"]
    assert a.citation.section == "Table A-2"
    assert f"FJC {fjc.value} (DNVGL-RP-F102 (2011) system" in b.citation.note


def test_no_field_joint_coating_has_no_max_temperature():
    assert FieldJointCoating.NONE.max_temperature_c is None


def test_2010_none_maps_to_2019_bare_steel_row():
    key, row = tbl.field_joint_coating_row(FieldJointCoating.NONE, "2019")
    assert key is FieldJointCoating2019.NONE_4E1_PU
    a, b = tbl.field_joint_coating_constants(FieldJointCoating.NONE, "2019")
    assert (a.value, b.value) == (0.30, 0.030)
    assert row.amended_2021_id.startswith("none")


@pytest.mark.parametrize("fjc", [f for f in FieldJointCoating if f is not FieldJointCoating.NONE])
def test_other_2010_field_joint_ids_raise_under_2019(fjc):
    with pytest.raises(ValueError, match="DNVGL-RP-F102 \\(2011\\) ids of FieldJointCoating2019"):
        tbl.field_joint_coating_constants(fjc, "2019")


def test_2019_field_joint_ids_raise_under_2010():
    with pytest.raises(ValueError, match="DNV-RP-F103 \\(2010\\) Table A.2 uses the FieldJointCoating ids"):
        tbl.field_joint_coating_constants(FieldJointCoating2019.FJC_3A_FBE, "2010")


# ---------------------------------------------------------------------------
# Bracelet utilisation and anode design values
# ---------------------------------------------------------------------------


def test_bracelet_utilisation_factor_2010_defers_to_b401_table_10_8():
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


def test_bracelet_utilisation_factor_2019_cites_clause_6_4_2():
    result = tbl.bracelet_utilisation_factor(edition="2019")
    assert result.value == pytest.approx(0.80)
    assert result.citation.code_id == "dnv-rp-f103"
    assert result.citation.revision == "2019-09"
    assert result.citation.section == "[6.4.2] (anode utilisation factor)"
    assert "provenance=verified-2019-tables" in result.citation.note


@pytest.mark.parametrize(
    ("material", "environment", "row"),
    [
        (material, environment, row)
        for material, rows in (
            (AnodeMaterial.ALUMINIUM, ("≤30", "60", "80")),
            (AnodeMaterial.ZINC, ("≤30", ">30to50")),
        )
        for environment in AnodeEnvironment
        for row in rows
    ],
)
def test_anode_values_2019_match_table_6_3(material, environment, row):
    potential, capacity = table_6_3()[(material, environment, row)]
    temperature = _TEMPERATURE_OF_ROW[row]
    cap = tbl.anode_capacity(material, environment, "2019", temperature)
    pot = tbl.anode_closed_circuit_potential(material, environment, "2019", temperature)
    voltage = tbl.design_driving_voltage(material, "2019", environment, temperature)
    assert cap.value == capacity
    assert pot.value == potential
    assert voltage.value == pytest.approx(-0.80 - potential)
    for cited in (cap, pot, voltage):
        assert cited.citation.code_id == "dnv-rp-f103"
        assert cited.citation.revision == "2019-09"
        assert cited.citation.section.startswith("Table 6-3")
        assert f"row {_ROW_LABEL_IN_MODULE[row]} °C" in cited.citation.note
    assert voltage.citation.section == "Table 6-3 with [6.7.11] (design protective potential)"


def test_anode_values_2010_defer_to_b401_2010():
    cap = tbl.anode_capacity(AnodeMaterial.ALUMINIUM, AnodeEnvironment.SEAWATER, "2010")
    pot = tbl.anode_closed_circuit_potential(AnodeMaterial.ZINC, AnodeEnvironment.SEAWATER, "2010")
    voltage = tbl.design_driving_voltage(AnodeMaterial.ALUMINIUM, "2010")
    assert (cap.value, pot.value, voltage.value) == (2000.0, -1.00, 0.25)
    for cited in (cap, pot, voltage):
        assert cited.citation.code_id == "dnv-rp-b401"
        assert cited.citation.revision == "2011"
        assert "defers to DNV-RP-B401 Table 10-6" in cited.citation.note
    assert tbl.protection_potential("2010").citation.section.startswith("Sec. 5")
    assert tbl.protection_potential("2019").citation.section == "[6.7.11] (design protective potential)"


def test_anode_temperature_above_30_rejected_under_2010():
    with pytest.raises(ValueError, match="Use edition '2019' \\(Table 6-3\\)"):
        tbl.anode_capacity(
            AnodeMaterial.ALUMINIUM, AnodeEnvironment.SEDIMENT, "2010", anode_surface_temperature_c=60.0
        )


# ---------------------------------------------------------------------------
# Editions and provenance
# ---------------------------------------------------------------------------


@pytest.mark.parametrize(
    ("edition", "expected"),
    [
        ("2010", "verified-2010-tables"),
        ("dnv-rp-f103-2010", "verified-2010-tables"),
        ("2019", "verified-2019-tables"),
        ("DNVGL-RP-F103-2019", "verified-2019-tables"),
        ("2021", "verified-2019-tables"),  # amended print alias
        ("dnv-rp-f103-2021", "verified-2019-tables"),
    ],
)
def test_edition_provenance(edition, expected):
    assert tbl.edition_provenance(edition) == expected


def test_edition_provenance_rejects_unknown_edition():
    with pytest.raises(ValueError, match="Unsupported DNV-RP-F103 edition"):
        tbl.edition_provenance("2003")


@pytest.mark.parametrize("token", ["2016", "DNVGL-RP-F103-2016", "f103_2016"])
def test_2016_aliases_to_2019_with_a_warning(token):
    """The 2016 print is not on file; 2019 republishes it unchanged (owner D3)."""
    with pytest.warns(UserWarning, match="July 2016 print is not on file; using the September 2019"):
        assert tbl.edition_provenance(token) == "verified-2019-tables"
    with pytest.warns(UserWarning, match="same tables"):
        result = tbl.mean_current_density(Exposure.NON_BURIED, 60.0, edition=token)
    assert result == tbl.mean_current_density(Exposure.NON_BURIED, 60.0, edition="2019")
    assert "DNVGL-RP-F103 (September 2019, amended May 2021)" in result.citation.note


def test_edition_none_warns_and_defaults_to_2019():
    """The default moved to 2019 (owner decision 2026-09-27, epic #2206).

    Buried at 25 C falls in the 2019 Table 6-2 "<= 25" column: 0.020 A/m2
    (the 2010 Table 5-1 "<= 50" column also gives 0.020, so only the
    citation distinguishes the editions here).
    """
    with pytest.warns(UserWarning, match="defaulting to DNV-RP-F103 2019"):
        result = tbl.mean_current_density(Exposure.BURIED, 25.0)
    assert result.value == pytest.approx(0.020)
    assert "edition=2019" in result.citation.note
    assert "provenance=verified-2019-tables" in result.citation.note


def test_2021_alias_gives_the_2019_tables():
    for exposure in Exposure:
        assert (
            tbl.mean_current_density(exposure, 130.0, edition="2021")
            == tbl.mean_current_density(exposure, 130.0, edition="2019")
        )
    assert tbl.b401_edition_for_f103("2021") == "2017"
    assert tbl.b401_edition_for_f103("2010") == "2010"


# ---------------------------------------------------------------------------
# Citations
# ---------------------------------------------------------------------------


def _all_f103_results(edition: F103Edition) -> list:
    if edition == "2010":
        a1, b1 = tbl.linepipe_coating_constants(LinepipeCoating.MULTI_LAYER_FBE_PP, edition=edition)
        a2, b2 = tbl.field_joint_coating_constants(FieldJointCoating.FJC_3D_FBE_PP, edition=edition)
        return [tbl.mean_current_density(Exposure.NON_BURIED, 90.0, edition=edition), a1, b1, a2, b2]
    a1, b1 = tbl.linepipe_coating_constants(LinepipeCoating.FBE_PP_THERMAL_INSULATION, edition=edition)
    a2, b2 = tbl.field_joint_coating_constants(FieldJointCoating2019.FJC_5ABC1_FBE_PP, edition=edition)
    return [
        tbl.mean_current_density(Exposure.NON_BURIED, 90.0, edition=edition),
        a1,
        b1,
        a2,
        b2,
        tbl.anode_capacity(AnodeMaterial.ZINC, AnodeEnvironment.SEDIMENT, edition, 40.0),
        tbl.anode_closed_circuit_potential(AnodeMaterial.ALUMINIUM, AnodeEnvironment.SEAWATER, edition),
        tbl.design_driving_voltage(AnodeMaterial.ALUMINIUM, edition),
        tbl.protection_potential(edition),
        tbl.bracelet_utilisation_factor(edition),
    ]


@pytest.mark.parametrize("edition", EDITIONS)
def test_every_result_carries_the_edition_citation(edition):
    revision, wiki_path, provenance, *_ = EXPECTED[edition]
    with warnings.catch_warnings():
        warnings.simplefilter("error")
        results = _all_f103_results(edition)
    for result in results:
        citation = result.citation
        assert citation.code_id == "dnv-rp-f103"
        assert citation.publisher == "DNV"
        assert citation.revision == revision
        assert citation.wiki_path == wiki_path
        assert f"provenance={provenance}" in citation.note
        assert result.units


def test_2019_citations_validate_against_vendored_fixture_pages():
    for result in _all_f103_results("2019"):
        validate_citation(result.citation, repo_root=CITATION_FIXTURES)


def test_citations_validate_against_live_wiki():
    try:
        page = resolve_wiki_path(tbl.F103_WIKI_PATH)
    except CitationResolutionError as exc:
        pytest.skip(f"wiki not resolvable: {exc.reason.splitlines()[0]}")
    if not page.is_file():
        pytest.skip(f"wiki page missing: {page}")
    for result in _all_f103_results(EDITION):
        validate_citation(result.citation)
    validate_citation(tbl.bracelet_utilisation_factor(edition=EDITION).citation)
