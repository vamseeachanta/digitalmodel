"""DNV-RP-B401 design tables as cited, edition-keyed lookups.

Every public lookup returns a :class:`~digitalmodel.citations.CitedValue`
whose :class:`~digitalmodel.citations.Citation` names the exact B401 table
(or clause) the number comes from, in the numbering of the requested
edition. The lookups are pure: they do not touch the wiki. Callers that need
the fail-closed guarantee call ``validate_citation(result.citation)`` on the
returned value.

Editions and their sources (issue #2208):

``"2005"`` / ``"2010"``
    DNV-RP-B401 October 2010 (2005 with 2008 amendments carries the same
    tables). Wiki page ``standards/dnv-rp-b401.md`` (revision ``"2011"``),
    dataset CSVs ``datasets/dnv-rp-b401/2005-with-2008-amendments/``.
    Tables 10-1 .. 10-8.
``"2017"``
    DNVGL-RP-B401 June 2017. Wiki page ``standards/dnv-rp-b401-2017.md``
    (revision ``"2017-06"``). Design tables moved to Appendix A
    (Table A-1 .. A-8); the values of A-1, A-2, A-3, A-4, A-6 and A-8 equal
    the 2010 tables.
``"2021"``
    DNV-RP-B401 May 2021. Wiki page ``standards/dnv-rp-b401-2021.md``
    (revision ``"2021-05"``). Design tables in Sec. 8 (Table 8-1 .. 8-8).
    Table 8-4 adds paint coating category IV; Table 8-6 tabulates the anode
    design values by anode surface temperature (Al-Zn-In at <= 30, 60 and
    80 °C; Zn at <= 30 and > 30-50 °C) and gives Zn in seawater -1.030 V.

All four editions are verified against the text layer of the licensed PDFs
(2026-09-26); ``edition_provenance`` returns ``verified-<edition>-tables``.

The anode resistance formulae (Table 10-7 / A-7 / 8-7) are not value tables
and stay in ``dnv_rp_b401.py``. The compositional limits (Table 10-5 / A-5 /
8-5) are out of scope.
"""

from __future__ import annotations

from enum import Enum
from typing import Final, NamedTuple

from digitalmodel.cathodic_protection._edition import (
    Edition,
    normalize_edition,
    standard_for_edition,
)
from digitalmodel.citations import Citation, CitedValue


_WIKI_STANDARDS: Final = "wikis/engineering-standards/wiki/standards/"

#: Wiki page of the October 2010 edition (frontmatter revision "2011").
B401_WIKI_PATH: Final = _WIKI_STANDARDS + "dnv-rp-b401.md"
#: Wiki page of the June 2017 edition (frontmatter revision "2017-06").
B401_WIKI_PATH_2017: Final = _WIKI_STANDARDS + "dnv-rp-b401-2017.md"
#: Wiki page of the May 2021 edition (frontmatter revision "2021-05").
B401_WIKI_PATH_2021: Final = _WIKI_STANDARDS + "dnv-rp-b401-2021.md"

_B401_CITATION_TEMPLATE: Final[dict[str, str]] = {
    "code_id": "dnv-rp-b401",
    "publisher": "DNV",
}


class EditionSource(NamedTuple):
    """Where an edition's numbers come from and how its tables are labelled."""

    revision: str
    wiki_path: str
    table_prefix: str
    provenance: str
    protection_potential_section: str
    buried_current_density_section: str


_SOURCE_BY_EDITION: Final[dict[Edition, EditionSource]] = {
    "2005": EditionSource(
        revision="2011",
        wiki_path=B401_WIKI_PATH,
        table_prefix="10",
        provenance="verified-2005-tables",
        protection_potential_section=(
            "Sec. 5 (structure-to-electrolyte potential criteria)"
        ),
        buried_current_density_section="Sec. 6.3 (buried surfaces)",
    ),
    "2010": EditionSource(
        revision="2011",
        wiki_path=B401_WIKI_PATH,
        table_prefix="10",
        provenance="verified-2010-tables",
        protection_potential_section=(
            "Sec. 5 (structure-to-electrolyte potential criteria)"
        ),
        buried_current_density_section="Sec. 6.3 (buried surfaces)",
    ),
    "2017": EditionSource(
        revision="2017-06",
        wiki_path=B401_WIKI_PATH_2017,
        table_prefix="A",
        provenance="verified-2017-tables",
        protection_potential_section="[5.4] (design protective potential)",
        buried_current_density_section="[6.3.8] (buried surfaces)",
    ),
    "2021": EditionSource(
        revision="2021-05",
        wiki_path=B401_WIKI_PATH_2021,
        table_prefix="8",
        provenance="verified-2021-tables",
        protection_potential_section="[2.4.2] (design protective potential)",
        buried_current_density_section="[3.3.8] (buried surfaces)",
    ),
}

#: Editions whose coating table has paint category IV (Table 8-4).
_CATEGORY_IV_EDITIONS: Final[frozenset[str]] = frozenset({"2021"})

#: Editions whose anode table is keyed by anode surface temperature (Table 8-6).
_ANODE_TEMPERATURE_EDITIONS: Final[frozenset[str]] = frozenset({"2021"})

#: Upper anode surface temperature of the ambient-temperature anode tables
#: (Table 10-6 / A-6, "at seawater ambient temperatures").
AMBIENT_ANODE_TEMPERATURE_C: Final = 30.0

UNITS_CURRENT_DENSITY: Final = "A/m2"
UNITS_CAPACITY: Final = "Ah/kg"
UNITS_POTENTIAL: Final = "V (Ag/AgCl/seawater)"
UNITS_DIMENSIONLESS: Final = "dimensionless"
UNITS_PER_YEAR: Final = "1/yr"


class Climate(str, Enum):
    """Climatic region by surface water temperature (Table 10-1 header)."""

    TROPICAL = "tropical"  # > 20 °C
    SUBTROPICAL = "subtropical"  # 12–20 °C
    TEMPERATE = "temperate"  # 7–11 °C (Table 10-1) / 7–12 °C (Table 10-2)
    ARCTIC = "arctic"  # < 7 °C


class DepthBand(str, Enum):
    """Depth bands of Tables 10-1 and 10-2; values are the CSV row labels."""

    M0_30 = "0-30"
    M30_100 = ">30-100"
    M100_300 = ">100-300"
    M300_PLUS = ">300"


class PaintCategory(str, Enum):
    """Paint coating categories I, II, III (Table 10-4 / A-4) and IV (Table 8-4).

    Category IV exists only in the May 2021 edition (Table 8-4, [3.4.6]);
    lookups raise ``ValueError`` for it under earlier editions.
    """

    I = "I"  # noqa: E741 - Table 10-4 column label
    II = "II"
    III = "III"
    IV = "IV"  # 2021 only


class AnodeMaterial(str, Enum):
    """Galvanic anode material types of Table 10-6 (Al-Zn-In and Zn in 8-6)."""

    ALUMINIUM = "Al-based"
    ZINC = "Zn-based"


class AnodeEnvironment(str, Enum):
    """Anode exposure environments of Table 10-6."""

    SEAWATER = "seawater"
    SEDIMENT = "sediments"


class AnodeShape(str, Enum):
    """Anode geometry classes of Tables 10-7 and 10-8."""

    LONG_SLENDER_STANDOFF = "long_slender_standoff"  # L >= 4r
    SHORT_SLENDER_STANDOFF = "short_slender_standoff"  # L < 4r
    LONG_FLUSH = "long_flush_mounted"  # L >= 4 width and L >= 4 thickness
    SHORT_FLUSH_BRACELET = "short_flush_bracelet_other"  # and other types


class DesignPhase(str, Enum):
    """Design current-density phases: initial and final (10-1), mean (10-2)."""

    INITIAL = "initial"
    MEAN = "mean"
    FINAL = "final"


class AnodeTemperatureRow(NamedTuple):
    """One cell pair of Table 8-6 (B401 2021) / Table 6-3 (F103 2019)."""

    material: AnodeMaterial
    environment: AnodeEnvironment
    row_label: str
    max_temperature_c: float
    closed_circuit_potential_V: float
    capacity_Ah_kg: float


# Climate boundaries from the Table 10-1 / 10-2 column headers (°C).
# 'Tropical' (> 20 °C); 'Sub-Tropical' (12–20 °C); 'Temperate' (7–11 °C in
# Table 10-1, 7–12 °C in Table 10-2); 'Arctic' (< 7 °C).
_TROPICAL_ABOVE_C: Final = 20.0
_SUBTROPICAL_FROM_C: Final = 12.0
_TEMPERATE_FROM_C: Final = 7.0

# Depth-band upper limits from the Table 10-1 / 10-2 row labels (m).
_DEPTH_BAND_UPPER_M: Final[dict[DepthBand, float]] = {
    DepthBand.M0_30: 30.0,  # row "0-30"
    DepthBand.M30_100: 100.0,  # row ">30-100"
    DepthBand.M100_300: 300.0,  # row ">100-300"
}

# Table 10-1 (= A-1 = 8-1): recommended INITIAL design current densities
# (A/m2) for seawater-exposed bare metal surfaces. Columns Tropical /
# Sub-Tropical / Temperate / Arctic; rows are depth bands.
_INITIAL_CURRENT_DENSITY: Final[dict[tuple[Climate, DepthBand], float]] = {
    (Climate.TROPICAL, DepthBand.M0_30): 0.150,  # row 0-30, Tropical initial
    (Climate.TROPICAL, DepthBand.M30_100): 0.120,  # row >30-100, Tropical
    (Climate.TROPICAL, DepthBand.M100_300): 0.140,  # row >100-300, Tropical
    (Climate.TROPICAL, DepthBand.M300_PLUS): 0.180,  # row >300, Tropical
    (Climate.SUBTROPICAL, DepthBand.M0_30): 0.170,  # row 0-30, Sub-Tropical
    (Climate.SUBTROPICAL, DepthBand.M30_100): 0.140,  # row >30-100, Sub-Trop.
    (Climate.SUBTROPICAL, DepthBand.M100_300): 0.160,  # row >100-300, Sub-Tr.
    (Climate.SUBTROPICAL, DepthBand.M300_PLUS): 0.200,  # row >300, Sub-Trop.
    (Climate.TEMPERATE, DepthBand.M0_30): 0.200,  # row 0-30, Temperate
    (Climate.TEMPERATE, DepthBand.M30_100): 0.170,  # row >30-100, Temperate
    (Climate.TEMPERATE, DepthBand.M100_300): 0.190,  # row >100-300, Temperate
    (Climate.TEMPERATE, DepthBand.M300_PLUS): 0.220,  # row >300, Temperate
    (Climate.ARCTIC, DepthBand.M0_30): 0.250,  # row 0-30, Arctic
    (Climate.ARCTIC, DepthBand.M30_100): 0.200,  # row >30-100, Arctic
    (Climate.ARCTIC, DepthBand.M100_300): 0.220,  # row >100-300, Arctic
    (Climate.ARCTIC, DepthBand.M300_PLUS): 0.220,  # row >300, Arctic
}

# Table 10-1 (= A-1 = 8-1): recommended FINAL design current densities (A/m2).
_FINAL_CURRENT_DENSITY: Final[dict[tuple[Climate, DepthBand], float]] = {
    (Climate.TROPICAL, DepthBand.M0_30): 0.100,  # row 0-30, Tropical final
    (Climate.TROPICAL, DepthBand.M30_100): 0.080,  # row >30-100, Tropical
    (Climate.TROPICAL, DepthBand.M100_300): 0.090,  # row >100-300, Tropical
    (Climate.TROPICAL, DepthBand.M300_PLUS): 0.130,  # row >300, Tropical
    (Climate.SUBTROPICAL, DepthBand.M0_30): 0.110,  # row 0-30, Sub-Tropical
    (Climate.SUBTROPICAL, DepthBand.M30_100): 0.090,  # row >30-100, Sub-Trop.
    (Climate.SUBTROPICAL, DepthBand.M100_300): 0.110,  # row >100-300, Sub-Tr.
    (Climate.SUBTROPICAL, DepthBand.M300_PLUS): 0.150,  # row >300, Sub-Trop.
    (Climate.TEMPERATE, DepthBand.M0_30): 0.130,  # row 0-30, Temperate
    (Climate.TEMPERATE, DepthBand.M30_100): 0.110,  # row >30-100, Temperate
    (Climate.TEMPERATE, DepthBand.M100_300): 0.140,  # row >100-300, Temperate
    (Climate.TEMPERATE, DepthBand.M300_PLUS): 0.170,  # row >300, Temperate
    (Climate.ARCTIC, DepthBand.M0_30): 0.170,  # row 0-30, Arctic
    (Climate.ARCTIC, DepthBand.M30_100): 0.130,  # row >30-100, Arctic
    (Climate.ARCTIC, DepthBand.M100_300): 0.170,  # row >100-300, Arctic
    (Climate.ARCTIC, DepthBand.M300_PLUS): 0.170,  # row >300, Arctic
}

# Table 10-2 (= A-2 = 8-2): recommended MEAN design current densities (A/m2)
# for seawater-exposed bare metal surfaces.
_MEAN_CURRENT_DENSITY: Final[dict[tuple[Climate, DepthBand], float]] = {
    (Climate.TROPICAL, DepthBand.M0_30): 0.070,  # row 0-30, Tropical
    (Climate.TROPICAL, DepthBand.M30_100): 0.060,  # row >30-100, Tropical
    (Climate.TROPICAL, DepthBand.M100_300): 0.070,  # row >100-300, Tropical
    (Climate.TROPICAL, DepthBand.M300_PLUS): 0.090,  # row >300, Tropical
    (Climate.SUBTROPICAL, DepthBand.M0_30): 0.080,  # row 0-30, Sub-Tropical
    (Climate.SUBTROPICAL, DepthBand.M30_100): 0.070,  # row >30-100, Sub-Trop.
    (Climate.SUBTROPICAL, DepthBand.M100_300): 0.080,  # row >100-300, Sub-Tr.
    (Climate.SUBTROPICAL, DepthBand.M300_PLUS): 0.100,  # row >300, Sub-Trop.
    (Climate.TEMPERATE, DepthBand.M0_30): 0.100,  # row 0-30, Temperate
    (Climate.TEMPERATE, DepthBand.M30_100): 0.080,  # row >30-100, Temperate
    (Climate.TEMPERATE, DepthBand.M100_300): 0.090,  # row >100-300, Temperate
    (Climate.TEMPERATE, DepthBand.M300_PLUS): 0.110,  # row >300, Temperate
    (Climate.ARCTIC, DepthBand.M0_30): 0.120,  # row 0-30, Arctic
    (Climate.ARCTIC, DepthBand.M30_100): 0.100,  # row >30-100, Arctic
    (Climate.ARCTIC, DepthBand.M100_300): 0.110,  # row >100-300, Arctic
    (Climate.ARCTIC, DepthBand.M300_PLUS): 0.110,  # row >300, Arctic
}

# (table number within the edition's numbering, values) per phase.
_CURRENT_DENSITY_BY_PHASE: Final[
    dict[DesignPhase, tuple[int, dict[tuple[Climate, DepthBand], float]]]
] = {
    DesignPhase.INITIAL: (1, _INITIAL_CURRENT_DENSITY),
    DesignPhase.FINAL: (1, _FINAL_CURRENT_DENSITY),
    DesignPhase.MEAN: (2, _MEAN_CURRENT_DENSITY),
}

# Table 10-3 has three depth rows: "0-30", ">30-100", ">100". Both deeper
# bands of Tables 10-1/10-2 fall in its ">100" row.
_TABLE_10_3_ROW: Final[dict[DepthBand, str]] = {
    DepthBand.M0_30: "0-30",
    DepthBand.M30_100: ">30-100",
    DepthBand.M100_300: ">100",
    DepthBand.M300_PLUS: ">100",
}

# Table 10-3 (= A-3 = 8-3): recommended mean design current densities (A/m2)
# for reinforcing steel in concrete, referred to the steel surface area
# (ref. 6.3.12). Keyed by (climate, Table 10-3 row label).
_REINFORCEMENT_CURRENT_DENSITY: Final[dict[tuple[Climate, str], float]] = {
    (Climate.TROPICAL, "0-30"): 0.0025,  # row 0-30, Tropical
    (Climate.TROPICAL, ">30-100"): 0.0020,  # row >30-100, Tropical
    (Climate.TROPICAL, ">100"): 0.0010,  # row >100, Tropical
    (Climate.SUBTROPICAL, "0-30"): 0.0015,  # row 0-30, Sub-Tropical
    (Climate.SUBTROPICAL, ">30-100"): 0.0010,  # row >30-100, Sub-Tropical
    (Climate.SUBTROPICAL, ">100"): 0.0008,  # row >100, Sub-Tropical
    (Climate.TEMPERATE, "0-30"): 0.0010,  # row 0-30, Temperate
    (Climate.TEMPERATE, ">30-100"): 0.0008,  # row >30-100, Temperate
    (Climate.TEMPERATE, ">100"): 0.0006,  # row >100, Temperate
    (Climate.ARCTIC, "0-30"): 0.0008,  # row 0-30, Arctic
    (Climate.ARCTIC, ">30-100"): 0.0006,  # row >30-100, Arctic
    (Climate.ARCTIC, ">100"): 0.0006,  # row >100, Arctic
}

# Table 10-4 (= A-4; 8-4 adds IV): constant a (initial breakdown) per paint
# coating category, from the column headers "I (a = 0.10)", "II (a = 0.05)",
# "III (a = 0.02)" and, in Table 8-4, "IV (a = 0.02)".
_COATING_A: Final[dict[PaintCategory, float]] = {
    PaintCategory.I: 0.10,  # column I header
    PaintCategory.II: 0.05,  # column II header
    PaintCategory.III: 0.02,  # column III header
    PaintCategory.IV: 0.02,  # column IV header (Table 8-4 only)
}

# Table 10-4 has two depth rows for b: "0-30" and ">30".
_TABLE_10_4_ROW: Final[dict[DepthBand, str]] = {
    DepthBand.M0_30: "0-30",
    DepthBand.M30_100: ">30",
    DepthBand.M100_300: ">30",
    DepthBand.M300_PLUS: ">30",
}

# Table 10-4 (= A-4; 8-4 adds IV): constant b (yearly breakdown rate, 1/yr)
# per category and depth row.
_COATING_B: Final[dict[tuple[PaintCategory, str], float]] = {
    (PaintCategory.I, "0-30"): 0.10,  # row 0-30, column I
    (PaintCategory.I, ">30"): 0.05,  # row >30, column I
    (PaintCategory.II, "0-30"): 0.025,  # row 0-30, column II
    (PaintCategory.II, ">30"): 0.015,  # row >30, column II
    (PaintCategory.III, "0-30"): 0.012,  # row 0-30, column III
    (PaintCategory.III, ">30"): 0.008,  # row >30, column III
    (PaintCategory.IV, "0-30"): 0.008,  # row 0-30, column IV (Table 8-4 only)
    (PaintCategory.IV, ">30"): 0.005,  # row >30, column IV (Table 8-4 only)
}

# Table 10-6 (= A-6): design electrochemical capacity (Ah/kg) at seawater
# ambient temperatures (ref. 6.5). Editions 2005, 2010 and 2017.
_ANODE_CAPACITY: Final[dict[tuple[AnodeMaterial, AnodeEnvironment], float]] = {
    (AnodeMaterial.ALUMINIUM, AnodeEnvironment.SEAWATER): 2000.0,  # Al, seawater
    (AnodeMaterial.ALUMINIUM, AnodeEnvironment.SEDIMENT): 1500.0,  # Al, sediments
    (AnodeMaterial.ZINC, AnodeEnvironment.SEAWATER): 780.0,  # Zn, seawater
    (AnodeMaterial.ZINC, AnodeEnvironment.SEDIMENT): 700.0,  # Zn, sediments
}

# Table 10-6 (= A-6): design closed circuit potential (V vs Ag/AgCl/seawater).
_ANODE_CLOSED_CIRCUIT_POTENTIAL: Final[
    dict[tuple[AnodeMaterial, AnodeEnvironment], float]
] = {
    (AnodeMaterial.ALUMINIUM, AnodeEnvironment.SEAWATER): -1.05,  # Al, seawater
    (AnodeMaterial.ALUMINIUM, AnodeEnvironment.SEDIMENT): -0.95,  # Al, sediments
    (AnodeMaterial.ZINC, AnodeEnvironment.SEAWATER): -1.00,  # Zn, seawater
    (AnodeMaterial.ZINC, AnodeEnvironment.SEDIMENT): -0.95,  # Zn, sediments
}

# Table 8-6 (B401 May 2021), identical to DNVGL-RP-F103 (2019) Table 6-3:
# design closed circuit potential (V) and electrochemical capacity (Ah/kg) by
# anode surface temperature. Rows are applied stepwise: the first row whose
# temperature is at or above the requested value is used (no interpolation),
# which is conservative for both capacity and driving voltage. The Zn
# seawater values (-1.030 V, 780 Ah/kg) are printed once across the "<=30"
# and "> 30 to 50" rows and are held for both. Above the last row (Al 80 °C,
# Zn 50 °C, footnote 1) the lookup raises.
_ANODE_ROWS_BY_TEMPERATURE: Final[tuple[AnodeTemperatureRow, ...]] = (
    # Al-Zn-In, seawater exposure: row ≤30 / 60 / 80
    AnodeTemperatureRow(
        AnodeMaterial.ALUMINIUM, AnodeEnvironment.SEAWATER, "≤30", 30.0, -1.050, 2000.0
    ),
    AnodeTemperatureRow(
        AnodeMaterial.ALUMINIUM, AnodeEnvironment.SEAWATER, "60", 60.0, -1.050, 1500.0
    ),
    AnodeTemperatureRow(
        AnodeMaterial.ALUMINIUM, AnodeEnvironment.SEAWATER, "80", 80.0, -1.000, 720.0
    ),
    # Al-Zn-In, sediment exposure: row ≤30 / 60 / 80
    AnodeTemperatureRow(
        AnodeMaterial.ALUMINIUM, AnodeEnvironment.SEDIMENT, "≤30", 30.0, -1.000, 1500.0
    ),
    AnodeTemperatureRow(
        AnodeMaterial.ALUMINIUM, AnodeEnvironment.SEDIMENT, "60", 60.0, -1.000, 680.0
    ),
    AnodeTemperatureRow(
        AnodeMaterial.ALUMINIUM, AnodeEnvironment.SEDIMENT, "80", 80.0, -1.000, 320.0
    ),
    # Zn, seawater exposure: one cell printed across rows ≤30 and > 30 to 50
    AnodeTemperatureRow(
        AnodeMaterial.ZINC, AnodeEnvironment.SEAWATER, "≤30", 30.0, -1.030, 780.0
    ),
    AnodeTemperatureRow(
        AnodeMaterial.ZINC, AnodeEnvironment.SEAWATER, "> 30 to 50", 50.0, -1.030, 780.0
    ),
    # Zn, sediment exposure: row ≤30 / > 30 to 50
    AnodeTemperatureRow(
        AnodeMaterial.ZINC, AnodeEnvironment.SEDIMENT, "≤30", 30.0, -0.980, 750.0
    ),
    AnodeTemperatureRow(
        AnodeMaterial.ZINC, AnodeEnvironment.SEDIMENT, "> 30 to 50", 50.0, -0.980, 580.0
    ),
)

# Table 10-8 (= A-8 = 8-8): recommended anode utilisation factors.
_UTILISATION_FACTOR: Final[dict[AnodeShape, float]] = {
    AnodeShape.LONG_SLENDER_STANDOFF: 0.90,  # "Long slender stand-off L >= 4r"
    AnodeShape.SHORT_SLENDER_STANDOFF: 0.85,  # "Short slender stand-off L < 4r"
    AnodeShape.LONG_FLUSH: 0.85,  # "Long flush mounted L >= 4 width/thickness"
    AnodeShape.SHORT_FLUSH_BRACELET: 0.80,  # "Short flush-mounted, bracelet..."
}

# Design protective potential for carbon and low-alloy steel in seawater
# (V vs Ag/AgCl/seawater): Sec. 5 (2005/2010), [5.4] (2017), [2.4.2] (2021).
_PROTECTION_POTENTIAL_V: Final = -0.80

# Design current density for bare metal surfaces buried in sediments (A/m2),
# applied to the initial, mean and final phases alike: §6.3 (2005/2010),
# [6.3.8] (2017), [3.3.8] (2021).
_BURIED_CURRENT_DENSITY: Final = 0.020

_POTENTIAL_DECIMALS: Final = 3


def citation_label(citation: Citation) -> str:
    """Render a citation as ``"code_id revision section"`` for report provenance.

    The label carries the edition through ``revision`` and the edition's
    table numbering through ``section``; the provenance flag lives in
    ``citation.note``.
    """
    return f"{citation.code_id} {citation.revision} {citation.section}"


def edition_source(edition: Edition | str | None = None) -> EditionSource:
    """Return the source record (revision, wiki page, table prefix) of an edition.

    Parameters
    ----------
    edition : Edition or str or None
        Edition token or alias accepted by ``normalize_edition``. ``None``
        warns and defaults like every other lookup in this module.
    """
    return _SOURCE_BY_EDITION[normalize_edition(edition, stacklevel=3)]


def edition_provenance(edition: Edition | str | None = None) -> str:
    """Return the provenance flag for a B401 edition token.

    Parameters
    ----------
    edition : Edition or str or None
        Edition token or alias accepted by ``normalize_edition``. ``None``
        warns and defaults like every other lookup in this module.

    Returns
    -------
    str
        ``"verified-<edition>-tables"``: every supported edition's tables are
        verified against its own print.
    """
    return edition_source(edition).provenance


def table_label(edition: Edition, number: int) -> str:
    """Section label of design table ``number`` in an edition's numbering.

    ``"Table 10-6"`` for 2005/2010, ``"Table A-6"`` for 2017, ``"Table 8-6"``
    for 2021.
    """
    return f"Table {_SOURCE_BY_EDITION[edition].table_prefix}-{number}"


def _cite(section: str, note: str, edition: Edition) -> Citation:
    """Build a B401 citation for ``edition`` with the provenance in its note."""
    source = _SOURCE_BY_EDITION[edition]
    full_note = (
        f"{note}; edition={edition} ({standard_for_edition(edition)}); "
        f"provenance={source.provenance}"
    )
    return Citation(
        section=section,
        note=full_note,
        revision=source.revision,
        wiki_path=source.wiki_path,
        **_B401_CITATION_TEMPLATE,
    )


def climate_from_temperature(surface_temperature_c: float) -> Climate:
    """Map a surface water temperature to a Table 10-1 climatic region.

    Boundary convention: the Table 10-1 headers read "> 20 °C", "12–20 °C",
    "7–11 °C" and "< 7 °C" (Table 10-2 prints the temperate band as
    7–12 °C). Exactly 20 °C is sub-tropical, exactly 12 °C is sub-tropical
    and exactly 7 °C is temperate; temperatures in the open gap 11–12 °C of
    the Table 10-1 header are treated as temperate, matching Table 10-2.

    Parameters
    ----------
    surface_temperature_c : float
        Surface water temperature [°C].

    Returns
    -------
    Climate
        Climatic region enum member.
    """
    if surface_temperature_c > _TROPICAL_ABOVE_C:
        return Climate.TROPICAL
    if surface_temperature_c >= _SUBTROPICAL_FROM_C:
        return Climate.SUBTROPICAL
    if surface_temperature_c >= _TEMPERATE_FROM_C:
        return Climate.TEMPERATE
    return Climate.ARCTIC


def depth_band(depth_m: float) -> DepthBand:
    """Map a water depth to a Table 10-1 / 10-2 depth band.

    Boundary convention: row labels are "0-30", ">30-100", ">100-300" and
    ">300", so exactly 30 m, 100 m and 300 m belong to the shallower band.

    Parameters
    ----------
    depth_m : float
        Water depth below the surface [m]; must be non-negative.

    Returns
    -------
    DepthBand
        Depth band enum member.

    Raises
    ------
    ValueError
        If ``depth_m`` is negative.
    """
    if depth_m < 0.0:
        raise ValueError(f"depth_m must be non-negative, got {depth_m!r}")
    for band, upper in _DEPTH_BAND_UPPER_M.items():
        if depth_m <= upper:
            return band
    return DepthBand.M300_PLUS


def anode_temperature_row(
    material: AnodeMaterial,
    environment: AnodeEnvironment,
    anode_surface_temperature_c: float,
) -> AnodeTemperatureRow:
    """Select the Table 8-6 (B401 2021) / Table 6-3 (F103 2019) row.

    The first row whose temperature is at or above the requested anode
    surface temperature is returned (stepwise, no interpolation).

    Parameters
    ----------
    material : AnodeMaterial
        Al-Zn-In or Zn anode alloy.
    environment : AnodeEnvironment
        Seawater or sediment exposure.
    anode_surface_temperature_c : float
        Anode surface temperature [°C].

    Returns
    -------
    AnodeTemperatureRow
        Row with its label, upper temperature, potential and capacity.

    Raises
    ------
    ValueError
        Above the last tabulated row (Al 80 °C; Zn 50 °C, footnote 1 asks
        for project-specific qualification).
    """
    mat = AnodeMaterial(material)
    env = AnodeEnvironment(environment)
    rows = [
        row
        for row in _ANODE_ROWS_BY_TEMPERATURE
        if row.material is mat and row.environment is env
    ]
    for row in rows:
        if anode_surface_temperature_c <= row.max_temperature_c:
            return row
    raise ValueError(
        f"No anode design row for {mat.value} in {env.value} at "
        f"{anode_surface_temperature_c} °C: the table ends at "
        f"{rows[-1].max_temperature_c:g} °C (row {rows[-1].row_label!r}); "
        "anode material must be qualified for the project-specific maximum "
        "temperature."
    )


def _ambient_anode_row(
    material: AnodeMaterial,
    environment: AnodeEnvironment,
    anode_surface_temperature_c: float,
    edition: Edition,
) -> tuple[str, float, float]:
    """Row note, potential and capacity for editions with Table 10-6 / A-6."""
    if anode_surface_temperature_c > AMBIENT_ANODE_TEMPERATURE_C:
        raise ValueError(
            f"{standard_for_edition(edition)} {table_label(edition, 6)} tabulates "
            "anode design values at seawater ambient temperature "
            f"(<= {AMBIENT_ANODE_TEMPERATURE_C:g} °C) only; no row for an anode "
            f"surface temperature of {anode_surface_temperature_c} °C. Use "
            "edition '2021' (Table 8-6) for anode surface temperatures above "
            f"{AMBIENT_ANODE_TEMPERATURE_C:g} °C."
        )
    key = (material, environment)
    return (
        f"seawater ambient temperature (<= {AMBIENT_ANODE_TEMPERATURE_C:g} °C)",
        _ANODE_CLOSED_CIRCUIT_POTENTIAL[key],
        _ANODE_CAPACITY[key],
    )


def _anode_row(
    material: AnodeMaterial,
    environment: AnodeEnvironment,
    anode_surface_temperature_c: float,
    edition: Edition,
) -> tuple[str, float, float]:
    """(row note, closed circuit potential, capacity) for an edition."""
    mat = AnodeMaterial(material)
    env = AnodeEnvironment(environment)
    if edition in _ANODE_TEMPERATURE_EDITIONS:
        row = anode_temperature_row(mat, env, anode_surface_temperature_c)
        note = (
            f"anode surface temperature row {row.row_label} °C "
            f"(requested {anode_surface_temperature_c:g} °C)"
        )
        return note, row.closed_circuit_potential_V, row.capacity_Ah_kg
    return _ambient_anode_row(mat, env, anode_surface_temperature_c, edition)


def design_current_density(
    climate: Climate,
    depth_band: DepthBand,
    phase: DesignPhase,
    edition: Edition | None = None,
) -> CitedValue:
    """Design current density for seawater-exposed bare metal surfaces.

    Initial and final values come from Table 10-1 / A-1 / 8-1, mean values
    from Table 10-2 / A-2 / 8-2; the numbers are identical in every edition.

    Parameters
    ----------
    climate : Climate
        Climatic region (see ``climate_from_temperature``).
    depth_band : DepthBand
        Depth band (see ``depth_band``).
    phase : DesignPhase
        Initial, mean or final design phase.
    edition : Edition, optional
        B401 edition token; ``None`` warns and defaults to 2021.

    Returns
    -------
    CitedValue
        Current density in A/m2 with a citation to the source table.
    """
    ed = normalize_edition(edition, stacklevel=3)
    number, values = _CURRENT_DENSITY_BY_PHASE[DesignPhase(phase)]
    value = values[(Climate(climate), DepthBand(depth_band))]
    citation = _cite(
        table_label(ed, number),
        f"{DesignPhase(phase).value} design current density, "
        f"{Climate(climate).value}, {DepthBand(depth_band).value} m, "
        "seawater-exposed bare metal",
        ed,
    )
    return CitedValue(value=value, citation=citation, units=UNITS_CURRENT_DENSITY)


def reinforcement_current_density(
    climate: Climate,
    depth_band: DepthBand,
    edition: Edition | None = None,
) -> CitedValue:
    """Mean design current density for reinforcing steel in concrete.

    Table 10-3 / A-3 / 8-3 gives one (mean) value per climate and depth row,
    referred to the steel reinforcement surface area. Its deepest row is
    ">100", so the ">100-300" and ">300" bands share one value.

    Parameters
    ----------
    climate : Climate
        Climatic region.
    depth_band : DepthBand
        Depth band; bands deeper than 100 m map to the ">100" row.
    edition : Edition, optional
        B401 edition token; ``None`` warns and defaults to 2021.

    Returns
    -------
    CitedValue
        Current density in A/m2 of steel surface, cited to the table.
    """
    ed = normalize_edition(edition, stacklevel=3)
    row = _TABLE_10_3_ROW[DepthBand(depth_band)]
    value = _REINFORCEMENT_CURRENT_DENSITY[(Climate(climate), row)]
    citation = _cite(
        table_label(ed, 3),
        f"mean design current density for reinforcing steel, "
        f"{Climate(climate).value}, row {row} m (steel surface area)",
        ed,
    )
    return CitedValue(value=value, citation=citation, units=UNITS_CURRENT_DENSITY)


def coating_breakdown_constants(
    category: PaintCategory,
    depth_band: DepthBand,
    edition: Edition | None = None,
) -> tuple[CitedValue, CitedValue]:
    """Paint coating breakdown constants ``a`` and ``b`` (Table 10-4 / A-4 / 8-4).

    ``a`` depends on the coating category only; ``b`` depends on category
    and on the depth row ("0-30" or ">30"), so every band deeper than 30 m
    maps to the ">30" row. Category IV exists only in Table 8-4 (2021).

    Parameters
    ----------
    category : PaintCategory
        Paint coating category I, II, III or (2021 only) IV.
    depth_band : DepthBand
        Depth band of the coated surface.
    edition : Edition, optional
        B401 edition token; ``None`` warns and defaults to 2021.

    Returns
    -------
    tuple of CitedValue
        ``(a, b)``: ``a`` dimensionless, ``b`` in 1/yr.

    Raises
    ------
    ValueError
        For category IV under an edition earlier than 2021.
    """
    ed = normalize_edition(edition, stacklevel=3)
    cat = PaintCategory(category)
    if cat is PaintCategory.IV and ed not in _CATEGORY_IV_EDITIONS:
        raise ValueError(
            "Paint coating category IV is defined only in DNV-RP-B401 (May 2021) "
            "Table 8-4 (edition '2021'); "
            f"{standard_for_edition(ed)} {table_label(ed, 4)} has categories "
            "I, II and III only."
        )
    row = _TABLE_10_4_ROW[DepthBand(depth_band)]
    table = table_label(ed, 4)
    a = CitedValue(
        value=_COATING_A[cat],
        citation=_cite(
            table, f"coating breakdown constant a, paint category {cat.value}", ed
        ),
        units=UNITS_DIMENSIONLESS,
    )
    b = CitedValue(
        value=_COATING_B[(cat, row)],
        citation=_cite(
            table,
            f"coating breakdown constant b, paint category {cat.value}, "
            f"row {row} m",
            ed,
        ),
        units=UNITS_PER_YEAR,
    )
    return a, b


def anode_capacity(
    material: AnodeMaterial,
    environment: AnodeEnvironment,
    edition: Edition | None = None,
    anode_surface_temperature_c: float = AMBIENT_ANODE_TEMPERATURE_C,
) -> CitedValue:
    """Design electrochemical capacity (Table 10-6 / A-6 / 8-6).

    Parameters
    ----------
    material : AnodeMaterial
        Aluminium- or zinc-based anode alloy.
    environment : AnodeEnvironment
        Seawater or sediment exposure.
    edition : Edition, optional
        B401 edition token; ``None`` warns and defaults to 2021.
    anode_surface_temperature_c : float, optional
        Anode surface temperature [°C], default 30 (seawater ambient). Only
        the 2021 edition (Table 8-6) tabulates values above 30 °C; earlier
        editions raise ``ValueError`` for a higher temperature.

    Returns
    -------
    CitedValue
        Capacity in Ah/kg.
    """
    ed = normalize_edition(edition, stacklevel=3)
    key = (AnodeMaterial(material), AnodeEnvironment(environment))
    row_note, _, capacity = _anode_row(key[0], key[1], anode_surface_temperature_c, ed)
    citation = _cite(
        table_label(ed, 6),
        f"design electrochemical capacity, {key[0].value}, {key[1].value}, "
        f"{row_note}",
        ed,
    )
    return CitedValue(value=capacity, citation=citation, units=UNITS_CAPACITY)


def anode_closed_circuit_potential(
    material: AnodeMaterial,
    environment: AnodeEnvironment,
    edition: Edition | None = None,
    anode_surface_temperature_c: float = AMBIENT_ANODE_TEMPERATURE_C,
) -> CitedValue:
    """Design closed circuit anode potential (Table 10-6 / A-6 / 8-6).

    Parameters
    ----------
    material : AnodeMaterial
        Aluminium- or zinc-based anode alloy.
    environment : AnodeEnvironment
        Seawater or sediment exposure.
    edition : Edition, optional
        B401 edition token; ``None`` warns and defaults to 2021.
    anode_surface_temperature_c : float, optional
        Anode surface temperature [°C], default 30 (see ``anode_capacity``).

    Returns
    -------
    CitedValue
        Potential in V vs Ag/AgCl/seawater (negative).
    """
    ed = normalize_edition(edition, stacklevel=3)
    key = (AnodeMaterial(material), AnodeEnvironment(environment))
    row_note, potential, _ = _anode_row(key[0], key[1], anode_surface_temperature_c, ed)
    citation = _cite(
        table_label(ed, 6),
        f"design closed circuit potential, {key[0].value}, {key[1].value}, "
        f"{row_note}",
        ed,
    )
    return CitedValue(value=potential, citation=citation, units=UNITS_POTENTIAL)


def utilisation_factor(shape: AnodeShape, edition: Edition | None = None) -> CitedValue:
    """Anode utilisation factor (Table 10-8 / A-8 / 8-8; identical values).

    Parameters
    ----------
    shape : AnodeShape
        Anode geometry class.
    edition : Edition, optional
        B401 edition token; ``None`` warns and defaults to 2021.

    Returns
    -------
    CitedValue
        Dimensionless utilisation factor.
    """
    ed = normalize_edition(edition, stacklevel=3)
    key = AnodeShape(shape)
    citation = _cite(table_label(ed, 8), f"anode utilisation factor, {key.value}", ed)
    return CitedValue(
        value=_UTILISATION_FACTOR[key], citation=citation, units=UNITS_DIMENSIONLESS
    )


def anode_resistance_citation(edition: Edition | None = None) -> Citation:
    """Citation for the edition-specific anode resistance formula table."""
    ed = normalize_edition(edition, stacklevel=3)
    return _cite(table_label(ed, 7), "anode resistance formula", ed)


def protection_potential(edition: Edition | None = None) -> CitedValue:
    """Design protective potential for carbon steel in seawater.

    Parameters
    ----------
    edition : Edition, optional
        B401 edition token; ``None`` warns and defaults to 2021.

    Returns
    -------
    CitedValue
        -0.80 V vs Ag/AgCl/seawater, cited to the edition's clause (Sec. 5
        in the 2005/2010 print, [5.4] in 2017, [2.4.2] in 2021).
    """
    ed = normalize_edition(edition, stacklevel=3)
    citation = _cite(
        _SOURCE_BY_EDITION[ed].protection_potential_section,
        "design protective potential for carbon and low-alloy steel in seawater",
        ed,
    )
    return CitedValue(
        value=_PROTECTION_POTENTIAL_V, citation=citation, units=UNITS_POTENTIAL
    )


def design_driving_voltage(
    material: AnodeMaterial,
    edition: Edition | None = None,
    environment: AnodeEnvironment = AnodeEnvironment.SEAWATER,
    anode_surface_temperature_c: float = AMBIENT_ANODE_TEMPERATURE_C,
) -> CitedValue:
    """Design driving voltage: protective potential minus anode potential.

    Computed as ``E_c - E_a`` with ``E_c = -0.80 V`` and ``E_a`` the closed
    circuit potential of the edition's anode table, rounded to millivolts.

    Parameters
    ----------
    material : AnodeMaterial
        Aluminium- or zinc-based anode alloy.
    edition : Edition, optional
        B401 edition token; ``None`` warns and defaults to 2021.
    environment : AnodeEnvironment, optional
        Anode exposure; default seawater.
    anode_surface_temperature_c : float, optional
        Anode surface temperature [°C], default 30 (see ``anode_capacity``).

    Returns
    -------
    CitedValue
        Driving voltage in V (0.25 V for Al in seawater; 0.20 V for Zn in
        seawater up to 2017, 0.23 V in 2021).
    """
    ed = normalize_edition(edition, stacklevel=3)
    key = (AnodeMaterial(material), AnodeEnvironment(environment))
    row_note, potential, _ = _anode_row(key[0], key[1], anode_surface_temperature_c, ed)
    value = round(_PROTECTION_POTENTIAL_V - potential, _POTENTIAL_DECIMALS)
    citation = _cite(
        f"{table_label(ed, 6)} with "
        f"{_SOURCE_BY_EDITION[ed].protection_potential_section}",
        f"design driving voltage E_c - E_a, {key[0].value}, {key[1].value}, "
        f"{row_note}",
        ed,
    )
    return CitedValue(value=value, citation=citation, units="V")


def buried_current_density(edition: Edition | None = None) -> CitedValue:
    """Design current density for bare metal surfaces buried in sediments.

    Every edition recommends 0.020 A/m2 for the initial, mean and final
    phases alike, independent of climate and depth.

    Parameters
    ----------
    edition : Edition, optional
        B401 edition token; ``None`` warns and defaults to 2021.

    Returns
    -------
    CitedValue
        Current density in A/m2, cited to the edition's clause (§6.3 in the
        2005/2010 print, [6.3.8] in 2017, [3.3.8] in 2021).
    """
    ed = normalize_edition(edition, stacklevel=3)
    citation = _cite(
        _SOURCE_BY_EDITION[ed].buried_current_density_section,
        "design current density for bare metal buried in sediments, all phases",
        ed,
    )
    return CitedValue(
        value=_BURIED_CURRENT_DENSITY,
        citation=citation,
        units=UNITS_CURRENT_DENSITY,
    )
