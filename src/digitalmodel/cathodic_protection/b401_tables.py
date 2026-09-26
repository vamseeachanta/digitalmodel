"""DNV-RP-B401 design tables as cited, edition-keyed lookups.

Every public lookup returns a :class:`~digitalmodel.citations.CitedValue`
whose :class:`~digitalmodel.citations.Citation` names the exact B401 table
(or clause) the number comes from. The lookups are pure: they do not touch
the wiki. Callers that need the fail-closed guarantee call
``validate_citation(result.citation)`` on the returned value.

Source of truth is the wiki page
``wikis/engineering-standards/wiki/standards/dnv-rp-b401.md`` (frontmatter
``revision: "2011"``, the October 2010 edition printed 2011) and the dataset
CSVs under ``wikis/engineering-standards/wiki/datasets/dnv-rp-b401/
2005-with-2008-amendments/``. Tables 10-1, 10-2, 10-3, 10-4, 10-6 and 10-8
are identical across the 2005/2008 and 2010/2011 editions, so editions
``"2005"`` and ``"2010"`` carry provenance ``verified-2011-tables``. No
2017 or 2021 text is held in this checkout, so those editions return the
same numbers with provenance ``inherited-2011-unverified`` (issue #2207).

``Citation.revision`` is always ``"2011"`` because the resolver fails closed
on a frontmatter mismatch; the requested edition and its provenance are
appended to the citation note.

Table 10-7 (anode resistance formulae) is not a value table and stays in
``dnv_rp_b401.py``. Table 10-5 (compositional limits) is out of scope.
"""

from __future__ import annotations

from enum import Enum
from typing import Final

from digitalmodel.cathodic_protection._edition import (
    Edition,
    normalize_edition,
    standard_for_edition,
)
from digitalmodel.citations import Citation, CitedValue


B401_WIKI_PATH: Final = "wikis/engineering-standards/wiki/standards/dnv-rp-b401.md"

_B401_CITATION_TEMPLATE: Final[dict[str, str]] = {
    "code_id": "dnv-rp-b401",
    "publisher": "DNV",
    "revision": "2011",
    "wiki_path": B401_WIKI_PATH,
}

PROVENANCE_VERIFIED: Final = "verified-2011-tables"
PROVENANCE_INHERITED: Final = "inherited-2011-unverified"

_PROVENANCE_BY_EDITION: Final[dict[Edition, str]] = {
    "2005": PROVENANCE_VERIFIED,  # 2005 w/ 2008 amendments: tables match 2010
    "2010": PROVENANCE_VERIFIED,  # October 2010 edition, wiki revision "2011"
    "2017": PROVENANCE_INHERITED,  # no DNVGL-RP-B401 (2017) text in checkout
    "2021": PROVENANCE_INHERITED,  # no DNV-RP-B401 (2021) text in checkout
}

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
    """Paint coating categories I, II, III (B401 §6.4.6, Table 10-4)."""

    I = "I"  # noqa: E741 - Table 10-4 column label
    II = "II"
    III = "III"


class AnodeMaterial(str, Enum):
    """Galvanic anode material types of Table 10-6."""

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

# Table 10-1: recommended INITIAL design current densities (A/m2) for
# seawater-exposed bare metal surfaces. Columns Tropical / Sub-Tropical /
# Temperate / Arctic; rows are depth bands.
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

# Table 10-1: recommended FINAL design current densities (A/m2).
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

# Table 10-2: recommended MEAN design current densities (A/m2) for
# seawater-exposed bare metal surfaces.
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

_CURRENT_DENSITY_BY_PHASE: Final[
    dict[DesignPhase, tuple[str, dict[tuple[Climate, DepthBand], float]]]
] = {
    DesignPhase.INITIAL: ("Table 10-1", _INITIAL_CURRENT_DENSITY),
    DesignPhase.FINAL: ("Table 10-1", _FINAL_CURRENT_DENSITY),
    DesignPhase.MEAN: ("Table 10-2", _MEAN_CURRENT_DENSITY),
}

# Table 10-3 has three depth rows: "0-30", ">30-100", ">100". Both deeper
# bands of Tables 10-1/10-2 fall in its ">100" row.
_TABLE_10_3_ROW: Final[dict[DepthBand, str]] = {
    DepthBand.M0_30: "0-30",
    DepthBand.M30_100: ">30-100",
    DepthBand.M100_300: ">100",
    DepthBand.M300_PLUS: ">100",
}

# Table 10-3: recommended mean design current densities (A/m2) for
# reinforcing steel in concrete, referred to the steel surface area
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

# Table 10-4: constant a (initial breakdown) per paint coating category,
# from the column headers "I (a = 0.10)", "II (a = 0.05)", "III (a = 0.02)".
_COATING_A: Final[dict[PaintCategory, float]] = {
    PaintCategory.I: 0.10,  # column I header
    PaintCategory.II: 0.05,  # column II header
    PaintCategory.III: 0.02,  # column III header
}

# Table 10-4 has two depth rows for b: "0-30" and ">30".
_TABLE_10_4_ROW: Final[dict[DepthBand, str]] = {
    DepthBand.M0_30: "0-30",
    DepthBand.M30_100: ">30",
    DepthBand.M100_300: ">30",
    DepthBand.M300_PLUS: ">30",
}

# Table 10-4: constant b (yearly breakdown rate, 1/yr) per category and
# Table 10-4 depth row.
_COATING_B: Final[dict[tuple[PaintCategory, str], float]] = {
    (PaintCategory.I, "0-30"): 0.10,  # row 0-30, column I
    (PaintCategory.I, ">30"): 0.05,  # row >30, column I
    (PaintCategory.II, "0-30"): 0.025,  # row 0-30, column II
    (PaintCategory.II, ">30"): 0.015,  # row >30, column II
    (PaintCategory.III, "0-30"): 0.012,  # row 0-30, column III
    (PaintCategory.III, ">30"): 0.008,  # row >30, column III
}

# Table 10-6: design electrochemical capacity (Ah/kg) at seawater ambient
# temperatures (ref. 6.5).
_ANODE_CAPACITY: Final[dict[tuple[AnodeMaterial, AnodeEnvironment], float]] = {
    (AnodeMaterial.ALUMINIUM, AnodeEnvironment.SEAWATER): 2000.0,  # Al, seawater
    (AnodeMaterial.ALUMINIUM, AnodeEnvironment.SEDIMENT): 1500.0,  # Al, sediments
    (AnodeMaterial.ZINC, AnodeEnvironment.SEAWATER): 780.0,  # Zn, seawater
    (AnodeMaterial.ZINC, AnodeEnvironment.SEDIMENT): 700.0,  # Zn, sediments
}

# Table 10-6: design closed circuit potential (V vs Ag/AgCl/seawater).
_ANODE_CLOSED_CIRCUIT_POTENTIAL: Final[
    dict[tuple[AnodeMaterial, AnodeEnvironment], float]
] = {
    (AnodeMaterial.ALUMINIUM, AnodeEnvironment.SEAWATER): -1.05,  # Al, seawater
    (AnodeMaterial.ALUMINIUM, AnodeEnvironment.SEDIMENT): -0.95,  # Al, sediments
    (AnodeMaterial.ZINC, AnodeEnvironment.SEAWATER): -1.00,  # Zn, seawater
    (AnodeMaterial.ZINC, AnodeEnvironment.SEDIMENT): -0.95,  # Zn, sediments
}

# Table 10-8: recommended anode utilisation factors.
_UTILISATION_FACTOR: Final[dict[AnodeShape, float]] = {
    AnodeShape.LONG_SLENDER_STANDOFF: 0.90,  # "Long slender stand-off L >= 4r"
    AnodeShape.SHORT_SLENDER_STANDOFF: 0.85,  # "Short slender stand-off L < 4r"
    AnodeShape.LONG_FLUSH: 0.85,  # "Long flush mounted L >= 4 width/thickness"
    AnodeShape.SHORT_FLUSH_BRACELET: 0.80,  # "Short flush-mounted, bracelet..."
}

# B401 Sec. 5 protective potential for carbon and low-alloy steel in seawater
# (V vs Ag/AgCl/seawater). The wiki page lists structure-to-electrolyte
# potential criteria under Sec. 5; the clause number (5.4 in the 2005/2010
# print) is not reproduced on the wiki page.
_PROTECTION_POTENTIAL_V: Final = -0.80
_PROTECTION_POTENTIAL_SECTION: Final = (
    "Sec. 5 (structure-to-electrolyte potential criteria)"
)

# B401 §6.3 design current density for bare metal surfaces buried in
# sediments (A/m2), applied to the initial, mean and final phases alike.
# §6.3 is the design-current-density clause referenced by the Table 10-1 and
# 10-2 captions ("ref. 6.3").
_BURIED_CURRENT_DENSITY: Final = 0.020
_BURIED_CURRENT_DENSITY_SECTION: Final = "Sec. 6.3 (buried surfaces)"

_POTENTIAL_DECIMALS: Final = 3


def citation_label(citation: Citation) -> str:
    """Render a citation as ``"code_id revision section"`` for report provenance.

    The edition and provenance live in ``citation.note``; the label is kept
    edition-free so per-zone result rows compare equal across editions whose
    tables are identical.
    """
    return f"{citation.code_id} {citation.revision} {citation.section}"


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
        ``"verified-2011-tables"`` for editions 2005 and 2010, whose tables
        are held in the wiki; ``"inherited-2011-unverified"`` for 2017 and
        2021, which reuse the 2010/2011 numbers without a verified source.
    """
    return _PROVENANCE_BY_EDITION[normalize_edition(edition, stacklevel=3)]


def _cite(section: str, note: str, edition: Edition) -> Citation:
    """Build a B401 citation carrying the edition and provenance in its note."""
    full_note = (
        f"{note}; edition={edition} ({standard_for_edition(edition)}); "
        f"provenance={_PROVENANCE_BY_EDITION[edition]}"
    )
    return Citation(section=section, note=full_note, **_B401_CITATION_TEMPLATE)


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


def design_current_density(
    climate: Climate,
    depth_band: DepthBand,
    phase: DesignPhase,
    edition: Edition | None = None,
) -> CitedValue:
    """Design current density for seawater-exposed bare metal surfaces.

    Initial and final values come from Table 10-1, mean values from
    Table 10-2.

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
    table, values = _CURRENT_DENSITY_BY_PHASE[DesignPhase(phase)]
    value = values[(Climate(climate), DepthBand(depth_band))]
    citation = _cite(
        table,
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

    Table 10-3 gives one (mean) value per climate and depth row, referred to
    the steel reinforcement surface area. Its deepest row is ">100", so the
    ">100-300" and ">300" bands share one value.

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
        Current density in A/m2 of steel surface, cited to Table 10-3.
    """
    ed = normalize_edition(edition, stacklevel=3)
    row = _TABLE_10_3_ROW[DepthBand(depth_band)]
    value = _REINFORCEMENT_CURRENT_DENSITY[(Climate(climate), row)]
    citation = _cite(
        "Table 10-3",
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
    """Paint coating breakdown constants ``a`` and ``b`` from Table 10-4.

    ``a`` depends on the coating category only; ``b`` depends on category
    and on the Table 10-4 depth row ("0-30" or ">30"), so every band deeper
    than 30 m maps to the ">30" row.

    Parameters
    ----------
    category : PaintCategory
        Paint coating category I, II or III (B401 §6.4.6).
    depth_band : DepthBand
        Depth band of the coated surface.
    edition : Edition, optional
        B401 edition token; ``None`` warns and defaults to 2021.

    Returns
    -------
    tuple of CitedValue
        ``(a, b)``: ``a`` dimensionless, ``b`` in 1/yr.
    """
    ed = normalize_edition(edition, stacklevel=3)
    cat = PaintCategory(category)
    row = _TABLE_10_4_ROW[DepthBand(depth_band)]
    a = CitedValue(
        value=_COATING_A[cat],
        citation=_cite(
            "Table 10-4",
            f"coating breakdown constant a, paint category {cat.value}",
            ed,
        ),
        units=UNITS_DIMENSIONLESS,
    )
    b = CitedValue(
        value=_COATING_B[(cat, row)],
        citation=_cite(
            "Table 10-4",
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
) -> CitedValue:
    """Design electrochemical capacity from Table 10-6.

    Parameters
    ----------
    material : AnodeMaterial
        Aluminium- or zinc-based anode alloy.
    environment : AnodeEnvironment
        Seawater or sediment exposure.
    edition : Edition, optional
        B401 edition token; ``None`` warns and defaults to 2021.

    Returns
    -------
    CitedValue
        Capacity in Ah/kg at seawater ambient temperature.
    """
    ed = normalize_edition(edition, stacklevel=3)
    key = (AnodeMaterial(material), AnodeEnvironment(environment))
    citation = _cite(
        "Table 10-6",
        f"design electrochemical capacity, {key[0].value}, {key[1].value}",
        ed,
    )
    return CitedValue(
        value=_ANODE_CAPACITY[key], citation=citation, units=UNITS_CAPACITY
    )


def anode_closed_circuit_potential(
    material: AnodeMaterial,
    environment: AnodeEnvironment,
    edition: Edition | None = None,
) -> CitedValue:
    """Design closed circuit anode potential from Table 10-6.

    Parameters
    ----------
    material : AnodeMaterial
        Aluminium- or zinc-based anode alloy.
    environment : AnodeEnvironment
        Seawater or sediment exposure.
    edition : Edition, optional
        B401 edition token; ``None`` warns and defaults to 2021.

    Returns
    -------
    CitedValue
        Potential in V vs Ag/AgCl/seawater (negative).
    """
    ed = normalize_edition(edition, stacklevel=3)
    key = (AnodeMaterial(material), AnodeEnvironment(environment))
    citation = _cite(
        "Table 10-6",
        f"design closed circuit potential, {key[0].value}, {key[1].value}",
        ed,
    )
    return CitedValue(
        value=_ANODE_CLOSED_CIRCUIT_POTENTIAL[key],
        citation=citation,
        units=UNITS_POTENTIAL,
    )


def utilisation_factor(shape: AnodeShape, edition: Edition | None = None) -> CitedValue:
    """Anode utilisation factor from Table 10-8.

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
    citation = _cite("Table 10-8", f"anode utilisation factor, {key.value}", ed)
    return CitedValue(
        value=_UTILISATION_FACTOR[key], citation=citation, units=UNITS_DIMENSIONLESS
    )


def protection_potential(edition: Edition | None = None) -> CitedValue:
    """Design protective potential for carbon steel in seawater.

    Parameters
    ----------
    edition : Edition, optional
        B401 edition token; ``None`` warns and defaults to 2021.

    Returns
    -------
    CitedValue
        -0.80 V vs Ag/AgCl/seawater, cited to B401 Sec. 5.
    """
    ed = normalize_edition(edition, stacklevel=3)
    citation = _cite(
        _PROTECTION_POTENTIAL_SECTION,
        "design protective potential for carbon and low-alloy steel in "
        "seawater; clause 5.4 in the 2005/2010 print, not reproduced on the "
        "wiki page",
        ed,
    )
    return CitedValue(
        value=_PROTECTION_POTENTIAL_V, citation=citation, units=UNITS_POTENTIAL
    )


def design_driving_voltage(
    material: AnodeMaterial,
    edition: Edition | None = None,
    environment: AnodeEnvironment = AnodeEnvironment.SEAWATER,
) -> CitedValue:
    """Design driving voltage: protective potential minus anode potential.

    Computed as ``E_c - E_a`` with ``E_c = -0.80 V`` (Sec. 5) and ``E_a``
    the Table 10-6 closed circuit potential, rounded to millivolts.

    Parameters
    ----------
    material : AnodeMaterial
        Aluminium- or zinc-based anode alloy.
    edition : Edition, optional
        B401 edition token; ``None`` warns and defaults to 2021.
    environment : AnodeEnvironment, optional
        Anode exposure; default seawater.

    Returns
    -------
    CitedValue
        Driving voltage in V (0.25 V for Al, 0.20 V for Zn in seawater).
    """
    ed = normalize_edition(edition, stacklevel=3)
    key = (AnodeMaterial(material), AnodeEnvironment(environment))
    value = round(
        _PROTECTION_POTENTIAL_V - _ANODE_CLOSED_CIRCUIT_POTENTIAL[key],
        _POTENTIAL_DECIMALS,
    )
    citation = _cite(
        f"Table 10-6 with {_PROTECTION_POTENTIAL_SECTION}",
        f"design driving voltage E_c - E_a, {key[0].value}, {key[1].value}",
        ed,
    )
    return CitedValue(value=value, citation=citation, units="V")


def buried_current_density(edition: Edition | None = None) -> CitedValue:
    """Design current density for bare metal surfaces buried in sediments.

    B401 §6.3 recommends 0.020 A/m2 for the initial, mean and final phases
    alike, independent of climate and depth.

    Parameters
    ----------
    edition : Edition, optional
        B401 edition token; ``None`` warns and defaults to 2021.

    Returns
    -------
    CitedValue
        Current density in A/m2.
    """
    ed = normalize_edition(edition, stacklevel=3)
    citation = _cite(
        _BURIED_CURRENT_DENSITY_SECTION,
        "design current density for bare metal buried in sediments, all "
        "phases; §6.3 is the clause the Table 10-1/10-2 captions reference",
        ed,
    )
    return CitedValue(
        value=_BURIED_CURRENT_DENSITY,
        citation=citation,
        units=UNITS_CURRENT_DENSITY,
    )
