"""DNV-RP-F103 design tables as cited, edition-keyed lookups.

Every public lookup returns a :class:`~digitalmodel.citations.CitedValue`
whose :class:`~digitalmodel.citations.Citation` names the exact F103 table
the number comes from. The lookups are pure; callers that need the
fail-closed guarantee call ``validate_citation(result.citation)``.

Source of truth is the wiki page
``wikis/engineering-standards/wiki/standards/dnv-rp-f103.md`` (frontmatter
``revision: "2010"``) and the dataset CSVs under
``wikis/engineering-standards/wiki/datasets/dnv-rp-f103/2010/``:
Table 5-1 (design mean current density), Table A.1 (linepipe coating
constants) and Table A.2 (field-joint coating constants). Edition
``"2010"`` carries provenance ``verified-2010-tables``; ``"2016"``
(DNVGL-RP-F103) has no text in this checkout and returns the same numbers
with provenance ``inherited-2010-unverified`` (issue #2207).

The Annex 1 tables print ``a x100`` and ``b x100``; the dicts below hold
the printed (x100) numbers and the lookups divide by 100, so a test can
reproduce the same float from the CSV cell.

F103 defers to DNV-RP-B401 Table 10-8 for the bracelet anode utilisation
factor; ``bracelet_utilisation_factor`` returns that B401-cited value.
"""

from __future__ import annotations

from dataclasses import replace
from enum import Enum
from typing import Final, NamedTuple

from digitalmodel.cathodic_protection._edition import (
    Edition,
    F103Edition,
    f103_standard_for_edition,
    normalize_f103_edition,
)
from digitalmodel.cathodic_protection.b401_tables import (
    UNITS_CURRENT_DENSITY,
    UNITS_DIMENSIONLESS,
    UNITS_PER_YEAR,
    AnodeShape,
    utilisation_factor,
)
from digitalmodel.citations import Citation, CitedValue


F103_WIKI_PATH: Final = "wikis/engineering-standards/wiki/standards/dnv-rp-f103.md"

_F103_CITATION_TEMPLATE: Final[dict[str, str]] = {
    "code_id": "dnv-rp-f103",
    "publisher": "DNV",
    "revision": "2010",
    "wiki_path": F103_WIKI_PATH,
}

PROVENANCE_VERIFIED: Final = "verified-2010-tables"
PROVENANCE_INHERITED: Final = "inherited-2010-unverified"

_PROVENANCE_BY_EDITION: Final[dict[F103Edition, str]] = {
    "2010": PROVENANCE_VERIFIED,  # October 2010 edition, wiki revision "2010"
    "2016": PROVENANCE_INHERITED,  # no DNVGL-RP-F103 (2016) text in checkout
}

# Companion B401 edition for the bracelet utilisation factor deferral.
_B401_EDITION_FOR_F103: Final[dict[F103Edition, Edition]] = {
    "2010": "2010",  # F103 (2010) references DNV-RP-B401 (2010)
    "2016": "2017",  # DNVGL-RP-F103 (2016) precedes DNVGL-RP-B401 (2017)
}

# Annex 1 tables print a and b multiplied by 100 ("a x100", "b x100").
_ANNEX_1_SCALE: Final = 100.0


class Exposure(str, Enum):
    """Table 5-1 exposure conditions; values are the CSV row labels."""

    NON_BURIED = "Non-Buried"
    BURIED = "Buried"


class FluidTemperatureBand(str, Enum):
    """Table 5-1 internal fluid temperature bands (°C)."""

    LE_50 = "<=50"
    GT_50_80 = ">50-80"
    GT_80_120 = ">80-120"
    GT_120 = ">120"


class LinepipeCoating(str, Enum):
    """Table A.1 linepipe coating systems (DNV-RP-F106 CDS numbering)."""

    GFR_ASPHALT_ENAMEL = "glass_fibre_reinforced_asphalt_enamel"  # CDS No. 5
    GFR_COAL_TAR_ENAMEL = "glass_fibre_reinforced_coal_tar_enamel"  # CDS No. 6
    FBE = "single_or_dual_layer_fbe"  # CDS No. 1
    THREE_LAYER_FBE_PE = "three_layer_fbe_pe"  # CDS No. 2
    THREE_LAYER_FBE_PP = "three_layer_fbe_pp"  # CDS No. 3
    MULTI_LAYER_FBE_PP = "multi_layer_fbe_pp"  # CDS No. 4
    POLYCHLOROPRENE = "polychloroprene"  # CDS No. 7

    @property
    def cds_number(self) -> int:
        """DNV-RP-F106 coating data sheet number (Table A.1 column 2)."""
        return _TABLE_A1[self].cds_number

    @property
    def max_temperature_c(self) -> float:
        """Indicative maximum continuous operating temperature (Table A.1)."""
        return _TABLE_A1[self].max_temperature_c


class FieldJointCoating(str, Enum):
    """Table A.2 field-joint coating systems (DNV-RP-F102 FJC numbering)."""

    NONE = "none"
    FJC_1A_TAPE_OR_HSS_MASTIC = "1A"
    FJC_2A_HSS_PE = "2A"
    FJC_2B_HSS_PP = "2B"
    FJC_3A_FBE = "3A"
    FJC_3B_FBE_PE_HSS = "3B"
    FJC_3C_FBE_PP_HSS = "3C"
    FJC_3D_FBE_PP = "3D"
    FJC_4A_POLYCHLOROPRENE = "4A"

    @property
    def max_temperature_c(self) -> float | None:
        """Indicative maximum temperature (Table A.2); ``None`` when blank."""
        return _TABLE_A2[self].max_temperature_c


class LinepipeCoatingRow(NamedTuple):
    """One row of Table A.1 as printed (a and b multiplied by 100)."""

    cds_number: int
    concrete_weight_coating: bool
    max_temperature_c: float
    a_x100: float
    b_x100: float


class FieldJointCoatingRow(NamedTuple):
    """One row of Table A.2 as printed (a and b multiplied by 100)."""

    infill: str
    max_temperature_c: float | None
    compatible_linepipe: str
    a_x100: float
    b_x100: float


# Table 5-1 fluid temperature limits from the column headers (°C).
_TEMPERATURE_BAND_UPPER_C: Final[dict[FluidTemperatureBand, float]] = {
    FluidTemperatureBand.LE_50: 50.0,  # column "<= 50"
    FluidTemperatureBand.GT_50_80: 80.0,  # column ">50 - 80"
    FluidTemperatureBand.GT_80_120: 120.0,  # column ">80 - 120"
}

# Table 5-1: recommended design mean current densities (A/m2) as a function
# of exposure condition and internal fluid temperature.
_MEAN_CURRENT_DENSITY: Final[dict[tuple[Exposure, FluidTemperatureBand], float]] = {
    (Exposure.NON_BURIED, FluidTemperatureBand.LE_50): 0.050,  # Non-Buried,<=50
    (Exposure.NON_BURIED, FluidTemperatureBand.GT_50_80): 0.060,  # >50-80
    (Exposure.NON_BURIED, FluidTemperatureBand.GT_80_120): 0.070,  # >80-120
    (Exposure.NON_BURIED, FluidTemperatureBand.GT_120): 0.100,  # >120
    (Exposure.BURIED, FluidTemperatureBand.LE_50): 0.020,  # Buried, <=50
    (Exposure.BURIED, FluidTemperatureBand.GT_50_80): 0.025,  # Buried, >50-80
    (Exposure.BURIED, FluidTemperatureBand.GT_80_120): 0.030,  # Buried, >80-120
    (Exposure.BURIED, FluidTemperatureBand.GT_120): 0.040,  # Buried, >120
}

# Table A.1: linepipe coating constants a x100 and b x100, with the CDS
# number, concrete-weight-coating flag and indicative max temperature.
_TABLE_A1: Final[dict[LinepipeCoating, LinepipeCoatingRow]] = {
    # row "Glass Fibre Reinforced Asphalt Enamel", No. 5, yes, 70, 0.3, 0.01
    LinepipeCoating.GFR_ASPHALT_ENAMEL: LinepipeCoatingRow(5, True, 70.0, 0.3, 0.01),
    # row "Glass Fibre Reinforced Coal Tar Enamel", No. 6, yes, 80, 0.3, 0.01
    LinepipeCoating.GFR_COAL_TAR_ENAMEL: LinepipeCoatingRow(6, True, 80.0, 0.3, 0.01),
    # row "Single or Dual Layer FBE", No. 1, yes, 90, 1, 0.03
    LinepipeCoating.FBE: LinepipeCoatingRow(1, True, 90.0, 1.0, 0.03),
    # row "3-layer FBE/PE", No. 2, yes, 80, 0.1, 0.003
    LinepipeCoating.THREE_LAYER_FBE_PE: LinepipeCoatingRow(2, True, 80.0, 0.1, 0.003),
    # row "3-layer FBE/PP", No. 3, no, 110, 0.1, 0.003
    LinepipeCoating.THREE_LAYER_FBE_PP: LinepipeCoatingRow(3, False, 110.0, 0.1, 0.003),
    # row "Multi-Layer FBE/PP", No. 4, no, 140, 0.03, 0.001
    LinepipeCoating.MULTI_LAYER_FBE_PP: LinepipeCoatingRow(
        4, False, 140.0, 0.03, 0.001
    ),
    # row "Polychloroprene", No. 7, no, 90, 0.1, 0.01
    LinepipeCoating.POLYCHLOROPRENE: LinepipeCoatingRow(7, False, 90.0, 0.1, 0.01),
}

# Table A.2: field-joint coating constants a x100 and b x100, with infill
# type, indicative max temperature and compatible linepipe coating examples.
_TABLE_A2: Final[dict[FieldJointCoating, FieldJointCoatingRow]] = {
    # row "None", infill II (PU), III (Concrete) or IV (PP), -, 30, 3
    FieldJointCoating.NONE: FieldJointCoatingRow(
        "II (PU), III (Concrete) or IV (PP)",
        None,
        "CDS no 1, 2, 5, 6 with concrete, CDS no. 4",
        30.0,
        3.0,
    ),
    # row "1A Adhesive Tape or Heat Shrink Sleeve (PVC/PE backing) with
    # mastic adhesive", I (Mastic), II or III, 70, 10, 1
    FieldJointCoating.FJC_1A_TAPE_OR_HSS_MASTIC: FieldJointCoatingRow(
        "I (Mastic), II or III", 70.0, "CDS no 5 and 6 with concrete", 10.0, 1.0
    ),
    # row "2A Heat Shrink Sleeve (Backing + adhesive in PE, LE primer)",
    # II or III, 70, 3, 0.3
    FieldJointCoating.FJC_2A_HSS_PE: FieldJointCoatingRow(
        "II or III", 70.0, "CDS no. 2 with concrete", 3.0, 0.3
    ),
    # row "2B Heat Shrink Sleeve (Backing + adhesive in PP, LE primer)",
    # none, 110, 3, 0.3
    FieldJointCoating.FJC_2B_HSS_PP: FieldJointCoatingRow(
        "none", 110.0, "CDS no. 3", 3.0, 0.3
    ),
    # row "3A FBE", none, 90, 3, 0.3
    FieldJointCoating.FJC_3A_FBE: FieldJointCoatingRow(
        "none", 90.0, "CDS no. 1", 3.0, 0.3
    ),
    # row "3B FBE with PE Heat Shrink Sleeve", II or III, 80, 1, 0.03
    FieldJointCoating.FJC_3B_FBE_PE_HSS: FieldJointCoatingRow(
        "II or III", 80.0, "CDS no. 2 with concrete", 1.0, 0.03
    ),
    # row "3C FBE with PP Heat Shrink Sleeve", None or II, 140, 1, 0.03
    FieldJointCoating.FJC_3C_FBE_PP_HSS: FieldJointCoatingRow(
        "None or II", 140.0, "CDS no. 3", 1.0, 0.03
    ),
    # row "3D FBE, PP adhesive and PP (wrapped, extruded or flame sprayed)",
    # None or II, 140, 1, 0.03
    FieldJointCoating.FJC_3D_FBE_PP: FieldJointCoatingRow(
        "None or II", 140.0, "CDS no. 3 or 4", 1.0, 0.03
    ),
    # row "4A Polychloroprene", none, 90, 1, 0.03
    FieldJointCoating.FJC_4A_POLYCHLOROPRENE: FieldJointCoatingRow(
        "none", 90.0, "CDS no. 7", 1.0, 0.03
    ),
}


def edition_provenance(edition: F103Edition | str | None = None) -> str:
    """Return the provenance flag for an F103 edition token.

    Parameters
    ----------
    edition : F103Edition or str or None
        Edition token or alias accepted by ``normalize_f103_edition``;
        ``None`` warns and defaults to 2010.

    Returns
    -------
    str
        ``"verified-2010-tables"`` for 2010, ``"inherited-2010-unverified"``
        for 2016.
    """
    return _PROVENANCE_BY_EDITION[normalize_f103_edition(edition, stacklevel=3)]


def _cite(section: str, note: str, edition: F103Edition) -> Citation:
    """Build an F103 citation carrying the edition and provenance in its note."""
    full_note = (
        f"{note}; edition={edition} ({f103_standard_for_edition(edition)}); "
        f"provenance={_PROVENANCE_BY_EDITION[edition]}"
    )
    return Citation(section=section, note=full_note, **_F103_CITATION_TEMPLATE)


def fluid_temperature_band(fluid_temp_c: float) -> FluidTemperatureBand:
    """Map an internal fluid temperature to a Table 5-1 column.

    Boundary convention follows the headers "<= 50", ">50 - 80", ">80 - 120"
    and ">120": exactly 50, 80 and 120 °C belong to the cooler band.

    Parameters
    ----------
    fluid_temp_c : float
        Internal fluid temperature [°C].

    Returns
    -------
    FluidTemperatureBand
        Temperature band enum member.
    """
    for band, upper in _TEMPERATURE_BAND_UPPER_C.items():
        if fluid_temp_c <= upper:
            return band
    return FluidTemperatureBand.GT_120


def mean_current_density(
    exposure: Exposure,
    fluid_temp_c: float,
    edition: F103Edition | None = None,
) -> CitedValue:
    """Design mean current density from Table 5-1.

    Parameters
    ----------
    exposure : Exposure
        Non-buried or buried pipeline section.
    fluid_temp_c : float
        Internal fluid temperature [°C] (see ``fluid_temperature_band``).
    edition : F103Edition, optional
        F103 edition token; ``None`` warns and defaults to 2010.

    Returns
    -------
    CitedValue
        Mean current density in A/m2, cited to Table 5-1.
    """
    ed = normalize_f103_edition(edition, stacklevel=3)
    band = fluid_temperature_band(fluid_temp_c)
    key = (Exposure(exposure), band)
    citation = _cite(
        "Table 5-1",
        f"design mean current density, {key[0].value}, fluid temperature "
        f"{band.value} °C",
        ed,
    )
    return CitedValue(
        value=_MEAN_CURRENT_DENSITY[key],
        citation=citation,
        units=UNITS_CURRENT_DENSITY,
    )


def _scaled_pair(
    a_x100: float,
    b_x100: float,
    table: str,
    label: str,
    edition: F103Edition,
) -> tuple[CitedValue, CitedValue]:
    """Divide printed x100 constants by 100 and wrap them as cited values."""
    a = CitedValue(
        value=a_x100 / _ANNEX_1_SCALE,
        citation=_cite(
            table, f"coating breakdown constant a (printed x100), {label}", edition
        ),
        units=UNITS_DIMENSIONLESS,
    )
    b = CitedValue(
        value=b_x100 / _ANNEX_1_SCALE,
        citation=_cite(
            table, f"coating breakdown constant b (printed x100), {label}", edition
        ),
        units=UNITS_PER_YEAR,
    )
    return a, b


def linepipe_coating_constants(
    coating: LinepipeCoating, edition: F103Edition | None = None
) -> tuple[CitedValue, CitedValue]:
    """Linepipe coating breakdown constants ``a`` and ``b`` from Table A.1.

    Parameters
    ----------
    coating : LinepipeCoating
        Linepipe coating system.
    edition : F103Edition, optional
        F103 edition token; ``None`` warns and defaults to 2010.

    Returns
    -------
    tuple of CitedValue
        ``(a, b)`` already divided by 100: ``a`` dimensionless, ``b`` in 1/yr.
    """
    ed = normalize_f103_edition(edition, stacklevel=3)
    key = LinepipeCoating(coating)
    row = _TABLE_A1[key]
    return _scaled_pair(
        row.a_x100,
        row.b_x100,
        "Table A.1",
        f"{key.value} (DNV-RP-F106 CDS No. {row.cds_number})",
        ed,
    )


def field_joint_coating_constants(
    fjc: FieldJointCoating, edition: F103Edition | None = None
) -> tuple[CitedValue, CitedValue]:
    """Field-joint coating breakdown constants ``a`` and ``b`` from Table A.2.

    Parameters
    ----------
    fjc : FieldJointCoating
        Field-joint coating system (DNV-RP-F102 numbering).
    edition : F103Edition, optional
        F103 edition token; ``None`` warns and defaults to 2010.

    Returns
    -------
    tuple of CitedValue
        ``(a, b)`` already divided by 100: ``a`` dimensionless, ``b`` in 1/yr.
    """
    ed = normalize_f103_edition(edition, stacklevel=3)
    key = FieldJointCoating(fjc)
    row = _TABLE_A2[key]
    return _scaled_pair(
        row.a_x100,
        row.b_x100,
        "Table A.2",
        f"FJC {key.value} (infill {row.infill})",
        ed,
    )


def bracelet_utilisation_factor(edition: F103Edition | None = None) -> CitedValue:
    """Bracelet anode utilisation factor, deferred to DNV-RP-B401 Table 10-8.

    DNV-RP-F103 does not tabulate a utilisation factor of its own; it refers
    bracelet anodes to the DNV-RP-B401 row "Short flush-mounted, bracelet and
    other types" (0.80). The returned citation is therefore a B401 citation
    whose note records the F103 edition that deferred to it.

    Parameters
    ----------
    edition : F103Edition, optional
        F103 edition token; ``None`` warns and defaults to 2010.

    Returns
    -------
    CitedValue
        Dimensionless utilisation factor cited to B401 Table 10-8.
    """
    ed = normalize_f103_edition(edition, stacklevel=3)
    b401 = utilisation_factor(
        AnodeShape.SHORT_FLUSH_BRACELET, edition=_B401_EDITION_FOR_F103[ed]
    )
    note = (
        f"{f103_standard_for_edition(ed)} defers to DNV-RP-B401 Table 10-8 "
        f"for the bracelet anode utilisation factor; f103_edition={ed}; "
        f"f103_provenance={_PROVENANCE_BY_EDITION[ed]}; "
        f"b401: {b401.citation.note}"
    )
    return replace(b401, citation=replace(b401.citation, note=note))
