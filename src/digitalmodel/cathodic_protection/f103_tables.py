"""DNV-RP-F103 design tables as cited, edition-keyed lookups.

Every public lookup returns a :class:`~digitalmodel.citations.CitedValue`
whose :class:`~digitalmodel.citations.Citation` names the exact F103 table
the number comes from. The lookups are pure; callers that need the
fail-closed guarantee call ``validate_citation(result.citation)``.

Editions and their sources (issue #2208):

``"2010"`` (pass explicitly to reproduce results from before the 2019 default)
    DNV-RP-F103 October 2010. Wiki page ``standards/dnv-rp-f103.md``
    (revision ``"2010"``), dataset CSVs ``datasets/dnv-rp-f103/2010/``:
    Table 5-1 (design mean current density, four fluid-temperature bands),
    Table A.1 (linepipe coating constants) and Table A.2 (field-joint
    coating constants). The Annex 1 tables print ``a x100`` and ``b x100``;
    the dicts hold the printed numbers and the lookups divide by 100. Anode
    design values and the bracelet utilisation factor defer to DNV-RP-B401
    (2010) Tables 10-6 and 10-8.
``"2019"`` (default; alias ``"2021"`` for the May 2021 amended print, same tables;
``"2016"`` also maps here with a warning since the 2016 print is not on file)
    DNVGL-RP-F103 September 2019, a republication of the July 2016 edition.
    Wiki page ``standards/dnv-rp-f103-2019.md`` (revision ``"2019-09"``).
    Table 6-2 (five fluid-temperature bands), Table 6-3 (anode design values
    by anode surface temperature, identical to DNV-RP-B401 2021 Table 8-6),
    Table A-1 (linepipe coatings, ``a`` and ``b`` printed directly, FBE split
    by concrete weight coating) and Table A-2 (field-joint coatings with the
    DNVGL-RP-F102 (2011) system ids). Bracelet utilisation 0.80 is [6.4.2];
    the design protective potential -0.80 V for CMn steel is [6.7.11].

Both editions are verified against the text layer of the licensed PDFs
(2026-09-26); ``edition_provenance`` returns ``verified-<edition>-tables``.
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
from digitalmodel.cathodic_protection import b401_tables as _b401
from digitalmodel.cathodic_protection.b401_tables import (
    AMBIENT_ANODE_TEMPERATURE_C,
    UNITS_CAPACITY,
    UNITS_CURRENT_DENSITY,
    UNITS_DIMENSIONLESS,
    UNITS_PER_YEAR,
    UNITS_POTENTIAL,
    AnodeEnvironment,
    AnodeMaterial,
    AnodeShape,
    anode_temperature_row,
    utilisation_factor,
)
from digitalmodel.citations import Citation, CitedValue


_WIKI_STANDARDS: Final = "wikis/engineering-standards/wiki/standards/"

#: Wiki page of the October 2010 edition (frontmatter revision "2010").
F103_WIKI_PATH: Final = _WIKI_STANDARDS + "dnv-rp-f103.md"
#: Wiki page of the September 2019 edition (frontmatter revision "2019-09").
F103_WIKI_PATH_2019: Final = _WIKI_STANDARDS + "dnv-rp-f103-2019.md"

_F103_CITATION_TEMPLATE: Final[dict[str, str]] = {
    "code_id": "dnv-rp-f103",
    "publisher": "DNV",
}


class F103EditionSource(NamedTuple):
    """Where an F103 edition's numbers come from and how tables are labelled."""

    revision: str
    wiki_path: str
    provenance: str
    current_density_table: str
    linepipe_table: str
    field_joint_table: str


_SOURCE_BY_EDITION: Final[dict[F103Edition, F103EditionSource]] = {
    "2010": F103EditionSource(
        revision="2010",
        wiki_path=F103_WIKI_PATH,
        provenance="verified-2010-tables",
        current_density_table="Table 5-1",
        linepipe_table="Table A.1",
        field_joint_table="Table A.2",
    ),
    "2019": F103EditionSource(
        revision="2019-09",
        wiki_path=F103_WIKI_PATH_2019,
        provenance="verified-2019-tables",
        current_density_table="Table 6-2",
        linepipe_table="Table A-1",
        field_joint_table="Table A-2",
    ),
}

# Companion B401 edition: F103 (2010) defers to DNV-RP-B401 (2010) for anode
# design values and the bracelet utilisation factor; DNVGL-RP-F103 (2019)
# carries its own Table 6-3 and [6.4.2] and is used alongside DNVGL-RP-B401
# (2017), the contemporaneous DNVGL edition it references.
_B401_EDITION_FOR_F103: Final[dict[F103Edition, Edition]] = {
    "2010": "2010",
    "2019": "2017",
}

#: Editions that tabulate their own anode design values (Table 6-3).
_OWN_ANODE_TABLE_EDITIONS: Final[frozenset[str]] = frozenset({"2019"})

_ANODE_TABLE_2019: Final = "Table 6-3"
_BRACELET_UTILISATION_SECTION_2019: Final = "[6.4.2] (anode utilisation factor)"
_PROTECTION_POTENTIAL_SECTION_2019: Final = "[6.7.11] (design protective potential)"
_BRACELET_UTILISATION_2019: Final = 0.80
_PROTECTION_POTENTIAL_V: Final = -0.80
_POTENTIAL_DECIMALS: Final = 3

# Annex 1 tables of the 2010 edition print a and b multiplied by 100.
_ANNEX_1_SCALE: Final = 100.0


class Exposure(str, Enum):
    """Table 5-1 / 6-2 exposure conditions; values are the 2010 CSV row labels."""

    NON_BURIED = "Non-Buried"
    BURIED = "Buried"


class FluidTemperatureBand(str, Enum):
    """Internal fluid temperature bands (°C) of Table 5-1 (2010) and 6-2 (2019).

    2010 uses ``<=50``, ``>50-80``, ``>80-120``, ``>120``; 2019 splits the
    first band into ``<=25`` and ``>25-50``.
    """

    LE_25 = "<=25"  # 2019 only
    GT_25_50 = ">25-50"  # 2019 only
    LE_50 = "<=50"  # 2010 only
    GT_50_80 = ">50-80"
    GT_80_120 = ">80-120"
    GT_120 = ">120"


class LinepipeCoating(str, Enum):
    """Linepipe coating systems of Table A.1 (2010) and Table A-1 (2019).

    The DNV-RP-F106 coating data sheet (CDS) number differs between the
    editions for the enamels and polychloroprene; ``linepipe_coating_row``
    returns the edition's row with its CDS label.
    """

    GFR_ASPHALT_ENAMEL = "glass_fibre_reinforced_asphalt_enamel"  # 2010 No. 5, 2019 No. 4
    GFR_COAL_TAR_ENAMEL = "glass_fibre_reinforced_coal_tar_enamel"  # 2010 No. 6 only
    FBE = "single_or_dual_layer_fbe"  # No. 1
    THREE_LAYER_FBE_PE = "three_layer_fbe_pe"  # No. 2
    THREE_LAYER_FBE_PP = "three_layer_fbe_pp"  # No. 3
    MULTI_LAYER_FBE_PP = "multi_layer_fbe_pp"  # 2010 No. 4 only
    POLYCHLOROPRENE = "polychloroprene"  # 2010 No. 7, 2019 No. 5
    FBE_PP_THERMAL_INSULATION = "fbe_pp_thermally_insulating"  # 2019 only
    FBE_PU_THERMAL_INSULATION = "fbe_pu_thermally_insulating"  # 2019 only

    @property
    def cds_number(self) -> int:
        """DNV-RP-F106 coating data sheet number of the 2010 Table A.1 row.

        Raises ``ValueError`` for coatings that only the 2019 table lists;
        use ``linepipe_coating_row(coating, "2019")`` for the 2019 CDS label.
        """
        return _table_a1_2010_row(self).cds_number

    @property
    def max_temperature_c(self) -> float:
        """Indicative maximum operating temperature of the 2010 Table A.1 row."""
        return _table_a1_2010_row(self).max_temperature_c


class FieldJointCoating(str, Enum):
    """Field-joint coating systems of the 2010 Table A.2 (DNV-RP-F102 numbering)."""

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


class FieldJointCoating2019(str, Enum):
    """Field-joint coating systems of the 2019 Table A-2.

    Values are the DNVGL-RP-F102 (2011) FJC system ids as printed in the
    September 2019 edition; the May 2021 amended print renames them
    (recorded as ``amended_2021_id`` on each row). Rows that differ only by
    infill share a member when the table prints one ``a``/``b`` pair for
    both; the 3A FBE row has two members because its ``a``/``b`` differ with
    and without the 4E(2) PU infill.
    """

    NONE_4E1_PU = "none+4E(1)"  # bare steel with 4E(1) moulded PU infill
    FJC_1D_2A_MASTIC = "1D/2A"  # 1D tape or 2A(1)/2A(2) HSS with mastic, 4E(2) infill
    FJC_2B1_HSS_PE = "2B(1)"  # PE HSS, LE primer; none or 4E(2) infill
    FJC_2C1_HSS_PP = "2C(1)"  # PP HSS, LE primer; none or 4E(2) infill
    FJC_3A_FBE = "3A"  # FBE, no infill
    FJC_3A_FBE_4E2_INFILL = "3A+4E(2)"  # FBE with 4E(2) moulded PU infill
    FJC_2B2_FBE_PE_HSS = "2B(2)"  # FBE with PE HSS; none or 4E(2) infill
    FJC_5D1_5E_FBE_PE = "5D(1)/5E"  # FBE with PE flame spray (5D(1)) or tape (5E)
    FJC_2C2_FBE_PP_HSS = "2C(2)"  # FBE with PP HSS
    FJC_5ABC1_FBE_PP = "5A/B/C(1)"  # FBE, PP adhesive and PP
    FJC_5C1_PE_ON_FBE = "5C(1)"  # "NA" row: moulded PE on FBE with PE adhesive
    FJC_5C2_PP_ON_FBE = "5C(2)"  # "NA" row: moulded PP on FBE with PP adhesive
    FJC_8A_POLYCHLOROPRENE = "8A"

    @property
    def max_temperature_c(self) -> float:
        """Tentative maximum temperature of the Table A-2 row [°C]."""
        return _TABLE_A2_2019[self].max_temperature_c


class LinepipeCoatingRow(NamedTuple):
    """One row of the 2010 Table A.1 as printed (a and b multiplied by 100)."""

    cds_number: int
    concrete_weight_coating: bool
    max_temperature_c: float
    a_x100: float
    b_x100: float


class LinepipeCoatingRow2019(NamedTuple):
    """One row of the 2019 Table A-1 as printed (a and b direct)."""

    cds_label: str
    concrete_weight_coating: bool
    max_temperature_c: float
    a: float
    b: float


class FieldJointCoatingRow(NamedTuple):
    """One row of the 2010 Table A.2 as printed (a and b multiplied by 100)."""

    infill: str
    max_temperature_c: float | None
    compatible_linepipe: str
    a_x100: float
    b_x100: float


class FieldJointCoatingRow2019(NamedTuple):
    """One row of the 2019 Table A-2 as printed (a and b direct)."""

    fjc_type: str
    infill: str
    max_temperature_c: float
    compatible_linepipe: str
    a: float
    b: float
    amended_2021_id: str


# Fluid temperature bands per edition, in ascending order, with the upper
# limit of each band from the column headers (None = open-ended last band).
_BANDS_BY_EDITION: Final[
    dict[F103Edition, tuple[tuple[FluidTemperatureBand, float | None], ...]]
] = {
    "2010": (  # Table 5-1: "<= 50", ">50 - 80", ">80 - 120", ">120"
        (FluidTemperatureBand.LE_50, 50.0),
        (FluidTemperatureBand.GT_50_80, 80.0),
        (FluidTemperatureBand.GT_80_120, 120.0),
        (FluidTemperatureBand.GT_120, None),
    ),
    "2019": (  # Table 6-2: "<= 25", "> 25 - 50", "> 50 - 80", "> 80 - 120", "> 120"
        (FluidTemperatureBand.LE_25, 25.0),
        (FluidTemperatureBand.GT_25_50, 50.0),
        (FluidTemperatureBand.GT_50_80, 80.0),
        (FluidTemperatureBand.GT_80_120, 120.0),
        (FluidTemperatureBand.GT_120, None),
    ),
}

# Table 5-1 (2010): recommended design mean current densities (A/m2) as a
# function of exposure condition and internal fluid temperature.
_MEAN_CURRENT_DENSITY_2010: Final[
    dict[tuple[Exposure, FluidTemperatureBand], float]
] = {
    (Exposure.NON_BURIED, FluidTemperatureBand.LE_50): 0.050,  # Non-Buried,<=50
    (Exposure.NON_BURIED, FluidTemperatureBand.GT_50_80): 0.060,  # >50-80
    (Exposure.NON_BURIED, FluidTemperatureBand.GT_80_120): 0.070,  # >80-120
    (Exposure.NON_BURIED, FluidTemperatureBand.GT_120): 0.100,  # >120
    (Exposure.BURIED, FluidTemperatureBand.LE_50): 0.020,  # Buried, <=50
    (Exposure.BURIED, FluidTemperatureBand.GT_50_80): 0.025,  # Buried, >50-80
    (Exposure.BURIED, FluidTemperatureBand.GT_80_120): 0.030,  # Buried, >80-120
    (Exposure.BURIED, FluidTemperatureBand.GT_120): 0.040,  # Buried, >120
}

# Table 6-2 (2019): recommended design mean current densities (A/m2) as a
# function of exposure condition and internal operating fluid temperature.
_MEAN_CURRENT_DENSITY_2019: Final[
    dict[tuple[Exposure, FluidTemperatureBand], float]
] = {
    (Exposure.NON_BURIED, FluidTemperatureBand.LE_25): 0.050,  # Non-buried, <=25
    (Exposure.NON_BURIED, FluidTemperatureBand.GT_25_50): 0.060,  # >25-50
    (Exposure.NON_BURIED, FluidTemperatureBand.GT_50_80): 0.075,  # >50-80
    (Exposure.NON_BURIED, FluidTemperatureBand.GT_80_120): 0.100,  # >80-120
    (Exposure.NON_BURIED, FluidTemperatureBand.GT_120): 0.130,  # >120
    (Exposure.BURIED, FluidTemperatureBand.LE_25): 0.020,  # Buried, <=25
    (Exposure.BURIED, FluidTemperatureBand.GT_25_50): 0.030,  # Buried, >25-50
    (Exposure.BURIED, FluidTemperatureBand.GT_50_80): 0.040,  # Buried, >50-80
    (Exposure.BURIED, FluidTemperatureBand.GT_80_120): 0.060,  # Buried, >80-120
    (Exposure.BURIED, FluidTemperatureBand.GT_120): 0.080,  # Buried, >120
}

_MEAN_CURRENT_DENSITY_BY_EDITION: Final[
    dict[F103Edition, dict[tuple[Exposure, FluidTemperatureBand], float]]
] = {
    "2010": _MEAN_CURRENT_DENSITY_2010,
    "2019": _MEAN_CURRENT_DENSITY_2019,
}

# Table A.1 (2010): linepipe coating constants a x100 and b x100, with the CDS
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

# Table A-1 (2019): linepipe coating constants a and b (printed directly),
# keyed by (coating, concrete weight coating). Rows that print one line per
# concrete flag are keyed by that flag; rows split "yes"/"no" carry both.
_TABLE_A1_2019: Final[dict[tuple[LinepipeCoating, bool], LinepipeCoatingRow2019]] = {
    # row "Glass fibre reinforced asphalt enamel", No. 4, yes, 70, 0.01, 0.0003
    (LinepipeCoating.GFR_ASPHALT_ENAMEL, True): LinepipeCoatingRow2019(
        "No. 4", True, 70.0, 0.01, 0.0003
    ),
    # row "FBE", No. 1, 90: yes 0.030, 0.0003 / no 0.030, 0.0010
    (LinepipeCoating.FBE, True): LinepipeCoatingRow2019("No. 1", True, 90.0, 0.030, 0.0003),
    (LinepipeCoating.FBE, False): LinepipeCoatingRow2019(
        "No. 1", False, 90.0, 0.030, 0.0010
    ),
    # row "3-layer FBE/PE", No. 2, 80: yes and no both 0.001, 0.00003
    (LinepipeCoating.THREE_LAYER_FBE_PE, True): LinepipeCoatingRow2019(
        "No. 2", True, 80.0, 0.001, 0.00003
    ),
    (LinepipeCoating.THREE_LAYER_FBE_PE, False): LinepipeCoatingRow2019(
        "No. 2", False, 80.0, 0.001, 0.00003
    ),
    # row "3-layer FBE/PP", No. 3, 110: yes and no both 0.001, 0.00003
    (LinepipeCoating.THREE_LAYER_FBE_PP, True): LinepipeCoatingRow2019(
        "No. 3", True, 110.0, 0.001, 0.00003
    ),
    (LinepipeCoating.THREE_LAYER_FBE_PP, False): LinepipeCoatingRow2019(
        "No. 3", False, 110.0, 0.001, 0.00003
    ),
    # row "FBE/PP thermally insulating coating", No. 3 (innermost 3LPP layer),
    # no, 140, 0.0003, 0.00001
    (LinepipeCoating.FBE_PP_THERMAL_INSULATION, False): LinepipeCoatingRow2019(
        "No. 3 (innermost 3LPP layer)", False, 140.0, 0.0003, 0.00001
    ),
    # row "FBE/PU thermally insulating coating", No. 1 (innermost FBE layer),
    # no, 70, 0.01, 0.003
    (LinepipeCoating.FBE_PU_THERMAL_INSULATION, False): LinepipeCoatingRow2019(
        "No. 1 (innermost FBE layer)", False, 70.0, 0.01, 0.003
    ),
    # row "Polychloroprene", No. 5, no, 90, 0.010, 0.001
    (LinepipeCoating.POLYCHLOROPRENE, False): LinepipeCoatingRow2019(
        "No. 5", False, 90.0, 0.010, 0.001
    ),
}

# Table A.2 (2010): field-joint coating constants a x100 and b x100, with
# infill type, indicative max temperature and compatible linepipe coating
# examples.
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

# Table A-2 (2019): field-joint coating constants a and b (printed directly)
# with the DNVGL-RP-F102 (2011) system ids, infill, tentative max temperature
# and compatibility examples; ``amended_2021_id`` is the FJC system name of
# the May 2021 amended print (editorial rename, same numbers).
_TABLE_A2_2019: Final[dict[FieldJointCoating2019, FieldJointCoatingRow2019]] = {
    # row "none", 4E(1) moulded PU on top bare steel (with primer), 70, 0.30, 0.030
    FieldJointCoating2019.NONE_4E1_PU: FieldJointCoatingRow2019(
        "none",
        "4E(1) moulded PU on top bare steel (with primer)",
        70.0,
        "FBE (CDS 1), 3LPE (CDS 2), AE (CDS 4), all with concrete",
        0.30,
        0.030,
        "none (moulded PU on top bare steel)",
    ),
    # row "1D Adhesive Tape or 2A(1)/2A-(2) HSS (PE/PP backing) with mastic
    # adhesive", 4E(2) moulded PU on top 1D or 2A(1)/2A(2), 70, 0.10, 0.010
    FieldJointCoating2019.FJC_1D_2A_MASTIC: FieldJointCoatingRow2019(
        "1D Adhesive Tape or 2A(1)/2A(2) HSS (PE/PP backing) with mastic adhesive",
        "4E(2) moulded PU on top 1D or 2A(1)/2A(2)",
        70.0,
        "3LPE (CDS 2), AE (CDS 4), all with concrete",
        0.10,
        0.010,
        "12-2 cold applied tape or 14A-2_PE/14A_PP HSS",
    ),
    # row "2B(1) HSS (backing + adhesive in PE with LE primer)", none or 4E(2)
    # moulded PU on top 2B(1), 70, 0.03, 0.003
    FieldJointCoating2019.FJC_2B1_HSS_PE: FieldJointCoatingRow2019(
        "2B(1) HSS (backing + adhesive in PE with LE primer)",
        "none or 4E(2) moulded PU on top 2B(1)",
        70.0,
        "3LPE (CDS 2); 3LPE (CDS 2) with concrete",
        0.03,
        0.003,
        "14B_LE",
    ),
    # row "2C (1) HSS (backing + adhesive in PP, LE primer)", none or 4E(2)
    # moulded PU on top 2B(1) [as printed], 110, 0.03, 0.003
    FieldJointCoating2019.FJC_2C1_HSS_PP: FieldJointCoatingRow2019(
        "2C(1) HSS (backing + adhesive in PP, LE primer)",
        "none or 4E(2) moulded PU on top 2B(1)",
        110.0,
        "3LPP (CDS 3) or FBE (CDS 1); with concrete",
        0.03,
        0.003,
        "14D_LE",
    ),
    # row "3A FBE", none, 90, 0.10, 0.010
    FieldJointCoating2019.FJC_3A_FBE: FieldJointCoatingRow2019(
        "3A FBE",
        "none",
        90.0,
        "FBE (CDS 1), 3LPE (CDS 2) and 3LPP (CDS 3)",
        0.10,
        0.010,
        "17A",
    ),
    # row "3A FBE", 4E(2) moulded PU on top, 90, 0.03, 0.003
    FieldJointCoating2019.FJC_3A_FBE_4E2_INFILL: FieldJointCoatingRow2019(
        "3A FBE",
        "4E(2) moulded PU on top",
        90.0,
        "FBE (CDS 1), 3LPE (CDS 2) and 3LPP (CDS 3) with concrete",
        0.03,
        0.003,
        "17A with moulded PU on top",
    ),
    # row "2B(2) FBE with PE HSS", none or 4E(2) moulded PU on top FBE + PE
    # HSS, 70, 0.01, 0.0003
    FieldJointCoating2019.FJC_2B2_FBE_PE_HSS: FieldJointCoatingRow2019(
        "2B(2) FBE with PE HSS",
        "none or 4E(2) moulded PU on top FBE + PE HSS",
        70.0,
        "FBE (CDS 1) and 3LPE (CDS 2); with concrete",
        0.01,
        0.0003,
        "14B_FBE",
    ),
    # row "5D(1) and 5E FBE with PE applied as flame spraying or tape,
    # respectively", none, 70, 0.01, 0.0003
    FieldJointCoating2019.FJC_5D1_5E_FBE_PE: FieldJointCoatingRow2019(
        "5D(1) and 5E FBE with PE applied as flame spraying or tape, respectively",
        "none",
        70.0,
        "3LPE (CDS 2)",
        0.01,
        0.0003,
        "19D and 19E",
    ),
    # row "2C(2) FBE with PP HSS", none, 140, 0.01, 0.0003
    FieldJointCoating2019.FJC_2C2_FBE_PP_HSS: FieldJointCoatingRow2019(
        "2C(2) FBE with PP HSS",
        "none",
        140.0,
        "3LPP (CDS 3) and FBE/PP thermal insulation coating",
        0.01,
        0.0003,
        "14D_FBE",
    ),
    # row "5A/B/C(1) FBE, PP adhesive and PP (wrapped, flame sprayed or
    # moulded)", none, 140, 0.01, 0.0003
    FieldJointCoating2019.FJC_5ABC1_FBE_PP: FieldJointCoatingRow2019(
        "5A/B/C(1) FBE, PP adhesive and PP (wrapped, flame sprayed or moulded)",
        "none",
        140.0,
        "3LPP (CDS 3) and FBE/PP thermal insulation coating",
        0.01,
        0.0003,
        "19A/B/C",
    ),
    # row "NA", 5C(1) moulded PE on top FBE with PE adhesive, 70, 0.01, 0.0003
    FieldJointCoating2019.FJC_5C1_PE_ON_FBE: FieldJointCoatingRow2019(
        "NA",
        "5C(1) moulded PE on top FBE with PE adhesive",
        70.0,
        "FBE/PU based thermally insulating coating",
        0.01,
        0.0003,
        "19C moulded PP on top of FBE with PP adhesive (as printed)",
    ),
    # row "NA", 5C(2) moulded PP on top FBE with PP adhesive, 140, 0.01, 0.0003
    FieldJointCoating2019.FJC_5C2_PP_ON_FBE: FieldJointCoatingRow2019(
        "NA",
        "5C(2) moulded PP on top FBE with PP adhesive",
        140.0,
        "FBE/PP thermal insulation coating",
        0.01,
        0.0003,
        "19C moulded PP on top of FBE with PP adhesive",
    ),
    # row "8A polychloroprene", none, 90, 0.03, 0.001
    FieldJointCoating2019.FJC_8A_POLYCHLOROPRENE: FieldJointCoatingRow2019(
        "8A polychloroprene",
        "none",
        90.0,
        "polychloroprene (CDS 5)",
        0.03,
        0.001,
        "16A polychloroprene and 16B EPDM coating",
    ),
}

# 2010 field-joint ids accepted under the 2019 edition. Only the bare-steel
# row is unambiguous (same meaning, same a/b); every other 2010 id must be
# re-selected from ``FieldJointCoating2019``.
_FJC_2010_TO_2019: Final[dict[FieldJointCoating, FieldJointCoating2019]] = {
    FieldJointCoating.NONE: FieldJointCoating2019.NONE_4E1_PU,
}


def _table_a1_2010_row(coating: LinepipeCoating) -> LinepipeCoatingRow:
    try:
        return _TABLE_A1[coating]
    except KeyError:
        raise ValueError(
            f"LinepipeCoating.{coating.name} has no row in DNV-RP-F103 (October "
            "2010) Table A.1; it is a DNVGL-RP-F103 (2019) Table A-1 coating. Use "
            "linepipe_coating_row(coating, edition='2019')."
        ) from None


def b401_edition_for_f103(edition: F103Edition | str | None = None) -> Edition:
    """Companion DNV-RP-B401 edition of an F103 edition.

    F103 (2010) defers to DNV-RP-B401 (2010) for anode design values and the
    bracelet utilisation factor. DNVGL-RP-F103 (2019) carries its own
    Table 6-3 and [6.4.2]; its companion is the contemporaneous DNVGL-RP-B401
    (June 2017) it references.
    """
    return _B401_EDITION_FOR_F103[normalize_f103_edition(edition, stacklevel=3)]


def edition_source(edition: F103Edition | str | None = None) -> F103EditionSource:
    """Return the source record (revision, wiki page, table labels) of an edition."""
    return _SOURCE_BY_EDITION[normalize_f103_edition(edition, stacklevel=3)]


def edition_provenance(edition: F103Edition | str | None = None) -> str:
    """Return the provenance flag for an F103 edition token.

    Parameters
    ----------
    edition : F103Edition or str or None
        Edition token or alias accepted by ``normalize_f103_edition``;
        ``None`` warns and defaults to 2019.

    Returns
    -------
    str
        ``"verified-2010-tables"`` or ``"verified-2019-tables"``.
    """
    return edition_source(edition).provenance


def temperature_bands(edition: F103Edition | str | None = None) -> tuple[FluidTemperatureBand, ...]:
    """Fluid temperature bands of the edition's current-density table, ascending."""
    ed = normalize_f103_edition(edition, stacklevel=3)
    return tuple(band for band, _ in _BANDS_BY_EDITION[ed])


def _cite(section: str, note: str, edition: F103Edition) -> Citation:
    """Build an F103 citation for ``edition`` with the provenance in its note."""
    source = _SOURCE_BY_EDITION[edition]
    full_note = (
        f"{note}; edition={edition} ({f103_standard_for_edition(edition)}); "
        f"provenance={source.provenance}"
    )
    return Citation(
        section=section,
        note=full_note,
        revision=source.revision,
        wiki_path=source.wiki_path,
        **_F103_CITATION_TEMPLATE,
    )


def fluid_temperature_band(
    fluid_temp_c: float, edition: F103Edition | None = None
) -> FluidTemperatureBand:
    """Map an internal fluid temperature to a Table 5-1 (2010) / 6-2 (2019) column.

    Boundary convention follows the headers ("<= 50", ">50 - 80", ... in
    2010; "<= 25", "> 25 - 50", ... in 2019): a temperature exactly on a
    limit belongs to the cooler band.

    Parameters
    ----------
    fluid_temp_c : float
        Internal fluid temperature [°C].
    edition : F103Edition, optional
        F103 edition token; ``None`` warns and defaults to 2019.

    Returns
    -------
    FluidTemperatureBand
        Temperature band enum member.
    """
    ed = normalize_f103_edition(edition, stacklevel=3)
    for band, upper in _BANDS_BY_EDITION[ed]:
        if upper is None or fluid_temp_c <= upper:
            return band
    raise AssertionError("unreachable: last band is open-ended")


def mean_current_density(
    exposure: Exposure,
    fluid_temp_c: float,
    edition: F103Edition | None = None,
) -> CitedValue:
    """Design mean current density from Table 5-1 (2010) or Table 6-2 (2019).

    Parameters
    ----------
    exposure : Exposure
        Non-buried or buried pipeline section.
    fluid_temp_c : float
        Internal fluid temperature [°C] (see ``fluid_temperature_band``).
    edition : F103Edition, optional
        F103 edition token; ``None`` warns and defaults to 2019.

    Returns
    -------
    CitedValue
        Mean current density in A/m2, cited to the edition's table.
    """
    ed = normalize_f103_edition(edition, stacklevel=3)
    band = fluid_temperature_band(fluid_temp_c, ed)
    key = (Exposure(exposure), band)
    citation = _cite(
        _SOURCE_BY_EDITION[ed].current_density_table,
        f"design mean current density, {key[0].value}, fluid temperature "
        f"{band.value} °C",
        ed,
    )
    return CitedValue(
        value=_MEAN_CURRENT_DENSITY_BY_EDITION[ed][key],
        citation=citation,
        units=UNITS_CURRENT_DENSITY,
    )


def _pair(
    a: float, b: float, table: str, label: str, edition: F103Edition
) -> tuple[CitedValue, CitedValue]:
    """Wrap breakdown constants as cited values."""
    return (
        CitedValue(
            value=a,
            citation=_cite(table, f"coating breakdown constant a, {label}", edition),
            units=UNITS_DIMENSIONLESS,
        ),
        CitedValue(
            value=b,
            citation=_cite(table, f"coating breakdown constant b, {label}", edition),
            units=UNITS_PER_YEAR,
        ),
    )


def _scaled_pair(
    a_x100: float,
    b_x100: float,
    table: str,
    label: str,
    edition: F103Edition,
) -> tuple[CitedValue, CitedValue]:
    """Divide printed x100 constants by 100 and wrap them as cited values."""
    return _pair(
        a_x100 / _ANNEX_1_SCALE,
        b_x100 / _ANNEX_1_SCALE,
        table,
        f"{label} (printed x100)",
        edition,
    )


def linepipe_coating_row(
    coating: LinepipeCoating,
    edition: F103Edition | None = None,
    concrete_weight_coating: bool = False,
) -> LinepipeCoatingRow | LinepipeCoatingRow2019:
    """Return the edition's Table A.1 / A-1 row for a linepipe coating.

    Parameters
    ----------
    coating : LinepipeCoating
        Linepipe coating system.
    edition : F103Edition, optional
        F103 edition token; ``None`` warns and defaults to 2019.
    concrete_weight_coating : bool, optional
        Whether a concrete weight coating is applied over the linepipe
        coating. Selects the row where the 2019 Table A-1 splits (FBE:
        ``b = 0.0003`` with concrete, ``0.0010`` without). Coatings printed
        with a single concrete flag return that row whatever the argument;
        the 2010 table is not split and ignores the flag.

    Raises
    ------
    ValueError
        For a coating the edition's table does not list.
    """
    ed = normalize_f103_edition(edition, stacklevel=3)
    key = LinepipeCoating(coating)
    if ed == "2010":
        return _table_a1_2010_row(key)
    row = _TABLE_A1_2019.get((key, concrete_weight_coating))
    if row is None:
        row = _TABLE_A1_2019.get((key, not concrete_weight_coating))
    if row is None:
        raise ValueError(
            f"LinepipeCoating.{key.name} is not tabulated in DNVGL-RP-F103 (2019) "
            "Table A-1 (the 2019 table lists asphalt enamel, FBE, 3-layer FBE/PE, "
            "3-layer FBE/PP, FBE/PP and FBE/PU thermally insulating coatings and "
            "polychloroprene). Use edition '2010' for the coal tar enamel and "
            "multi-layer FBE/PP rows."
        )
    return row


def linepipe_coating_constants(
    coating: LinepipeCoating,
    edition: F103Edition | None = None,
    concrete_weight_coating: bool = False,
) -> tuple[CitedValue, CitedValue]:
    """Linepipe coating breakdown constants ``a`` and ``b`` (Table A.1 / A-1).

    Parameters
    ----------
    coating : LinepipeCoating
        Linepipe coating system.
    edition : F103Edition, optional
        F103 edition token; ``None`` warns and defaults to 2019.
    concrete_weight_coating : bool, optional
        Concrete weight coating flag; see ``linepipe_coating_row``. Default
        ``False`` selects the larger ``b`` where the 2019 table splits.

    Returns
    -------
    tuple of CitedValue
        ``(a, b)``: ``a`` dimensionless, ``b`` in 1/yr (the 2010 x100 print
        already divided by 100).
    """
    ed = normalize_f103_edition(edition, stacklevel=3)
    key = LinepipeCoating(coating)
    row = linepipe_coating_row(key, ed, concrete_weight_coating)
    table = _SOURCE_BY_EDITION[ed].linepipe_table
    if isinstance(row, LinepipeCoatingRow):
        return _scaled_pair(
            row.a_x100,
            row.b_x100,
            table,
            f"{key.value} (DNV-RP-F106 CDS No. {row.cds_number})",
            ed,
        )
    return _pair(
        row.a,
        row.b,
        table,
        f"{key.value} (DNVGL-RP-F106 CDS {row.cds_label}, concrete weight "
        f"coating {'yes' if row.concrete_weight_coating else 'no'})",
        ed,
    )


def field_joint_coating_row(
    fjc: FieldJointCoating | FieldJointCoating2019,
    edition: F103Edition | None = None,
) -> tuple[FieldJointCoating | FieldJointCoating2019, FieldJointCoatingRow | FieldJointCoatingRow2019]:
    """Return the edition's Table A.2 / A-2 member and row for a field joint.

    Under the 2019 edition ``FieldJointCoating.NONE`` maps to
    ``FieldJointCoating2019.NONE_4E1_PU`` (bare steel with moulded PU
    infill, same constants); every other 2010 id raises, as does a 2019 id
    under the 2010 edition.
    """
    ed = normalize_f103_edition(edition, stacklevel=3)
    if ed == "2010":
        if isinstance(fjc, FieldJointCoating2019):
            raise ValueError(
                f"FieldJointCoating2019.{fjc.name} is a DNVGL-RP-F103 (2019) "
                "Table A-2 system; DNV-RP-F103 (2010) Table A.2 uses the "
                "FieldJointCoating ids (none, 1A, 2A, 2B, 3A, 3B, 3C, 3D, 4A)."
            )
        key_2010 = FieldJointCoating(fjc)
        return key_2010, _TABLE_A2[key_2010]
    if isinstance(fjc, FieldJointCoating2019):
        return fjc, _TABLE_A2_2019[fjc]
    key_2010 = FieldJointCoating(fjc)
    if key_2010 in _FJC_2010_TO_2019:
        key_2019 = _FJC_2010_TO_2019[key_2010]
        return key_2019, _TABLE_A2_2019[key_2019]
    raise ValueError(
        f"FieldJointCoating.{key_2010.name} is a DNV-RP-F103 (2010) Table A.2 "
        "system; DNVGL-RP-F103 (2019) Table A-2 uses the DNVGL-RP-F102 (2011) "
        "ids of FieldJointCoating2019 (none+4E(1), 1D/2A, 2B(1), 2C(1), 3A, "
        "3A+4E(2), 2B(2), 5D(1)/5E, 2C(2), 5A/B/C(1), 5C(1), 5C(2), 8A)."
    )


def field_joint_coating_constants(
    fjc: FieldJointCoating | FieldJointCoating2019,
    edition: F103Edition | None = None,
) -> tuple[CitedValue, CitedValue]:
    """Field-joint coating breakdown constants ``a`` and ``b`` (Table A.2 / A-2).

    Parameters
    ----------
    fjc : FieldJointCoating or FieldJointCoating2019
        Field-joint coating system (DNV-RP-F102 numbering of the edition;
        see ``field_joint_coating_row`` for the accepted crosswalk).
    edition : F103Edition, optional
        F103 edition token; ``None`` warns and defaults to 2019.

    Returns
    -------
    tuple of CitedValue
        ``(a, b)``: ``a`` dimensionless, ``b`` in 1/yr (the 2010 x100 print
        already divided by 100).
    """
    ed = normalize_f103_edition(edition, stacklevel=3)
    key, row = field_joint_coating_row(fjc, ed)
    table = _SOURCE_BY_EDITION[ed].field_joint_table
    if isinstance(row, FieldJointCoatingRow):
        return _scaled_pair(
            row.a_x100, row.b_x100, table, f"FJC {key.value} (infill {row.infill})", ed
        )
    return _pair(
        row.a,
        row.b,
        table,
        f"FJC {key.value} (DNVGL-RP-F102 (2011) system; infill {row.infill})",
        ed,
    )


def bracelet_utilisation_factor(edition: F103Edition | None = None) -> CitedValue:
    """Bracelet anode utilisation factor (0.80).

    DNV-RP-F103 (2010) does not tabulate a utilisation factor of its own; it
    refers bracelet anodes to the DNV-RP-B401 (2010) Table 10-8 row "Short
    flush-mounted, bracelet and other types", so the 2010 result carries a
    B401 citation whose note records the deferral. DNVGL-RP-F103 (2019)
    states the maximum 0.80 for bracelet anodes in [6.4.2] and is cited
    directly.

    Parameters
    ----------
    edition : F103Edition, optional
        F103 edition token; ``None`` warns and defaults to 2019.

    Returns
    -------
    CitedValue
        Dimensionless utilisation factor.
    """
    ed = normalize_f103_edition(edition, stacklevel=3)
    if ed in _OWN_ANODE_TABLE_EDITIONS:
        citation = _cite(
            _BRACELET_UTILISATION_SECTION_2019,
            "maximum design anode utilisation factor for bracelet anodes "
            "(minimum thickness 50 mm)",
            ed,
        )
        return CitedValue(
            value=_BRACELET_UTILISATION_2019,
            citation=citation,
            units=UNITS_DIMENSIONLESS,
        )
    b401 = utilisation_factor(
        AnodeShape.SHORT_FLUSH_BRACELET, edition=_B401_EDITION_FOR_F103[ed]
    )
    return _deferred_to_b401(b401, ed, b401.citation.section)


def _deferred_to_b401(b401: CitedValue, edition: F103Edition, what: str) -> CitedValue:
    """Re-note a B401 cited value as deferred to by an F103 edition."""
    note = (
        f"{f103_standard_for_edition(edition)} defers to DNV-RP-B401 {what}; "
        f"f103_edition={edition}; "
        f"f103_provenance={_SOURCE_BY_EDITION[edition].provenance}; "
        f"b401: {b401.citation.note}"
    )
    return replace(b401, citation=replace(b401.citation, note=note))


def _check_ambient_for_2010(anode_surface_temperature_c: float) -> None:
    if anode_surface_temperature_c > AMBIENT_ANODE_TEMPERATURE_C:
        raise ValueError(
            "DNV-RP-F103 (October 2010) defers to DNV-RP-B401 (2010) Table 10-6, "
            "which tabulates anode design values at seawater ambient temperature "
            f"(<= {AMBIENT_ANODE_TEMPERATURE_C:g} °C) only; no row for an anode "
            f"surface temperature of {anode_surface_temperature_c} °C. Use "
            "edition '2019' (Table 6-3) for anode surface temperatures above "
            f"{AMBIENT_ANODE_TEMPERATURE_C:g} °C."
        )


def anode_capacity(
    material: AnodeMaterial,
    environment: AnodeEnvironment,
    edition: F103Edition | None = None,
    anode_surface_temperature_c: float = AMBIENT_ANODE_TEMPERATURE_C,
) -> CitedValue:
    """Design electrochemical capacity for a pipeline anode.

    2010 defers to DNV-RP-B401 (2010) Table 10-6 (ambient temperature only);
    2019 uses Table 6-3, keyed by anode surface temperature (stepwise rows,
    see ``b401_tables.anode_temperature_row``).

    Parameters
    ----------
    material : AnodeMaterial
        Al-Zn-In or Zn anode alloy.
    environment : AnodeEnvironment
        Seawater (non-buried) or sediment (buried) exposure.
    edition : F103Edition, optional
        F103 edition token; ``None`` warns and defaults to 2019.
    anode_surface_temperature_c : float, optional
        Anode surface temperature [°C], default 30. F103 (2019) [6.4.4]: the
        ambient seawater temperature for non-buried anodes; for buried
        anodes the internal fluid temperature is a conservative estimate.

    Returns
    -------
    CitedValue
        Capacity in Ah/kg.
    """
    ed = normalize_f103_edition(edition, stacklevel=3)
    mat = AnodeMaterial(material)
    env = AnodeEnvironment(environment)
    if ed in _OWN_ANODE_TABLE_EDITIONS:
        row = anode_temperature_row(mat, env, anode_surface_temperature_c)
        citation = _cite(
            _ANODE_TABLE_2019,
            f"design electrochemical capacity, {mat.value}, {env.value}, anode "
            f"surface temperature row {row.row_label} °C "
            f"(requested {anode_surface_temperature_c:g} °C)",
            ed,
        )
        return CitedValue(value=row.capacity_Ah_kg, citation=citation, units=UNITS_CAPACITY)
    _check_ambient_for_2010(anode_surface_temperature_c)
    b401 = _b401.anode_capacity(mat, env, _B401_EDITION_FOR_F103[ed])
    return _deferred_to_b401(b401, ed, b401.citation.section)


def anode_closed_circuit_potential(
    material: AnodeMaterial,
    environment: AnodeEnvironment,
    edition: F103Edition | None = None,
    anode_surface_temperature_c: float = AMBIENT_ANODE_TEMPERATURE_C,
) -> CitedValue:
    """Design closed circuit anode potential for a pipeline anode.

    Same sources and arguments as ``anode_capacity``.

    Returns
    -------
    CitedValue
        Potential in V vs Ag/AgCl/seawater (negative).
    """
    ed = normalize_f103_edition(edition, stacklevel=3)
    mat = AnodeMaterial(material)
    env = AnodeEnvironment(environment)
    if ed in _OWN_ANODE_TABLE_EDITIONS:
        row = anode_temperature_row(mat, env, anode_surface_temperature_c)
        citation = _cite(
            _ANODE_TABLE_2019,
            f"design closed circuit potential, {mat.value}, {env.value}, anode "
            f"surface temperature row {row.row_label} °C "
            f"(requested {anode_surface_temperature_c:g} °C)",
            ed,
        )
        return CitedValue(
            value=row.closed_circuit_potential_V, citation=citation, units=UNITS_POTENTIAL
        )
    _check_ambient_for_2010(anode_surface_temperature_c)
    b401 = _b401.anode_closed_circuit_potential(mat, env, _B401_EDITION_FOR_F103[ed])
    return _deferred_to_b401(b401, ed, b401.citation.section)


def protection_potential(edition: F103Edition | None = None) -> CitedValue:
    """Design protective potential of CMn steel linepipe (-0.80 V).

    2010 defers to DNV-RP-B401 (2010) Sec. 5; 2019 states -0.80 V for CMn
    steel linepipe in [6.7.11].
    """
    ed = normalize_f103_edition(edition, stacklevel=3)
    if ed in _OWN_ANODE_TABLE_EDITIONS:
        citation = _cite(
            _PROTECTION_POTENTIAL_SECTION_2019,
            "design protective potential E_c for CMn steel linepipe",
            ed,
        )
        return CitedValue(
            value=_PROTECTION_POTENTIAL_V, citation=citation, units=UNITS_POTENTIAL
        )
    b401 = _b401.protection_potential(_B401_EDITION_FOR_F103[ed])
    return _deferred_to_b401(b401, ed, b401.citation.section)


def design_driving_voltage(
    material: AnodeMaterial,
    edition: F103Edition | None = None,
    environment: AnodeEnvironment = AnodeEnvironment.SEAWATER,
    anode_surface_temperature_c: float = AMBIENT_ANODE_TEMPERATURE_C,
) -> CitedValue:
    """Design driving voltage ``E_c - E_a`` for a pipeline anode.

    2010 defers to DNV-RP-B401 (2010) Table 10-6 with Sec. 5; 2019 uses
    Table 6-3 with [6.7.11] (Equation (6)).

    Returns
    -------
    CitedValue
        Driving voltage in V, rounded to millivolts.
    """
    ed = normalize_f103_edition(edition, stacklevel=3)
    mat = AnodeMaterial(material)
    env = AnodeEnvironment(environment)
    if ed in _OWN_ANODE_TABLE_EDITIONS:
        row = anode_temperature_row(mat, env, anode_surface_temperature_c)
        value = round(
            _PROTECTION_POTENTIAL_V - row.closed_circuit_potential_V, _POTENTIAL_DECIMALS
        )
        citation = _cite(
            f"{_ANODE_TABLE_2019} with {_PROTECTION_POTENTIAL_SECTION_2019}",
            f"design driving voltage E_c - E_a, {mat.value}, {env.value}, anode "
            f"surface temperature row {row.row_label} °C "
            f"(requested {anode_surface_temperature_c:g} °C)",
            ed,
        )
        return CitedValue(value=value, citation=citation, units="V")
    _check_ambient_for_2010(anode_surface_temperature_c)
    b401 = _b401.design_driving_voltage(mat, _B401_EDITION_FOR_F103[ed], env)
    return _deferred_to_b401(b401, ed, b401.citation.section)
