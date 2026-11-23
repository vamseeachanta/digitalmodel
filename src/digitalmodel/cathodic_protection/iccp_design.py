"""Impressed current cathodic protection (ICCP) system design.

Covers rectifier sizing (voltage and current), anode ground-bed design
(shallow vertical arrays, deep wells, horizontal columns), anode life by
material, and cable sizing for ICCP systems protecting pipelines, tanks,
and marine structures.

Issue #2247 (owner direction 2026-09-27): the governing standards (NACE
SP0169 / SP0572, ISO 15589-1) are not on file. Anode material data and the
ground-bed resistance formulas are taken from openly published literature
and carried as :class:`~digitalmodel.cathodic_protection._provisional.ProvisionalValue`
records with ``provisional=True``; where a source gives a range both ends
are stored and the conservative end is used. The anode-life figure stays behind
``experimental=True``.

References
----------
- U.S. DoD TSEWG, Electrical Technical Paper 16 (March 2017), impressed
  current anode selection and design (open, public domain) -- graphite,
  HSCI and scrap-steel data; multi-anode, deep-well and horizontal
  ground-bed resistance equations; utilisation factor.
- U.S. Naval Academy EN380 notes, "Appendix: Cathodic Protection Design",
  Table 7.1 (after Swain 1996) -- platinised and magnetite anode data.
- German Cathodic Protection datasheet 04-200-R1 (HSCI anodes).
- Cathodic Protection Co. Ltd datasheet 2.2.1 Rev. 0 (2020) (MMO anodes).
- NACE SP0169 (2013) and API RP 1632 -- context only; not on file, no
  value in this module is taken from them.
"""

from __future__ import annotations

import math
from dataclasses import dataclass, field
from enum import Enum
from typing import Optional

from pydantic import BaseModel, Field

from digitalmodel.cathodic_protection._evidence import iccp_evidence
from digitalmodel.cathodic_protection._experimental import require_experimental
from digitalmodel.cathodic_protection._provisional import ProvisionalValue, render_provisional

# --- Literature sources (author, title, year, URL) ---------------------------

SRC_DOD_TP16 = (
    "U.S. DoD Tri-Service Electrical Working Group (TSEWG), 'Electrical "
    "Technical Paper 16: Impressed Current Anode Material Selection and Design "
    "Considerations (non-mandatory)', March 2017, "
    "https://nibs-s3-wbdg3-production.s3.us-east-1.amazonaws.com/FFC/DOD/STC/tsewg_tp16.pdf"
)
SRC_USNA_EN380 = (
    "U.S. Naval Academy, EN380 course notes, 'Appendix: Cathodic Protection "
    "Design' (after G. Swain class notes, 1996), Table 7.1, n.d., "
    "https://www.usna.edu/NAOE/_files/documents/Courses/EN380/Course_Notes/"
    "zAppendix_A_Cathodic_Protection_Design.pdf"
)
SRC_GCP_HSCI = (
    "German Cathodic Protection (GCP), 'Impressed Current Anodes - Silicon iron "
    "anodes', datasheet 04-200-R1, n.d., "
    "https://www.gcp.de/wp-content/uploads/04-200-Silicon-iron-anodes.pdf"
)
SRC_CPC_MMO = (
    "Cathodic Protection Co. Ltd, 'Datasheet 2.2.1 - Mixed Metal Oxide Tubular "
    "Anodes', Rev. 0, July 2020, "
    "https://www.cathodic.co.uk/wp-content/uploads/"
    "2.2.1-Mixed-Metal-Oxide-Tubular-Anodes-Rev.0-July-2020.pdf"
)

LB_TO_KG = 0.45359237
FT2_TO_M2 = 0.09290304


class AnodeBedType(str, Enum):
    """Anode bed configuration."""

    SHALLOW_HORIZONTAL = "shallow_horizontal"
    SHALLOW_VERTICAL = "shallow_vertical"
    DEEP_WELL = "deep_well"
    DISTRIBUTED = "distributed"


class AnodeMaterial(str, Enum):
    """Impressed current anode materials."""

    HIGH_SILICON_CAST_IRON = "high_silicon_cast_iron"
    MIXED_METAL_OXIDE = "mixed_metal_oxide"
    GRAPHITE = "graphite"
    PLATINIZED_TITANIUM = "platinized_titanium"
    PLATINIZED_NIOBIUM = "platinized_niobium"
    MAGNETITE = "magnetite"
    SCRAP_STEEL = "scrap_steel"


class IccpEnvironment(str, Enum):
    """Electrolyte around the anode (selects the per-environment material data)."""

    SOIL = "soil"
    FRESHWATER = "freshwater"
    SEAWATER = "seawater"


class AnodeLifeBasis(str, Enum):
    """How an anode material wears out."""

    MASS = "mass"  # bulk consumable: life = m*u / (C*I)
    COATING_WEAR = "coating_wear"  # dimensionally stable: life = w*A / (C*I)


_PENDING = "NACE SP0169 / NACE SP0572 / ISO 15589-1 (not on file)"
_PER_FT2 = 1.0 / FT2_TO_M2  # A/ft² -> A/m²


def _pv(value: float, units: str, source: str, note: str) -> ProvisionalValue:
    """A single-valued provisional constant (the source gives no range)."""
    evidence_class, evidence_source = iccp_evidence(source, note)
    return ProvisionalValue(
        value=value, units=units, source=source, note=note, pending_standard=_PENDING,
        evidence_class=evidence_class, evidence_source=evidence_source,
    )


def _pv_range(
    low: float, high: float, conservative_end: str, units: str, source: str, note: str
) -> ProvisionalValue:
    """A ranged constant: both ends stored, the design value is the conservative end.

    Owner decision 2026-09-27: the conservative end is the highest consumption
    rate, the lowest current-density limit, and for any other quantity the end
    that shortens life or lowers capacity.
    """
    evidence_class, evidence_source = iccp_evidence(source, note)
    return ProvisionalValue(
        value=low if conservative_end == "low" else high,
        units=units,
        source=source,
        note=note,
        pending_standard=_PENDING,
        range_low=low,
        range_high=high,
        conservative_end=conservative_end,
        evidence_class=evidence_class, evidence_source=evidence_source,
    )


def _per_env(pv: ProvisionalValue) -> dict[IccpEnvironment, ProvisionalValue]:
    return {env: pv for env in IccpEnvironment}


@dataclass(frozen=True)
class IccpAnodeMaterialRecord:
    """Provisional literature data for one ICCP anode material.

    ``density`` is ``None`` where no open source gives it (the anode mass
    must then be supplied); a missing ``max_current_density`` entry means
    the source gives no limit (the limit must then be supplied).
    """

    material: AnodeMaterial
    life_basis: AnodeLifeBasis
    consumption_rate: dict[IccpEnvironment, ProvisionalValue]
    max_current_density: dict[IccpEnvironment, ProvisionalValue] = field(default_factory=dict)
    density: Optional[ProvisionalValue] = None


ICCP_ANODE_RECORDS: dict[AnodeMaterial, IccpAnodeMaterialRecord] = {
    AnodeMaterial.HIGH_SILICON_CAST_IRON: IccpAnodeMaterialRecord(
        material=AnodeMaterial.HIGH_SILICON_CAST_IRON,
        life_basis=AnodeLifeBasis.MASS,
        density=_pv(7000.0, "kg/m3", SRC_GCP_HSCI, "7.0 g/cm3; TP16 Table 5 also gives SG 7"),
        consumption_rate={
            IccpEnvironment.FRESHWATER: _pv(0.15, "kg/(A*yr)", SRC_GCP_HSCI, "freshwater row"),
            IccpEnvironment.SEAWATER: _pv(0.50, "kg/(A*yr)", SRC_GCP_HSCI, "saltwater row"),
            IccpEnvironment.SOIL: _pv(
                0.30, "kg/(A*yr)", SRC_GCP_HSCI, "soil row; TP16 s.1.2 quotes ~1 lb/A-yr (0.45)"
            ),
        },
        max_current_density={
            IccpEnvironment.FRESHWATER: _pv_range(
                10.0, 30.0, "low", "A/m2", SRC_GCP_HSCI, "freshwater row 10-30"
            ),
            IccpEnvironment.SEAWATER: _pv_range(
                10.0, 50.0, "low", "A/m2", SRC_GCP_HSCI, "saltwater row 10-50"
            ),
            IccpEnvironment.SOIL: _pv_range(
                10.0, 30.0, "low", "A/m2", SRC_GCP_HSCI, "soil row 10-30"
            ),
        },
    ),
    AnodeMaterial.GRAPHITE: IccpAnodeMaterialRecord(
        material=AnodeMaterial.GRAPHITE,
        life_basis=AnodeLifeBasis.MASS,
        # TP-16 Table 1 p2 gives only a maximum density, not nominal mass.
        # Lifetime therefore requires measured anode_mass_kg (issue #2264).
        consumption_rate={
            IccpEnvironment.SOIL: _pv(
                2.5 * LB_TO_KG, "kg/(A*yr)", SRC_DOD_TP16, "s.1.1.4.3: ~2.5 lb/A-yr soil"
            ),
            IccpEnvironment.FRESHWATER: _pv(
                2.5 * LB_TO_KG, "kg/(A*yr)", SRC_DOD_TP16, "s.1.1.4.3: ~2.5 lb/A-yr fresh water"
            ),
            IccpEnvironment.SEAWATER: _pv_range(
                1.6 * LB_TO_KG,
                2.5 * LB_TO_KG,
                "high",
                "kg/(A*yr)",
                SRC_DOD_TP16,
                "s.1.1.4.3: 1.6-2.5 lb/A-yr in seawater",
            ),
        },
        max_current_density={
            IccpEnvironment.SEAWATER: _pv(
                3.75 * _PER_FT2, "A/m2", SRC_DOD_TP16, "Table 3: 3.75 A/ft2"
            ),
            IccpEnvironment.FRESHWATER: _pv(
                0.25 * _PER_FT2, "A/m2", SRC_DOD_TP16, "Table 3: 0.25 A/ft2"
            ),
            IccpEnvironment.SOIL: _pv(1.0 * _PER_FT2, "A/m2", SRC_DOD_TP16, "Table 3: 1 A/ft2"),
        },
    ),
    AnodeMaterial.SCRAP_STEEL: IccpAnodeMaterialRecord(
        material=AnodeMaterial.SCRAP_STEEL,
        life_basis=AnodeLifeBasis.MASS,
        consumption_rate=_per_env(
            _pv(
                20.0 * LB_TO_KG,
                "kg/(A*yr)",
                SRC_DOD_TP16,
                "s.1.0: ~20 lb/A-yr (Faraday for Fe2+ gives 9.1 kg/A-yr)",
            )
        ),
        # No density (scrap geometry is irregular: supply anode_mass_kg) and no
        # current-density limit (USNA Table 7.1: "varies"; supply the limit).
    ),
    AnodeMaterial.MAGNETITE: IccpAnodeMaterialRecord(
        material=AnodeMaterial.MAGNETITE,
        life_basis=AnodeLifeBasis.MASS,
        consumption_rate=_per_env(
            _pv(0.040, "kg/(A*yr)", SRC_USNA_EN380, "Table 7.1: 40 g/A-yr")
        ),
        max_current_density=_per_env(
            _pv_range(10.0, 500.0, "low", "A/m2", SRC_USNA_EN380, "Table 7.1: 10-500")
        ),
        # No open density value found: supply anode_mass_kg.
    ),
    AnodeMaterial.MIXED_METAL_OXIDE: IccpAnodeMaterialRecord(
        material=AnodeMaterial.MIXED_METAL_OXIDE,
        life_basis=AnodeLifeBasis.COATING_WEAR,
        consumption_rate=_per_env(
            _pv_range(
                0.5e-6,
                4.0e-6,
                "high",
                "kg/(A*yr)",
                SRC_CPC_MMO,
                "0.5-4.0 mg/A/yr depending on CP application conditions",
            )
        ),
        max_current_density={
            IccpEnvironment.SOIL: _pv(
                50.0,
                "A/m2",
                SRC_CPC_MMO,
                "carbonaceous backfill row (calcined coke row: 100; lower row taken)",
            ),
            IccpEnvironment.FRESHWATER: _pv(100.0, "A/m2", SRC_CPC_MMO, "freshwater row"),
            IccpEnvironment.SEAWATER: _pv(600.0, "A/m2", SRC_CPC_MMO, "seawater row"),
        },
    ),
    AnodeMaterial.PLATINIZED_TITANIUM: IccpAnodeMaterialRecord(
        material=AnodeMaterial.PLATINIZED_TITANIUM,
        life_basis=AnodeLifeBasis.COATING_WEAR,
        consumption_rate=_per_env(
            _pv(1.0e-5, "kg/(A*yr)", SRC_USNA_EN380, "Table 7.1: 0.01 g/A-yr (Pt)")
        ),
        max_current_density=_per_env(
            _pv_range(250.0, 700.0, "low", "A/m2", SRC_USNA_EN380, "Table 7.1: 250-700; 9 V max")
        ),
    ),
    AnodeMaterial.PLATINIZED_NIOBIUM: IccpAnodeMaterialRecord(
        material=AnodeMaterial.PLATINIZED_NIOBIUM,
        life_basis=AnodeLifeBasis.COATING_WEAR,
        consumption_rate=_per_env(
            _pv(
                1.0e-5,
                "kg/(A*yr)",
                SRC_USNA_EN380,
                "Table 7.1 'platinized columbium': 0.01 g/A-yr "
                "(TP16 s.1.6 gives 1e-5 lb/A-yr = 4.5 mg for Pt; the higher rate is used)",
            )
        ),
        max_current_density=_per_env(
            _pv_range(
                500.0, 1000.0, "low", "A/m2", SRC_USNA_EN380, "Table 7.1: 500-1000; 100 V max"
            )
        ),
    ),
}

DEFAULT_UTILISATION_FACTOR = _pv_range(
    0.80,
    0.85,
    "low",
    "dimensionless",
    SRC_DOD_TP16,
    "s.5 worked examples: 'usually 85%' in one, U = 0.8 in two others; "
    "the lower (life-shortening) value is used",
)

# Copper cable resistivity [ohm-mm²/m] at 20°C
COPPER_RESISTIVITY = 0.0175  # ohm-mm²/m


class RectifierSizingInput(BaseModel):
    """Input parameters for rectifier sizing."""

    total_current_A: float = Field(..., gt=0, description="Total current demand [A]")
    ground_bed_resistance_ohm: float = Field(
        ..., gt=0, description="Anode bed resistance [ohm]"
    )
    structure_coating_resistance_ohm: float = Field(
        default=1.0, ge=0, description="Structure/coating resistance [ohm]"
    )
    cable_resistance_ohm: float = Field(
        default=0.0, ge=0, description="Total cable resistance [ohm]"
    )
    back_emf_V: float = Field(
        default=2.0,
        ge=0,
        description="Back EMF from anode/structure potential difference [V]",
    )
    safety_factor: float = Field(
        default=1.25, gt=1.0, description="Safety factor on voltage (typically 1.2-1.5)"
    )


class RectifierSizingResult(BaseModel):
    """Output of rectifier sizing calculation."""

    dc_voltage_V: float = Field(..., description="Required DC output voltage [V]")
    dc_current_A: float = Field(..., description="Required DC output current [A]")
    power_W: float = Field(..., description="Required rectifier power [W]")
    recommended_rating_V: float = Field(
        ..., description="Recommended standard rectifier voltage rating [V]"
    )
    recommended_rating_A: float = Field(
        ..., description="Recommended standard rectifier current rating [A]"
    )
    exceeds_standard_range: bool = Field(
        default=False,
        description=(
            "True when the required voltage or current exceeds the largest "
            "standard single-unit rating; recommended_rating_* are then per unit"
        ),
    )
    units_required: int = Field(
        default=1,
        ge=1,
        description=(
            "Number of largest-standard units needed: ceil(required / largest) "
            "for whichever of voltage or current exceeds the range (the larger "
            "of the two if both do), 1 otherwise"
        ),
    )


def rectifier_sizing(
    input_params: RectifierSizingInput,
) -> RectifierSizingResult:
    """Size a transformer-rectifier for an ICCP system.

    Calculates required DC voltage as:
        V_dc = I * (R_gb + R_struct + R_cable) + V_back_emf

    Then applies safety factor and rounds up to standard ratings. If the
    required voltage or current exceeds the largest standard rating the
    result is flagged (``exceeds_standard_range``) with ``units_required``
    set to the number of largest-standard units, instead of silently
    capping at the largest size (issue #2209).

    Parameters
    ----------
    input_params : RectifierSizingInput
        System parameters for sizing.

    Returns
    -------
    RectifierSizingResult
        Required voltage, current, and power with standard (per-unit) ratings.
    """
    total_resistance = (
        input_params.ground_bed_resistance_ohm
        + input_params.structure_coating_resistance_ohm
        + input_params.cable_resistance_ohm
    )

    v_dc = (
        input_params.total_current_A * total_resistance
        + input_params.back_emf_V
    ) * input_params.safety_factor

    power = v_dc * input_params.total_current_A

    # Standard rectifier ratings (common sizes)
    standard_voltages = [12, 24, 36, 48, 72, 96, 120]
    standard_currents = [5, 10, 16, 25, 40, 50, 75, 100, 150, 200]

    rec_voltage = next(
        (v for v in standard_voltages if v >= v_dc),
        standard_voltages[-1],
    )
    rec_current = next(
        (c for c in standard_currents if c >= input_params.total_current_A),
        standard_currents[-1],
    )

    # Never cap silently: report how many largest-standard units are needed.
    voltage_units = math.ceil(v_dc / standard_voltages[-1])
    current_units = math.ceil(input_params.total_current_A / standard_currents[-1])
    units_required = max(1, voltage_units, current_units)
    exceeds = units_required > 1

    return RectifierSizingResult(
        dc_voltage_V=round(v_dc, 2),
        dc_current_A=input_params.total_current_A,
        power_W=round(power, 2),
        recommended_rating_V=float(rec_voltage),
        recommended_rating_A=float(rec_current),
        exceeds_standard_range=exceeds,
        units_required=units_required,
    )


class AnodeBedResult(BaseModel):
    """Output of anode bed design."""

    bed_type: str = Field(..., description="Anode bed type")
    anode_material: str = Field(..., description="Anode material")
    environment: str = Field(..., description="Electrolyte around the anodes")
    number_of_anodes: int = Field(..., description="Number of anodes")
    anode_length_m: float = Field(..., description="Individual anode length [m]")
    anode_diameter_m: float = Field(..., description="Individual anode diameter [m]")
    anode_spacing_m: Optional[float] = Field(
        None, description="Anode center-to-center spacing [m] (vertical arrays)"
    )
    anode_current_density_A_m2: float = Field(
        ..., description="Operating current density on each anode [A/m²]"
    )
    max_current_density_A_m2: float = Field(
        ..., description="Material current-density limit used for the anode count [A/m²]"
    )
    bed_resistance_ohm: float = Field(
        ..., description="Total anode bed resistance to remote earth [ohm]"
    )
    resistance_formula: str = Field(..., description="Resistance equation and its source")
    life_basis: str = Field(..., description="'mass' (consumable) or 'coating_wear'")
    estimated_life_years: Optional[float] = Field(
        None,
        description=(
            "Estimated anode bed life [years]; None unless anode_bed_design was "
            "called with experimental=True (provisional literature data, #2247)"
        ),
    )
    provisional_sources: list[str] = Field(
        default_factory=list,
        description="Literature sources of every provisional value used",
    )


def _require_positive(**values: Optional[float]) -> None:
    for name, v in values.items():
        if v is not None and not v > 0:
            raise ValueError(f"{name} must be positive, got {v!r}")


def single_vertical_anode_resistance(
    resistivity_ohm_m: float, length_m: float, diameter_m: float
) -> float:
    """Resistance of one vertical anode (or backfill column) to remote earth [ohm].

    ``R = rho / (2 pi L) * (ln(8L/d) - 1)`` -- Dwight's single-rod form as
    used in DoD TSEWG TP-16 (2017) s.5 (``0.0052 rho/L [ln(8L/d) - 1]`` in
    ohm-cm and feet; 0.0052 = 1/(2 pi x 30.48)). Provisional (#2247).
    """
    _require_positive(
        resistivity_ohm_m=resistivity_ohm_m, length_m=length_m, diameter_m=diameter_m
    )
    return resistivity_ohm_m / (2.0 * math.pi * length_m) * (
        math.log(8.0 * length_m / diameter_m) - 1.0
    )


def multiple_vertical_anode_resistance(
    resistivity_ohm_m: float,
    length_m: float,
    diameter_m: float,
    number_of_anodes: int,
    spacing_m: float,
) -> float:
    """Resistance of N parallel vertical anodes in a line, spacing S [ohm].

    ``R = rho / (2 pi N L) * [ln(8L/d) - 1 + (2L/S) ln(0.656 N)]``

    Source: DoD TSEWG TP-16 (2017) s.5 worked examples
    (``Ra = 0.0052 rho/(N L) [ln(8L/d) - 1 + (2L/S) ln(0.656 N)]``, rho in
    ohm-cm, L, d, S in feet). The form is widely attributed to Sunde (Earth
    Conduction Effects in Transmission Systems, 1949); that attribution has
    not been checked against the original. N = 1 returns the single-anode
    value. Provisional (#2247).
    """
    if number_of_anodes < 1:
        raise ValueError("number_of_anodes must be >= 1")
    if number_of_anodes == 1:
        return single_vertical_anode_resistance(resistivity_ohm_m, length_m, diameter_m)
    _require_positive(
        resistivity_ohm_m=resistivity_ohm_m,
        length_m=length_m,
        diameter_m=diameter_m,
        spacing_m=spacing_m,
    )
    n, L = number_of_anodes, length_m
    return resistivity_ohm_m / (2.0 * math.pi * n * L) * (
        math.log(8.0 * L / diameter_m) - 1.0 + (2.0 * L / spacing_m) * math.log(0.656 * n)
    )


def deep_well_resistance(
    resistivity_ohm_m: float, column_length_m: float, column_diameter_m: float
) -> float:
    """Deep-well ground bed: the active backfill column as one vertical electrode [ohm].

    ``R = rho / (2 pi L_col) * (ln(8 L_col/d_col) - 1)`` with the active
    backfill column length and diameter -- DoD TSEWG TP-16 (2017) s.5
    deep-well examples. Replaces the uncited x0.6 "deep well" factor
    (#2209 / #2247). ``resistivity_ohm_m`` is the design resistivity along
    the column (TP-16 uses the near-surface value as a conservative bound).
    """
    return single_vertical_anode_resistance(
        resistivity_ohm_m, column_length_m, column_diameter_m
    )


def horizontal_column_resistance(
    resistivity_ohm_m: float,
    column_length_m: float,
    column_diameter_m: float,
    depth_m: float,
) -> float:
    """Horizontal backfill column at depth h [ohm].

    ``R = rho / (2 pi L) * [ln(4L/d) + ln(L/h) - 2 + 2h/L]``

    SI form of DoD TSEWG TP-16 (2017) s.5
    ``RS = 1.64 rho / (pi L) [ln(48L/d) + ln(L/h) - 2 + 2h/L]`` (rho in
    ohm-m, L and h in feet, d in inches; 1.64 x 0.3048 = 0.499872,
    approximated here by 0.5, and
    48 L_ft/d_in = 4 L/d). Provisional (#2247): the form differs from other
    published Dwight horizontal-conductor expressions and is flagged in
    the standards-verification checklist.
    """
    _require_positive(
        resistivity_ohm_m=resistivity_ohm_m,
        column_length_m=column_length_m,
        column_diameter_m=column_diameter_m,
        depth_m=depth_m,
    )
    L, d, h = column_length_m, column_diameter_m, depth_m
    return resistivity_ohm_m / (2.0 * math.pi * L) * (
        math.log(4.0 * L / d) + math.log(L / h) - 2.0 + 2.0 * h / L
    )


def iccp_anode_life(
    total_current_A: float,
    number_of_anodes: int,
    anode_material: AnodeMaterial,
    environment: IccpEnvironment = IccpEnvironment.SOIL,
    *,
    anode_mass_kg: Optional[float] = None,
    anode_length_m: Optional[float] = None,
    anode_diameter_m: Optional[float] = None,
    utilisation_factor: Optional[float] = None,
    coating_loading_kg_m2: Optional[float] = None,
    consumption_rate_kg_A_yr: Optional[float] = None,
    experimental: bool = False,
) -> float:
    """Anode bed life [years] from provisional literature consumption data.

    * Consumable anodes (HSCI, graphite, magnetite, scrap steel):
      ``life = N * m * u / (C * I)`` (DoD TSEWG TP-16 s.5 ``L = N W u / (S I)``),
      with m the mass of one anode (``anode_mass_kg``, or solid-rod mass
      ``rho * pi d^2/4 * L`` from the cited density for HSCI only).
      Graphite requires measured mass: TP-16 Table 1 p2 is a maximum density.
      Here u is the utilisation
      factor (default 0.80, the low end of TP-16's 0.80-0.85) and C the cited consumption rate
      (``consumption_rate_kg_A_yr`` overrides it, e.g. with a supplier value).
    * Coated, dimensionally stable anodes (MMO, Pt/Ti, Pt/Nb):
      ``life = N * w * A / (C * I)``, with w the active coating loading
      [kg/m²] (required input: no open source gives a loading) and
      A = pi d L the active area of one anode. The caller must keep the
      anode current density within the rated limit (``anode_bed_design``
      sizes N for that).

    Experimental
    ------------
    Returns a number only with ``experimental=True`` (issue #2209/#2247):
    every material constant is provisional literature data pending
    NACE SP0572 / ISO 15589-1.
    """
    require_experimental(
        experimental,
        model="iccp_design.iccp_anode_life",
        reason="anode consumption data are provisional open-literature values",
        standard="NACE SP0572 / ISO 15589-1",
    )
    _require_positive(total_current_A=total_current_A)
    if number_of_anodes < 1:
        raise ValueError("number_of_anodes must be >= 1")
    rec = ICCP_ANODE_RECORDS[AnodeMaterial(anode_material)]
    env = IccpEnvironment(environment)
    rate = (
        rec.consumption_rate[env].value
        if consumption_rate_kg_A_yr is None
        else consumption_rate_kg_A_yr
    )
    _require_positive(consumption_rate_kg_A_yr=rate)
    if rec.life_basis is AnodeLifeBasis.MASS:
        if anode_mass_kg is None:
            if rec.density is None:
                raise ValueError(
                    f"no qualified nominal density for {rec.material.value}; "
                    "supply anode_mass_kg"
                )
            if anode_length_m is None or anode_diameter_m is None:
                raise ValueError("supply anode_mass_kg or anode length and diameter")
            _require_positive(anode_length_m=anode_length_m, anode_diameter_m=anode_diameter_m)
            anode_mass_kg = (
                rec.density.value * math.pi * (anode_diameter_m / 2.0) ** 2 * anode_length_m
            )
        u = DEFAULT_UTILISATION_FACTOR.value if utilisation_factor is None else utilisation_factor
        _require_positive(anode_mass_kg=anode_mass_kg, utilisation_factor=u)
        if u > 1.0:
            raise ValueError("utilisation_factor must be <= 1")
        return number_of_anodes * anode_mass_kg * u / (rate * total_current_A)
    if coating_loading_kg_m2 is None:
        raise ValueError(
            f"{rec.material.value} life is coating-wear based: supply "
            "coating_loading_kg_m2 (active coating mass per unit area; no open "
            "source gives a default)"
        )
    if anode_length_m is None or anode_diameter_m is None:
        raise ValueError("supply anode length and diameter for the active area")
    _require_positive(
        coating_loading_kg_m2=coating_loading_kg_m2,
        anode_length_m=anode_length_m,
        anode_diameter_m=anode_diameter_m,
    )
    area = math.pi * anode_diameter_m * anode_length_m
    return number_of_anodes * coating_loading_kg_m2 * area / (rate * total_current_A)


def anode_bed_design(
    total_current_A: float,
    soil_resistivity_ohm_m: float,
    design_life_years: float = 25.0,
    bed_type: AnodeBedType = AnodeBedType.SHALLOW_VERTICAL,
    anode_material: AnodeMaterial = AnodeMaterial.HIGH_SILICON_CAST_IRON,
    anode_length_m: float = 1.5,
    anode_diameter_m: float = 0.075,
    *,
    environment: IccpEnvironment = IccpEnvironment.SOIL,
    anode_spacing_m: Optional[float] = None,
    number_of_anodes: Optional[int] = None,
    max_current_density_A_m2: Optional[float] = None,
    backfill_column_length_m: Optional[float] = None,
    backfill_column_diameter_m: Optional[float] = None,
    burial_depth_m: Optional[float] = None,
    anode_mass_kg: Optional[float] = None,
    utilisation_factor: Optional[float] = None,
    coating_loading_kg_m2: Optional[float] = None,
    consumption_rate_kg_A_yr: Optional[float] = None,
    experimental: bool = False,
) -> AnodeBedResult:
    """Design an impressed current anode bed.

    Anode count: ``N = max(ceil(I / (pi d L * J_max)), number_of_anodes)``,
    with J_max the provisional per-material, per-environment limit from
    :data:`ICCP_ANODE_RECORDS` (or ``max_current_density_A_m2``).

    Bed resistance by ``bed_type`` (all DoD TSEWG TP-16, 2017, provisional):

    * ``SHALLOW_VERTICAL``: :func:`multiple_vertical_anode_resistance`
      (``anode_spacing_m`` required when N > 1). The backfill column
      length/diameter are used when given, else the anode dimensions.
    * ``DEEP_WELL``: :func:`deep_well_resistance` on the active backfill
      column (``backfill_column_length_m`` and ``backfill_column_diameter_m``
      required). The former uncited x0.6 factor is removed.
    * ``SHALLOW_HORIZONTAL``: :func:`horizontal_column_resistance`
      (column length, diameter and ``burial_depth_m`` required).
    * ``DISTRIBUTED``: no cited formula is implemented; raises ``ValueError``.

    ``design_life_years`` is kept for API compatibility; the life figure is
    reported, not used to size N, so the geometry and resistance are the
    same with or without ``experimental``.

    Experimental
    ------------
    ``estimated_life_years`` is ``None`` unless ``experimental=True``; it
    is then :func:`iccp_anode_life` (mass-based for consumable anodes,
    coating-wear based for MMO / Pt). Every material constant is a
    provisional literature value pending NACE SP0572 / ISO 15589-1.

    Raises
    ------
    ValueError
        On non-positive inputs, a missing input that has no open-literature
        default (spacing, column geometry, J_max for scrap steel, mass or
        coating loading for the life), or ``DISTRIBUTED`` beds.
    """
    _require_positive(
        total_current_A=total_current_A,
        soil_resistivity_ohm_m=soil_resistivity_ohm_m,
        design_life_years=design_life_years,
        anode_length_m=anode_length_m,
        anode_diameter_m=anode_diameter_m,
        anode_spacing_m=anode_spacing_m,
        max_current_density_A_m2=max_current_density_A_m2,
        backfill_column_length_m=backfill_column_length_m,
        backfill_column_diameter_m=backfill_column_diameter_m,
        burial_depth_m=burial_depth_m,
    )
    material = AnodeMaterial(anode_material)
    env = IccpEnvironment(environment)
    bed = AnodeBedType(bed_type)
    rec = ICCP_ANODE_RECORDS[material]
    sources: set[str] = set()
    if experimental and consumption_rate_kg_A_yr is None:
        sources.add(render_provisional(rec.consumption_rate[env], "consumption_rate"))

    if max_current_density_A_m2 is None:
        cited = rec.max_current_density.get(env)
        if cited is None:
            raise ValueError(
                f"no open-literature current-density limit for {material.value} in "
                f"{env.value}; supply max_current_density_A_m2"
            )
        j_max = cited.value
        sources.add(render_provisional(cited, "max_current_density"))
    else:
        j_max = max_current_density_A_m2

    anode_area = math.pi * anode_diameter_m * anode_length_m
    n_current = math.ceil(total_current_A / (anode_area * j_max))
    if number_of_anodes is not None and number_of_anodes < 1:
        raise ValueError("number_of_anodes must be >= 1")
    n_anodes = max(n_current, number_of_anodes or 1, 1)

    spacing: Optional[float] = None
    if bed is AnodeBedType.SHALLOW_VERTICAL:
        col_l = backfill_column_length_m or anode_length_m
        col_d = backfill_column_diameter_m or anode_diameter_m
        if n_anodes > 1:
            if anode_spacing_m is None:
                raise ValueError(
                    "anode_spacing_m is required for a vertical array of "
                    f"{n_anodes} anodes (no open-literature default spacing)"
                )
            spacing = anode_spacing_m
        r_bed = multiple_vertical_anode_resistance(
            soil_resistivity_ohm_m, col_l, col_d, n_anodes, spacing or 1.0
        )
        formula = (
            "rho/(2 pi N L)[ln(8L/d) - 1 + (2L/S) ln(0.656 N)] (DoD TSEWG TP-16 2017)"
        )
    elif bed is AnodeBedType.DEEP_WELL:
        if backfill_column_length_m is None or backfill_column_diameter_m is None:
            raise ValueError(
                "deep-well resistance needs backfill_column_length_m and "
                "backfill_column_diameter_m (active column)"
            )
        r_bed = deep_well_resistance(
            soil_resistivity_ohm_m, backfill_column_length_m, backfill_column_diameter_m
        )
        formula = "rho/(2 pi L_col)[ln(8 L_col/d_col) - 1] (DoD TSEWG TP-16 2017)"
    elif bed is AnodeBedType.SHALLOW_HORIZONTAL:
        if (
            backfill_column_length_m is None
            or backfill_column_diameter_m is None
            or burial_depth_m is None
        ):
            raise ValueError(
                "horizontal-bed resistance needs backfill_column_length_m, "
                "backfill_column_diameter_m and burial_depth_m"
            )
        r_bed = horizontal_column_resistance(
            soil_resistivity_ohm_m,
            backfill_column_length_m,
            backfill_column_diameter_m,
            burial_depth_m,
        )
        formula = "rho/(2 pi L)[ln(4L/d) + ln(L/h) - 2 + 2h/L] (DoD TSEWG TP-16 2017)"
    else:
        raise ValueError(
            "no cited resistance formula for DISTRIBUTED beds; model each anode "
            "with single_vertical_anode_resistance or use another bed type"
        )
    sources.add(SRC_DOD_TP16)

    estimated_life: Optional[float] = None
    if experimental:
        estimated_life = round(
            iccp_anode_life(
                total_current_A,
                n_anodes,
                material,
                env,
                anode_mass_kg=anode_mass_kg,
                anode_length_m=anode_length_m,
                anode_diameter_m=anode_diameter_m,
                utilisation_factor=utilisation_factor,
                coating_loading_kg_m2=coating_loading_kg_m2,
                consumption_rate_kg_A_yr=consumption_rate_kg_A_yr,
                experimental=True,
            ),
            1,
        )
        if rec.density is not None and anode_mass_kg is None:
            sources.add(render_provisional(rec.density, "density"))
        if rec.life_basis is AnodeLifeBasis.MASS and utilisation_factor is None:
            sources.add(render_provisional(DEFAULT_UTILISATION_FACTOR, "utilisation_factor"))

    return AnodeBedResult(
        bed_type=bed.value,
        anode_material=material.value,
        environment=env.value,
        number_of_anodes=n_anodes,
        anode_length_m=anode_length_m,
        anode_diameter_m=anode_diameter_m,
        anode_spacing_m=spacing,
        anode_current_density_A_m2=total_current_A / (n_anodes * anode_area),
        max_current_density_A_m2=j_max,
        bed_resistance_ohm=round(r_bed, 4),
        resistance_formula=formula,
        life_basis=rec.life_basis.value,
        estimated_life_years=estimated_life,
        provisional_sources=sorted(sources),
    )


def cable_sizing(
    current_A: float,
    cable_length_m: float,
    max_voltage_drop_V: float = 2.0,
    temperature_c: float = 20.0,
) -> dict[str, float]:
    """Calculate minimum cable cross-section for ICCP system.

    Uses copper resistivity with temperature correction to determine
    minimum cross-sectional area for acceptable voltage drop.

    Parameters
    ----------
    current_A : float
        Maximum cable current [A].
    cable_length_m : float
        One-way cable length [m].
    max_voltage_drop_V : float
        Maximum acceptable voltage drop [V] (default 2.0 V).
    temperature_c : float
        Cable operating temperature [°C].

    Returns
    -------
    dict[str, float]
        Cable sizing results including area, resistance, and voltage drop.
    """
    # Temperature correction for copper
    rho = COPPER_RESISTIVITY * (1.0 + 0.00393 * (temperature_c - 20.0))

    # Minimum cross-section: A = rho * I * 2L / V_drop (2L for round trip)
    min_area_mm2 = rho * current_A * 2.0 * cable_length_m / max_voltage_drop_V

    # Standard cable sizes [mm²]
    standard_sizes = [2.5, 4.0, 6.0, 10.0, 16.0, 25.0, 35.0, 50.0, 70.0, 95.0, 120.0]
    selected_size = next(
        (s for s in standard_sizes if s >= min_area_mm2),
        standard_sizes[-1],
    )

    actual_resistance = rho * 2.0 * cable_length_m / selected_size
    actual_voltage_drop = current_A * actual_resistance

    return {
        "min_area_mm2": round(min_area_mm2, 2),
        "selected_area_mm2": selected_size,
        "cable_resistance_ohm": round(actual_resistance, 4),
        "voltage_drop_V": round(actual_voltage_drop, 3),
        "cable_length_m": cable_length_m,
    }
