"""Anode sizing calculator per DNV-RP-B401.

Sacrificial anode sizing for offshore structures, pipelines and marine
vessels with stand-off, bracelet and flush-mounted anode types. Every
formula is evaluated by :mod:`digitalmodel.cathodic_protection._kernels`;
the anode resistance is the DNV-RP-B401 Table 10-7 form selected by the
anode type and its length ratios, and ``design_cp_system`` runs the full
B401 Sec. 7 loop ``N = max(N_mass, N_initial, N_final)`` (issue #2211).

References
----------
- DNV-RP-B401 "Cathodic Protection Design" Sec. 7 and Tables 10-6 to 10-8
- DNV-RP-F103 "Cathodic Protection of Submarine Pipelines" (bracelet
  utilisation, see ``dnv_rp_f103`` for the pipeline design)
"""

from __future__ import annotations

import math
from enum import Enum
from typing import Any, Final

from pydantic import BaseModel, Field, model_validator

from digitalmodel.cathodic_protection import _kernels as kernel
from digitalmodel.cathodic_protection._edition import (
    DEFAULT_EDITION,
    Edition,
    normalize_edition,
    standard_for_edition,
)
from digitalmodel.cathodic_protection.b401_tables import (
    AnodeEnvironment,
    AnodeMaterial,
    AnodeShape,
    anode_capacity,
    anode_closed_circuit_potential,
    design_driving_voltage,
    protection_potential,
    utilisation_factor,
)


# ---------------------------------------------------------------------------
# Constants derived from the cited DNV-RP-B401 table lookups (issue #2207).
# The values are identical across all supported editions, so the package
# default edition is used here without a warning.
# ---------------------------------------------------------------------------
_TABLE_EDITION: Edition = DEFAULT_EDITION

# Protection potentials vs Ag/AgCl (B401 Sec. 5 and Table 10-6)
PROTECTION_POTENTIAL_AGAGCL: float = protection_potential(_TABLE_EDITION).value
ANODE_CLOSED_CIRCUIT_POTENTIAL: float = anode_closed_circuit_potential(
    AnodeMaterial.ALUMINIUM, AnodeEnvironment.SEAWATER, _TABLE_EDITION
).value  # V vs Ag/AgCl, Al-based anode in seawater

# Design driving voltage E_c - E_a (0.25 V for Al in seawater)
DESIGN_DRIVING_VOLTAGE: float = design_driving_voltage(
    AnodeMaterial.ALUMINIUM, _TABLE_EDITION
).value

# Al-based anode capacity (Table 10-6) and utilisation factors (Table 10-8)
DEFAULT_ANODE_CAPACITY: float = anode_capacity(
    AnodeMaterial.ALUMINIUM, AnodeEnvironment.SEAWATER, _TABLE_EDITION
).value  # A-h/kg
DEFAULT_UTILIZATION_STANDOFF: float = utilisation_factor(
    AnodeShape.LONG_SLENDER_STANDOFF, _TABLE_EDITION
).value
DEFAULT_UTILIZATION_FLUSH: float = utilisation_factor(
    AnodeShape.LONG_FLUSH, _TABLE_EDITION
).value
DEFAULT_UTILIZATION_BRACELET: float = utilisation_factor(
    AnodeShape.SHORT_FLUSH_BRACELET, _TABLE_EDITION
).value

_MA_PER_A: Final = 1000.0

GOVERNING_MASS: Final = "mass"
GOVERNING_INITIAL: Final = "initial"
GOVERNING_FINAL: Final = "final"


class AnodeType(str, Enum):
    """Anode installation type."""

    STAND_OFF = "stand_off"
    BRACELET = "bracelet"
    FLUSH_MOUNT = "flush_mount"


class AnodeSizingInput(BaseModel):
    """Input parameters for CP system anode sizing.

    The mean current density and breakdown factor size the anode mass; the
    optional initial and final values drive the B401 Sec. 7.8 current-output
    checks and default to the mean values when omitted.
    """

    surface_area_m2: float = Field(
        ..., gt=0, description="Total surface area to protect [m²]"
    )
    coating_breakdown_factor: float = Field(
        ..., ge=0.0, le=1.0, description="Mean coating breakdown factor (0-1)"
    )
    current_density_mA_m2: float = Field(
        ..., gt=0, description="Design mean current density [mA/m²]"
    )
    initial_breakdown_factor: float | None = Field(
        default=None, ge=0.0, le=1.0, description="Initial breakdown factor (0-1)"
    )
    final_breakdown_factor: float | None = Field(
        default=None, ge=0.0, le=1.0, description="Final breakdown factor (0-1)"
    )
    initial_current_density_mA_m2: float | None = Field(
        default=None, gt=0, description="Design initial current density [mA/m²]"
    )
    final_current_density_mA_m2: float | None = Field(
        default=None, gt=0, description="Design final current density [mA/m²]"
    )
    design_life_years: float = Field(
        ..., gt=0, description="CP system design life [years]"
    )
    anode_type: AnodeType = Field(
        default=AnodeType.STAND_OFF, description="Anode installation type"
    )
    anode_length_m: float = Field(
        ..., gt=0, description="Anode length [m]"
    )
    anode_radius_m: float = Field(
        ...,
        gt=0,
        description=(
            "Anode equivalent radius [m] (stand-off cross-section; bracelet "
            "outer radius; flush-mounted half-width when no width is given)"
        ),
    )
    anode_width_m: float | None = Field(
        default=None, gt=0, description="Flush-mounted anode width [m]"
    )
    anode_thickness_m: float | None = Field(
        default=None, gt=0, description="Flush-mounted anode thickness [m]"
    )
    anode_exposed_area_m2: float | None = Field(
        default=None,
        gt=0,
        description="Exposed area of a bracelet / short flush anode [m²]",
    )
    anode_net_mass_kg: float = Field(
        ..., gt=0, description="Net mass of a single anode [kg]"
    )
    anode_density_kg_m3: float = Field(
        default=kernel.ANODE_DENSITY_ALZNI,
        gt=0,
        description="Anode alloy density [kg/m³] for the depleted-geometry check",
    )
    resistivity_ohm_m: float = Field(
        default=0.30, gt=0, description="Electrolyte resistivity [ohm-m]"
    )
    anode_capacity_Ah_kg: float = Field(
        default=DEFAULT_ANODE_CAPACITY,
        gt=0,
        description="Electrochemical capacity [A-h/kg]",
    )
    utilization_factor: float = Field(
        default=DEFAULT_UTILIZATION_STANDOFF,
        gt=0,
        le=1.0,
        description="Anode utilization factor",
    )
    driving_voltage_V: float = Field(
        default=DESIGN_DRIVING_VOLTAGE,
        gt=0,
        description="Design driving voltage [V]",
    )


class AnodeSizingResult(BaseModel):
    """Output of CP system anode sizing design."""

    current_demand_A: float = Field(
        ..., description="Mean current demand [A]"
    )
    initial_current_demand_A: float = Field(
        default=0.0, description="Initial current demand [A]"
    )
    final_current_demand_A: float = Field(
        default=0.0, description="Final current demand [A]"
    )
    total_anode_mass_kg: float = Field(
        ..., description="Total required anode mass [kg]"
    )
    number_of_anodes: int = Field(
        ..., description="Number of anodes required (max of the three cases)"
    )
    number_of_anodes_mass: int = Field(
        default=0, description="Anodes required by mass (B401 Eq. 2)"
    )
    number_of_anodes_initial: int = Field(
        default=0, description="Anodes required by the initial current-output check"
    )
    number_of_anodes_final: int = Field(
        default=0,
        description="Anodes required by the final (depleted anode) current-output check",
    )
    governing_case: str = Field(
        default=GOVERNING_MASS, description="'mass', 'initial' or 'final'"
    )
    anode_resistance_ohm: float = Field(
        ..., description="Individual anode resistance, fresh geometry [ohm]"
    )
    anode_current_output_A: float = Field(
        ..., description="Current output per anode, fresh geometry [A]"
    )
    final_anode_resistance_ohm: float = Field(
        default=0.0, description="Individual anode resistance, depleted geometry [ohm]"
    )
    final_anode_current_output_A: float = Field(
        default=0.0, description="Current output per anode, depleted geometry [A]"
    )
    driving_voltage_V: float = Field(
        ..., description="Design driving voltage [V]"
    )
    anode_type: str = Field(
        ..., description="Anode type used"
    )
    mass_check_ok: bool = Field(
        ..., description="Whether anode count provides sufficient total mass"
    )
    edition_used: Edition = Field(
        ..., description="DNV-RP-B401 edition used for the design"
    )
    standard: str = Field(
        ..., description="Standards reference matching the selected edition"
    )

    @model_validator(mode="before")
    @classmethod
    def _default_legacy_metadata(cls, data: Any) -> Any:
        if not isinstance(data, dict):
            return data

        values = dict(data)
        edition = values.get("edition_used") or DEFAULT_EDITION
        values["edition_used"] = edition
        if not values.get("standard"):
            try:
                values["standard"] = standard_for_edition(edition)
            except KeyError:
                pass
        return values


def calculate_current_demand(
    surface_area_m2: float,
    coating_breakdown_factor: float,
    current_density_mA_m2: float,
) -> float:
    """Calculate current demand for a coated structure (DNV-RP-B401 §7.4.1).

    I_c = A * f_c * i_c / 1000

    Parameters
    ----------
    surface_area_m2 : float
        Total surface area to protect [m²].
    coating_breakdown_factor : float
        Coating breakdown factor (0 = perfect coating, 1 = bare steel).
    current_density_mA_m2 : float
        Design current density [mA/m²].

    Returns
    -------
    float
        Current demand [A].
    """
    return kernel.current_demand(
        surface_area_m2, current_density_mA_m2 / _MA_PER_A, coating_breakdown_factor
    )


def calculate_anode_mass(
    current_demand_A: float,
    design_life_years: float,
    utilization_factor: float = DEFAULT_UTILIZATION_STANDOFF,
    anode_capacity_Ah_kg: float = DEFAULT_ANODE_CAPACITY,
) -> float:
    """Calculate total anode mass requirement (DNV-RP-B401 §7.7.1, Eq 2).

    M_a = (I_cm * t_f * 8760) / (u * epsilon)

    Parameters
    ----------
    current_demand_A : float
        Mean current demand [A].
    design_life_years : float
        Design life [years].
    utilization_factor : float
        Anode utilization factor (0-1).
    anode_capacity_Ah_kg : float
        Electrochemical capacity [A-h/kg].

    Returns
    -------
    float
        Required total anode mass [kg].
    """
    return kernel.anode_mass(
        current_demand_A, design_life_years, anode_capacity_Ah_kg, utilization_factor
    )


def calculate_anode_resistance(
    anode_type: AnodeType,
    length_m: float,
    radius_m: float,
    resistivity_ohm_m: float,
    width_m: float | None = None,
    thickness_m: float | None = None,
    exposed_area_m2: float | None = None,
) -> float:
    """Anode-to-electrolyte resistance per DNV-RP-B401 Table 10-7.

    Stand-off: long slender ``rho/(2 pi L) (ln(4L/r) - 1)`` when L >= 4r,
    else the short slender stand-off formula.

    Flush-mounted: long flush ``rho / (2 S)`` (S the mean of length and
    width) when L >= 4 width and L >= 4 thickness, else short flush
    ``0.315 rho / sqrt(A)``. Without an explicit width the anode is taken
    as ``width = 2 r`` (the equivalent-cylinder diameter); the thickness
    defaults to the width; the exposed area defaults to ``L * width``.

    Bracelet: ``0.315 rho / sqrt(A)`` with ``A`` the exposed area, by
    default the outer cylindrical surface ``2 pi r L`` for the bracelet
    outer radius ``r``.

    Parameters
    ----------
    anode_type : AnodeType
        Anode installation type.
    length_m : float
        Anode length [m].
    radius_m : float
        Anode equivalent radius (stand-off), outer radius (bracelet) or
        half-width (flush-mounted without ``width_m``) [m].
    resistivity_ohm_m : float
        Electrolyte resistivity [ohm-m].
    width_m, thickness_m : float, optional
        Flush-mounted anode width and thickness [m].
    exposed_area_m2 : float, optional
        Exposed surface area for the short flush / bracelet formula [m²].

    Returns
    -------
    float
        Anode-to-electrolyte resistance [ohm].
    """
    rho = resistivity_ohm_m
    L = length_m
    r = radius_m
    kind = AnodeType(anode_type)

    if kind is AnodeType.BRACELET:
        area = exposed_area_m2 if exposed_area_m2 is not None else 2.0 * math.pi * r * L
        return kernel.short_flush_or_bracelet(rho, area)

    if kind is AnodeType.FLUSH_MOUNT:
        width = width_m if width_m is not None else 2.0 * r
        thickness = thickness_m if thickness_m is not None else width
        if (
            L >= kernel.FLUSH_LENGTH_RATIO * width
            and L >= kernel.FLUSH_LENGTH_RATIO * thickness
        ):
            return kernel.long_flush(rho, L, width, thickness)
        area = exposed_area_m2 if exposed_area_m2 is not None else L * width
        return kernel.short_flush_or_bracelet(rho, area)

    return kernel.slender_standoff(rho, L, r)


def depleted_equivalent_radius(
    anode_net_mass_kg: float,
    anode_length_m: float,
    utilization_factor: float,
    anode_density_kg_m3: float = kernel.ANODE_DENSITY_ALZNI,
) -> float:
    """Equivalent radius of the anode consumed to its utilisation limit.

    B401 Sec. 7.8 asks for the final anode resistance with the anode
    consumed to its utilisation factor. Assumption (issue #2211): the
    depleted anode keeps its length and is a cylinder of the remaining
    mass ``(1 - u) m_a``, so ``r_final = sqrt((1 - u) m_a / (pi L rho))``.
    With ``u = 1`` nothing remains and the fresh radius is returned.
    """
    remaining = (1.0 - utilization_factor) * anode_net_mass_kg
    if remaining <= 0.0:
        return kernel.equivalent_radius_from_mass(
            anode_net_mass_kg, anode_length_m, anode_density_kg_m3
        )
    return kernel.equivalent_radius_from_mass(remaining, anode_length_m, anode_density_kg_m3)


def governing_case(n_mass: int, n_initial: int, n_final: int) -> str:
    """Name of the case that sets ``max(n_mass, n_initial, n_final)``."""
    n = max(n_mass, n_initial, n_final)
    if n_mass == n:
        return GOVERNING_MASS
    if n_initial == n:
        return GOVERNING_INITIAL
    return GOVERNING_FINAL


def design_cp_system(
    input_params: AnodeSizingInput,
    edition: Edition | None = None,
) -> AnodeSizingResult:
    """Design a complete sacrificial anode CP system (DNV-RP-B401 Sec. 7).

    Current demand → anode mass → ``N_mass``; anode resistance and current
    output for the fresh anode → ``N_initial``; the same for the anode
    consumed to its utilisation limit (``depleted_equivalent_radius``) →
    ``N_final``; ``N = max(N_mass, N_initial, N_final)`` with the governing
    case reported. Initial and final demands default to the mean demand
    when the input gives no separate densities or breakdown factors.

    Parameters
    ----------
    input_params : AnodeSizingInput
        Full design input parameters.

    Returns
    -------
    AnodeSizingResult
        Complete CP system design result.
    """
    ed = normalize_edition(edition, stacklevel=3)
    p = input_params

    # Step 1: current demands (mean sizes the mass; initial/final the output)
    i_mean = calculate_current_demand(
        p.surface_area_m2, p.coating_breakdown_factor, p.current_density_mA_m2
    )
    f_initial = (
        p.initial_breakdown_factor
        if p.initial_breakdown_factor is not None
        else p.coating_breakdown_factor
    )
    f_final = (
        p.final_breakdown_factor
        if p.final_breakdown_factor is not None
        else p.coating_breakdown_factor
    )
    i_initial = calculate_current_demand(
        p.surface_area_m2,
        f_initial,
        p.initial_current_density_mA_m2 or p.current_density_mA_m2,
    )
    i_final = calculate_current_demand(
        p.surface_area_m2,
        f_final,
        p.final_current_density_mA_m2 or p.current_density_mA_m2,
    )

    # Step 2: total anode mass and mass-based count
    total_mass = calculate_anode_mass(
        i_mean, p.design_life_years, p.utilization_factor, p.anode_capacity_Ah_kg
    )
    n_mass = kernel.anode_count(total_mass, p.anode_net_mass_kg)

    # Step 3: fresh anode resistance and output → initial count
    R_a = calculate_anode_resistance(
        p.anode_type,
        p.anode_length_m,
        p.anode_radius_m,
        p.resistivity_ohm_m,
        width_m=p.anode_width_m,
        thickness_m=p.anode_thickness_m,
        exposed_area_m2=p.anode_exposed_area_m2,
    )
    i_anode = kernel.anode_current_output(p.driving_voltage_V, R_a)
    n_initial = kernel.anodes_for_current(i_initial, i_anode)

    # Step 4: depleted anode (B401 7.8) → final count. The depleted geometry
    # is derived from the mass-based equivalent radius for every anode type;
    # explicit width / area overrides describe the fresh anode only.
    r_final = depleted_equivalent_radius(
        p.anode_net_mass_kg, p.anode_length_m, p.utilization_factor, p.anode_density_kg_m3
    )
    R_final = calculate_anode_resistance(
        p.anode_type, p.anode_length_m, r_final, p.resistivity_ohm_m
    )
    i_anode_final = kernel.anode_current_output(p.driving_voltage_V, R_final)
    n_final = kernel.anodes_for_current(i_final, i_anode_final)

    # Step 5: governing count
    n_anodes = max(1, n_mass, n_initial, n_final)
    provided_mass = n_anodes * p.anode_net_mass_kg
    mass_check_ok = provided_mass >= total_mass

    return AnodeSizingResult(
        current_demand_A=round(i_mean, 4),
        initial_current_demand_A=round(i_initial, 4),
        final_current_demand_A=round(i_final, 4),
        total_anode_mass_kg=round(total_mass, 2),
        number_of_anodes=n_anodes,
        number_of_anodes_mass=n_mass,
        number_of_anodes_initial=n_initial,
        number_of_anodes_final=n_final,
        governing_case=governing_case(n_mass, n_initial, n_final),
        anode_resistance_ohm=round(R_a, 6),
        anode_current_output_A=round(i_anode, 4),
        final_anode_resistance_ohm=round(R_final, 6),
        final_anode_current_output_A=round(i_anode_final, 4),
        driving_voltage_V=p.driving_voltage_V,
        anode_type=p.anode_type.value,
        mass_check_ok=mass_check_ok,
        edition_used=ed,
        standard=standard_for_edition(ed),
    )
