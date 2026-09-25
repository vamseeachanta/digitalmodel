"""DNV-RP-F103 — Cathodic Protection of Submarine Pipelines by Galvanic Anodes.

Bracelet-anode CP design for a coated submarine pipeline (editions ``"2010"``
and ``"2016"`` via :func:`~digitalmodel.cathodic_protection._edition.normalize_f103_edition`):

1. surface area ``A = pi * D * L`` split into linepipe and field-joint areas;
2. design mean current density from Table 5-1 (exposure, fluid temperature);
3. mean and final coating breakdown factors ``f_cm = a + b * t_f / 2`` and
   ``f_cf = a + b * t_f`` from the Table A.1 (linepipe) and Table A.2
   (field-joint) constants;
4. mean and final current demand ``I_cm = A * i_cm * f_cm`` and
   ``I_cf = A * i_cm * f_cf`` (F103 uses the mean current density for both);
5. total net anode mass ``M = I_cm * t_f * 8760 / (u * epsilon)`` with the
   bracelet utilisation factor (B401 Table 10-8, 0.80) and the alloy capacity
   (B401 Table 10-6);
6. anode count ``N = max(N_mass, N_final)`` where the final current-output
   check uses the bracelet resistance ``R_a = 0.315 rho / sqrt(A)`` (B401
   Table 10-7) and the design driving voltage (B401 Table 10-6);
7. anode spacing ``L / N`` checked against twice the protected length of
   Eq. 14 (Sec. 5.6.7), so that every point on the pipe lies within one
   protected length of an anode.

All formulas are evaluated by :mod:`digitalmodel.cathodic_protection._kernels`;
every table value used is returned with its citation (issue #2211).
"""

from __future__ import annotations

import math
from typing import Any, Final

from pydantic import BaseModel, Field, model_validator

from digitalmodel.cathodic_protection import _kernels as kernel
from digitalmodel.cathodic_protection._edition import (
    DEFAULT_F103_EDITION,
    F103Edition,
    f103_standard_for_edition,
    normalize_f103_edition,
)
from digitalmodel.cathodic_protection.b401_tables import (
    AnodeEnvironment,
    AnodeMaterial,
    anode_capacity,
    citation_label,
    design_driving_voltage,
)
from digitalmodel.cathodic_protection.f103_tables import (
    Exposure,
    FieldJointCoating,
    LinepipeCoating,
    b401_edition_for_f103,
    bracelet_utilisation_factor,
    edition_provenance,
    field_joint_coating_constants,
    linepipe_coating_constants,
    mean_current_density,
)
from digitalmodel.citations import CitedValue

#: Steel resistivity [ohm-m] (DNV-RP-F103 Sec. 5.6.10).
STEEL_RESISTIVITY: Final = 2.0e-7

#: Metallic voltage drop [V] allowed along the pipe (DNV-RP-F103 Sec. 5.6.3).
DEFAULT_METALLIC_VOLTAGE_DROP_V: Final = 0.15

#: Seawater resistivity [ohm-m] default for the bracelet resistance.
DEFAULT_SEAWATER_RESISTIVITY_OHM_M: Final = 0.30

GOVERNING_MASS: Final = "mass"
GOVERNING_FINAL: Final = "final"


class BraceletDesignInput(BaseModel):
    """Pipeline, coating, environment and bracelet anode data for F103 design."""

    outer_diameter_m: float = Field(..., gt=0, description="Pipe outer diameter D [m]")
    wall_thickness_m: float = Field(..., gt=0, description="Pipe wall thickness WT [m]")
    length_m: float = Field(..., gt=0, description="Pipeline length to protect [m]")
    linepipe_coating: LinepipeCoating = Field(
        ..., description="Linepipe coating system (Table A.1 row)"
    )
    field_joint_coating: FieldJointCoating = Field(
        default=FieldJointCoating.NONE,
        description="Field-joint coating system (Table A.2 row)",
    )
    field_joint_area_fraction: float | None = Field(
        default=None,
        ge=0.0,
        lt=1.0,
        description=(
            "Fraction of the pipe surface that is field joint; None with no "
            "joint count means the whole surface is linepipe coating"
        ),
    )
    field_joint_count: int | None = Field(
        default=None,
        ge=0,
        description="Number of field joints (alternative to the area fraction)",
    )
    field_joint_length_m: float | None = Field(
        default=None,
        gt=0,
        description="Axial length of one coated field joint [m] (with a joint count)",
    )
    exposure: Exposure = Field(
        default=Exposure.NON_BURIED, description="Table 5-1 exposure condition"
    )
    fluid_temperature_c: float = Field(
        ..., description="Internal fluid temperature [°C] (Table 5-1 column)"
    )
    design_life_years: float = Field(..., gt=0, description="Design life t_f [years]")
    seawater_resistivity_ohm_m: float = Field(
        default=DEFAULT_SEAWATER_RESISTIVITY_OHM_M,
        gt=0,
        description="Seawater (or sediment) resistivity at the anode [ohm-m]",
    )
    steel_resistivity_ohm_m: float = Field(
        default=STEEL_RESISTIVITY, gt=0, description="Pipe steel resistivity [ohm-m]"
    )
    anode_material: AnodeMaterial = Field(
        default=AnodeMaterial.ALUMINIUM, description="Anode alloy (B401 Table 10-6)"
    )
    anode_environment: AnodeEnvironment | None = Field(
        default=None,
        description=(
            "Anode exposure for the Table 10-6 capacity; None derives it from "
            "the pipeline exposure (buried -> sediments, else seawater)"
        ),
    )
    bracelet_net_mass_kg: float = Field(
        ..., gt=0, description="Net alloy mass of one bracelet [kg]"
    )
    bracelet_length_m: float = Field(..., gt=0, description="Bracelet axial length [m]")
    bracelet_thickness_m: float | None = Field(
        default=None,
        gt=0,
        description="Bracelet radial thickness [m]; sets the exposed area",
    )
    bracelet_exposed_area_m2: float | None = Field(
        default=None,
        gt=0,
        description="Exposed surface area of one bracelet [m2] (overrides thickness)",
    )
    delta_E_me_V: float = Field(
        default=DEFAULT_METALLIC_VOLTAGE_DROP_V,
        gt=0,
        description="Metallic voltage drop allowed along the pipe [V] (Sec. 5.6.3)",
    )

    @model_validator(mode="after")
    def _check_geometry(self) -> BraceletDesignInput:
        if self.wall_thickness_m >= self.outer_diameter_m / 2.0:
            raise ValueError("wall_thickness_m must be less than half the outer diameter")
        if self.field_joint_area_fraction is not None and self.field_joint_count is not None:
            raise ValueError(
                "give either field_joint_area_fraction or field_joint_count, not both"
            )
        if self.field_joint_count and self.field_joint_length_m is None:
            raise ValueError("field_joint_length_m is required with field_joint_count")
        if self.bracelet_thickness_m is None and self.bracelet_exposed_area_m2 is None:
            raise ValueError(
                "give bracelet_thickness_m or bracelet_exposed_area_m2 for the "
                "bracelet exposed area"
            )
        return self

    @property
    def surface_area_m2(self) -> float:
        """External pipe surface ``pi * D * L`` [m2]."""
        return math.pi * self.outer_diameter_m * self.length_m

    @property
    def field_joint_fraction(self) -> float:
        """Fraction of the surface that is field joint (0 when none is given)."""
        if self.field_joint_area_fraction is not None:
            return self.field_joint_area_fraction
        if self.field_joint_count and self.field_joint_length_m is not None:
            return min(
                self.field_joint_count * self.field_joint_length_m / self.length_m, 1.0
            )
        return 0.0

    @property
    def resolved_anode_environment(self) -> AnodeEnvironment:
        """Anode environment: explicit, else sediments when buried."""
        if self.anode_environment is not None:
            return AnodeEnvironment(self.anode_environment)
        if Exposure(self.exposure) is Exposure.BURIED:
            return AnodeEnvironment.SEDIMENT
        return AnodeEnvironment.SEAWATER

    @property
    def bracelet_area_m2(self) -> float:
        """Exposed area of one bracelet: given, else ``pi (D + 2 t) L_a``."""
        if self.bracelet_exposed_area_m2 is not None:
            return self.bracelet_exposed_area_m2
        assert self.bracelet_thickness_m is not None  # enforced by the validator
        return (
            math.pi
            * (self.outer_diameter_m + 2.0 * self.bracelet_thickness_m)
            * self.bracelet_length_m
        )


class BraceletDesignResult(BaseModel):
    """Result of a DNV-RP-F103 bracelet anode design."""

    surface_area_m2: float = Field(..., description="Pipe external surface area [m2]")
    linepipe_area_m2: float = Field(..., description="Linepipe-coated area [m2]")
    field_joint_area_m2: float = Field(..., description="Field-joint area [m2]")
    mean_current_density_A_m2: float = Field(
        ..., description="Design mean current density i_cm (Table 5-1) [A/m2]"
    )
    f_cm_linepipe: float = Field(..., description="Mean breakdown factor, linepipe")
    f_cf_linepipe: float = Field(..., description="Final breakdown factor, linepipe")
    f_cm_field_joint: float = Field(..., description="Mean breakdown factor, field joint")
    f_cf_field_joint: float = Field(..., description="Final breakdown factor, field joint")
    mean_current_demand_A: float = Field(..., description="I_cm [A]")
    final_current_demand_A: float = Field(..., description="I_cf [A]")
    anode_capacity_Ah_kg: float = Field(..., description="Alloy capacity (Table 10-6)")
    utilisation_factor: float = Field(..., description="Bracelet utilisation (Table 10-8)")
    driving_voltage_V: float = Field(..., description="Design driving voltage [V]")
    total_net_mass_kg: float = Field(..., description="Required net anode mass [kg]")
    number_of_anodes: int = Field(..., description="Anodes required, max of the cases")
    number_of_anodes_mass: int = Field(..., description="Anodes required by mass")
    number_of_anodes_final: int = Field(
        ..., description="Anodes required by the final current-output check"
    )
    governing_case: str = Field(..., description="'mass' or 'final'")
    anode_spacing_m: float = Field(..., description="Anode spacing L / N [m]")
    protected_length_m: float = Field(..., description="Protected length, Eq. 14 [m]")
    spacing_ok: bool = Field(..., description="Spacing <= 2 * protected length")
    bracelet_exposed_area_m2: float = Field(..., description="Exposed area of one bracelet")
    anode_resistance_ohm: float = Field(..., description="0.315 rho / sqrt(A) [ohm]")
    anode_current_output_A: float = Field(..., description="Output of one bracelet [A]")
    current_output_ok: bool = Field(..., description="N * I_a >= I_cf")
    citations: list[str] = Field(
        default_factory=list,
        description="Rendered citations of every table value used, in first-use order",
    )
    edition_used: F103Edition = Field(..., description="DNV-RP-F103 edition token")
    standard: str = Field(..., description="Report-facing standard string")
    provenance: str = Field(..., description="Table provenance flag for the edition")

    @model_validator(mode="before")
    @classmethod
    def _default_legacy_metadata(cls, data: Any) -> Any:
        if not isinstance(data, dict):
            return data
        values = dict(data)
        edition = values.get("edition_used") or DEFAULT_F103_EDITION
        values["edition_used"] = edition
        values.setdefault("standard", f103_standard_for_edition(edition))
        values.setdefault("provenance", edition_provenance(edition))
        return values


def protected_length(
    delta_E_me: float,
    WT: float,
    D: float,
    rho_me: float,
    f_cf: float,
    i_cm: float,
    edition: F103Edition | None = None,
) -> float:
    """Protected length of a line pipe (DNV-RP-F103 Sec. 5.6.7, Eq. 14).

    ``PL = sqrt(delta_E_me * WT * (D - WT) / (rho_me * D * f_cf * i_cm))``

    Parameters
    ----------
    delta_E_me : float
        Metallic voltage drop [V] (0.15 V per Sec. 5.6.3).
    WT : float
        Wall thickness [m].
    D : float
        Outer diameter [m].
    rho_me : float
        Pipe steel resistivity [ohm-m].
    f_cf : float
        Final coating breakdown factor.
    i_cm : float
        Design mean current density [A/m2].
    edition : F103Edition, optional
        F103 edition token; ``None`` warns and defaults to 2010. Eq. 14 is
        the same in both editions.

    Returns
    -------
    float
        Protected length on each side of an anode [m].
    """
    _ = normalize_f103_edition(edition, stacklevel=3)
    for name, value in (
        ("delta_E_me", delta_E_me),
        ("WT", WT),
        ("D", D),
        ("rho_me", rho_me),
        ("f_cf", f_cf),
        ("i_cm", i_cm),
    ):
        if not math.isfinite(value) or value <= 0.0:
            raise ValueError(f"{name} must be a positive finite number, got {value!r}")
    if WT >= D:
        raise ValueError(f"WT must be less than D (WT={WT!r}, D={D!r})")
    return math.sqrt((delta_E_me * WT * (D - WT)) / (rho_me * D * f_cf * i_cm))


def _add_citation(labels: list[str], cited: CitedValue) -> None:
    label = citation_label(cited.citation)
    if label not in labels:
        labels.append(label)


def design_bracelet_cp(
    inp: BraceletDesignInput,
    edition: F103Edition | None = None,
) -> BraceletDesignResult:
    """Design bracelet anodes for a coated submarine pipeline per DNV-RP-F103.

    Parameters
    ----------
    inp : BraceletDesignInput
        Pipeline, coating, environment and bracelet data.
    edition : F103Edition, optional
        F103 edition token; ``None`` warns and defaults to 2010.

    Returns
    -------
    BraceletDesignResult
        Demands, mass, count, spacing, protected length and checks.
    """
    ed = normalize_f103_edition(edition, stacklevel=3)
    b401_ed = b401_edition_for_f103(ed)
    citations: list[str] = []

    area = inp.surface_area_m2
    fjc_fraction = inp.field_joint_fraction
    area_fjc = area * fjc_fraction
    area_lp = area - area_fjc

    i_cm = mean_current_density(inp.exposure, inp.fluid_temperature_c, ed)
    _add_citation(citations, i_cm)

    a_lp, b_lp = linepipe_coating_constants(inp.linepipe_coating, ed)
    _add_citation(citations, a_lp)
    f_cm_lp = kernel.coating_breakdown_mean(a_lp.value, b_lp.value, inp.design_life_years)
    f_cf_lp = kernel.coating_breakdown_final(a_lp.value, b_lp.value, inp.design_life_years)

    a_fj, b_fj = field_joint_coating_constants(inp.field_joint_coating, ed)
    if area_fjc > 0.0:
        _add_citation(citations, a_fj)
    f_cm_fj = kernel.coating_breakdown_mean(a_fj.value, b_fj.value, inp.design_life_years)
    f_cf_fj = kernel.coating_breakdown_final(a_fj.value, b_fj.value, inp.design_life_years)

    I_cm = kernel.current_demand(area_lp, i_cm.value, f_cm_lp) + kernel.current_demand(
        area_fjc, i_cm.value, f_cm_fj
    )
    I_cf = kernel.current_demand(area_lp, i_cm.value, f_cf_lp) + kernel.current_demand(
        area_fjc, i_cm.value, f_cf_fj
    )

    env = inp.resolved_anode_environment
    u = bracelet_utilisation_factor(ed)
    _add_citation(citations, u)
    eps = anode_capacity(inp.anode_material, env, b401_ed)
    _add_citation(citations, eps)
    delta_E = design_driving_voltage(inp.anode_material, b401_ed, env)
    _add_citation(citations, delta_E)

    total_mass = kernel.anode_mass(I_cm, inp.design_life_years, eps.value, u.value)
    n_mass = kernel.anode_count(total_mass, inp.bracelet_net_mass_kg)

    anode_area = inp.bracelet_area_m2
    R_a = kernel.short_flush_or_bracelet(inp.seawater_resistivity_ohm_m, anode_area)
    I_a = kernel.anode_current_output(delta_E.value, R_a)
    n_final = kernel.anodes_for_current(I_cf, I_a)

    n_anodes = max(1, n_mass, n_final)
    governing = GOVERNING_MASS if n_mass >= n_final else GOVERNING_FINAL
    spacing = inp.length_m / n_anodes

    # Eq. 14 with the area-weighted final breakdown factor: equals f_cf of the
    # linepipe coating when there is no field-joint area, and is larger (a
    # shorter, conservative protected length) when bare or lesser-coated
    # field joints share the surface.
    f_cf_effective = I_cf / (area * i_cm.value)
    PL = protected_length(
        inp.delta_E_me_V,
        inp.wall_thickness_m,
        inp.outer_diameter_m,
        inp.steel_resistivity_ohm_m,
        f_cf_effective,
        i_cm.value,
        edition=ed,
    )

    return BraceletDesignResult(
        surface_area_m2=area,
        linepipe_area_m2=area_lp,
        field_joint_area_m2=area_fjc,
        mean_current_density_A_m2=i_cm.value,
        f_cm_linepipe=f_cm_lp,
        f_cf_linepipe=f_cf_lp,
        f_cm_field_joint=f_cm_fj,
        f_cf_field_joint=f_cf_fj,
        mean_current_demand_A=I_cm,
        final_current_demand_A=I_cf,
        anode_capacity_Ah_kg=eps.value,
        utilisation_factor=u.value,
        driving_voltage_V=delta_E.value,
        total_net_mass_kg=total_mass,
        number_of_anodes=n_anodes,
        number_of_anodes_mass=n_mass,
        number_of_anodes_final=n_final,
        governing_case=governing,
        anode_spacing_m=spacing,
        protected_length_m=PL,
        spacing_ok=spacing <= 2.0 * PL,
        bracelet_exposed_area_m2=anode_area,
        anode_resistance_ohm=R_a,
        anode_current_output_A=I_a,
        current_output_ok=n_anodes * I_a >= I_cf,
        citations=citations,
        edition_used=ed,
        standard=f103_standard_for_edition(ed),
        provenance=edition_provenance(ed),
    )


__all__ = [
    "DEFAULT_METALLIC_VOLTAGE_DROP_V",
    "DEFAULT_SEAWATER_RESISTIVITY_OHM_M",
    "GOVERNING_FINAL",
    "GOVERNING_MASS",
    "STEEL_RESISTIVITY",
    "BraceletDesignInput",
    "BraceletDesignResult",
    "design_bracelet_cp",
    "protected_length",
]
