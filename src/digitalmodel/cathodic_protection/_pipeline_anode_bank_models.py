"""Typed input and result contracts for terminal anode-bank design."""

from __future__ import annotations

import math
from typing import Any

from pydantic import BaseModel, Field, model_validator


class PhaseValues(BaseModel):
    initial: float
    mean: float
    final: float


class StructureDemandInput(BaseModel):
    area_m2: float = Field(ge=0)
    initial_current_density_A_m2: float = Field(ge=0)
    mean_current_density_A_m2: float = Field(ge=0)
    final_current_density_A_m2: float = Field(ge=0)
    initial_breakdown_factor: float = Field(ge=0, le=1)
    mean_breakdown_factor: float = Field(ge=0, le=1)
    final_breakdown_factor: float = Field(ge=0, le=1)


class BankAnodeInput(BaseModel):
    material: str
    net_mass_kg: float = Field(gt=0)
    length_m: float = Field(gt=0)
    density_kg_m3: float = Field(gt=0)
    utilisation_factor: float = Field(gt=0, lt=1)
    electrolyte_resistivity_ohm_m: float = Field(gt=0)
    surface_temperature_c: float = 10.0
    interaction_factor: float = Field(default=1.0, ge=1.0)
    cable_resistance_ohm: float = Field(default=0.0, ge=0)


class PipelineSideInput(BaseModel):
    side_id: str = Field(min_length=1)
    outer_diameter_m: float = Field(gt=0)
    wall_thickness_m: float = Field(gt=0)
    length_m: float = Field(gt=0)
    linepipe_coating: str
    exposure: str
    fluid_temperature_c: float
    steel_resistivity_ohm_m: float = Field(default=2.0e-7, gt=0)
    concrete_weight_coating: bool = False
    field_joint_coating: str | None = None
    field_joint_area_fraction: float = Field(default=0.0, ge=0, lt=1)
    field_joint_count: int | None = Field(default=None, ge=0)
    field_joint_length_m: float | None = Field(default=None, ge=0)

    @model_validator(mode="after")
    def validate_joint_area(self) -> "PipelineSideInput":
        if self.field_joint_count is not None:
            if self.field_joint_length_m is None:
                raise ValueError("field_joint_length_m is required with field_joint_count")
            fraction = self.field_joint_count * self.field_joint_length_m / self.length_m
            if self.field_joint_area_fraction and not math.isclose(
                fraction, self.field_joint_area_fraction, rel_tol=1e-9
            ):
                raise ValueError("field-joint area inputs disagree")
            self.field_joint_area_fraction = fraction
        if self.field_joint_area_fraction >= 1.0:
            raise ValueError("field-joint length must be less than total length")
        if self.field_joint_area_fraction > 0 and self.field_joint_coating is None:
            raise ValueError("field_joint_coating is required for nonzero field-joint area")
        return self


class BankInput(BaseModel):
    bank_id: str = Field(min_length=1)
    installed_anode_count: int = Field(gt=0)
    structure: StructureDemandInput
    anode: BankAnodeInput
    sides: list[PipelineSideInput] = Field(min_length=1)


class AnodeBankDesignInput(BaseModel):
    design_life_years: float = Field(gt=0)
    banks: list[BankInput] = Field(min_length=1)
    edition: str = "2019"
    max_anode_count: int = Field(default=10000, gt=0)


class CoatingResult(BaseModel):
    linepipe_initial_factor: float
    linepipe_mean_factor: float
    linepipe_final_factor: float
    field_joint_initial_factor: float
    field_joint_mean_factor: float
    field_joint_final_factor: float
    field_joint_to_linepipe_ratio: float


class EnvelopeResult(BaseModel):
    distance_m: list[float]
    potential_V: list[float]


class SideResult(BaseModel):
    side_id: str
    geometry_m: dict[str, float]
    coating: CoatingResult
    current_demand_A: PhaseValues
    effective_final_breakdown_factor: float
    longitudinal_resistance_ohm_m: float
    f103_eq20_protected_length_m: float
    extended_protected_length_m: float | None
    far_potential_V: float
    protection_margin_V: float
    protection_ok: bool
    potential_envelope: EnvelopeResult


class ResistanceResult(BaseModel):
    individual: PhaseValues
    parallel: PhaseValues
    total: PhaseValues
    interaction_factor: float
    cable: float
    group_formula: str


class DemandResult(BaseModel):
    structure: PhaseValues
    pipeline: PhaseValues
    total: PhaseValues


class StatusResult(BaseModel):
    result: str
    governing_case: str
    reason: str
    checks: dict[str, bool]
    use_status: str | None = None


class BankResult(BaseModel):
    bank_id: str
    current_demand_A: DemandResult
    anode_resistance_ohm: ResistanceResult
    anode_requirements: dict[str, Any]
    sides: list[SideResult]
    check_scores: dict[str, float]
    status: StatusResult


class AnodeBankDesignResult(BaseModel):
    standard: str
    edition: str
    provenance: str
    design_life_years: float
    banks: list[BankResult]
    citations: list[str]
    formula_references: list[str]
    model_limitations: list[str]
    status: StatusResult
