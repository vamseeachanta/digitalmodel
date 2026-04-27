"""Pydantic v2 schema models for subsea product cross-sections."""

from __future__ import annotations

import math
from typing import Any, Literal

from pydantic import BaseModel, ConfigDict, Field, field_validator, model_validator

LengthUnit = Literal["mm", "m", "inch"]
AllowedUnit = Literal[
    "mm",
    "m",
    "inch",
    "kg/m^3",
    "kV",
    "V",
    "bar",
    "MPa",
    "degC",
    "kN/m",
    "kg/m",
]
SourceType = Literal[
    "wiki",
    "standard",
    "vendor_catalogue",
    "project_assumption",
    "calculation",
]
Duty = Literal["static", "dynamic", "transition"]
Family = Literal[
    "offshore_wind_inter_array_cable",
    "offshore_wind_hvac_export_cable",
    "offshore_wind_hvdc_export_cable",
    "steel_tube_electro_hydraulic_umbilical",
    "thermoplastic_electro_hydraulic_umbilical",
    "power_optical_hybrid_umbilical",
    "rigid_pipeline_flowline",
]
Severity = Literal["error", "warning"]

LENGTH_UNITS = {"mm", "m", "inch"}
DENSITY_UNITS = {"kg/m^3"}
PRESSURE_UNITS = {"bar", "MPa"}
TEMPERATURE_UNITS = {"degC"}
VOLTAGE_UNITS = {"kV", "V"}
TOLERANCE = 1e-6


class UnitValue(BaseModel):
    """Scalar engineering value with an explicit controlled unit string."""

    model_config = ConfigDict(extra="forbid")

    value: float
    unit: AllowedUnit

    @field_validator("value")
    @classmethod
    def value_must_be_finite(cls, value: float) -> float:
        if not math.isfinite(value):
            raise ValueError("value must be finite")
        return value

    def require_positive(self, field_name: str) -> None:
        if self.value <= 0:
            raise ValueError(f"{field_name} must be positive")

    def require_nonnegative(self, field_name: str) -> None:
        if self.value < 0:
            raise ValueError(f"{field_name} must be non-negative")


class Provenance(BaseModel):
    """Traceability record for fixture data and derived values."""

    model_config = ConfigDict(extra="forbid")

    source_id: str
    source_type: SourceType
    citation: str | None = None
    url_or_path: str | None = None
    note: str | None = None
    derived_from: list[str] = Field(default_factory=list)

    @field_validator("source_id")
    @classmethod
    def source_id_must_not_be_blank(cls, value: str) -> str:
        if not value.strip():
            raise ValueError("source_id is required")
        return value

    @model_validator(mode="after")
    def calculation_requires_derived_from(self) -> "Provenance":
        if self.source_type == "calculation" and not self.derived_from:
            raise ValueError("calculation provenance requires derived_from")
        return self


class RadialLayer(BaseModel):
    """Ordered radial layer in a cable or pipeline stack."""

    model_config = ConfigDict(extra="forbid")

    name: str
    role: str
    material: str
    inner_diameter: UnitValue | None = None
    outer_diameter: UnitValue | None = None
    thickness: UnitValue | None = None
    density: UnitValue | None = None
    provenance: Provenance

    @field_validator("name", "role", "material")
    @classmethod
    def text_must_not_be_blank(cls, value: str) -> str:
        if not value.strip():
            raise ValueError("text field is required")
        return value

    @model_validator(mode="after")
    def validate_geometry(self) -> "RadialLayer":
        geometry = [self.inner_diameter, self.outer_diameter, self.thickness]
        if sum(item is not None for item in geometry) < 2:
            raise ValueError(
                "radial layer requires at least two of inner_diameter, outer_diameter, thickness"
            )
        for field_name in ("inner_diameter", "outer_diameter", "thickness"):
            value = getattr(self, field_name)
            if value is None:
                continue
            if value.unit not in LENGTH_UNITS:
                raise ValueError(f"{field_name} must use a length unit")
            if field_name == "inner_diameter":
                value.require_nonnegative(field_name)
            else:
                value.require_positive(field_name)
        if self.inner_diameter and self.outer_diameter and self.thickness:
            self._require_same_length_units(
                [self.inner_diameter, self.outer_diameter, self.thickness]
            )
            expected = self.inner_diameter.value + 2 * self.thickness.value
            if abs(self.outer_diameter.value - expected) > TOLERANCE:
                raise ValueError("outer_diameter must equal inner_diameter + 2 * thickness")
        if self.inner_diameter and self.outer_diameter:
            if self.inner_diameter.unit == self.outer_diameter.unit:
                if self.outer_diameter.value <= self.inner_diameter.value:
                    raise ValueError("outer_diameter must be greater than inner_diameter")
        if self.density is not None:
            if self.density.unit not in DENSITY_UNITS:
                raise ValueError("density must use kg/m^3")
            self.density.require_positive("density")
        return self

    @staticmethod
    def _require_same_length_units(values: list[UnitValue]) -> None:
        units = {value.unit for value in values if value is not None}
        if len(units) > 1:
            raise ValueError("radial dimensions must use the same length unit")

    @property
    def derived_inner_diameter(self) -> UnitValue | None:
        if self.inner_diameter is not None:
            return self.inner_diameter
        if self.outer_diameter is not None and self.thickness is not None:
            return UnitValue(
                value=self.outer_diameter.value - 2 * self.thickness.value,
                unit=self.outer_diameter.unit,
            )
        return None

    @property
    def derived_outer_diameter(self) -> UnitValue | None:
        if self.outer_diameter is not None:
            return self.outer_diameter
        if self.inner_diameter is not None and self.thickness is not None:
            return UnitValue(
                value=self.inner_diameter.value + 2 * self.thickness.value,
                unit=self.inner_diameter.unit,
            )
        return None


class PackedComponent(BaseModel):
    """Non-radial packed component used in umbilical bundle schemas."""

    model_config = ConfigDict(extra="forbid")

    name: str
    component_type: str
    service_role: str
    material: str
    count: int = Field(ge=1)
    diameter: UnitValue | None = None
    wall_thickness: UnitValue | None = None
    pressure_rating: UnitValue | None = None
    voltage_rating: UnitValue | None = None
    provenance: Provenance

    @field_validator("name", "component_type", "service_role", "material")
    @classmethod
    def text_must_not_be_blank(cls, value: str) -> str:
        if not value.strip():
            raise ValueError("text field is required")
        return value

    @model_validator(mode="after")
    def validate_geometry(self) -> "PackedComponent":
        for field_name in ("diameter", "wall_thickness"):
            value = getattr(self, field_name)
            if value is not None:
                if value.unit not in LENGTH_UNITS:
                    raise ValueError(f"{field_name} must use a length unit")
                value.require_positive(field_name)
        if self.pressure_rating is not None:
            if self.pressure_rating.unit not in PRESSURE_UNITS:
                raise ValueError("pressure_rating must use bar or MPa")
            self.pressure_rating.require_positive("pressure_rating")
        if self.voltage_rating is not None:
            if self.voltage_rating.unit not in VOLTAGE_UNITS:
                raise ValueError("voltage_rating must use kV or V")
            self.voltage_rating.require_positive("voltage_rating")
        return self


class DesignMetadata(BaseModel):
    """Optional design metadata attached to a cross-section definition."""

    model_config = ConfigDict(extra="forbid")

    water_depth: UnitValue | None = None
    design_temperature: UnitValue | None = None
    design_pressure: UnitValue | None = None
    voltage_class: UnitValue | None = None
    notes: str | None = None
    extra_metadata: dict[str, str] = Field(default_factory=dict)

    @model_validator(mode="after")
    def validate_metadata_units(self) -> "DesignMetadata":
        if self.water_depth is not None:
            if self.water_depth.unit not in LENGTH_UNITS:
                raise ValueError("water_depth must use a length unit")
            self.water_depth.require_nonnegative("water_depth")
        if self.design_temperature is not None:
            if self.design_temperature.unit not in TEMPERATURE_UNITS:
                raise ValueError("design_temperature must use degC")
        if self.design_pressure is not None:
            if self.design_pressure.unit not in PRESSURE_UNITS:
                raise ValueError("design_pressure must use bar or MPa")
            self.design_pressure.require_positive("design_pressure")
        if self.voltage_class is not None:
            if self.voltage_class.unit not in VOLTAGE_UNITS:
                raise ValueError("voltage_class must use kV or V")
            self.voltage_class.require_positive("voltage_class")
        return self


class ValidationIssue(BaseModel):
    """Stable validation issue shape for downstream automation."""

    model_config = ConfigDict(extra="forbid")

    code: str
    path: str
    message: str
    severity: Severity = "error"


class ValidationReport(BaseModel):
    """Stable validation report contract."""

    model_config = ConfigDict(extra="forbid")

    is_valid: bool
    errors: list[ValidationIssue] = Field(default_factory=list)
    warnings: list[ValidationIssue] = Field(default_factory=list)
    summary: str


class CrossSectionDefinition(BaseModel):
    """Complete source-backed cross-section definition."""

    model_config = ConfigDict(extra="forbid")

    id: str
    name: str
    family: Family
    duty: Duty
    description: str | None = None
    design_metadata: DesignMetadata = Field(default_factory=DesignMetadata)
    radial_layers: list[RadialLayer] = Field(default_factory=list)
    packed_components: list[PackedComponent] = Field(default_factory=list)
    provenance: list[Provenance]

    @field_validator("id", "name")
    @classmethod
    def text_must_not_be_blank(cls, value: str) -> str:
        if not value.strip():
            raise ValueError("text field is required")
        return value

    @model_validator(mode="after")
    def validate_definition(self) -> "CrossSectionDefinition":
        source_ids = {source.source_id for source in self.provenance}
        if not source_ids:
            raise ValueError("at least one provenance source is required")
        for index, layer in enumerate(self.radial_layers):
            if layer.provenance.source_id not in source_ids:
                raise ValueError(f"radial_layers[{index}] provenance source_id is not declared")
        for index, component in enumerate(self.packed_components):
            if component.provenance.source_id not in source_ids:
                raise ValueError(
                    f"packed_components[{index}] provenance source_id is not declared"
                )
        if self.family in {
            "offshore_wind_inter_array_cable",
            "offshore_wind_hvac_export_cable",
            "offshore_wind_hvdc_export_cable",
            "rigid_pipeline_flowline",
        } and not self.radial_layers:
            raise ValueError(f"family {self.family} requires radial_layers")
        if self.family in {
            "steel_tube_electro_hydraulic_umbilical",
            "thermoplastic_electro_hydraulic_umbilical",
            "power_optical_hybrid_umbilical",
        } and not self.packed_components:
            raise ValueError(f"family {self.family} requires packed_components")
        self._validate_radial_contiguity()
        return self

    def _validate_radial_contiguity(self) -> None:
        previous_outer: UnitValue | None = None
        for index, layer in enumerate(self.radial_layers):
            inner = layer.derived_inner_diameter
            outer = layer.derived_outer_diameter
            if inner is None or outer is None:
                raise ValueError(f"radial_layers[{index}] geometry could not be materialized")
            if inner.unit != outer.unit:
                raise ValueError(f"radial_layers[{index}] mixed length units are invalid")
            if previous_outer is not None:
                if inner.unit != previous_outer.unit:
                    raise ValueError("mixed radial length units are invalid")
                if abs(inner.value - previous_outer.value) > TOLERANCE:
                    relation = "gap" if inner.value > previous_outer.value else "overlap"
                    raise ValueError(f"radial layer {relation} at index {index}")
            previous_outer = outer

    def to_serializable_dict(self) -> dict[str, Any]:
        return self.model_dump(mode="json", exclude_none=True)
