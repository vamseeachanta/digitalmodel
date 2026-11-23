"""Tests for subsea cross-section schema public API and model contracts."""

import math

import pytest
from pydantic import ValidationError

from digitalmodel.subsea.cross_sections import (
    CrossSectionDefinition,
    PackedComponent,
    Provenance,
    RadialLayer,
    UnitValue,
    ValidationIssue,
    ValidationReport,
    dump_cross_section_fixture,
    load_cross_section_fixture,
    validate_cross_section,
)
from digitalmodel.subsea.cross_sections.schema import DesignMetadata


def test_public_api_exports_expected_names():
    assert CrossSectionDefinition
    assert RadialLayer
    assert PackedComponent
    assert Provenance
    assert UnitValue
    assert ValidationIssue
    assert ValidationReport
    assert validate_cross_section
    assert load_cross_section_fixture
    assert dump_cross_section_fixture


def test_unit_value_requires_unit():
    with pytest.raises(ValidationError):
        UnitValue(value=10)


def test_unit_value_rejects_nonfinite():
    with pytest.raises(ValidationError):
        UnitValue(value=math.nan, unit="mm")


def test_unknown_unit_rejected():
    with pytest.raises(ValidationError):
        UnitValue(value=10, unit="mms")


def test_provenance_requires_source_id_and_type():
    with pytest.raises(ValidationError):
        Provenance(source_id="", source_type="wiki")


def test_calculation_provenance_requires_derived_from():
    with pytest.raises(ValidationError):
        Provenance(source_id="calc-1", source_type="calculation", derived_from=[])


def test_density_requires_density_units():
    with pytest.raises(ValidationError):
        RadialLayer(
            name="bad density",
            role="coating",
            material="concrete",
            inner_diameter=UnitValue(value=10, unit="mm"),
            outer_diameter=UnitValue(value=20, unit="mm"),
            density=UnitValue(value=3, unit="kV"),
            provenance=Provenance(source_id="source", source_type="project_assumption"),
        )


def test_component_rating_units_are_semantic():
    provenance = Provenance(source_id="source", source_type="project_assumption")
    with pytest.raises(ValidationError):
        PackedComponent(
            name="bad pressure",
            component_type="tube",
            service_role="hydraulic",
            material="steel",
            count=1,
            pressure_rating=UnitValue(value=5, unit="mm"),
            provenance=provenance,
        )
    with pytest.raises(ValidationError):
        PackedComponent(
            name="bad voltage",
            component_type="power_core",
            service_role="power",
            material="copper",
            count=1,
            voltage_rating=UnitValue(value=33, unit="degC"),
            provenance=provenance,
        )


def test_design_metadata_units_are_semantic():
    with pytest.raises(ValidationError):
        DesignMetadata(design_pressure=UnitValue(value=10, unit="mm"))
    with pytest.raises(ValidationError):
        DesignMetadata(design_temperature=UnitValue(value=20, unit="bar"))
    with pytest.raises(ValidationError):
        DesignMetadata(voltage_class=UnitValue(value=33, unit="degC"))
    with pytest.raises(ValidationError):
        DesignMetadata(water_depth=UnitValue(value=100, unit="kg/m"))
