"""Tests for subsea cross-section validation rules."""

import pytest
from pydantic import ValidationError

from digitalmodel.subsea.cross_sections import (
    CrossSectionDefinition,
    PackedComponent,
    Provenance,
    RadialLayer,
    UnitValue,
    validate_cross_section,
)


def prov(source_id="source-1", source_type="wiki"):
    return Provenance(
        source_id=source_id,
        source_type=source_type,
        citation="Test source",
        url_or_path="knowledge/wikis/marine-engineering/wiki/sources/offshore-cable-umbilical-cross-section-recon-2026-04-26.md",
    )


def uv(value, unit="mm"):
    return UnitValue(value=value, unit=unit)


def layer(name, inner, outer, source_id="source-1"):
    return RadialLayer(
        name=name,
        role="test_role",
        material="test_material",
        inner_diameter=uv(inner),
        outer_diameter=uv(outer),
        provenance=prov(source_id),
    )


def valid_radial_definition(**overrides):
    data = {
        "id": "test-cable",
        "name": "Test Cable",
        "family": "offshore_wind_inter_array_cable",
        "duty": "static",
        "description": "test",
        "radial_layers": [layer("core", 0, 20), layer("sheath", 20, 30)],
        "packed_components": [],
        "provenance": [prov()],
    }
    data.update(overrides)
    return CrossSectionDefinition(**data)


def valid_packed_definition(**overrides):
    data = {
        "id": "test-umbilical",
        "name": "Test Umbilical",
        "family": "steel_tube_electro_hydraulic_umbilical",
        "duty": "dynamic",
        "description": "test",
        "radial_layers": [],
        "packed_components": [
            PackedComponent(
                name="tube",
                component_type="steel_tube",
                service_role="hydraulic",
                material="super duplex stainless steel",
                count=2,
                diameter=uv(12),
                wall_thickness=uv(1),
                pressure_rating=UnitValue(value=345, unit="bar"),
                provenance=prov(),
            )
        ],
        "provenance": [prov()],
    }
    data.update(overrides)
    return CrossSectionDefinition(**data)


def test_radial_layer_requires_two_geometry_terms():
    with pytest.raises(ValidationError):
        RadialLayer(
            name="bad",
            role="insulation",
            material="xlpe",
            thickness=uv(5),
            provenance=prov(),
        )


def test_radial_layer_triplet_consistency_checked():
    with pytest.raises(ValidationError):
        RadialLayer(
            name="bad",
            role="insulation",
            material="xlpe",
            inner_diameter=uv(10),
            outer_diameter=uv(30),
            thickness=uv(3),
            provenance=prov(),
        )


def test_radial_layer_gap_rejected():
    with pytest.raises(ValidationError):
        valid_radial_definition(radial_layers=[layer("core", 0, 20), layer("sheath", 21, 30)])


def test_radial_layer_overlap_rejected():
    with pytest.raises(ValidationError):
        valid_radial_definition(radial_layers=[layer("core", 0, 20), layer("sheath", 19, 30)])


def test_mixed_radial_length_units_rejected_or_converted():
    mixed = [
        layer("core", 0, 20),
        RadialLayer(
            name="sheath",
            role="jacket",
            material="polyethylene",
            inner_diameter=UnitValue(value=20, unit="mm"),
            outer_diameter=UnitValue(value=1.5, unit="inch"),
            provenance=prov("source-2"),
        ),
    ]
    with pytest.raises(ValidationError):
        valid_radial_definition(radial_layers=mixed)


def test_invalid_unsupported_family_rejected():
    with pytest.raises(ValidationError):
        valid_radial_definition(family="generic_cylinder")


def test_negative_thickness_rejected():
    with pytest.raises(ValidationError):
        RadialLayer(
            name="bad",
            role="jacket",
            material="polyethylene",
            inner_diameter=uv(10),
            thickness=uv(-1),
            provenance=prov(),
        )


def test_zero_component_count_rejected():
    with pytest.raises(ValidationError):
        PackedComponent(
            name="bad",
            component_type="tube",
            service_role="hydraulic",
            material="steel",
            count=0,
            provenance=prov(),
        )


def test_missing_component_provenance_rejected():
    with pytest.raises(ValidationError):
        PackedComponent(
            name="bad",
            component_type="tube",
            service_role="hydraulic",
            material="steel",
            count=1,
        )


def test_validation_report_has_stable_shape():
    report = validate_cross_section({"family": "generic_cylinder", "radial_layers": []})
    assert report.is_valid is False
    assert report.errors
    first_error = report.errors[0]
    assert first_error.code
    assert first_error.path
    assert first_error.message
    assert first_error.severity == "error"


def test_valid_packed_definition_accepts_components():
    definition = valid_packed_definition()
    assert definition.packed_components[0].service_role == "hydraulic"
