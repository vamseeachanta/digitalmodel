# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Recorded fixture geometry and complete IO/schema/validation round trips.

Input source: bundled cross_sections/fixtures/*.yml, radial_layers and
packed_components. These are representative schema examples, not qualified
designs. The area coefficients below are independent hand calculations in mm²:
each is (OD²-ID²)/4; multiplying by pi gives area. Mass and elastic stiffness
are NOT_EVALUATED because the fixtures do not specify all densities and moduli.
"""

from copy import deepcopy
import math

import pytest

from digitalmodel.subsea.cross_sections.io import dump_cross_section_fixture, load_cross_section_fixture
from digitalmodel.subsea.cross_sections.reporting import fixture_paths
from digitalmodel.subsea.cross_sections.schema import CrossSectionDefinition
from digitalmodel.subsea.cross_sections.validation import _error_to_issue, validate_cross_section


# Hand arithmetic from the cited fixture diameter pairs; counts are literal sums.
EXPECTED = {
    "66kv_inter_array_cable.yml": ("offshore_wind_inter_array_cable", 130, [306.25, 1100, 1900, 918.75], 0),
    "220kv_hvac_export_cable.yml": ("offshore_wind_hvac_export_cable", 205, [841, 3255, 1988, 4422.25], 0),
    "concrete_coated_pipeline.yml": ("rigid_pipeline_flowline", 430, [22500, 3744, 981, 19000], 0),
    "power_optical_hybrid_umbilical.yml": ("power_optical_hybrid_umbilical", None, [], 4),
    "steel_tube_electro_hydraulic_umbilical.yml": ("steel_tube_electro_hydraulic_umbilical", None, [], 7),
}


@pytest.mark.parametrize("path", fixture_paths(), ids=lambda p: p.stem)
@pytest.mark.parametrize("extension", ["yml", "json"])
def test_all_fixtures_round_trip_with_recorded_results(path, extension, tmp_path):
    """Independent area oracle and exact full-model preservation, including sources."""
    original = load_cross_section_fixture(path)
    output = tmp_path / f"roundtrip.{extension}"
    dump_cross_section_fixture(original if extension == "yml" else original.to_serializable_dict(), output)
    restored = load_cross_section_fixture(output)
    assert restored.to_serializable_dict() == original.to_serializable_dict()
    assert validate_cross_section(restored).is_valid
    assert validate_cross_section(restored.to_serializable_dict()).is_valid
    family, od, coefficients, count = EXPECTED[path.name]
    assert restored.family == family
    assert sum(component.count for component in restored.packed_components) == count
    if od is not None:
        assert restored.radial_layers[-1].derived_outer_diameter.value == od
    for item, coefficient in zip(restored.radial_layers, coefficients, strict=True):
        inner, outer = item.derived_inner_diameter, item.derived_outer_diameter
        assert inner.unit == outer.unit == "mm"
        # Geometry-only independent check; no missing material property is filled.
        assert math.pi * (outer.value**2 - inner.value**2) / 4 == pytest.approx(coefficient * math.pi)


def valid_mapping():
    return load_cross_section_fixture(fixture_paths()[0]).to_serializable_dict()


@pytest.mark.parametrize("field", ["name", "role", "material"])
def test_blank_layer_text(field):
    data = valid_mapping()
    data["radial_layers"][0][field] = " "
    assert not validate_cross_section(data).is_valid


@pytest.mark.parametrize("field,value,unit", [
    ("inner_diameter", -1, "mm"), ("outer_diameter", 0, "mm"),
    ("thickness", 0, "mm"), ("outer_diameter", 10, "bar"),
    ("density", 0, "kg/m^3"), ("density", -1, "kg/m^3"),
])
def test_layer_scalar_rules(field, value, unit):
    data = valid_mapping()
    data["radial_layers"][0][field] = dict(value=value, unit=unit)
    assert not validate_cross_section(data).is_valid


@pytest.mark.parametrize("geometry", [
    dict(outer_diameter=dict(value=2, unit="m"), thickness=dict(value=2, unit="m")),
    dict(inner_diameter=dict(value=1, unit="m"), thickness=dict(value=2, unit="mm")),
    dict(outer_diameter=dict(value=2, unit="m"), thickness=dict(value=2, unit="mm")),
])
def test_derived_geometry_must_be_physical_and_same_unit(geometry):
    data = valid_mapping()
    original = data["radial_layers"][0]
    data["radial_layers"] = [{k: v for k, v in original.items()
                              if k not in {"inner_diameter", "outer_diameter", "thickness"}} | geometry]
    assert not validate_cross_section(data).is_valid


@pytest.mark.parametrize("change,message", [
    ("source", "not declared"), ("sources", "at least one provenance"),
    ("layers", "requires radial_layers"), ("unit", "mixed radial length units"),
])
def test_definition_rules(change, message):
    data = valid_mapping()
    if change == "source":
        data["radial_layers"][0]["provenance"]["source_id"] = "undeclared"
    elif change == "sources":
        data["provenance"] = []
    elif change == "layers":
        data["radial_layers"] = []
    else:
        for name in ("inner_diameter", "outer_diameter"):
            data["radial_layers"][1][name]["unit"] = "m"
    report = validate_cross_section(data)
    assert not report.is_valid
    assert any(message in error.message for error in report.errors)


@pytest.mark.parametrize("field,unit,value", [
    ("diameter", "bar", 1), ("wall_thickness", "mm", -1),
    ("pressure_rating", "bar", 0), ("voltage_rating", "kV", 0),
])
def test_component_rules(field, unit, value):
    data = load_cross_section_fixture(next(p for p in fixture_paths() if p.stem.startswith("steel"))).to_serializable_dict()
    data["packed_components"][0][field] = dict(value=value, unit=unit)
    assert not validate_cross_section(data).is_valid


def test_packed_sources_and_required_components():
    data = load_cross_section_fixture(next(p for p in fixture_paths() if p.stem.startswith("steel"))).to_serializable_dict()
    missing = deepcopy(data)
    missing["packed_components"] = []
    assert "requires packed_components" in validate_cross_section(missing).errors[0].message
    data["packed_components"][0]["provenance"]["source_id"] = "undeclared"
    assert "not declared" in validate_cross_section(data).errors[0].message


@pytest.mark.parametrize("field", ["name", "component_type", "service_role", "material"])
def test_blank_component_text(field):
    data = load_cross_section_fixture(next(p for p in fixture_paths() if p.stem.startswith("steel"))).to_serializable_dict()
    data["packed_components"][0][field] = " "
    assert not validate_cross_section(data).is_valid


@pytest.mark.parametrize("field", ["id", "name"])
def test_blank_definition_text(field):
    data = valid_mapping()
    data[field] = " "
    assert not validate_cross_section(data).is_valid


def test_unknown_definition_field():
    data = valid_mapping()
    data["unexpected"] = "value"
    assert validate_cross_section(data).errors[0].code == "extra_forbidden"


@pytest.mark.parametrize("field,value,unit", [
    ("water_depth", -1, "m"), ("design_pressure", 0, "bar"),
    ("voltage_class", -1, "V"),
])
def test_metadata_scalar_rules(field, value, unit):
    data = valid_mapping()
    data["design_metadata"][field] = dict(value=value, unit=unit)
    assert not validate_cross_section(data).is_valid


def test_validation_revalidates_mutated_model():
    definition = CrossSectionDefinition.model_validate(valid_mapping())
    definition.radial_layers[0].outer_diameter.value = -1
    report = validate_cross_section(definition)
    assert not report.is_valid
    assert report.errors[0].path == "radial_layers.0"
    assert report.errors[0].code == "value_error"
    assert report.errors[0].severity == "error"
    assert report.warnings == []
    assert report.summary == f"{len(report.errors)} validation error(s)"


def test_validation_error_fallback_and_nested_location():
    issue = _error_to_issue({})
    assert (issue.path, issue.code, issue.message) == ("definition", "validation_error", "validation error")
    assert _error_to_issue({"loc": ("radial_layers", 1, "density")}).path == "radial_layers.1.density"


@pytest.mark.parametrize("text", ["[]", "null", "42"])
def test_non_mapping_yaml_rejected(text, tmp_path):
    path = tmp_path / "invalid.yml"
    path.write_text(text)
    with pytest.raises(ValueError, match="must contain a mapping"):
        load_cross_section_fixture(path)
