"""Tests for source-backed subsea cross-section fixtures and I/O."""

from importlib import resources

from digitalmodel.subsea.cross_sections import (
    CrossSectionDefinition,
    dump_cross_section_fixture,
    load_cross_section_fixture,
    validate_cross_section,
)

FIXTURES = [
    ("66kv_inter_array_cable.yml", "offshore_wind_inter_array_cable"),
    ("220kv_hvac_export_cable.yml", "offshore_wind_hvac_export_cable"),
    ("steel_tube_electro_hydraulic_umbilical.yml", "steel_tube_electro_hydraulic_umbilical"),
    ("power_optical_hybrid_umbilical.yml", "power_optical_hybrid_umbilical"),
    ("concrete_coated_pipeline.yml", "rigid_pipeline_flowline"),
]

KNOWN_SOURCE_IDS = {
    "wiki-offshore-cross-section-assessment",
    "wiki-cross-section-recon-2026-04-26",
    "dnv-st-f101-registry",
    "project-assumption-2514",
    "prysmian-66kv-example",
    "guide-floating-offshore-wind-66kv",
    "guide-floating-offshore-wind-220kv",
    "sut-umbilical-reference",
    "prysmian-power-optical-umbilical",
    "vallourec-coating-reference",
    "octal-concrete-weight-coating-reference",
}


def fixture_path(name):
    return resources.files("digitalmodel.subsea.cross_sections.fixtures").joinpath(name)


def test_valid_66kv_inter_array_cable_layers():
    definition = load_cross_section_fixture(fixture_path("66kv_inter_array_cable.yml"))
    assert definition.family == "offshore_wind_inter_array_cable"
    assert definition.radial_layers
    assert definition.design_metadata.voltage_class.value == 66


def test_valid_220kv_hvac_export_cable_layers():
    definition = load_cross_section_fixture(fixture_path("220kv_hvac_export_cable.yml"))
    assert definition.family == "offshore_wind_hvac_export_cable"
    assert definition.design_metadata.voltage_class.value == 220
    assert definition.radial_layers[-1].outer_diameter.value > definition.radial_layers[0].outer_diameter.value


def test_valid_steel_tube_umbilical_packed_components():
    definition = load_cross_section_fixture(fixture_path("steel_tube_electro_hydraulic_umbilical.yml"))
    roles = {component.service_role for component in definition.packed_components}
    assert {"hydraulic", "electrical", "fiber_optic"}.issubset(roles)


def test_valid_power_optical_hybrid_umbilical():
    definition = load_cross_section_fixture(fixture_path("power_optical_hybrid_umbilical.yml"))
    roles = {component.service_role for component in definition.packed_components}
    assert {"power", "fiber_optic"}.issubset(roles)


def test_valid_concrete_coated_pipeline_layers():
    definition = load_cross_section_fixture(fixture_path("concrete_coated_pipeline.yml"))
    assert definition.family == "rigid_pipeline_flowline"
    assert [layer.role for layer in definition.radial_layers] == [
        "bore",
        "line_pipe",
        "corrosion_coating",
        "concrete_weight_coating",
    ]


def test_every_fixture_layer_provenance_resolves_to_known_source_page():
    for filename, _family in FIXTURES:
        definition = load_cross_section_fixture(fixture_path(filename))
        fixture_sources = {source.source_id for source in definition.provenance}
        assert fixture_sources <= KNOWN_SOURCE_IDS
        for layer in definition.radial_layers:
            assert layer.provenance.source_id in fixture_sources
        for component in definition.packed_components:
            assert component.provenance.source_id in fixture_sources


def test_fixture_roundtrip_preserves_family_units_and_provenance(tmp_path):
    source = load_cross_section_fixture(fixture_path("66kv_inter_array_cable.yml"))
    output = tmp_path / "roundtrip.yml"
    dump_cross_section_fixture(source, output)
    roundtrip = load_cross_section_fixture(output)
    assert roundtrip.family == source.family
    assert roundtrip.radial_layers[0].outer_diameter.unit == source.radial_layers[0].outer_diameter.unit
    assert [p.source_id for p in roundtrip.provenance] == [p.source_id for p in source.provenance]


def test_fixture_package_data_available_after_install_metadata():
    fixture_names = {path.name for path in resources.files("digitalmodel.subsea.cross_sections.fixtures").iterdir()}
    assert {name for name, _family in FIXTURES}.issubset(fixture_names)
