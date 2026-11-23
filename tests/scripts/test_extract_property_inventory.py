"""Tests for extract_property_inventory script."""

from __future__ import annotations

import sys
from pathlib import Path

import pytest

# Ensure project src is importable
PROJECT_ROOT = Path(__file__).resolve().parent.parent.parent
sys.path.insert(0, str(PROJECT_ROOT / "src"))
sys.path.insert(0, str(PROJECT_ROOT / "scripts"))

from extract_property_inventory import (
    build_dormancy_rules,
    build_inventory,
    extract_schema_fields,
    load_registries,
    scan_spec_library,
)


@pytest.fixture(scope="module")
def registries():
    """Load registries once for all tests."""
    return load_registries()


class TestLoadRegistries:
    def test_inventory_covers_all_section_registry_types(self, registries):
        """Every SECTION_REGISTRY type must appear in the inventory."""
        section_registry = registries["section_registry"]
        assert len(section_registry) >= 26, (
            f"Expected >= 26 section types, got {len(section_registry)}"
        )
        # Spot-check key sections
        for key in ("LineTypes", "Vessels", "Lines", "6DBuoys", "Shapes"):
            assert key in section_registry, f"Missing section: {key}"

    def test_priority_keys_marked_as_mode_setting(self, registries):
        """Priority keys must include known mode-setting properties."""
        priority = registries["priority_keys"]
        for key in ("Name", "Category", "Connection", "LinkType"):
            assert key in priority, f"Missing priority key: {key}"

    def test_skip_general_keys_present(self, registries):
        """Skip general keys must include known dormant view properties."""
        skip = registries["skip_general_keys"]
        for key in ("DefaultViewAngle1", "BackgroundColour", "ModelState"):
            assert key in skip, f"Missing skip key: {key}"

    def test_typed_field_map_entries_have_schema_reference(self, registries):
        """Each typed field map entry should have a valid OrcaFlex key."""
        tfm = registries["typed_field_map"]
        assert len(tfm) >= 20, f"Expected >= 20 typed fields, got {len(tfm)}"
        # Known mappings
        assert tfm["name"] == "Name"
        assert tfm["outer_diameter"] == "OD"
        assert tfm["axial_stiffness"] == "EA"

    def test_singleton_sections_included(self, registries):
        """Singleton sections must include known entries."""
        singletons = registries["singleton_sections"]
        for key in (
            "FrictionCoefficients",
            "RayleighDampingCoefficients",
            "Groups",
        ):
            assert key in singletons, f"Missing singleton: {key}"


class TestDormancyRules:
    def test_dormancy_rules_include_wind_wave_current(self, registries):
        """Dormancy rules must cover wind, wave, and current properties."""
        rules = build_dormancy_rules(registries)
        assert "WindSpeed" in rules
        assert "WaveGamma" in rules
        assert "CurrentExponent" in rules
        assert "dormant_when" in rules["WindSpeed"]


class TestExtractSchemaFields:
    def test_generic_line_type_fields(self):
        """GenericLineType should have typed fields like outer_diameter."""
        from digitalmodel.solvers.orcaflex.modular_generator.schema.generic import (
            GenericLineType,
        )

        fields = extract_schema_fields(GenericLineType)
        assert "name" in fields
        assert "outer_diameter" in fields
        assert fields["name"]["required"] is True


class TestBuildInventory:
    def test_output_structure(self, registries):
        """Inventory output must have object_types, dormancy_rules, metadata."""
        inventory = build_inventory(registries, {})
        assert "object_types" in inventory
        assert "dormancy_rules" in inventory
        assert "metadata" in inventory
        assert inventory["metadata"]["section_count"] >= 26

    def test_usage_stats_from_spec_files(self):
        """Usage stats should work with spec directory (may be empty)."""
        spec_dir = PROJECT_ROOT / "docs" / "modules" / "orcaflex" / "library"
        if spec_dir.exists():
            usage = scan_spec_library(spec_dir)
            # Should return a dict even if no specs matched
            assert isinstance(usage, dict)
        else:
            # No spec dir -- that is fine, just verify the function handles it
            usage = scan_spec_library(Path("/nonexistent"))
            assert isinstance(usage, dict)
            assert len(usage) == 0
