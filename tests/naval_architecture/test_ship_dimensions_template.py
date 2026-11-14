# ABOUTME: Tests for ship dimensions template loading and validation
# ABOUTME: Ensures templates can be parsed and merged into ship_data registry
"""Tests for ship dimension templates — loading and validation."""

import os
import re
import tempfile

import pytest
import yaml


SAMPLE_TEMPLATE = {
    "vessels": [
        {
            "hull_id": "DD-963",
            "name": "Spruance-class destroyer",
            "loa_ft": 563.0,
            "lwl_ft": 529.0,
            "beam_ft": 55.0,
            "draft_ft": 29.0,
            "displacement_lt": 7800.0,
            "source": "Jane's Fighting Ships",
        },
        {
            "hull_id": "CG-47",
            "name": "Ticonderoga-class cruiser",
            "loa_ft": 567.0,
            "lwl_ft": 529.0,
            "beam_ft": 55.0,
            "draft_ft": 31.0,
            "displacement_lt": 9600.0,
            "source": "Jane's Fighting Ships",
        },
    ]
}


@pytest.fixture
def template_header():
    from digitalmodel.naval_architecture.ship_dimensions import (
        default_dimension_template_path,
    )

    template = default_dimension_template_path().read_text(encoding="utf-8")
    header, separator, _ = template.partition("\nvessels:")
    assert separator, "The canonical template must contain vessel entries"
    return header


class TestTemplateLoading:
    """Load and validate ship dimension templates."""

    def test_default_template_documents_manual_curation_contract(self, template_header):
        header = template_header
        assert "canonical" in header.lower()
        assert "461" in header
        assert "manual" in header.lower()
        assert "load_dimension_template" in header
        assert "validate_vessel_entry" in header

        fields = {
            "hull_id", "name", "loa_ft", "lwl_ft", "beam_ft", "draft_ft",
            "depth_ft", "displacement_lt", "speed_kt", "source", "entry_status",
            "verification_source",
        }
        for field in fields:
            assert re.search(rf"\b{field}\b", header), f"Undocumented field: {field}"
        assert '"draft"' in header and '"verified"' in header
        assert "long tons" in header

    def test_documented_validator_requirements_match_runtime(self, template_header):
        from digitalmodel.naval_architecture.ship_dimensions import _REQUIRED_FIELDS

        required = template_header.split("# Required by the validator:\n", 1)[1]
        required = required.split("#\n", 1)[0]
        groups = re.findall(r"^#   ([a-z][a-z0-9_, ]*) \(", required, re.MULTILINE)
        fields = {field.strip() for group in groups for field in group.split(",")}
        assert fields == set(_REQUIRED_FIELDS)
        assert "hull_number" in required and "alias for hull_id" in required

    def test_manual_completeness_and_speed_convention(self, template_header):
        complete = template_header.split(
            "# Complete manual entry (not enforced by the validator):\n", 1
        )[1].split("#\n", 1)[0]
        fields = {field.strip() for field in complete.splitlines()[0][4:].split(",")}
        assert fields == {
            "loa_ft", "beam_ft", "draft_ft", "depth_ft", "displacement_lt", "speed_kt",
        }
        assert "depth_ft (positive float)" in complete
        assert "speed_kt (positive float): design speed" in complete
        assert "trial or maximum speed" in complete
        assert "Leave unknown values blank" in complete

    def test_documented_dimension_bases_and_references(self, template_header):
        """Check curation guidance; loading bases are not runtime validation."""
        basis = template_header.split("# Dimension basis for manual curation:\n", 1)[1]
        basis = basis.split("#\n", 1)[0]
        for guidance in (
            "displacement_lt: total ship weight at the source-stated design condition",
            "Record standard, lightship or full-load explicitly in source",
            "Do not substitute deadweight for displacement",
            "draft_ft: mean molded design draft at that same loading condition",
            "depth_ft: molded baseline to the uppermost continuous deck at side",
            "lwl_ft: waterline length at draft_ft and the same loading condition",
            "Leave a value blank when its basis is unknown",
            "The validator does not check loading condition or dimension basis",
        ):
            assert guidance in basis
        for chapter in ("02.02%20Chapter%202.pdf", "02.03%20Chapter%203.pdf"):
            reference = "https://www.usna.edu/NAOE/_files/documents/Courses/EN400/"
            assert reference + chapter in basis

    def test_superseded_generator_schema_mapping_documentation(self, template_header):
        """Check documentation text; no generator or migration is executed."""
        assert "Superseded generator (not canonical): workspace-hub" in template_header
        assert (
            "scripts/data/doc-intelligence/generate-ship-dimension-template.py"
            in template_header
        )
        assert (
            "scripts/data/doc_intelligence/generate_ship_dimension_template.py"
            in template_header
        )
        assert "entries -> vessels" in template_header
        assert "flatten dimensions:" in template_header
        assert "length_overall_ft -> loa_ft" in template_header
        assert "speed_kts -> speed_kt" in template_header
        assert "entry_status: pending -> draft" in template_header

    def test_generator_identifier_mapping_documentation(self, template_header):
        """Check identifier guidance; validator behavior is exercised separately."""
        assert "hull_number is an integer or null" in template_header
        assert (
            "hull_id = upper(hull_code) + '-' + str(hull_number)"
            in template_header
        )
        assert (
            "For null hull_number, resolve the identifier from the source"
            in template_header
        )
        assert "Do not copy the integer into the hull_number alias" in template_header

    def test_numbered_generator_migration_example_validates(self, template_header):
        from digitalmodel.naval_architecture.ship_dimensions import validate_vessel_entry

        example = re.search(
            r'^#   (\{hull_code:.*?\}) -> (\{hull_id:.*?\})$',
            template_header,
            re.MULTILINE,
        )
        assert example, "A numbered generator migration example is required"
        original = yaml.safe_load(example.group(1))
        migrated = yaml.safe_load(example.group(2))
        assert original == {"hull_code": "bb", "hull_number": 34}
        assert migrated == {"hull_id": "BB-34"}
        # Synthetic dimensions exercise the real validator, not source data.
        dimensions = {
            "name": "Migration example",
            "loa_ft": 100.0,
            "beam_ft": 20.0,
            "draft_ft": 8.0,
        }
        assert "Missing required field: hull_id" in validate_vessel_entry(
            {**dimensions, **original}
        )
        assert validate_vessel_entry({**dimensions, **migrated}) == []

    def test_load_default_template(self):
        from digitalmodel.naval_architecture.ship_dimensions import (
            load_dimension_template,
            validate_vessel_entry,
        )

        vessels = load_dimension_template()
        assert len(vessels) >= 1
        assert any(v["hull_id"] == "BB-42" for v in vessels)
        assert len({v["hull_id"] for v in vessels}) == len(vessels)
        for vessel in vessels:
            assert validate_vessel_entry(vessel) == []

    def test_load_template(self):
        from digitalmodel.naval_architecture.ship_dimensions import (
            load_dimension_template,
        )

        with tempfile.NamedTemporaryFile(
            mode="w", suffix=".yaml", delete=False
        ) as f:
            yaml.dump(SAMPLE_TEMPLATE, f)
            path = f.name

        try:
            vessels = load_dimension_template(path)
            assert len(vessels) == 2
            assert vessels[0]["hull_id"] == "DD-963"
        finally:
            os.unlink(path)

    def test_validate_required_fields(self):
        from digitalmodel.naval_architecture.ship_dimensions import (
            validate_vessel_entry,
        )

        valid = {
            "hull_id": "TEST-1",
            "name": "Test Ship",
            "loa_ft": 100.0,
            "beam_ft": 20.0,
            "draft_ft": 8.0,
        }
        errors = validate_vessel_entry(valid)
        assert len(errors) == 0

    def test_validate_missing_fields(self):
        from digitalmodel.naval_architecture.ship_dimensions import (
            validate_vessel_entry,
        )

        invalid = {"hull_id": "TEST-2"}
        errors = validate_vessel_entry(invalid)
        assert len(errors) >= 3  # missing name, loa, beam, draft

    def test_validate_negative_dimensions(self):
        from digitalmodel.naval_architecture.ship_dimensions import (
            validate_vessel_entry,
        )

        bad = {
            "hull_id": "TEST-3",
            "name": "Bad Ship",
            "loa_ft": -100.0,
            "beam_ft": 20.0,
            "draft_ft": 8.0,
        }
        errors = validate_vessel_entry(bad)
        assert any("loa_ft" in e for e in errors)

    def test_validate_hull_number_alias(self):
        from digitalmodel.naval_architecture.ship_dimensions import (
            validate_vessel_entry,
        )

        valid = {
            "hull_number": "TEST-4",
            "name": "Alias Ship",
            "loa_ft": 120.0,
            "beam_ft": 20.0,
            "draft_ft": 8.0,
        }
        errors = validate_vessel_entry(valid)
        assert len(errors) == 0


class TestMergeIntoRegistry:
    """Merge template data into ship_data registry."""

    def test_merge_adds_new_ships(self):
        from digitalmodel.naval_architecture.ship_dimensions import (
            merge_template_into_registry,
        )

        vessels = SAMPLE_TEMPLATE["vessels"]
        added, skipped = merge_template_into_registry(vessels)
        assert added >= 1  # at least DD-963 or CG-47 should be new

    def test_merge_skips_existing(self):
        from digitalmodel.naval_architecture.ship_dimensions import (
            merge_template_into_registry,
        )

        # DDG-51 already exists in registry
        vessels = [{"hull_id": "DDG-51", "name": "Existing"}]
        added, skipped = merge_template_into_registry(vessels)
        assert skipped == 1
        assert added == 0

    def test_merge_normalizes_registry_shape(self):
        from digitalmodel.naval_architecture.ship_data import get_ship
        from digitalmodel.naval_architecture.ship_dimensions import (
            merge_template_into_registry,
        )

        vessels = [
            {
                "hull_id": "LSD-41",
                "name": "Whidbey Island-class dock landing ship",
                "loa_ft": 610.0,
                "beam_ft": 84.0,
                "draft_ft": 20.0,
                "source": "NavSource Naval History",
            }
        ]
        added, skipped = merge_template_into_registry(vessels)
        assert added == 1
        assert skipped == 0

        ship = get_ship("LSD-41")
        assert ship is not None
        assert ship["hull_number"] == "LSD-41"
        assert "source" not in ship
