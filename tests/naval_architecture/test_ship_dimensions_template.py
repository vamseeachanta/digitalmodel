# ABOUTME: Tests for ship dimensions template loading and validation
# ABOUTME: Ensures templates can be parsed and merged into ship_data registry
"""Tests for ship dimension templates — loading and validation."""

import os
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


class TestTemplateLoading:
    """Load and validate ship dimension templates."""

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
