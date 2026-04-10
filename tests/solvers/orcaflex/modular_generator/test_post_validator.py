"""Tests for post-generation cross-builder validation.

Validates that PostGenerationValidator correctly detects:
- Missing line type references
- Missing vessel references in line connections
- Duplicate object names
- Invalid winch vessel references
- Valid built-in connection targets (Anchored, Fixed, etc.)
"""

from __future__ import annotations

import pytest

from digitalmodel.solvers.orcaflex.modular_generator.post_validator import (
    PostGenerationValidator,
    ValidationWarning,
)


@pytest.fixture
def validator() -> PostGenerationValidator:
    return PostGenerationValidator()


def _make_valid_model() -> dict[str, dict]:
    """Build a minimal valid model with consistent cross-references."""
    return {
        "04_line_types.yml": {
            "LineTypes": [
                {"Name": "Chain_LT"},
                {"Name": "Wire_LT"},
            ],
        },
        "05_vessel_types.yml": {
            "VesselTypes": [
                {"Name": "Barge Type"},
            ],
        },
        "06_vessels.yml": {
            "Vessels": [
                {"Name": "Barge1", "VesselType": "Barge Type"},
            ],
        },
        "07_lines.yml": {
            "Lines": [
                {
                    "Name": "Mooring1",
                    "LineType, Length, TargetSegmentLength": [
                        ["Chain_LT", 100, 5],
                        ["Wire_LT", 200, 5],
                    ],
                    "Connection, ConnectionX, ConnectionY, ConnectionZ, "
                    "ConnectionAzimuth, ConnectionDeclination, ConnectionGamma, "
                    "ConnectionReleaseStage, ConnectionzRelativeTo": [
                        ["Barge1", 0, 0, 0, 0, 90, 0, None],
                        ["Anchored", 100, 0, 0, 0, 90, 0, None],
                    ],
                },
            ],
        },
        "11_winches.yml": {
            "Winches": [
                {
                    "Name": "Tensioner1",
                    "Connection": ["Barge1", "Mooring1"],
                },
            ],
        },
    }


class TestValidModel:
    """Tests with a valid model (expect zero warnings)."""

    def test_valid_model_no_warnings(self, validator: PostGenerationValidator):
        files = _make_valid_model()
        warnings = validator.validate_data(files)
        assert warnings == []

    def test_anchored_connection_no_warning(self, validator: PostGenerationValidator):
        """'Anchored' is a built-in OrcaFlex keyword, not a user object."""
        files = {
            "04_line_types.yml": {
                "LineTypes": [{"Name": "Chain_LT"}],
            },
            "07_lines.yml": {
                "Lines": [
                    {
                        "Name": "Line1",
                        "LineType, Length, TargetSegmentLength": [
                            ["Chain_LT", 100, 5],
                        ],
                        "Connection, ConnectionX, ConnectionY, ConnectionZ, "
                        "ConnectionAzimuth, ConnectionDeclination, ConnectionGamma, "
                        "ConnectionReleaseStage, ConnectionzRelativeTo": [
                            ["Fixed", 0, 0, 0, 0, 90, 0, None],
                            ["Anchored", 100, 0, 0, 0, 90, 0, None],
                        ],
                    },
                ],
            },
        }
        warnings = validator.validate_data(files)
        assert warnings == []

    def test_fixed_connection_no_warning(self, validator: PostGenerationValidator):
        """'Fixed' is a built-in OrcaFlex keyword."""
        files = {
            "04_line_types.yml": {
                "LineTypes": [{"Name": "Chain_LT"}],
            },
            "07_lines.yml": {
                "Lines": [
                    {
                        "Name": "Line1",
                        "LineType, Length, TargetSegmentLength": [
                            ["Chain_LT", 100, 5],
                        ],
                        "Connection, ConnectionX, ConnectionY, ConnectionZ, "
                        "ConnectionAzimuth, ConnectionDeclination, ConnectionGamma, "
                        "ConnectionReleaseStage, ConnectionzRelativeTo": [
                            ["Fixed", 0, 0, 0, 0, 90, 0, None],
                            ["Fixed", 100, 0, 0, 0, 90, 0, None],
                        ],
                    },
                ],
            },
        }
        warnings = validator.validate_data(files)
        assert warnings == []


class TestLineTypeReferences:
    """Tests for missing line type references."""

    def test_missing_line_type_error(self, validator: PostGenerationValidator):
        files = {
            "04_line_types.yml": {
                "LineTypes": [{"Name": "Chain_LT"}],
            },
            "07_lines.yml": {
                "Lines": [
                    {
                        "Name": "Line1",
                        "LineType, Length, TargetSegmentLength": [
                            ["Chain_LT", 100, 5],
                            ["NONEXISTENT_LT", 200, 5],
                        ],
                        "Connection, ConnectionX, ConnectionY, ConnectionZ, "
                        "ConnectionAzimuth, ConnectionDeclination, ConnectionGamma, "
                        "ConnectionReleaseStage, ConnectionzRelativeTo": [
                            ["Fixed", 0, 0, 0, 0, 90, 0, None],
                            ["Anchored", 100, 0, 0, 0, 90, 0, None],
                        ],
                    },
                ],
            },
        }
        warnings = validator.validate_data(files)
        errors = [w for w in warnings if w.level == "error" and w.category == "reference"]
        assert len(errors) == 1
        assert "NONEXISTENT_LT" in errors[0].message
        assert "Line1" in errors[0].message

    def test_no_line_types_section_skips_check(self, validator: PostGenerationValidator):
        """If no LineTypes section exists at all, skip the check."""
        files = {
            "07_lines.yml": {
                "Lines": [
                    {
                        "Name": "Line1",
                        "LineType, Length, TargetSegmentLength": [
                            ["AnyType", 100, 5],
                        ],
                        "Connection, ConnectionX, ConnectionY, ConnectionZ, "
                        "ConnectionAzimuth, ConnectionDeclination, ConnectionGamma, "
                        "ConnectionReleaseStage, ConnectionzRelativeTo": [
                            ["Fixed", 0, 0, 0, 0, 90, 0, None],
                            ["Anchored", 100, 0, 0, 0, 90, 0, None],
                        ],
                    },
                ],
            },
        }
        warnings = validator.validate_data(files)
        lt_errors = [
            w for w in warnings
            if w.category == "reference" and "LineType" in w.message
        ]
        assert lt_errors == []


class TestVesselReferences:
    """Tests for vessel reference validation in line connections."""

    def test_missing_vessel_connection_error(self, validator: PostGenerationValidator):
        files = {
            "04_line_types.yml": {
                "LineTypes": [{"Name": "Chain_LT"}],
            },
            "07_lines.yml": {
                "Lines": [
                    {
                        "Name": "Mooring1",
                        "LineType, Length, TargetSegmentLength": [
                            ["Chain_LT", 100, 5],
                        ],
                        "Connection, ConnectionX, ConnectionY, ConnectionZ, "
                        "ConnectionAzimuth, ConnectionDeclination, ConnectionGamma, "
                        "ConnectionReleaseStage, ConnectionzRelativeTo": [
                            ["GhostVessel", 0, 0, 0, 0, 90, 0, None],
                            ["Anchored", 100, 0, 0, 0, 90, 0, None],
                        ],
                    },
                ],
            },
        }
        warnings = validator.validate_data(files)
        errors = [
            w for w in warnings
            if w.level == "error" and "GhostVessel" in w.message
        ]
        assert len(errors) >= 1
        assert "End A" in errors[0].message

    def test_connection_to_6d_buoy_valid(self, validator: PostGenerationValidator):
        """Lines can connect to 6D buoys, not just vessels."""
        files = {
            "04_line_types.yml": {
                "LineTypes": [{"Name": "Chain_LT"}],
            },
            "08_buoys.yml": {
                "6DBuoys": [{"Name": "6D buoy1"}],
            },
            "07_lines.yml": {
                "Lines": [
                    {
                        "Name": "Line1",
                        "LineType, Length, TargetSegmentLength": [
                            ["Chain_LT", 100, 5],
                        ],
                        "Connection, ConnectionX, ConnectionY, ConnectionZ, "
                        "ConnectionAzimuth, ConnectionDeclination, ConnectionGamma, "
                        "ConnectionReleaseStage, ConnectionzRelativeTo": [
                            ["Fixed", 0, 0, 0, 0, 90, 0, None],
                            ["6D buoy1", 0, 0, 0, 0, 90, 0, None],
                        ],
                    },
                ],
            },
        }
        warnings = validator.validate_data(files)
        vessel_errors = [
            w for w in warnings
            if w.category == "reference" and "Vessel" in w.message
        ]
        assert vessel_errors == []


class TestWinchReferences:
    """Tests for winch vessel/line reference validation."""

    def test_winch_missing_vessel_error(self, validator: PostGenerationValidator):
        files = {
            "07_lines.yml": {
                "Lines": [{"Name": "Pipeline1"}],
            },
            "11_winches.yml": {
                "Winches": [
                    {
                        "Name": "Tensioner1",
                        "Connection": ["NonExistentVessel", "Pipeline1"],
                    },
                ],
            },
        }
        warnings = validator.validate_data(files)
        errors = [
            w for w in warnings
            if w.level == "error" and "NonExistentVessel" in w.message
        ]
        assert len(errors) == 1
        assert "vessel" in errors[0].message.lower()

    def test_winch_missing_line_error(self, validator: PostGenerationValidator):
        files = {
            "06_vessels.yml": {
                "Vessels": [{"Name": "Barge1"}],
            },
            "11_winches.yml": {
                "Winches": [
                    {
                        "Name": "Tensioner1",
                        "Connection": ["Barge1", "MissingLine"],
                    },
                ],
            },
        }
        warnings = validator.validate_data(files)
        errors = [
            w for w in warnings
            if w.level == "error" and "MissingLine" in w.message
        ]
        assert len(errors) == 1
        assert "line" in errors[0].message.lower()

    def test_winch_valid_references(self, validator: PostGenerationValidator):
        files = {
            "06_vessels.yml": {
                "Vessels": [{"Name": "Barge1"}],
            },
            "07_lines.yml": {
                "Lines": [{"Name": "Pipeline1"}],
            },
            "11_winches.yml": {
                "Winches": [
                    {
                        "Name": "Tensioner1",
                        "Connection": ["Barge1", "Pipeline1"],
                    },
                ],
            },
        }
        warnings = validator.validate_data(files)
        winch_errors = [
            w for w in warnings
            if w.category == "reference" and "Winch" in w.message
        ]
        assert winch_errors == []


class TestDuplicateNames:
    """Tests for duplicate object name detection."""

    def test_duplicate_vessel_name_warning(self, validator: PostGenerationValidator):
        files = {
            "06_vessels.yml": {
                "Vessels": [
                    {"Name": "Barge1"},
                    {"Name": "Barge1"},
                ],
            },
        }
        warnings = validator.validate_data(files)
        dups = [w for w in warnings if w.category == "duplicate"]
        assert len(dups) == 1
        assert "Barge1" in dups[0].message
        assert dups[0].level == "warning"

    def test_duplicate_across_files_warning(self, validator: PostGenerationValidator):
        """Same vessel name in two different files should warn."""
        files = {
            "06_vessels.yml": {
                "Vessels": [{"Name": "Barge1"}],
            },
            "20_generic_objects.yml": {
                "Vessels": [{"Name": "Barge1"}],
            },
        }
        warnings = validator.validate_data(files)
        dups = [w for w in warnings if w.category == "duplicate"]
        assert len(dups) == 1
        assert "Barge1" in dups[0].message

    def test_no_duplicate_different_sections(self, validator: PostGenerationValidator):
        """Same name in different section types is fine (e.g. Line and Vessel)."""
        files = {
            "06_vessels.yml": {
                "Vessels": [{"Name": "Barge1"}],
            },
            "07_lines.yml": {
                "Lines": [{"Name": "Barge1"}],
            },
        }
        warnings = validator.validate_data(files)
        dups = [w for w in warnings if w.category == "duplicate"]
        assert dups == []


class TestConnectionTargets:
    """Tests for broad connection target validation."""

    def test_link_invalid_connection(self, validator: PostGenerationValidator):
        files = {
            "10_links.yml": {
                "Links": [
                    {
                        "Name": "Link1",
                        "Connection": ["NonExistentA", "NonExistentB"],
                    },
                ],
            },
        }
        warnings = validator.validate_data(files)
        errors = [
            w for w in warnings
            if w.level == "error" and "Link1" in w.message
        ]
        assert len(errors) == 2

    def test_constraint_invalid_connection(self, validator: PostGenerationValidator):
        files = {
            "12_constraints.yml": {
                "Constraints": [
                    {
                        "Name": "Constraint1",
                        "InFrameConnection": "GhostObject",
                    },
                ],
            },
        }
        warnings = validator.validate_data(files)
        errors = [
            w for w in warnings
            if w.level == "error" and "Constraint1" in w.message
        ]
        assert len(errors) == 1
        assert "GhostObject" in errors[0].message


class TestEdgeCases:
    """Tests for edge cases and empty inputs."""

    def test_empty_files_no_crash(self, validator: PostGenerationValidator):
        warnings = validator.validate_data({})
        assert warnings == []

    def test_files_without_objects_no_crash(self, validator: PostGenerationValidator):
        files = {
            "01_general.yml": {"General": {"SomeKey": "SomeValue"}},
        }
        warnings = validator.validate_data(files)
        assert warnings == []

    def test_lines_without_segment_table_no_crash(self, validator: PostGenerationValidator):
        """Lines with no segment table key should not crash."""
        files = {
            "04_line_types.yml": {
                "LineTypes": [{"Name": "Chain_LT"}],
            },
            "07_lines.yml": {
                "Lines": [{"Name": "Line1"}],
            },
        }
        warnings = validator.validate_data(files)
        # No crash, and no spurious line type errors
        lt_errors = [
            w for w in warnings
            if "LineType" in w.message
        ]
        assert lt_errors == []
