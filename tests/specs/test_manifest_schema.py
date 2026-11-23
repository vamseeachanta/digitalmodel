"""Tests for the manifest.yaml Pydantic schema.

Validates that ModuleManifest correctly enforces the per-module manifest
structure defined in the project standards (D-05/D-06).
"""

import json
from pathlib import Path

import pytest
import yaml

from digitalmodel.specs.manifest_schema import (
    FunctionEntry,
    ModuleManifest,
    StandardRef,
    validate_manifest_file,
)


VALID_MANIFEST_DATA = {
    "module": "subsea/on_bottom_stability",
    "description": "Pipeline on-bottom stability design per DNV-RP-F109",
    "primary_standard": {
        "id": "DNV-RP-F109",
        "edition": 2021,
        "title": "On-Bottom Stability Design of Submarine Pipelines",
    },
    "functions": [
        {
            "name": "hydrodynamic_force_per_meter",
            "clause": "S3.2.1",
            "equation": "Eq 3.1",
            "description": "Inline hydrodynamic force per unit length",
            "inputs": ["rho_w_kg_m3", "D_outer_m", "U_m_s", "a_m_s2", "C_D", "C_M"],
            "outputs": ["force_N_per_m"],
        }
    ],
}


class TestModuleManifestValid:
    """Test 1: Valid manifest data loads without error."""

    def test_valid_manifest_loads(self):
        manifest = ModuleManifest(**VALID_MANIFEST_DATA)
        assert manifest.module == "subsea/on_bottom_stability"
        assert manifest.primary_standard.id == "DNV-RP-F109"
        assert len(manifest.functions) == 1
        assert manifest.functions[0].name == "hydrodynamic_force_per_meter"

    def test_edition_accepts_string(self):
        data = VALID_MANIFEST_DATA.copy()
        data["primary_standard"] = {
            "id": "DNV-RP-F109",
            "edition": "2021-09",
            "title": "On-Bottom Stability Design of Submarine Pipelines",
        }
        manifest = ModuleManifest(**data)
        assert manifest.primary_standard.edition == "2021-09"


class TestModuleManifestMissingFields:
    """Tests 2-5: Missing required fields raise ValidationError."""

    def test_missing_module_raises_validation_error(self):
        data = VALID_MANIFEST_DATA.copy()
        del data["module"]
        with pytest.raises(Exception):
            ModuleManifest(**data)

    def test_missing_primary_standard_id_raises_validation_error(self):
        data = VALID_MANIFEST_DATA.copy()
        data["primary_standard"] = {
            "edition": 2021,
            "title": "On-Bottom Stability Design",
        }
        with pytest.raises(Exception):
            ModuleManifest(**data)

    def test_empty_functions_raises_validation_error(self):
        data = VALID_MANIFEST_DATA.copy()
        data["functions"] = []
        with pytest.raises(Exception):
            ModuleManifest(**data)

    def test_function_missing_clause_raises_validation_error(self):
        data = VALID_MANIFEST_DATA.copy()
        data["functions"] = [
            {
                "name": "some_func",
                "description": "A function",
                # clause is missing
            }
        ]
        with pytest.raises(Exception):
            ModuleManifest(**data)


class TestValidateManifestFile:
    """Tests 6-7: validate_manifest_file reads YAML and returns ModuleManifest."""

    def test_valid_yaml_file_returns_manifest(self, tmp_path):
        manifest_path = tmp_path / "manifest.yaml"
        manifest_path.write_text(yaml.dump(VALID_MANIFEST_DATA))

        result = validate_manifest_file(manifest_path)
        assert isinstance(result, ModuleManifest)
        assert result.module == "subsea/on_bottom_stability"

    def test_invalid_yaml_file_raises_with_path(self, tmp_path):
        manifest_path = tmp_path / "manifest.yaml"
        invalid_data = {"module": "test", "description": "test"}
        manifest_path.write_text(yaml.dump(invalid_data))

        with pytest.raises(ValueError, match=str(manifest_path)):
            validate_manifest_file(manifest_path)


class TestJsonSchemaExport:
    """Test 8: model_json_schema() produces valid JSON Schema."""

    def test_json_schema_is_valid(self):
        schema = ModuleManifest.model_json_schema()
        assert isinstance(schema, dict)
        # Verify it serializes to valid JSON
        json_str = json.dumps(schema)
        assert len(json_str) > 0
        # Check key schema properties exist
        assert "properties" in schema
        assert "module" in schema["properties"]
        assert "functions" in schema["properties"]
