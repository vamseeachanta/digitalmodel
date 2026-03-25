"""
ABOUTME: Tests for data catalog system
Tests catalog creation, discovery, schema validation, metadata tracking, and versioning
"""

import pytest
import yaml
import json
import hashlib
from pathlib import Path
from datetime import datetime
from unittest.mock import Mock, patch
import pandas as pd

from digitalmodel.data.catalog import (
    DataCatalog,
    CatalogEntry,
    CatalogDiscovery,
    SchemaValidator,
    CatalogError,
    SchemaValidationError,
)


class TestCatalogEntry:
    """Test CatalogEntry data model"""

    def test_create_minimal_entry(self):
        """Test creating entry with minimal required fields"""
        entry = CatalogEntry(
            name="test_dataset",
            file="data/test.xlsx",
            format="excel",
            version="1.0.0",
            description="Test dataset",
        )

        assert entry.name == "test_dataset"
        assert entry.file == "data/test.xlsx"
        assert entry.format == "excel"
        assert entry.version == "1.0.0"
        assert entry.description == "Test dataset"

    def test_create_full_entry(self):
        """Test creating entry with all fields"""
        entry = CatalogEntry(
            name="shackles_g2100",
            file="data/crosby/204_subsea_shackles_g2100.xlsx",
            format="excel",
            schema="schemas/shackles.schema.json",
            version="2024.1",
            description="Crosby G2100 subsea shackles",
            source="Crosby Catalog",
            tags=["rigging", "shackles", "subsea"],
            units="metric",
            engineering_standard="API 2C",
        )

        assert entry.name == "shackles_g2100"
        assert entry.tags == ["rigging", "shackles", "subsea"]
        assert entry.units == "metric"
        assert entry.engineering_standard == "API 2C"

    def test_entry_has_auto_metadata(self):
        """Test that entry auto-generates metadata"""
        entry = CatalogEntry(
            name="test",
            file="data/test.xlsx",
            format="excel",
            version="1.0.0",
            description="Test",
        )

        assert entry.created_at is not None
        assert entry.schema_version == "1.0.0"  # Default catalog schema version

    def test_entry_calculates_file_hash(self, tmp_path):
        """Test that entry can calculate file hash"""
        test_file = tmp_path / "test.xlsx"
        test_file.write_text("test content")

        entry = CatalogEntry(
            name="test",
            file=str(test_file),
            format="excel",
            version="1.0.0",
            description="Test",
        )

        entry.update_hash()
        assert entry.hash is not None
        assert len(entry.hash) == 64  # SHA256 hash length

    def test_entry_to_dict(self):
        """Test converting entry to dictionary"""
        entry = CatalogEntry(
            name="test",
            file="data/test.xlsx",
            format="excel",
            version="1.0.0",
            description="Test",
            tags=["test"],
        )

        data = entry.to_dict()
        assert data["name"] == "test"
        assert data["file"] == "data/test.xlsx"
        assert data["tags"] == ["test"]

    def test_entry_from_dict(self):
        """Test creating entry from dictionary"""
        data = {
            "name": "test",
            "file": "data/test.xlsx",
            "format": "excel",
            "version": "1.0.0",
            "description": "Test",
            "tags": ["test"],
        }

        entry = CatalogEntry.from_dict(data)
        assert entry.name == "test"
        assert entry.tags == ["test"]


class TestSchemaValidator:
    """Test JSON schema validation"""

    def test_create_validator(self, tmp_path):
        """Test creating schema validator"""
        schema_dir = tmp_path / "schemas"
        schema_dir.mkdir()

        validator = SchemaValidator(schema_dir)
        assert validator.schema_dir == schema_dir

    def test_load_schema(self, tmp_path):
        """Test loading JSON schema"""
        schema_dir = tmp_path / "schemas"
        schema_dir.mkdir()

        schema = {
            "$schema": "http://json-schema.org/draft-07/schema#",
            "type": "object",
            "properties": {
                "name": {"type": "string"},
                "size": {"type": "number"},
            },
            "required": ["name"],
        }

        schema_file = schema_dir / "test.schema.json"
        schema_file.write_text(json.dumps(schema))

        validator = SchemaValidator(schema_dir)
        loaded = validator.load_schema("test.schema.json")
        assert loaded["properties"]["name"]["type"] == "string"

    def test_validate_data_success(self, tmp_path):
        """Test successful data validation"""
        schema_dir = tmp_path / "schemas"
        schema_dir.mkdir()

        schema = {
            "$schema": "http://json-schema.org/draft-07/schema#",
            "type": "object",
            "properties": {
                "name": {"type": "string"},
            },
            "required": ["name"],
        }

        schema_file = schema_dir / "test.schema.json"
        schema_file.write_text(json.dumps(schema))

        validator = SchemaValidator(schema_dir)
        data = {"name": "test"}

        # Should not raise exception
        validator.validate(data, "test.schema.json")

    def test_validate_data_failure_strict(self, tmp_path):
        """Test validation failure in strict mode"""
        schema_dir = tmp_path / "schemas"
        schema_dir.mkdir()

        schema = {
            "$schema": "http://json-schema.org/draft-07/schema#",
            "type": "object",
            "properties": {
                "name": {"type": "string"},
            },
            "required": ["name"],
        }

        schema_file = schema_dir / "test.schema.json"
        schema_file.write_text(json.dumps(schema))

        validator = SchemaValidator(schema_dir, strict=True)
        data = {"age": 25}  # Missing required 'name'

        with pytest.raises(SchemaValidationError):
            validator.validate(data, "test.schema.json")

    def test_validate_permissive_unknown_fields(self, tmp_path):
        """Test permissive validation with unknown fields"""
        schema_dir = tmp_path / "schemas"
        schema_dir.mkdir()

        schema = {
            "$schema": "http://json-schema.org/draft-07/schema#",
            "type": "object",
            "properties": {
                "name": {"type": "string"},
            },
            "required": ["name"],
        }

        schema_file = schema_dir / "test.schema.json"
        schema_file.write_text(json.dumps(schema))

        validator = SchemaValidator(schema_dir, strict=False)
        data = {"name": "test", "unknown_field": "value"}

        # Should warn but not raise
        with pytest.warns(UserWarning):
            validator.validate(data, "test.schema.json")


class TestCatalogDiscovery:
    """Test auto-discovery of data files"""

    def test_discover_files(self, tmp_path):
        """Test discovering data files"""
        data_dir = tmp_path / "data"
        data_dir.mkdir()

        # Create test files
        (data_dir / "test1.xlsx").write_text("test")
        (data_dir / "test2.csv").write_text("test")
        (data_dir / "test3.yml").write_text("test")

        discovery = CatalogDiscovery(data_dir)
        files = discovery.discover()

        assert len(files) == 3
        assert any(f.name == "test1.xlsx" for f in files)
        assert any(f.name == "test2.csv" for f in files)
        assert any(f.name == "test3.yml" for f in files)

    def test_discover_recursive(self, tmp_path):
        """Test recursive discovery"""
        data_dir = tmp_path / "data"
        data_dir.mkdir()
        subdir = data_dir / "subdir"
        subdir.mkdir()

        (data_dir / "test1.xlsx").write_text("test")
        (subdir / "test2.xlsx").write_text("test")

        discovery = CatalogDiscovery(data_dir)
        files = discovery.discover()

        assert len(files) == 2

    def test_discovery_exclusions(self, tmp_path):
        """Test exclusion patterns"""
        data_dir = tmp_path / "data"
        data_dir.mkdir()

        (data_dir / "test.xlsx").write_text("test")
        (data_dir / "test_backup.xlsx").write_text("test")
        (data_dir / "test.tmp").write_text("test")
        (data_dir / "__pycache__").mkdir()
        (data_dir / "__pycache__" / "test.pyc").write_text("test")

        discovery = CatalogDiscovery(data_dir)
        files = discovery.discover()

        # Should only find test.xlsx
        assert len(files) == 1
        assert files[0].name == "test.xlsx"

    def test_infer_format(self, tmp_path):
        """Test format inference from extension"""
        data_dir = tmp_path / "data"
        data_dir.mkdir()

        (data_dir / "test.xlsx").write_text("test")

        discovery = CatalogDiscovery(data_dir)
        files = discovery.discover()

        format_type = discovery.infer_format(files[0])
        assert format_type == "excel"

    def test_create_entry_from_file(self, tmp_path):
        """Test creating catalog entry from discovered file"""
        data_dir = tmp_path / "data"
        data_dir.mkdir()

        test_file = data_dir / "shackles_g2100.xlsx"
        test_file.write_text("test")

        discovery = CatalogDiscovery(data_dir)
        files = discovery.discover()

        entry = discovery.create_entry(files[0])
        assert entry.name == "shackles_g2100"
        assert entry.format == "excel"
        assert entry.file == str(test_file.relative_to(tmp_path))


class TestDataCatalog:
    """Test main DataCatalog class"""

    def test_create_catalog(self, tmp_path):
        """Test creating empty catalog"""
        catalog_file = tmp_path / "catalog.yml"

        catalog = DataCatalog(catalog_file)
        assert catalog.catalog_file == catalog_file
        assert len(catalog.datasets) == 0

    def test_add_entry(self, tmp_path):
        """Test adding entry to catalog"""
        catalog_file = tmp_path / "catalog.yml"
        catalog = DataCatalog(catalog_file)

        entry = CatalogEntry(
            name="test",
            file="data/test.xlsx",
            format="excel",
            version="1.0.0",
            description="Test",
        )

        catalog.add_entry(entry)
        assert "test" in catalog.datasets
        assert catalog.datasets["test"] == entry

    def test_get_entry(self, tmp_path):
        """Test getting entry from catalog"""
        catalog_file = tmp_path / "catalog.yml"
        catalog = DataCatalog(catalog_file)

        entry = CatalogEntry(
            name="test",
            file="data/test.xlsx",
            format="excel",
            version="1.0.0",
            description="Test",
        )
        catalog.add_entry(entry)

        retrieved = catalog.get_entry("test")
        assert retrieved.name == "test"

    def test_get_nonexistent_entry(self, tmp_path):
        """Test getting non-existent entry raises error"""
        catalog_file = tmp_path / "catalog.yml"
        catalog = DataCatalog(catalog_file)

        with pytest.raises(CatalogError):
            catalog.get_entry("nonexistent")

    def test_save_catalog(self, tmp_path):
        """Test saving catalog to YAML"""
        catalog_file = tmp_path / "catalog.yml"
        catalog = DataCatalog(catalog_file)

        entry = CatalogEntry(
            name="test",
            file="data/test.xlsx",
            format="excel",
            version="1.0.0",
            description="Test",
        )
        catalog.add_entry(entry)

        catalog.save()

        assert catalog_file.exists()
        with open(catalog_file) as f:
            data = yaml.safe_load(f)

        assert data["catalog_version"] == "1.0.0"
        assert "test" in data["datasets"]
        assert data["datasets"]["test"]["format"] == "excel"

    def test_load_catalog(self, tmp_path):
        """Test loading catalog from YAML"""
        catalog_file = tmp_path / "catalog.yml"

        catalog_data = {
            "catalog_version": "1.0.0",
            "datasets": {
                "test": {
                    "name": "test",
                    "file": "data/test.xlsx",
                    "format": "excel",
                    "version": "1.0.0",
                    "description": "Test",
                }
            },
        }

        with open(catalog_file, "w") as f:
            yaml.dump(catalog_data, f)

        catalog = DataCatalog.load_catalog(catalog_file)

        assert len(catalog.datasets) == 1
        assert "test" in catalog.datasets

    def test_search_by_format(self, tmp_path):
        """Test searching catalog by format"""
        catalog_file = tmp_path / "catalog.yml"
        catalog = DataCatalog(catalog_file)

        catalog.add_entry(
            CatalogEntry(
                name="test1",
                file="data/test1.xlsx",
                format="excel",
                version="1.0.0",
                description="Test 1",
            )
        )
        catalog.add_entry(
            CatalogEntry(
                name="test2",
                file="data/test2.csv",
                format="csv",
                version="1.0.0",
                description="Test 2",
            )
        )

        results = catalog.search(format="excel")
        assert len(results) == 1
        assert results[0].name == "test1"

    def test_search_by_tags(self, tmp_path):
        """Test searching catalog by tags"""
        catalog_file = tmp_path / "catalog.yml"
        catalog = DataCatalog(catalog_file)

        catalog.add_entry(
            CatalogEntry(
                name="test1",
                file="data/test1.xlsx",
                format="excel",
                version="1.0.0",
                description="Test 1",
                tags=["rigging", "shackles"],
            )
        )
        catalog.add_entry(
            CatalogEntry(
                name="test2",
                file="data/test2.xlsx",
                format="excel",
                version="1.0.0",
                description="Test 2",
                tags=["rigging", "slings"],
            )
        )

        results = catalog.search(tags=["shackles"])
        assert len(results) == 1
        assert results[0].name == "test1"

    def test_search_by_description(self, tmp_path):
        """Test searching catalog by description text"""
        catalog_file = tmp_path / "catalog.yml"
        catalog = DataCatalog(catalog_file)

        catalog.add_entry(
            CatalogEntry(
                name="test1",
                file="data/test1.xlsx",
                format="excel",
                version="1.0.0",
                description="Crosby G2100 subsea shackles",
            )
        )
        catalog.add_entry(
            CatalogEntry(
                name="test2",
                file="data/test2.xlsx",
                format="excel",
                version="1.0.0",
                description="Wire rope slings",
            )
        )

        results = catalog.search(description="subsea")
        assert len(results) == 1
        assert results[0].name == "test1"

    def test_load_dataset_excel(self, tmp_path):
        """Test loading Excel dataset"""
        catalog_file = tmp_path / "catalog.yml"
        data_file = tmp_path / "data" / "test.xlsx"
        data_file.parent.mkdir()

        # Create test Excel file
        df = pd.DataFrame({"A": [1, 2, 3], "B": [4, 5, 6]})
        df.to_excel(data_file, index=False)

        catalog = DataCatalog(catalog_file, base_path=tmp_path)
        catalog.add_entry(
            CatalogEntry(
                name="test",
                file="data/test.xlsx",
                format="excel",
                version="1.0.0",
                description="Test",
            )
        )

        loaded_df = catalog.load("test")
        assert isinstance(loaded_df, pd.DataFrame)
        assert len(loaded_df) == 3

    def test_version_bump_on_hash_change(self, tmp_path):
        """Test auto-version bump when file hash changes"""
        catalog_file = tmp_path / "catalog.yml"
        data_file = tmp_path / "data" / "test.xlsx"
        data_file.parent.mkdir()

        # Create initial file
        data_file.write_text("version 1")

        catalog = DataCatalog(catalog_file, base_path=tmp_path)
        entry = CatalogEntry(
            name="test",
            file="data/test.xlsx",
            format="excel",
            version="1.0.0",
            description="Test",
        )
        entry.update_hash()
        initial_hash = entry.hash
        catalog.add_entry(entry)

        # Modify file
        data_file.write_text("version 2")

        # Check version
        updated_version = catalog.check_version("test")
        assert updated_version != "1.0.0"  # Should auto-bump
        assert catalog.datasets["test"].hash != initial_hash

    def test_merge_catalogs(self, tmp_path):
        """Test merging module catalogs into master"""
        master_file = tmp_path / "data" / "catalog.yml"
        master_file.parent.mkdir()
        module_file = tmp_path / "data" / "crosby" / "catalog.yml"
        module_file.parent.mkdir(parents=True)

        # Create master catalog
        master = DataCatalog(master_file)
        master.add_entry(
            CatalogEntry(
                name="master_data",
                file="data/master.xlsx",
                format="excel",
                version="1.0.0",
                description="Master data",
            )
        )

        # Create module catalog
        module = DataCatalog(module_file)
        module.add_entry(
            CatalogEntry(
                name="module_data",
                file="data/crosby/module.xlsx",
                format="excel",
                version="1.0.0",
                description="Module data",
            )
        )

        # Merge
        master.merge(module)

        assert len(master.datasets) == 2
        assert "master_data" in master.datasets
        assert "module_data" in master.datasets

    def test_list_datasets(self, tmp_path):
        """Test listing all datasets"""
        catalog_file = tmp_path / "catalog.yml"
        catalog = DataCatalog(catalog_file)

        catalog.add_entry(
            CatalogEntry(
                name="test1",
                file="data/test1.xlsx",
                format="excel",
                version="1.0.0",
                description="Test 1",
            )
        )
        catalog.add_entry(
            CatalogEntry(
                name="test2",
                file="data/test2.csv",
                format="csv",
                version="1.0.0",
                description="Test 2",
            )
        )

        datasets = catalog.list_datasets()
        assert len(datasets) == 2
        assert "test1" in datasets
        assert "test2" in datasets


class TestCatalogIntegration:
    """Integration tests for full catalog workflow"""

    def test_full_discovery_and_catalog_creation(self, tmp_path):
        """Test complete workflow: discover files and create catalog"""
        data_dir = tmp_path / "data"
        data_dir.mkdir()
        crosby_dir = data_dir / "crosby"
        crosby_dir.mkdir()

        # Create test files
        (crosby_dir / "204_subsea_shackles_g2100.xlsx").write_text("test")
        (crosby_dir / "26_shackles_bolt_type_anchor_g2130.xlsx").write_text("test")

        # Discover files
        discovery = CatalogDiscovery(data_dir)
        files = discovery.discover()

        # Create catalog
        catalog_file = data_dir / "catalog.yml"
        catalog = DataCatalog(catalog_file, base_path=tmp_path)

        for file in files:
            entry = discovery.create_entry(file)
            catalog.add_entry(entry)

        # Verify
        assert len(catalog.datasets) == 2
        assert "204_subsea_shackles_g2100" in catalog.datasets

        # Save and reload
        catalog.save()
        loaded_catalog = DataCatalog.load_catalog(catalog_file, base_path=tmp_path)
        assert len(loaded_catalog.datasets) == 2

    def test_schema_validation_on_load(self, tmp_path):
        """Test schema validation when loading data"""
        schema_dir = tmp_path / "schemas"
        schema_dir.mkdir()
        data_dir = tmp_path / "data"
        data_dir.mkdir()

        # Create schema
        schema = {
            "$schema": "http://json-schema.org/draft-07/schema#",
            "type": "object",
            "properties": {
                "Size": {"type": "string"},
                "WLL": {"type": "number"},
            },
            "required": ["Size", "WLL"],
        }
        schema_file = schema_dir / "shackles.schema.json"
        schema_file.write_text(json.dumps(schema))

        # Create valid data file
        data_file = data_dir / "shackles.xlsx"
        df = pd.DataFrame({
            "Size": ["1/2", "5/8"],
            "WLL": [2.0, 3.25],
        })
        df.to_excel(data_file, index=False)

        # Create catalog with schema
        catalog_file = tmp_path / "catalog.yml"
        catalog = DataCatalog(catalog_file, base_path=tmp_path, schema_dir=schema_dir)
        catalog.add_entry(
            CatalogEntry(
                name="shackles",
                file="data/shackles.xlsx",
                format="excel",
                version="1.0.0",
                description="Test shackles",
                schema="shackles.schema.json",
            )
        )

        # Load with validation
        loaded_df = catalog.load("shackles", validate=True)
        assert len(loaded_df) == 2
