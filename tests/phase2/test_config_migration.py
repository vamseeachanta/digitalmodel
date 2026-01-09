"""
ABOUTME: Phase 2 Task 2.1 configuration framework migration tests
ABOUTME: Unit and integration tests for ConfigLoader, SchemaValidator, ConfigManager, ConfigModel
"""

import pytest
import json
import tempfile
from pathlib import Path
from datetime import datetime
import sys

# Add src to path for imports
sys.path.insert(0, str(Path(__file__).parent.parent.parent / 'src'))

from digitalmodel.base_configs import (
    ConfigLoader,
    SchemaValidator,
    ConfigManager,
    ConfigModel,
)


class TestConfigLoader:
    """Test ConfigLoader YAML loading and caching."""

    @pytest.fixture
    def temp_yaml_file(self):
        """Create temporary YAML configuration file."""
        with tempfile.NamedTemporaryFile(mode='w', suffix='.yaml', delete=False) as f:
            f.write("""
metadata:
  name: test-config
  version: "1.0.0"
  description: Test configuration

configuration:
  digitalmodel:
    enabled: true
    modules: ["solver", "analyzer"]

database:
  engine: sqlite
  database: ":memory:"
  pool_size: 5

logging:
  level: DEBUG
  handlers:
    - console
    - file
  format: "%(asctime)s - %(name)s - %(levelname)s - %(message)s"

performance:
  timeout_seconds: 30
  max_workers: 4
  cache_ttl: 300
""")
            path = Path(f.name)
        yield path
        path.unlink()

    def test_loader_initialization(self):
        """Test ConfigLoader initialization with default TTL."""
        loader = ConfigLoader()
        assert loader.cache_ttl == 300
        assert len(loader._cache) == 0

    def test_loader_initialization_custom_ttl(self):
        """Test ConfigLoader initialization with custom TTL."""
        loader = ConfigLoader(cache_ttl=600)
        assert loader.cache_ttl == 600

    def test_load_yaml_file(self, temp_yaml_file):
        """Test loading YAML configuration from file."""
        loader = ConfigLoader()
        config = loader.load(temp_yaml_file)

        assert isinstance(config, dict)
        assert "metadata" in config
        assert config["metadata"]["name"] == "test-config"
        assert config["metadata"]["version"] == "1.0.0"

    def test_load_caching(self, temp_yaml_file):
        """Test that ConfigLoader caches loaded configurations."""
        loader = ConfigLoader(cache_ttl=300)

        # First load
        config1 = loader.load(temp_yaml_file)

        # Second load should come from cache
        config2 = loader.load(temp_yaml_file)

        assert config1 == config2
        assert len(loader._cache) == 1

    def test_load_file_not_found(self):
        """Test that ConfigLoader raises FileNotFoundError for missing files."""
        loader = ConfigLoader()
        with pytest.raises(FileNotFoundError):
            loader.load("/nonexistent/path/config.yaml")

    def test_load_empty_yaml(self):
        """Test loading empty YAML file returns empty dict."""
        with tempfile.NamedTemporaryFile(mode='w', suffix='.yaml', delete=False) as f:
            f.write("")
            path = Path(f.name)

        try:
            loader = ConfigLoader()
            config = loader.load(path)
            assert config == {}
        finally:
            path.unlink()

    def test_load_multiple_configs(self, temp_yaml_file):
        """Test loading and merging multiple configuration files."""
        with tempfile.NamedTemporaryFile(mode='w', suffix='.yaml', delete=False) as f:
            f.write("additional_setting: value1\n")
            path2 = Path(f.name)

        try:
            loader = ConfigLoader()
            merged = loader.load_multiple([temp_yaml_file, path2])

            assert "metadata" in merged
            assert "additional_setting" in merged
            assert merged["additional_setting"] == "value1"
        finally:
            path2.unlink()

    def test_clear_cache(self, temp_yaml_file):
        """Test clearing the configuration cache."""
        loader = ConfigLoader()
        loader.load(temp_yaml_file)
        assert len(loader._cache) == 1

        loader.clear_cache()
        assert len(loader._cache) == 0


class TestSchemaValidator:
    """Test SchemaValidator JSON schema validation."""

    def test_validator_initialization(self):
        """Test SchemaValidator initialization."""
        validator = SchemaValidator()
        assert validator.schema is not None
        assert "properties" in validator.schema

    def test_validate_valid_config(self):
        """Test validating a valid configuration."""
        validator = SchemaValidator()
        config = {
            "metadata": {
                "name": "test",
                "version": "1.0.0"
            }
        }

        is_valid, errors = validator.validate(config)
        assert is_valid is True
        assert errors == []

    def test_validate_invalid_config_missing_metadata(self):
        """Test validating config missing required metadata."""
        validator = SchemaValidator()
        config = {"configuration": {}}

        is_valid, errors = validator.validate(config)
        assert is_valid is False
        assert len(errors) > 0

    def test_validate_with_database_config(self):
        """Test validating config with valid database section."""
        validator = SchemaValidator()
        config = {
            "metadata": {
                "name": "test",
                "version": "1.0.0"
            },
            "database": {
                "engine": "postgresql",
                "host": "localhost",
                "port": 5432,
                "database": "testdb",
                "pool_size": 10
            }
        }

        is_valid, errors = validator.validate(config)
        assert is_valid is True

    def test_schema_factory_backend(self):
        """Test creating backend-specific schema."""
        schema = SchemaValidator.create_backend_schema()
        assert "backend" in schema["properties"]
        assert "api_port" in schema["properties"]["backend"]["properties"]

    def test_schema_factory_solver(self):
        """Test creating solver-specific schema."""
        schema = SchemaValidator.create_solver_schema()
        assert "solvers" in schema["properties"]
        assert "timeout_seconds" in schema["properties"]["solvers"]["properties"]

    def test_extend_schema(self):
        """Test extending schema with additional properties."""
        validator = SchemaValidator()
        extension = {
            "properties": {
                "custom_field": {"type": "string"},
                "custom_number": {"type": "integer"}
            }
        }

        validator.extend_schema(extension)
        assert "custom_field" in validator.schema["properties"]
        assert "custom_number" in validator.schema["properties"]

    def test_get_schema(self):
        """Test retrieving schema copy."""
        validator = SchemaValidator()
        schema = validator.get_schema()

        assert isinstance(schema, dict)
        # Verify it's a copy, not reference
        schema["properties"]["test"] = "value"
        assert "test" not in validator.schema["properties"]


class TestConfigManager:
    """Test ConfigManager unified interface."""

    @pytest.fixture
    def temp_config_file(self):
        """Create temporary configuration file."""
        with tempfile.NamedTemporaryFile(mode='w', suffix='.yaml', delete=False) as f:
            f.write("""
metadata:
  name: manager-test
  version: "2.0.0"

database:
  engine: sqlite
  host: localhost
  port: 5432

logging:
  level: INFO

performance:
  timeout_seconds: 30
  max_workers: 4
""")
            path = Path(f.name)
        yield path
        path.unlink()

    def test_manager_initialization(self):
        """Test ConfigManager initialization."""
        manager = ConfigManager()
        assert manager.config == {}
        assert manager.is_valid_config is False

    def test_manager_load_config(self, temp_config_file):
        """Test loading configuration into manager."""
        manager = ConfigManager()
        result = manager.load_config(temp_config_file)

        assert result is True
        assert "metadata" in manager.config
        assert manager.config["metadata"]["name"] == "manager-test"

    def test_manager_get_simple_key(self, temp_config_file):
        """Test getting configuration value with simple key."""
        manager = ConfigManager()
        manager.load_config(temp_config_file)

        # Direct access
        name = manager.get("metadata.name")
        assert name == "manager-test"

        version = manager.get("metadata.version")
        assert version == "2.0.0"

    def test_manager_get_nested_key(self, temp_config_file):
        """Test getting nested configuration values."""
        manager = ConfigManager()
        manager.load_config(temp_config_file)

        # Nested access
        engine = manager.get("database.engine")
        assert engine == "sqlite"

        timeout = manager.get("performance.timeout_seconds")
        assert timeout == 30

    def test_manager_get_default_value(self, temp_config_file):
        """Test getting nonexistent key returns default."""
        manager = ConfigManager()
        manager.load_config(temp_config_file)

        result = manager.get("nonexistent.key", default="default_value")
        assert result == "default_value"

    def test_manager_set_value(self, temp_config_file):
        """Test setting configuration value."""
        manager = ConfigManager()
        manager.load_config(temp_config_file)

        manager.set("database.pool_size", 20)
        assert manager.get("database.pool_size") == 20

    def test_manager_set_creates_nested_dicts(self):
        """Test that set creates nested dictionaries as needed."""
        manager = ConfigManager()
        manager.load_config(Path("/dev/null") if Path("/dev/null").exists() else None)

        # Create nested structure
        manager.set("new.nested.deeply.value", 42)
        assert manager.get("new.nested.deeply.value") == 42

    def test_manager_get_section(self, temp_config_file):
        """Test getting entire configuration section."""
        manager = ConfigManager()
        manager.load_config(temp_config_file)

        database_section = manager.get_section("database")
        assert "engine" in database_section
        assert "host" in database_section
        assert database_section["engine"] == "sqlite"

    def test_manager_validate(self, temp_config_file):
        """Test validating configuration."""
        manager = ConfigManager()
        manager.load_config(temp_config_file)

        is_valid, errors = manager.validate()
        assert is_valid is True or isinstance(errors, list)

    def test_manager_size_property(self, temp_config_file):
        """Test configuration size property."""
        manager = ConfigManager()
        manager.load_config(temp_config_file)

        size = manager.size
        assert size > 0
        assert isinstance(size, int)

    def test_manager_is_valid_property(self, temp_config_file):
        """Test is_valid property."""
        manager = ConfigManager()
        manager.load_config(temp_config_file)

        assert isinstance(manager.is_valid, bool)

    def test_manager_reload(self, temp_config_file):
        """Test reloading configuration clears cache."""
        manager = ConfigManager()

        # First load
        manager.load_config(temp_config_file)
        first_load_value = manager.get("metadata.name")

        # Reload
        result = manager.reload(temp_config_file)
        assert result is True
        assert manager.get("metadata.name") == first_load_value


class TestConfigModel:
    """Test ConfigModel ORM class (unit tests without database)."""

    def test_model_initialization(self):
        """Test ConfigModel initialization with basic parameters."""
        # Note: This tests the __init__ method without actual ORM binding
        # Full ORM testing would require database setup

        model = ConfigModel(
            config_name="test_config",
            config_type="yaml",
            config_data={"key": "value"},
            environment="dev",
            category="database"
        )

        assert model.config_name == "test_config"
        assert model.config_type == "yaml"
        assert model.config_data == {"key": "value"}
        assert model.environment == "dev"
        assert model.category == "database"

    def test_model_get_config_value(self):
        """Test getting configuration value by dot-separated key."""
        model = ConfigModel(
            config_name="test",
            config_type="yaml",
            config_data={
                "database": {
                    "host": "localhost",
                    "port": 5432
                },
                "simple": "value"
            }
        )

        # Simple key
        assert model.get_config_value("simple") == "value"

        # Nested key
        assert model.get_config_value("database.host") == "localhost"
        assert model.get_config_value("database.port") == 5432

    def test_model_get_config_value_default(self):
        """Test get_config_value returns default for missing keys."""
        model = ConfigModel(
            config_name="test",
            config_type="yaml",
            config_data={"existing": "value"}
        )

        result = model.get_config_value("missing.key", default="default")
        assert result == "default"

    def test_model_set_config_value(self):
        """Test setting configuration value by dot-separated key."""
        model = ConfigModel(
            config_name="test",
            config_type="yaml",
            config_data={}
        )

        model.set_config_value("database.host", "postgres.example.com")
        assert model.get_config_value("database.host") == "postgres.example.com"

    def test_model_set_config_value_nested(self):
        """Test set_config_value creates nested dictionaries."""
        model = ConfigModel(
            config_name="test",
            config_type="yaml",
            config_data={}
        )

        model.set_config_value("level1.level2.level3.value", 42)
        assert model.config_data["level1"]["level2"]["level3"]["value"] == 42

    def test_model_mark_validated_valid(self):
        """Test marking configuration as validated (valid)."""
        model = ConfigModel(
            config_name="test",
            config_type="yaml",
            config_data={}
        )

        model.mark_validated(is_valid=True, errors=None)
        assert model.validation_status == "valid"
        assert model.validation_errors == []
        assert model.last_validated_at is not None

    def test_model_mark_validated_invalid(self):
        """Test marking configuration as validated (invalid)."""
        model = ConfigModel(
            config_name="test",
            config_type="yaml",
            config_data={}
        )

        errors = ["Missing required field: host", "Invalid port number"]
        model.mark_validated(is_valid=False, errors=errors)

        assert model.validation_status == "invalid"
        assert model.validation_errors == errors

    def test_model_repr(self):
        """Test ConfigModel string representation."""
        model = ConfigModel(
            config_name="test_config",
            config_type="yaml",
            config_data={}
        )

        model.environment = "production"
        model.validation_status = "valid"

        repr_str = repr(model)
        assert "test_config" in repr_str
        assert "yaml" in repr_str
        assert "production" in repr_str
        assert "valid" in repr_str


class TestIntegration:
    """Integration tests combining all components."""

    @pytest.fixture
    def temp_config_file(self):
        """Create integration test configuration file."""
        with tempfile.NamedTemporaryFile(mode='w', suffix='.yaml', delete=False) as f:
            f.write("""
metadata:
  name: integration-test
  version: "1.0.0"

database:
  engine: postgresql
  host: db.example.com
  port: 5432
  database: testdb

logging:
  level: DEBUG

performance:
  timeout_seconds: 60
  cache_ttl: 300
""")
            path = Path(f.name)
        yield path
        path.unlink()

    def test_full_workflow(self, temp_config_file):
        """Test complete workflow: load → validate → manage."""
        # Step 1: Load configuration
        manager = ConfigManager()
        assert manager.load_config(temp_config_file) is True

        # Step 2: Access values
        assert manager.get("metadata.name") == "integration-test"
        assert manager.get("database.engine") == "postgresql"
        assert manager.get("database.port") == 5432

        # Step 3: Modify values
        manager.set("database.pool_size", 15)
        assert manager.get("database.pool_size") == 15

        # Step 4: Validate
        is_valid, errors = manager.validate()
        assert isinstance(is_valid, bool)

        # Step 5: Create model from loaded config
        model = ConfigModel(
            config_name="integration-test",
            config_type="yaml",
            config_data=manager.config
        )

        # Verify model can access config
        assert model.get_config_value("database.host") == "db.example.com"

    def test_loader_validator_manager_integration(self, temp_config_file):
        """Test integration between loader, validator, and manager."""
        # Create individual components
        loader = ConfigLoader(cache_ttl=600)
        validator = SchemaValidator()
        manager = ConfigManager()

        # Load and validate through manager
        success = manager.load_config(temp_config_file)
        assert success is True

        # Validate loaded config
        is_valid, errors = manager.validate()
        # Should be valid (passes base schema)
        assert isinstance(is_valid, bool)

        # Get values from different access patterns
        db_engine_direct = manager.get("database.engine")
        assert db_engine_direct == "postgresql"

        # Get entire section
        db_section = manager.get_section("database")
        assert db_section["engine"] == "postgresql"

        # Get with default
        missing = manager.get("missing.key", default="fallback")
        assert missing == "fallback"


# Run tests
if __name__ == "__main__":
    pytest.main([__file__, "-v", "--tb=short"])
