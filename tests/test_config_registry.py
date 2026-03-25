# ABOUTME: Comprehensive TDD tests for ConfigRegistry system
# ABOUTME: Tests auto-discovery, env overrides, inheritance, validation, caching, and hot-reload

"""
ConfigRegistry Test Suite
==========================

Comprehensive tests for the configuration registry system following TDD principles.

Test Coverage:
- Auto-discovery of YAML configs (recursive, with exclusions)
- Environment variable overrides (DM_ prefix with double underscore nesting)
- Config inheritance (YAML extends + preset loading)
- Validation with warnings for unknown keys
- LRU caching for performance
- Hot-reload functionality
- JSON schema generation
- Backward compatibility
"""

import os
import tempfile
import pytest
from pathlib import Path
from unittest.mock import patch, MagicMock
import yaml
import json

# Import will fail initially (TDD - tests first!)
try:
    from digitalmodel.infrastructure.config.registry import ConfigRegistry, ConfigValidationError
    from digitalmodel.infrastructure.config.compat import load_config  # Backward compatibility shim
except ImportError:
    pytest.skip("ConfigRegistry not implemented yet", allow_module_level=True)


class TestConfigAutoDiscovery:
    """Test automatic discovery of YAML configuration files."""

    @pytest.fixture
    def temp_config_dir(self, tmp_path):
        """Create temporary config directory structure."""
        base = tmp_path / "base_configs" / "modules"

        # Create module directories with configs
        (base / "mooring").mkdir(parents=True)
        (base / "orcaflex").mkdir(parents=True)
        (base / "catenary").mkdir(parents=True)

        # Create valid configs
        mooring_config = {
            "meta": {"library": "digitalmodel", "basename": "mooring"},
            "default": {"log_level": "DEBUG", "tolerance": 0.001}
        }
        with open(base / "mooring" / "mooring.yml", "w") as f:
            yaml.dump(mooring_config, f)

        orcaflex_config = {
            "meta": {"library": "orcaflex", "basename": "orcaflex"},
            "default": {"analysis": {"static": True, "dynamic": False}}
        }
        with open(base / "orcaflex" / "orcaflex.yaml", "w") as f:
            yaml.dump(orcaflex_config, f)

        # Create excluded files (should be ignored)
        with open(base / "mooring" / "mooring_template.yml", "w") as f:
            yaml.dump({"template": True}, f)

        with open(base / "mooring" / "mooring_example.yml", "w") as f:
            yaml.dump({"example": True}, f)

        with open(base / "mooring" / "mooring_test.yml", "w") as f:
            yaml.dump({"test": True}, f)

        # Create nested config
        (base / "catenary" / "risers").mkdir(parents=True)
        nested_config = {
            "meta": {"library": "catenary", "basename": "riser"},
            "default": {"pipe": {"OD": 10.75, "WT": 0.5}}
        }
        with open(base / "catenary" / "risers" / "riser.yml", "w") as f:
            yaml.dump(nested_config, f)

        return base

    def test_discover_all_valid_configs(self, temp_config_dir):
        """Test that registry discovers all valid .yml and .yaml files."""
        registry = ConfigRegistry(config_base_path=temp_config_dir.parent.parent)

        discovered = registry.list_configs()

        # Should find mooring.yml, orcaflex.yaml, and nested riser.yml
        assert len(discovered) == 3
        assert "mooring" in discovered
        assert "orcaflex" in discovered
        assert "riser" in discovered or "catenary.risers.riser" in discovered

    def test_exclude_template_files(self, temp_config_dir):
        """Test that template files are excluded from discovery."""
        registry = ConfigRegistry(config_base_path=temp_config_dir.parent.parent)

        discovered = registry.list_configs()

        # Should NOT include template, example, or test files
        config_names = [str(c) for c in discovered]
        assert not any("template" in name for name in config_names)
        assert not any("example" in name for name in config_names)
        assert not any("test" in name for name in config_names)

    def test_recursive_discovery(self, temp_config_dir):
        """Test that discovery works recursively through subdirectories."""
        registry = ConfigRegistry(config_base_path=temp_config_dir.parent.parent)

        discovered = registry.list_configs()

        # Should find nested riser.yml in catenary/risers/
        assert any("riser" in str(name) for name in discovered)

    def test_both_yml_and_yaml_extensions(self, temp_config_dir):
        """Test that both .yml and .yaml extensions are discovered."""
        registry = ConfigRegistry(config_base_path=temp_config_dir.parent.parent)

        configs = registry._discover_configs()

        extensions = [path.suffix for path in configs.values()]
        assert ".yml" in extensions
        assert ".yaml" in extensions


class TestEnvironmentVariableOverrides:
    """Test environment variable overrides with DM_ prefix."""

    @pytest.fixture
    def simple_config_dir(self, tmp_path):
        """Create simple config for env var testing."""
        base = tmp_path / "base_configs" / "modules" / "mooring"
        base.mkdir(parents=True)

        config = {
            "meta": {"library": "digitalmodel", "basename": "mooring"},
            "default": {
                "log_level": "INFO",
                "analysis": {
                    "tolerance": 0.001,
                    "max_iterations": 100
                },
                "output": {
                    "format": "csv",
                    "precision": 6
                }
            }
        }

        with open(base / "mooring.yml", "w") as f:
            yaml.dump(config, f)

        return base

    def test_simple_env_override(self, simple_config_dir):
        """Test simple top-level environment variable override."""
        with patch.dict(os.environ, {"DM_LOG_LEVEL": "DEBUG"}):
            registry = ConfigRegistry(config_base_path=simple_config_dir.parent.parent)
            config = registry.get_config("mooring")

            assert config["default"]["log_level"] == "DEBUG"

    def test_nested_env_override_double_underscore(self, simple_config_dir):
        """Test nested env var override using double underscore."""
        with patch.dict(os.environ, {
            "DM_ANALYSIS__TOLERANCE": "0.0001",
            "DM_ANALYSIS__MAX_ITERATIONS": "200"
        }):
            registry = ConfigRegistry(config_base_path=simple_config_dir.parent.parent)
            config = registry.get_config("mooring")

            assert config["default"]["analysis"]["tolerance"] == 0.0001
            assert config["default"]["analysis"]["max_iterations"] == 200

    def test_deep_nested_env_override(self, simple_config_dir):
        """Test deeply nested env var override."""
        with patch.dict(os.environ, {
            "DM_OUTPUT__FORMAT": "json",
            "DM_OUTPUT__PRECISION": "8"
        }):
            registry = ConfigRegistry(config_base_path=simple_config_dir.parent.parent)
            config = registry.get_config("mooring")

            assert config["default"]["output"]["format"] == "json"
            assert config["default"]["output"]["precision"] == 8

    def test_nonexistent_env_var_logs_warning(self, simple_config_dir, caplog):
        """Test that non-matching env vars log warning but don't fail."""
        with patch.dict(os.environ, {"DM_NONEXISTENT__KEY": "value"}):
            registry = ConfigRegistry(config_base_path=simple_config_dir.parent.parent)
            config = registry.get_config("mooring")

            # Should log warning
            assert "DM_NONEXISTENT__KEY" in caplog.text or "does not match" in caplog.text.lower()

            # Config should still load successfully
            assert config is not None

    def test_type_coercion_from_env_vars(self, simple_config_dir):
        """Test automatic type coercion from string env vars."""
        with patch.dict(os.environ, {
            "DM_ANALYSIS__TOLERANCE": "0.0005",  # String -> float
            "DM_ANALYSIS__MAX_ITERATIONS": "150",  # String -> int
        }):
            registry = ConfigRegistry(config_base_path=simple_config_dir.parent.parent)
            config = registry.get_config("mooring")

            assert isinstance(config["default"]["analysis"]["tolerance"], float)
            assert isinstance(config["default"]["analysis"]["max_iterations"], int)


class TestConfigInheritance:
    """Test config inheritance via YAML extends and preset loading."""

    @pytest.fixture
    def inheritance_config_dir(self, tmp_path):
        """Create config directory with inheritance setup."""
        base = tmp_path / "base_configs" / "modules"
        (base / "mooring").mkdir(parents=True)
        (base / "presets").mkdir(parents=True)

        # Base config
        base_config = {
            "meta": {"library": "digitalmodel", "basename": "base_mooring"},
            "default": {
                "log_level": "INFO",
                "analysis": {
                    "tolerance": 0.001,
                    "max_iterations": 100
                },
                "solver": {
                    "method": "newton",
                    "damping": 0.8
                }
            }
        }
        with open(base / "mooring" / "base_mooring.yml", "w") as f:
            yaml.dump(base_config, f)

        # Child config with extends
        child_config = {
            "extends": "base_mooring.yml",
            "default": {
                "log_level": "DEBUG",  # Override
                "analysis": {
                    "tolerance": 0.0001  # Override nested
                    # max_iterations should inherit
                }
            }
        }
        with open(base / "mooring" / "advanced_mooring.yml", "w") as f:
            yaml.dump(child_config, f)

        # Production preset
        prod_preset = {
            "default": {
                "log_level": "WARNING",
                "analysis": {
                    "tolerance": 0.00001,
                    "max_iterations": 500
                }
            }
        }
        with open(base / "presets" / "production.yml", "w") as f:
            yaml.dump(prod_preset, f)

        # Config with preset reference
        preset_config = {
            "preset": "production",
            "default": {
                "solver": {
                    "method": "quasi-newton"
                }
            }
        }
        with open(base / "mooring" / "production_mooring.yml", "w") as f:
            yaml.dump(preset_config, f)

        return base

    def test_yaml_extends_inheritance(self, inheritance_config_dir):
        """Test YAML-based config inheritance using extends directive."""
        registry = ConfigRegistry(config_base_path=inheritance_config_dir.parent)
        config = registry.get_config("advanced_mooring")

        # Should override log_level
        assert config["default"]["log_level"] == "DEBUG"

        # Should override nested tolerance
        assert config["default"]["analysis"]["tolerance"] == 0.0001

        # Should inherit max_iterations from base
        assert config["default"]["analysis"]["max_iterations"] == 100

        # Should inherit entire solver section
        assert config["default"]["solver"]["method"] == "newton"

    def test_preset_based_inheritance(self, inheritance_config_dir):
        """Test preset-based inheritance."""
        registry = ConfigRegistry(config_base_path=inheritance_config_dir.parent)
        config = registry.get_config("production_mooring")

        # Should inherit from production preset
        assert config["default"]["log_level"] == "WARNING"
        assert config["default"]["analysis"]["tolerance"] == 0.00001
        assert config["default"]["analysis"]["max_iterations"] == 500

        # Should merge with own config
        assert config["default"]["solver"]["method"] == "quasi-newton"

    def test_deep_merge_preserves_non_overlapping_keys(self, inheritance_config_dir):
        """Test that deep merge preserves non-overlapping keys at all levels."""
        registry = ConfigRegistry(config_base_path=inheritance_config_dir.parent)
        config = registry.get_config("advanced_mooring")

        # Child doesn't define solver, should inherit entire section
        assert "solver" in config["default"]
        assert config["default"]["solver"]["method"] == "newton"
        assert config["default"]["solver"]["damping"] == 0.8

    def test_child_overrides_parent_completely(self, inheritance_config_dir):
        """Test that child can completely override parent values."""
        registry = ConfigRegistry(config_base_path=inheritance_config_dir.parent)
        config = registry.get_config("advanced_mooring")

        # log_level should be completely overridden
        assert config["default"]["log_level"] == "DEBUG"
        assert config["default"]["log_level"] != "INFO"  # Not the parent value


class TestConfigValidation:
    """Test Pydantic-based validation with warnings for unknown keys."""

    @pytest.fixture
    def validation_config_dir(self, tmp_path):
        """Create config directory for validation testing."""
        base = tmp_path / "base_configs" / "modules" / "mooring"
        base.mkdir(parents=True)

        # Valid config
        valid_config = {
            "meta": {"library": "digitalmodel", "basename": "mooring"},
            "default": {
                "log_level": "DEBUG",
                "analysis": {"tolerance": 0.001}
            }
        }
        with open(base / "valid.yml", "w") as f:
            yaml.dump(valid_config, f)

        # Config with unknown keys
        unknown_keys_config = {
            "meta": {"library": "digitalmodel", "basename": "mooring"},
            "default": {
                "log_level": "DEBUG",
                "unknown_key": "should_warn",
                "analysis": {
                    "tolerance": 0.001,
                    "another_unknown": "also_warn"
                }
            }
        }
        with open(base / "unknown_keys.yml", "w") as f:
            yaml.dump(unknown_keys_config, f)

        return base

    def test_valid_config_passes_validation(self, validation_config_dir):
        """Test that valid configs pass validation without warnings."""
        registry = ConfigRegistry(config_base_path=validation_config_dir.parent.parent)
        config = registry.get_config("valid")

        assert config is not None
        assert config["default"]["log_level"] == "DEBUG"

    def test_unknown_keys_log_warning_but_dont_fail(self, validation_config_dir, caplog):
        """Test that unknown keys generate warnings but don't fail validation."""
        registry = ConfigRegistry(config_base_path=validation_config_dir.parent.parent)
        config = registry.get_config("unknown_keys")

        # Should load successfully
        assert config is not None

        # Should log warnings about unknown keys
        log_text = caplog.text.lower()
        assert "unknown" in log_text or "warn" in log_text

    def test_validation_preserves_unknown_keys(self, validation_config_dir):
        """Test that validation warnings preserve the unknown keys in config."""
        registry = ConfigRegistry(config_base_path=validation_config_dir.parent.parent)
        config = registry.get_config("unknown_keys")

        # Unknown keys should still be in the config (permissive validation)
        assert config["default"]["unknown_key"] == "should_warn"
        assert config["default"]["analysis"]["another_unknown"] == "also_warn"


class TestLRUCaching:
    """Test LRU caching for config loading performance."""

    @pytest.fixture
    def cache_config_dir(self, tmp_path):
        """Create config directory for caching tests."""
        base = tmp_path / "base_configs" / "modules" / "mooring"
        base.mkdir(parents=True)

        config = {
            "meta": {"library": "digitalmodel", "basename": "mooring"},
            "default": {"log_level": "DEBUG"}
        }
        with open(base / "mooring.yml", "w") as f:
            yaml.dump(config, f)

        return base

    def test_cache_hit_returns_same_object(self, cache_config_dir):
        """Test that cached configs return same object reference."""
        registry = ConfigRegistry(config_base_path=cache_config_dir.parent.parent)

        config1 = registry.get_config("mooring")
        config2 = registry.get_config("mooring")

        # Should return same cached object
        assert config1 is config2

    def test_cache_invalidation_on_clear(self, cache_config_dir):
        """Test that cache can be cleared."""
        registry = ConfigRegistry(config_base_path=cache_config_dir.parent.parent)

        config1 = registry.get_config("mooring")
        registry.clear_cache()
        config2 = registry.get_config("mooring")

        # Should be different objects after cache clear
        assert config1 is not config2

    def test_cache_respects_maxsize(self, cache_config_dir):
        """Test that cache respects LRU maxsize."""
        # Create registry with small cache
        registry = ConfigRegistry(
            config_base_path=cache_config_dir.parent.parent,
            cache_maxsize=2
        )

        # Add 3 configs to cache (exceeds maxsize)
        config1 = registry.get_config("mooring")

        # Create more configs
        (cache_config_dir / "config2.yml").write_text(yaml.dump({"default": {}}))
        (cache_config_dir / "config3.yml").write_text(yaml.dump({"default": {}}))

        config2 = registry.get_config("config2")
        config3 = registry.get_config("config3")

        # First config should have been evicted
        config1_again = registry.get_config("mooring")
        assert config1 is not config1_again  # Cache miss, new object


class TestHotReload:
    """Test hot-reload functionality for config changes."""

    @pytest.fixture
    def reload_config_dir(self, tmp_path):
        """Create config directory for hot-reload testing."""
        base = tmp_path / "base_configs" / "modules" / "mooring"
        base.mkdir(parents=True)

        config = {
            "meta": {"library": "digitalmodel", "basename": "mooring"},
            "default": {"log_level": "INFO", "value": 100}
        }

        config_file = base / "mooring.yml"
        with open(config_file, "w") as f:
            yaml.dump(config, f)

        return base, config_file

    def test_reload_detects_file_changes(self, reload_config_dir):
        """Test that reload() reloads changed configs."""
        base, config_file = reload_config_dir

        registry = ConfigRegistry(config_base_path=base.parent.parent)
        original_config = registry.get_config("mooring")
        assert original_config["default"]["value"] == 100

        # Modify config file
        modified_config = {
            "meta": {"library": "digitalmodel", "basename": "mooring"},
            "default": {"log_level": "DEBUG", "value": 200}
        }
        with open(config_file, "w") as f:
            yaml.dump(modified_config, f)

        # Reload
        registry.reload()

        # Should get new values
        new_config = registry.get_config("mooring")
        assert new_config["default"]["value"] == 200
        assert new_config["default"]["log_level"] == "DEBUG"

    def test_hot_reload_env_var_control(self, reload_config_dir):
        """Test that hot-reload can be controlled via DM_HOT_RELOAD env var."""
        base, config_file = reload_config_dir

        with patch.dict(os.environ, {"DM_HOT_RELOAD": "true"}):
            registry = ConfigRegistry(config_base_path=base.parent.parent)

            # Hot reload should be enabled
            assert registry.hot_reload_enabled is True

    def test_reload_clears_cache(self, reload_config_dir):
        """Test that reload() clears the config cache."""
        base, config_file = reload_config_dir

        registry = ConfigRegistry(config_base_path=base.parent.parent)
        config1 = registry.get_config("mooring")

        registry.reload()

        config2 = registry.get_config("mooring")

        # Should be different objects after reload
        assert config1 is not config2


class TestJSONSchemaGeneration:
    """Test automatic JSON schema generation from configs."""

    @pytest.fixture
    def schema_config_dir(self, tmp_path):
        """Create config directory for schema testing."""
        base = tmp_path / "base_configs" / "modules" / "mooring"
        base.mkdir(parents=True)

        config = {
            "meta": {"library": "digitalmodel", "basename": "mooring"},
            "default": {
                "log_level": "DEBUG",
                "analysis": {
                    "tolerance": 0.001,
                    "max_iterations": 100,
                    "enabled": True
                },
                "output": {
                    "format": "csv",
                    "precision": 6
                }
            }
        }

        with open(base / "mooring.yml", "w") as f:
            yaml.dump(config, f)

        return base

    def test_generate_json_schema_from_config(self, schema_config_dir):
        """Test JSON schema generation from existing config."""
        registry = ConfigRegistry(config_base_path=schema_config_dir.parent.parent)
        schema = registry.generate_json_schema("mooring")

        assert schema is not None
        assert "$schema" in schema
        assert "properties" in schema

    def test_json_schema_infers_types(self, schema_config_dir):
        """Test that JSON schema correctly infers types."""
        registry = ConfigRegistry(config_base_path=schema_config_dir.parent.parent)
        schema = registry.generate_json_schema("mooring")

        # Should infer types from values
        default_props = schema["properties"]["default"]["properties"]

        assert default_props["log_level"]["type"] == "string"

        analysis_props = default_props["analysis"]["properties"]
        assert analysis_props["tolerance"]["type"] == "number"
        assert analysis_props["max_iterations"]["type"] == "integer"
        assert analysis_props["enabled"]["type"] == "boolean"

    def test_export_schema_to_file(self, schema_config_dir, tmp_path):
        """Test exporting schema to JSON file."""
        registry = ConfigRegistry(config_base_path=schema_config_dir.parent.parent)

        schema_file = tmp_path / "mooring_schema.json"
        registry.export_json_schema("mooring", schema_file)

        assert schema_file.exists()

        # Validate it's valid JSON
        with open(schema_file) as f:
            schema = json.load(f)

        assert "$schema" in schema
        assert "properties" in schema


class TestBackwardCompatibility:
    """Test backward compatibility with existing config_loader.py."""

    @pytest.fixture
    def compat_config_dir(self, tmp_path):
        """Create config directory for compatibility testing."""
        base = tmp_path / "base_configs" / "modules" / "mooring"
        base.mkdir(parents=True)

        config = {
            "meta": {"library": "digitalmodel", "basename": "mooring"},
            "default": {
                "log_level": "DEBUG",
                "analysis": {"tolerance": 0.001}
            }
        }

        config_file = base / "mooring.yml"
        with open(config_file, "w") as f:
            yaml.dump(config, f)

        return config_file

    def test_load_config_compatibility_function(self, compat_config_dir):
        """Test that load_config() shim works like old ConfigLoader."""
        # Should work with path to config file
        config = load_config(str(compat_config_dir))

        assert config is not None
        assert "default" in config
        assert config["default"]["log_level"] == "DEBUG"

    def test_load_config_returns_dict(self, compat_config_dir):
        """Test that compatibility function returns dict like original."""
        config = load_config(str(compat_config_dir))

        assert isinstance(config, dict)

    def test_load_config_supports_env_vars(self, compat_config_dir):
        """Test that compatibility shim supports env var overrides."""
        with patch.dict(os.environ, {"DM_LOG_LEVEL": "WARNING"}):
            config = load_config(str(compat_config_dir))

            assert config["default"]["log_level"] == "WARNING"


class TestConfigRegistryAPI:
    """Test ConfigRegistry public API."""

    @pytest.fixture
    def api_config_dir(self, tmp_path):
        """Create config directory for API testing."""
        base = tmp_path / "base_configs" / "modules"
        (base / "mooring").mkdir(parents=True)
        (base / "orcaflex").mkdir(parents=True)

        mooring_config = {
            "meta": {"library": "digitalmodel", "basename": "mooring"},
            "default": {"log_level": "DEBUG"}
        }
        with open(base / "mooring" / "mooring.yml", "w") as f:
            yaml.dump(mooring_config, f)

        orcaflex_config = {
            "meta": {"library": "orcaflex", "basename": "orcaflex"},
            "default": {"analysis": {"static": True}}
        }
        with open(base / "orcaflex" / "orcaflex.yml", "w") as f:
            yaml.dump(orcaflex_config, f)

        return base

    def test_list_configs_returns_all_discovered(self, api_config_dir):
        """Test list_configs() returns all discovered configs."""
        registry = ConfigRegistry(config_base_path=api_config_dir.parent)
        configs = registry.list_configs()

        assert len(configs) >= 2
        assert "mooring" in configs or any("mooring" in str(c) for c in configs)

    def test_get_config_by_name(self, api_config_dir):
        """Test get_config() retrieves config by name."""
        registry = ConfigRegistry(config_base_path=api_config_dir.parent)
        config = registry.get_config("mooring")

        assert config is not None
        assert config["default"]["log_level"] == "DEBUG"

    def test_get_config_raises_on_not_found(self, api_config_dir):
        """Test get_config() raises error for non-existent config."""
        registry = ConfigRegistry(config_base_path=api_config_dir.parent)

        with pytest.raises((KeyError, FileNotFoundError)):
            registry.get_config("nonexistent_config")

    def test_has_config_checks_existence(self, api_config_dir):
        """Test has_config() checks if config exists."""
        registry = ConfigRegistry(config_base_path=api_config_dir.parent)

        assert registry.has_config("mooring") is True
        assert registry.has_config("nonexistent") is False

    def test_get_config_with_dot_notation(self, api_config_dir):
        """Test accessing nested config values with dot notation."""
        registry = ConfigRegistry(config_base_path=api_config_dir.parent)

        # Get nested value using dot notation
        value = registry.get_value("orcaflex", "default.analysis.static")

        assert value is True

    def test_get_value_with_default(self, api_config_dir):
        """Test get_value() with default for non-existent keys."""
        registry = ConfigRegistry(config_base_path=api_config_dir.parent)

        value = registry.get_value("mooring", "nonexistent.key", default="default_value")

        assert value == "default_value"


class TestEdgeCases:
    """Test edge cases and error handling."""

    def test_empty_config_directory(self, tmp_path):
        """Test registry behavior with empty config directory."""
        base = tmp_path / "base_configs" / "modules"
        base.mkdir(parents=True)

        registry = ConfigRegistry(config_base_path=base.parent)
        configs = registry.list_configs()

        assert len(configs) == 0

    def test_malformed_yaml_config(self, tmp_path):
        """Test handling of malformed YAML files."""
        base = tmp_path / "base_configs" / "modules" / "mooring"
        base.mkdir(parents=True)

        # Create invalid YAML
        config_file = base / "invalid.yml"
        config_file.write_text("invalid: yaml: content: [unclosed")

        registry = ConfigRegistry(config_base_path=base.parent.parent)

        with pytest.raises(yaml.YAMLError):
            registry.get_config("invalid")

    def test_circular_inheritance(self, tmp_path):
        """Test detection of circular inheritance."""
        base = tmp_path / "base_configs" / "modules" / "mooring"
        base.mkdir(parents=True)

        # Config A extends B
        config_a = {"extends": "config_b.yml", "default": {"a": 1}}
        with open(base / "config_a.yml", "w") as f:
            yaml.dump(config_a, f)

        # Config B extends A (circular!)
        config_b = {"extends": "config_a.yml", "default": {"b": 2}}
        with open(base / "config_b.yml", "w") as f:
            yaml.dump(config_b, f)

        registry = ConfigRegistry(config_base_path=base.parent.parent)

        with pytest.raises((RecursionError, ValueError)):
            registry.get_config("config_a")

    def test_missing_base_config_in_extends(self, tmp_path):
        """Test error handling when extended config doesn't exist."""
        base = tmp_path / "base_configs" / "modules" / "mooring"
        base.mkdir(parents=True)

        config = {"extends": "nonexistent.yml", "default": {"a": 1}}
        with open(base / "child.yml", "w") as f:
            yaml.dump(config, f)

        registry = ConfigRegistry(config_base_path=base.parent.parent)

        with pytest.raises(FileNotFoundError):
            registry.get_config("child")

    def test_nonexistent_preset(self, tmp_path):
        """Test error handling when preset doesn't exist."""
        base = tmp_path / "base_configs" / "modules" / "mooring"
        base.mkdir(parents=True)

        config = {"preset": "nonexistent_preset", "default": {"a": 1}}
        with open(base / "config.yml", "w") as f:
            yaml.dump(config, f)

        registry = ConfigRegistry(config_base_path=base.parent.parent)

        with pytest.raises(FileNotFoundError):
            registry.get_config("config")


# Integration tests
class TestFullIntegration:
    """Integration tests combining multiple features."""

    @pytest.fixture
    def full_config_dir(self, tmp_path):
        """Create comprehensive config structure."""
        base = tmp_path / "base_configs" / "modules"
        (base / "mooring").mkdir(parents=True)
        (base / "presets").mkdir(parents=True)

        # Base config
        base_config = {
            "meta": {"library": "digitalmodel", "basename": "base"},
            "default": {
                "log_level": "INFO",
                "analysis": {"tolerance": 0.001, "max_iterations": 100},
                "solver": {"method": "newton"}
            }
        }
        with open(base / "mooring" / "base.yml", "w") as f:
            yaml.dump(base_config, f)

        # Production preset
        prod_preset = {
            "default": {
                "log_level": "WARNING",
                "analysis": {"tolerance": 0.00001}
            }
        }
        with open(base / "presets" / "production.yml", "w") as f:
            yaml.dump(prod_preset, f)

        # Complex config: extends base + uses preset + has own overrides
        complex_config = {
            "extends": "base.yml",
            "preset": "production",
            "default": {
                "solver": {"method": "quasi-newton", "damping": 0.9}
            }
        }
        with open(base / "mooring" / "complex.yml", "w") as f:
            yaml.dump(complex_config, f)

        return base

    def test_combined_inheritance_and_env_override(self, full_config_dir):
        """Test combination of extends, preset, and env var overrides."""
        with patch.dict(os.environ, {
            "DM_ANALYSIS__MAX_ITERATIONS": "500"
        }):
            registry = ConfigRegistry(config_base_path=full_config_dir.parent)
            config = registry.get_config("complex")

            # From preset
            assert config["default"]["log_level"] == "WARNING"
            assert config["default"]["analysis"]["tolerance"] == 0.00001

            # From base (inherited)
            assert config["default"]["analysis"]["max_iterations"] == 500  # Overridden by env

            # From own config
            assert config["default"]["solver"]["method"] == "quasi-newton"
            assert config["default"]["solver"]["damping"] == 0.9

    def test_full_workflow_with_caching(self, full_config_dir):
        """Test complete workflow: discover, load, cache, reload."""
        registry = ConfigRegistry(config_base_path=full_config_dir.parent)

        # Discovery
        configs = registry.list_configs()
        assert len(configs) >= 2

        # Load and cache
        config1 = registry.get_config("complex")
        config2 = registry.get_config("complex")
        assert config1 is config2  # Cached

        # Generate schema
        schema = registry.generate_json_schema("complex")
        assert schema is not None

        # Reload
        registry.reload()
        config3 = registry.get_config("complex")
        assert config1 is not config3  # New object after reload
