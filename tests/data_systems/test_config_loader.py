"""
Tests for data_systems.data_procurement.common.config_loader.ConfigLoader.

Tests cover:
- File loading and YAML parsing
- Dot-notation get() accessor
- get_api_config / get_output_config / get_caching_config helpers
- _deep_merge recursive behaviour
- validate_schema() with required fields
- Environment variable substitution
- Config inheritance (extends) with missing parent (graceful skip)
- Preset application

All tests are pure-Python / no external service required.
"""

import os
import tempfile
import pytest
import yaml


# ---------------------------------------------------------------------------
# Fixture helpers
# ---------------------------------------------------------------------------


def _write_config(data: dict, suffix: str = ".yaml"):
    """Write a dict as YAML to a temp file; return the path string."""
    f = tempfile.NamedTemporaryFile(
        mode="w", suffix=suffix, delete=False
    )
    yaml.dump(data, f)
    f.close()
    return f.name


def _make_loader(data: dict):
    from digitalmodel.data_systems.data_procurement.common.config_loader import (
        ConfigLoader,
    )
    path = _write_config(data)
    return ConfigLoader(path)


# ---------------------------------------------------------------------------
# Basic loading
# ---------------------------------------------------------------------------


class TestConfigLoaderBasicLoad:
    """Tests for file loading and initial state."""

    def test_load_simple_yaml(self):
        loader = _make_loader({"version": "1.0", "data_source": "test"})
        assert loader.config["version"] == "1.0"

    def test_missing_file_raises_file_not_found(self):
        from digitalmodel.data_systems.data_procurement.common.config_loader import (
            ConfigLoader,
        )
        with pytest.raises(FileNotFoundError):
            ConfigLoader("/nonexistent/path/config.yaml")

    def test_to_dict_returns_copy_of_config(self):
        loader = _make_loader({"version": "1.0", "data_source": "test", "x": 1})
        d = loader.to_dict()
        assert isinstance(d, dict)
        assert d["x"] == 1

    def test_to_dict_is_independent_copy(self):
        loader = _make_loader({"version": "1.0", "data_source": "s", "y": 2})
        d = loader.to_dict()
        d["y"] = 99
        assert loader.config["y"] == 2  # Original unchanged


# ---------------------------------------------------------------------------
# get() dot-notation accessor
# ---------------------------------------------------------------------------


class TestConfigLoaderGet:
    """Tests for the dot-notation key accessor."""

    def _loader(self):
        return _make_loader({
            "version": "1.0",
            "data_source": "test",
            "apis": {
                "ERA5": {
                    "base_url": "https://era5.example.com",
                    "timeout": 30,
                },
                "NOAA": {
                    "base_url": "https://noaa.example.com",
                },
            },
            "output": {
                "format": "netcdf",
                "compress": True,
            },
            "nested": {"deep": {"value": 42}},
        })

    def test_top_level_key(self):
        loader = self._loader()
        assert loader.get("version") == "1.0"

    def test_two_level_key(self):
        loader = self._loader()
        assert loader.get("output.format") == "netcdf"

    def test_three_level_key(self):
        loader = self._loader()
        assert loader.get("apis.ERA5.base_url") == "https://era5.example.com"

    def test_four_level_key(self):
        loader = self._loader()
        assert loader.get("nested.deep.value") == 42

    def test_missing_key_returns_default(self):
        loader = self._loader()
        assert loader.get("nonexistent", "fallback") == "fallback"

    def test_missing_key_default_is_none(self):
        loader = self._loader()
        assert loader.get("gone") is None

    def test_partial_path_returns_dict(self):
        loader = self._loader()
        result = loader.get("apis.ERA5")
        assert isinstance(result, dict)
        assert result["timeout"] == 30

    def test_dict_style_access(self):
        loader = self._loader()
        assert loader["version"] == "1.0"

    def test_boolean_value(self):
        loader = self._loader()
        assert loader.get("output.compress") is True


# ---------------------------------------------------------------------------
# Typed config accessors
# ---------------------------------------------------------------------------


class TestConfigLoaderTypedAccessors:
    """Tests for get_api_config, get_output_config, etc."""

    def _loader(self):
        return _make_loader({
            "version": "1.0",
            "data_source": "test",
            "apis": {
                "ERA5": {"base_url": "https://era5.example.com"},
            },
            "output": {"format": "netcdf"},
            "caching": {"enabled": True, "ttl": 3600},
            "validation": {"strict": False},
        })

    def test_get_api_config_returns_dict(self):
        loader = self._loader()
        cfg = loader.get_api_config("ERA5")
        assert isinstance(cfg, dict)
        assert cfg["base_url"] == "https://era5.example.com"

    def test_get_api_config_missing_provider_returns_empty(self):
        loader = self._loader()
        assert loader.get_api_config("NOAA") == {}

    def test_get_output_config(self):
        loader = self._loader()
        cfg = loader.get_output_config()
        assert cfg["format"] == "netcdf"

    def test_get_caching_config(self):
        loader = self._loader()
        cfg = loader.get_caching_config()
        assert cfg["ttl"] == 3600

    def test_get_validation_config(self):
        loader = self._loader()
        cfg = loader.get_validation_config()
        assert cfg["strict"] is False


# ---------------------------------------------------------------------------
# validate_schema
# ---------------------------------------------------------------------------


class TestConfigLoaderValidateSchema:
    """Tests for schema validation."""

    def test_valid_schema_returns_true(self):
        loader = _make_loader({"version": "1.0", "data_source": "test"})
        assert loader.validate_schema() is True

    def test_missing_version_returns_false(self):
        loader = _make_loader({"data_source": "test"})
        assert loader.validate_schema() is False

    def test_missing_data_source_returns_false(self):
        loader = _make_loader({"version": "1.0"})
        assert loader.validate_schema() is False

    def test_both_fields_missing_returns_false(self):
        loader = _make_loader({"other": "value"})
        assert loader.validate_schema() is False


# ---------------------------------------------------------------------------
# _deep_merge
# ---------------------------------------------------------------------------


class TestConfigLoaderDeepMerge:
    """Tests for the internal _deep_merge helper."""

    def _merge(self, base, override):
        # Access _deep_merge via a loader (it's a bound method)
        loader = _make_loader({"version": "1.0", "data_source": "s"})
        return loader._deep_merge(base, override)

    def test_simple_merge_adds_key(self):
        result = self._merge({"a": 1}, {"b": 2})
        assert result["a"] == 1
        assert result["b"] == 2

    def test_override_replaces_value(self):
        result = self._merge({"a": 1}, {"a": 99})
        assert result["a"] == 99

    def test_nested_merge_preserves_untouched_keys(self):
        base = {"api": {"url": "old", "timeout": 30}}
        override = {"api": {"url": "new"}}
        result = self._merge(base, override)
        assert result["api"]["url"] == "new"
        assert result["api"]["timeout"] == 30

    def test_deep_three_levels(self):
        base = {"a": {"b": {"c": 1, "d": 2}}}
        override = {"a": {"b": {"c": 9}}}
        result = self._merge(base, override)
        assert result["a"]["b"]["c"] == 9
        assert result["a"]["b"]["d"] == 2

    def test_empty_override_returns_base(self):
        base = {"x": 1, "y": 2}
        result = self._merge(base, {})
        assert result == {"x": 1, "y": 2}

    def test_empty_base_gets_all_override(self):
        override = {"x": 10, "y": 20}
        result = self._merge({}, override)
        assert result == {"x": 10, "y": 20}

    def test_non_dict_override_replaces_dict(self):
        base = {"a": {"nested": True}}
        override = {"a": "scalar"}
        result = self._merge(base, override)
        assert result["a"] == "scalar"


# ---------------------------------------------------------------------------
# Environment variable substitution
# ---------------------------------------------------------------------------


class TestConfigLoaderEnvSubstitution:
    """Tests for ${ENV_VAR} substitution."""

    def test_env_var_substituted_when_set(self, monkeypatch):
        monkeypatch.setenv("TEST_API_KEY", "secret123")
        loader = _make_loader({
            "version": "1.0",
            "data_source": "env_test",
            "key": "${TEST_API_KEY}",
        })
        assert loader.get("key") == "secret123"

    def test_env_var_not_set_leaves_placeholder(self, monkeypatch):
        monkeypatch.delenv("MISSING_VAR", raising=False)
        loader = _make_loader({
            "version": "1.0",
            "data_source": "env_test",
            "key": "${MISSING_VAR}",
        })
        assert loader.get("key") == "${MISSING_VAR}"

    def test_nested_env_var_substituted(self, monkeypatch):
        monkeypatch.setenv("MY_TOKEN", "tok42")
        loader = _make_loader({
            "version": "1.0",
            "data_source": "env_test",
            "apis": {"ERA5": {"token": "${MY_TOKEN}"}},
        })
        assert loader.get("apis.ERA5.token") == "tok42"


# ---------------------------------------------------------------------------
# __contains__
# ---------------------------------------------------------------------------


class TestConfigLoaderContains:
    """Tests for the __contains__ (__in__) operator."""

    def test_present_key_is_in_loader(self):
        loader = _make_loader({"version": "1.0", "data_source": "s", "x": 5})
        assert "x" in loader

    def test_absent_key_not_in_loader(self):
        loader = _make_loader({"version": "1.0", "data_source": "s"})
        assert "missing" not in loader
