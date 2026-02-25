"""
ABOUTME: Tests for centralised configuration system
ABOUTME: Validates settings, environment variables, and validation logic
"""

import pytest
from pathlib import Path
import os
from digitalmodel.config import GlobalSettings, get_settings, override_settings, reset_settings


class TestGlobalSettings:
    """Test GlobalSettings class"""

    def test_default_settings(self):
        """Test that default settings are created correctly"""
        settings = GlobalSettings()

        # Path defaults
        assert settings.data_dir == Path("./data").resolve()
        assert settings.output_dir == Path("./reports").resolve()
        assert settings.log_dir == Path("./logs").resolve()
        assert settings.cache_dir == Path("./cache").resolve()

        # Engineering defaults
        assert settings.safety_factor == 1.5
        assert settings.material_database is None
        assert settings.sn_curve_database is None

        # Analysis defaults
        assert settings.default_analysis_mode == "deterministic"
        assert settings.max_iterations == 1000
        assert settings.convergence_tolerance == 1e-6

        # OrcaFlex defaults
        assert settings.orcaflex_workers == 30
        assert settings.orcaflex_license_timeout == 300

        # Reporting defaults
        assert settings.report_format == "html"
        assert settings.interactive_plots is True
        assert settings.plot_theme == "plotly_white"

        # Performance defaults
        assert settings.enable_caching is True
        assert settings.max_memory_mb == 4096

        # Logging defaults
        assert settings.log_level == "INFO"
        assert settings.log_format == "standard"

        # Development defaults
        assert settings.environment == "production"
        assert settings.debug_mode is False

    def test_custom_settings(self):
        """Test creating settings with custom values"""
        settings = GlobalSettings(
            safety_factor=2.0,
            orcaflex_workers=50,
            log_level="DEBUG",
            environment="development",
            debug_mode=True
        )

        assert settings.safety_factor == 2.0
        assert settings.orcaflex_workers == 50
        assert settings.log_level == "DEBUG"
        assert settings.environment == "development"
        assert settings.debug_mode is True

    def test_validation_safety_factor_range(self):
        """Test safety factor must be within valid range"""
        # Valid range: 1.0 to 5.0
        GlobalSettings(safety_factor=1.0)  # Min valid
        GlobalSettings(safety_factor=5.0)  # Max valid

        # Invalid values
        with pytest.raises(ValueError):
            GlobalSettings(safety_factor=0.5)  # Too low

        with pytest.raises(ValueError):
            GlobalSettings(safety_factor=6.0)  # Too high

    def test_validation_max_iterations_range(self):
        """Test max_iterations must be within valid range"""
        # Valid range: 10 to 100000
        GlobalSettings(max_iterations=10)     # Min valid
        GlobalSettings(max_iterations=100000) # Max valid

        # Invalid values
        with pytest.raises(ValueError):
            GlobalSettings(max_iterations=5)  # Too low

        with pytest.raises(ValueError):
            GlobalSettings(max_iterations=200000)  # Too high

    def test_validation_convergence_tolerance(self):
        """Test convergence_tolerance must be positive"""
        GlobalSettings(convergence_tolerance=1e-10)  # Valid

        # Invalid values
        with pytest.raises(ValueError):
            GlobalSettings(convergence_tolerance=0.0)  # Zero not allowed

        with pytest.raises(ValueError):
            GlobalSettings(convergence_tolerance=-1e-6)  # Negative not allowed

    def test_validation_orcaflex_workers_range(self):
        """Test orcaflex_workers must be within valid range"""
        # Valid range: 1 to 100
        GlobalSettings(orcaflex_workers=1)    # Min valid
        GlobalSettings(orcaflex_workers=100)  # Max valid

        # Invalid values
        with pytest.raises(ValueError):
            GlobalSettings(orcaflex_workers=0)  # Too low

        with pytest.raises(ValueError):
            GlobalSettings(orcaflex_workers=150)  # Too high

    def test_path_expansion(self):
        """Test that paths are expanded to absolute paths"""
        settings = GlobalSettings(data_dir="~/data")

        # Should be expanded and absolute
        assert settings.data_dir.is_absolute()
        assert str(settings.data_dir) != "~/data"

    def test_literal_fields_validation(self):
        """Test that literal fields only accept valid values"""
        # Valid values
        GlobalSettings(default_analysis_mode="deterministic")
        GlobalSettings(default_analysis_mode="probabilistic")

        GlobalSettings(report_format="html")
        GlobalSettings(report_format="json")
        GlobalSettings(report_format="csv")
        GlobalSettings(report_format="all")

        GlobalSettings(environment="development")
        GlobalSettings(environment="testing")
        GlobalSettings(environment="production")

        # Invalid values
        with pytest.raises(ValueError):
            GlobalSettings(default_analysis_mode="invalid")

        with pytest.raises(ValueError):
            GlobalSettings(report_format="pdf")

        with pytest.raises(ValueError):
            GlobalSettings(environment="staging")


class TestSettingsSingleton:
    """Test settings singleton pattern"""

    def setup_method(self):
        """Reset settings before each test"""
        reset_settings()

    def test_get_settings_returns_same_instance(self):
        """Test that get_settings returns the same instance"""
        settings1 = get_settings()
        settings2 = get_settings()

        assert settings1 is settings2

    def test_get_settings_creates_directories(self, tmp_path):
        """Test that get_settings creates necessary directories"""
        data_dir = tmp_path / "data"
        output_dir = tmp_path / "output"
        log_dir = tmp_path / "logs"
        cache_dir = tmp_path / "cache"

        # Override with custom paths
        settings = override_settings(
            data_dir=data_dir,
            output_dir=output_dir,
            log_dir=log_dir,
            cache_dir=cache_dir
        )

        # Directories should exist
        assert data_dir.exists()
        assert output_dir.exists()
        assert log_dir.exists()
        assert cache_dir.exists()

    def test_override_settings_creates_new_instance(self):
        """Test that override_settings creates a new instance"""
        settings1 = get_settings()
        settings2 = override_settings(safety_factor=2.0)

        assert settings1 is not settings2
        assert settings2.safety_factor == 2.0

    def test_reset_settings_clears_instance(self):
        """Test that reset_settings clears the singleton"""
        settings1 = get_settings()
        reset_settings()
        settings2 = get_settings()

        assert settings1 is not settings2


class TestEnvironmentVariables:
    """Test environment variable configuration"""

    def setup_method(self):
        """Clear environment variables before each test"""
        reset_settings()
        for key in list(os.environ.keys()):
            if key.startswith("DM_"):
                del os.environ[key]

    def teardown_method(self):
        """Clean up environment variables after each test"""
        for key in list(os.environ.keys()):
            if key.startswith("DM_"):
                del os.environ[key]

    def test_environment_variable_override(self):
        """Test that environment variables override defaults"""
        os.environ["DM_SAFETY_FACTOR"] = "2.5"
        os.environ["DM_ORCAFLEX_WORKERS"] = "50"
        os.environ["DM_LOG_LEVEL"] = "DEBUG"

        reset_settings()  # Force reload
        settings = get_settings()

        assert settings.safety_factor == 2.5
        assert settings.orcaflex_workers == 50
        assert settings.log_level == "DEBUG"

    def test_environment_variable_path_override(self):
        """Test that environment variables work for paths"""
        os.environ["DM_DATA_DIR"] = "/custom/data"

        reset_settings()
        settings = get_settings()

        assert settings.data_dir == Path("/custom/data").resolve()

    def test_environment_variable_boolean_override(self):
        """Test that environment variables work for booleans"""
        os.environ["DM_DEBUG_MODE"] = "true"
        os.environ["DM_INTERACTIVE_PLOTS"] = "false"

        reset_settings()
        settings = get_settings()

        assert settings.debug_mode is True
        assert settings.interactive_plots is False


class TestIntegrationScenarios:
    """Test real-world usage scenarios"""

    def setup_method(self):
        """Reset settings before each test"""
        reset_settings()

    def test_development_configuration(self):
        """Test typical development configuration"""
        settings = override_settings(
            environment="development",
            debug_mode=True,
            log_level="DEBUG",
            enable_caching=False,
            orcaflex_workers=2
        )

        assert settings.environment == "development"
        assert settings.debug_mode is True
        assert settings.log_level == "DEBUG"
        assert settings.enable_caching is False
        assert settings.orcaflex_workers == 2

    def test_production_configuration(self):
        """Test typical production configuration"""
        settings = override_settings(
            environment="production",
            log_level="WARNING",
            enable_caching=True,
            max_memory_mb=8192,
            orcaflex_workers=50
        )

        assert settings.environment == "production"
        assert settings.log_level == "WARNING"
        assert settings.enable_caching is True
        assert settings.max_memory_mb == 8192
        assert settings.orcaflex_workers == 50

    def test_testing_configuration(self, tmp_path):
        """Test typical testing configuration"""
        test_data = tmp_path / "test_data"
        test_output = tmp_path / "test_output"

        settings = override_settings(
            environment="testing",
            data_dir=test_data,
            output_dir=test_output,
            orcaflex_workers=1,
            enable_caching=False
        )

        assert settings.environment == "testing"
        assert settings.data_dir == test_data.resolve()
        assert settings.output_dir == test_output.resolve()
        assert settings.orcaflex_workers == 1
        assert settings.enable_caching is False

        # Test directories should exist
        assert test_data.exists()
        assert test_output.exists()


class TestConfigurationUsage:
    """Test how configuration is used in modules"""

    def setup_method(self):
        """Reset settings before each test"""
        reset_settings()

    def test_module_can_access_settings(self):
        """Test that modules can easily access settings"""
        from digitalmodel.config import get_settings

        settings = get_settings()

        # Simulate module using settings
        safety_factor = settings.safety_factor
        data_dir = settings.data_dir

        assert isinstance(safety_factor, float)
        assert isinstance(data_dir, Path)

    def test_settings_can_be_overridden_for_testing(self):
        """Test that settings can be easily overridden for testing"""
        from digitalmodel.config import override_settings

        # Override for test
        test_settings = override_settings(
            safety_factor=3.0,
            debug_mode=True
        )

        assert test_settings.safety_factor == 3.0
        assert test_settings.debug_mode is True

        # Clean up
        reset_settings()

        # Back to defaults
        default_settings = get_settings()
        assert default_settings.safety_factor == 1.5
        assert default_settings.debug_mode is False
