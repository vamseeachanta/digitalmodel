#!/usr/bin/env python3
"""
ABOUTME: Manual test script for configuration system
ABOUTME: Verifies configuration loading, validation, and environment variable support
"""

import sys
from pathlib import Path

# Add src to path for testing
sys.path.insert(0, str(Path(__file__).parent.parent / 'src'))

from digitalmodel.config import GlobalSettings, get_settings, override_settings, reset_settings


def test_basic_configuration():
    """Test basic configuration loading"""
    print("=" * 70)
    print("TEST 1: Basic Configuration Loading")
    print("=" * 70)

    settings = GlobalSettings()

    assert settings.safety_factor == 1.5, "Default safety factor incorrect"
    assert settings.orcaflex_workers == 30, "Default workers incorrect"
    assert settings.report_format == "html", "Default report format incorrect"
    assert settings.environment == "production", "Default environment incorrect"

    print("✓ All default settings correct")
    print(f"  - Safety factor: {settings.safety_factor}")
    print(f"  - OrcaFlex workers: {settings.orcaflex_workers}")
    print(f"  - Report format: {settings.report_format}")
    print(f"  - Environment: {settings.environment}")
    print()


def test_custom_configuration():
    """Test custom configuration values"""
    print("=" * 70)
    print("TEST 2: Custom Configuration Values")
    print("=" * 70)

    settings = GlobalSettings(
        safety_factor=2.5,
        orcaflex_workers=50,
        log_level="DEBUG",
        environment="development",
        debug_mode=True
    )

    assert settings.safety_factor == 2.5
    assert settings.orcaflex_workers == 50
    assert settings.log_level == "DEBUG"
    assert settings.environment == "development"
    assert settings.debug_mode is True

    print("✓ Custom settings applied correctly")
    print(f"  - Safety factor: {settings.safety_factor}")
    print(f"  - OrcaFlex workers: {settings.orcaflex_workers}")
    print(f"  - Log level: {settings.log_level}")
    print(f"  - Environment: {settings.environment}")
    print(f"  - Debug mode: {settings.debug_mode}")
    print()


def test_validation():
    """Test configuration validation"""
    print("=" * 70)
    print("TEST 3: Configuration Validation")
    print("=" * 70)

    # Test safety factor range
    try:
        GlobalSettings(safety_factor=0.5)  # Too low
        print("✗ FAILED: Should reject safety_factor < 1.0")
        sys.exit(1)
    except ValueError:
        print("✓ Correctly rejects safety_factor < 1.0")

    try:
        GlobalSettings(safety_factor=6.0)  # Too high
        print("✗ FAILED: Should reject safety_factor > 5.0")
        sys.exit(1)
    except ValueError:
        print("✓ Correctly rejects safety_factor > 5.0")

    # Test max_iterations range
    try:
        GlobalSettings(max_iterations=5)  # Too low
        print("✗ FAILED: Should reject max_iterations < 10")
        sys.exit(1)
    except ValueError:
        print("✓ Correctly rejects max_iterations < 10")

    # Test literal field validation
    try:
        GlobalSettings(environment="staging")  # Invalid value
        print("✗ FAILED: Should reject invalid environment value")
        sys.exit(1)
    except ValueError:
        print("✓ Correctly rejects invalid literal values")

    print()


def test_singleton_pattern():
    """Test singleton pattern of get_settings()"""
    print("=" * 70)
    print("TEST 4: Singleton Pattern")
    print("=" * 70)

    reset_settings()

    settings1 = get_settings()
    settings2 = get_settings()

    assert settings1 is settings2, "get_settings() should return same instance"

    print("✓ Singleton pattern working correctly")
    print(f"  - settings1 ID: {id(settings1)}")
    print(f"  - settings2 ID: {id(settings2)}")
    print(f"  - Same instance: {settings1 is settings2}")
    print()


def test_override_settings():
    """Test override_settings() function"""
    print("=" * 70)
    print("TEST 5: Override Settings")
    print("=" * 70)

    reset_settings()

    settings1 = get_settings()
    original_safety_factor = settings1.safety_factor

    settings2 = override_settings(safety_factor=3.0, debug_mode=True)

    assert settings2.safety_factor == 3.0
    assert settings2.debug_mode is True
    assert settings1 is not settings2, "override_settings should create new instance"

    print("✓ Override settings working correctly")
    print(f"  - Original safety factor: {original_safety_factor}")
    print(f"  - New safety factor: {settings2.safety_factor}")
    print(f"  - Debug mode: {settings2.debug_mode}")
    print()


def test_path_expansion():
    """Test path expansion and resolution"""
    print("=" * 70)
    print("TEST 6: Path Expansion")
    print("=" * 70)

    settings = GlobalSettings(data_dir="~/data")

    assert settings.data_dir.is_absolute(), "Paths should be expanded to absolute"
    assert "~" not in str(settings.data_dir), "Tilde should be expanded"

    print("✓ Path expansion working correctly")
    print(f"  - Input: ~/data")
    print(f"  - Expanded: {settings.data_dir}")
    print(f"  - Is absolute: {settings.data_dir.is_absolute()}")
    print()


def test_directory_creation():
    """Test that get_settings() creates directories"""
    print("=" * 70)
    print("TEST 7: Directory Creation")
    print("=" * 70)

    reset_settings()

    settings = get_settings()

    # Check if directories exist
    for dir_name, dir_path in [
        ("data_dir", settings.data_dir),
        ("output_dir", settings.output_dir),
        ("log_dir", settings.log_dir),
        ("cache_dir", settings.cache_dir)
    ]:
        if dir_path.exists():
            print(f"✓ {dir_name} exists: {dir_path}")
        else:
            print(f"✗ FAILED: {dir_name} not created: {dir_path}")

    print()


def main():
    """Run all tests"""
    print()
    print("╔════════════════════════════════════════════════════════════════════╗")
    print("║     digitalmodel - Configuration System Manual Tests              ║")
    print("╚════════════════════════════════════════════════════════════════════╝")
    print()

    try:
        test_basic_configuration()
        test_custom_configuration()
        test_validation()
        test_singleton_pattern()
        test_override_settings()
        test_path_expansion()
        test_directory_creation()

        print("=" * 70)
        print("✅ ALL TESTS PASSED")
        print("=" * 70)
        print()
        print("Configuration system is working correctly!")
        print()

    except AssertionError as e:
        print()
        print("=" * 70)
        print("❌ TEST FAILED")
        print("=" * 70)
        print(f"Error: {e}")
        print()
        sys.exit(1)

    except Exception as e:
        print()
        print("=" * 70)
        print("❌ UNEXPECTED ERROR")
        print("=" * 70)
        print(f"Error: {e}")
        import traceback
        traceback.print_exc()
        print()
        sys.exit(1)


if __name__ == '__main__':
    main()
