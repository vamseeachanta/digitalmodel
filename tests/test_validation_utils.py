#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
ABOUTME: Test script for modular input validation utility functions
ABOUTME: Validates core utilities like YAML loading, parameter extraction, and comparisons
"""

import sys
import io
from pathlib import Path
import tempfile
import yaml

# Force UTF-8 encoding for stdout
if sys.platform == 'win32':
    sys.stdout = io.TextIOWrapper(sys.stdout.buffer, encoding='utf-8')

# Add src to path
sys.path.insert(0, str(Path(__file__).parent.parent / 'src'))

from digitalmodel.orcaflex.modular_input_validation.utils import (
    load_yaml_file,
    extract_includefiles,
    resolve_include_path,
    extract_parameter_from_yaml,
    is_within_range,
    is_within_tolerance,
    calculate_percentage_difference,
    format_file_size,
    sanitize_filename,
    merge_dicts_deep
)


def test_load_yaml_file():
    """Test YAML file loading."""
    print("\n" + "="*60)
    print("TEST: load_yaml_file()")
    print("="*60)

    # Create temporary YAML file
    with tempfile.NamedTemporaryFile(mode='w', suffix='.yml', delete=False) as f:
        yaml_content = {
            'test_key': 'test_value',
            'nested': {
                'key': 123
            }
        }
        yaml.dump(yaml_content, f)
        temp_file = Path(f.name)

    try:
        # Test valid file
        success, content, error = load_yaml_file(temp_file)
        assert success, f"Should load valid YAML: {error}"
        assert content['test_key'] == 'test_value', "Should parse YAML correctly"
        assert content['nested']['key'] == 123, "Should handle nested structures"
        print("✓ Valid YAML file loaded successfully")

        # Test non-existent file
        success, content, error = load_yaml_file(Path('/nonexistent/file.yml'))
        assert not success, "Should fail for non-existent file"
        assert 'not found' in error.lower(), f"Should report file not found: {error}"
        print("✓ Non-existent file handled correctly")

        # Test invalid YAML
        with tempfile.NamedTemporaryFile(mode='w', suffix='.yml', delete=False) as f:
            f.write("invalid: yaml: content:\n  bad indentation")
            invalid_file = Path(f.name)

        success, content, error = load_yaml_file(invalid_file)
        assert not success, "Should fail for invalid YAML"
        assert 'YAML syntax error' in error, f"Should report YAML error: {error}"
        print("✓ Invalid YAML handled correctly")
        invalid_file.unlink()

    finally:
        temp_file.unlink()

    print("\n✅ All load_yaml_file() tests passed!")


def test_extract_includefiles():
    """Test includefile extraction."""
    print("\n" + "="*60)
    print("TEST: extract_includefiles()")
    print("="*60)

    # Test simple includefile
    content1 = {
        'includefile': 'simple.yml'
    }
    includes = extract_includefiles(content1)
    assert includes == ['simple.yml'], f"Should extract simple includefile: {includes}"
    print("✓ Simple includefile extracted")

    # Test nested includefiles
    content2 = {
        'Environment': {
            'includefile': 'env.yml'
        },
        'Lines': [
            {'includefile': 'line1.yml'},
            {'includefile': 'line2.yml'}
        ]
    }
    includes = extract_includefiles(content2)
    assert len(includes) == 3, f"Should find 3 includefiles: {includes}"
    assert 'env.yml' in includes, "Should find env.yml"
    assert 'line1.yml' in includes, "Should find line1.yml"
    assert 'line2.yml' in includes, "Should find line2.yml"
    print("✓ Nested includefiles extracted")

    # Test no includefiles
    content3 = {
        'key1': 'value1',
        'nested': {'key2': 'value2'}
    }
    includes = extract_includefiles(content3)
    assert includes == [], f"Should return empty list: {includes}"
    print("✓ Empty case handled")

    print("\n✅ All extract_includefiles() tests passed!")


def test_resolve_include_path():
    """Test include path resolution."""
    print("\n" + "="*60)
    print("TEST: resolve_include_path()")
    print("="*60)

    # Create actual temp directory to test real path resolution
    with tempfile.TemporaryDirectory() as tmpdir:
        tmpdir_path = Path(tmpdir)

        # Create subdirectories
        models_dir = tmpdir_path / 'models'
        env_dir = tmpdir_path / 'env'
        models_dir.mkdir()
        env_dir.mkdir()

        # Create base file
        base_file = models_dir / 'main.yml'
        base_file.touch()

        # Test relative path (go up and into env)
        include_file = '../env/environment.yml'
        resolved = resolve_include_path(base_file, include_file)
        expected = (env_dir / 'environment.yml').resolve()
        assert resolved == expected, f"Expected {expected}, got {resolved}"
        print(f"✓ Relative path resolved: {include_file} -> {resolved.name}")

        # Test same directory
        include_file2 = 'lines.yml'
        resolved2 = resolve_include_path(base_file, include_file2)
        expected2 = (models_dir / 'lines.yml').resolve()
        assert resolved2 == expected2, f"Expected {expected2}, got {resolved2}"
        print(f"✓ Same directory resolved: {include_file2} -> {resolved2.name}")

        # Test absolute path
        abs_path = tmpdir_path / 'absolute.yml'
        abs_path.touch()
        resolved3 = resolve_include_path(base_file, str(abs_path))
        # Compare using samefile to handle Windows short path names
        assert abs_path.samefile(resolved3), f"Expected {abs_path}, got {resolved3}"
        print(f"✓ Absolute path preserved: {resolved3.name}")

    print("\n✅ All resolve_include_path() tests passed!")


def test_extract_parameter_from_yaml():
    """Test parameter extraction from nested YAML."""
    print("\n" + "="*60)
    print("TEST: extract_parameter_from_yaml()")
    print("="*60)

    content = {
        'Environment': {
            'WaveHs': 2.5,
            'WaveTp': 8.0,
            'Current': {
                'Speed': 1.2,
                'Direction': 180
            }
        },
        'General': {
            'WaterDepth': 150.0
        }
    }

    # Test simple nested path
    value = extract_parameter_from_yaml(content, 'Environment.WaveHs')
    assert value == 2.5, f"Expected 2.5, got {value}"
    print(f"✓ Extracted Environment.WaveHs = {value}")

    # Test deeper nesting
    value2 = extract_parameter_from_yaml(content, 'Environment.Current.Speed')
    assert value2 == 1.2, f"Expected 1.2, got {value2}"
    print(f"✓ Extracted Environment.Current.Speed = {value2}")

    # Test top-level key
    value3 = extract_parameter_from_yaml(content, 'General.WaterDepth')
    assert value3 == 150.0, f"Expected 150.0, got {value3}"
    print(f"✓ Extracted General.WaterDepth = {value3}")

    # Test non-existent path with default
    value4 = extract_parameter_from_yaml(content, 'NonExistent.Path', default='DEFAULT')
    assert value4 == 'DEFAULT', f"Expected 'DEFAULT', got {value4}"
    print(f"✓ Non-existent path returned default: {value4}")

    # Test non-existent path without default
    value5 = extract_parameter_from_yaml(content, 'NonExistent.Path')
    assert value5 is None, f"Expected None, got {value5}"
    print(f"✓ Non-existent path returned None")

    print("\n✅ All extract_parameter_from_yaml() tests passed!")


def test_is_within_range():
    """Test range checking."""
    print("\n" + "="*60)
    print("TEST: is_within_range()")
    print("="*60)

    # Test within range
    assert is_within_range(5.0, 0.0, 10.0), "5 should be within [0, 10]"
    print("✓ Value within range detected")

    # Test at boundaries
    assert is_within_range(0.0, 0.0, 10.0), "0 should be within [0, 10]"
    assert is_within_range(10.0, 0.0, 10.0), "10 should be within [0, 10]"
    print("✓ Boundary values handled correctly")

    # Test below range
    assert not is_within_range(-1.0, 0.0, 10.0), "-1 should be outside [0, 10]"
    print("✓ Below range detected")

    # Test above range
    assert not is_within_range(11.0, 0.0, 10.0), "11 should be outside [0, 10]"
    print("✓ Above range detected")

    # Test no minimum
    assert is_within_range(-100.0, None, 10.0), "-100 should be valid with no min"
    print("✓ No minimum handled")

    # Test no maximum
    assert is_within_range(1000.0, 0.0, None), "1000 should be valid with no max"
    print("✓ No maximum handled")

    # Test no limits
    assert is_within_range(999.0, None, None), "Any value should be valid with no limits"
    print("✓ No limits handled")

    print("\n✅ All is_within_range() tests passed!")


def test_calculate_percentage_difference():
    """Test percentage difference calculation."""
    print("\n" + "="*60)
    print("TEST: calculate_percentage_difference()")
    print("="*60)

    # Test positive difference
    diff = calculate_percentage_difference(110.0, 100.0)
    assert abs(diff - 10.0) < 0.01, f"Expected 10%, got {diff}%"
    print(f"✓ Positive difference: 110 vs 100 = {diff}%")

    # Test negative difference
    diff2 = calculate_percentage_difference(90.0, 100.0)
    assert abs(diff2 - (-10.0)) < 0.01, f"Expected -10%, got {diff2}%"
    print(f"✓ Negative difference: 90 vs 100 = {diff2}%")

    # Test zero difference
    diff3 = calculate_percentage_difference(100.0, 100.0)
    assert abs(diff3) < 0.01, f"Expected 0%, got {diff3}%"
    print(f"✓ Zero difference: 100 vs 100 = {diff3}%")

    # Test with zero expected (edge case)
    diff4 = calculate_percentage_difference(5.0, 0.0)
    assert diff4 == 100.0, f"Expected 100%, got {diff4}%"
    print(f"✓ Zero expected handled: 5 vs 0 = {diff4}%")

    # Test both zero
    diff5 = calculate_percentage_difference(0.0, 0.0)
    assert diff5 == 0.0, f"Expected 0%, got {diff5}%"
    print(f"✓ Both zero handled: 0 vs 0 = {diff5}%")

    # Test negative expected value
    # -110 is MORE negative than -100, so the difference is -10%
    diff6 = calculate_percentage_difference(-110.0, -100.0)
    assert abs(diff6 - (-10.0)) < 0.01, f"Expected -10%, got {diff6}%"
    print(f"✓ Negative expected: -110 vs -100 = {diff6}%")

    print("\n✅ All calculate_percentage_difference() tests passed!")


def test_is_within_tolerance():
    """Test tolerance checking."""
    print("\n" + "="*60)
    print("TEST: is_within_tolerance()")
    print("="*60)

    # Test within tolerance
    assert is_within_tolerance(105.0, 100.0, 10.0), "105 should be within 10% of 100"
    print("✓ Within tolerance detected")

    # Test at tolerance boundary
    assert is_within_tolerance(110.0, 100.0, 10.0), "110 should be within 10% of 100"
    assert is_within_tolerance(90.0, 100.0, 10.0), "90 should be within 10% of 100"
    print("✓ Tolerance boundaries handled")

    # Test outside tolerance
    assert not is_within_tolerance(111.0, 100.0, 10.0), "111 should be outside 10% of 100"
    assert not is_within_tolerance(89.0, 100.0, 10.0), "89 should be outside 10% of 100"
    print("✓ Outside tolerance detected")

    # Test with negative values
    assert is_within_tolerance(-105.0, -100.0, 10.0), "-105 should be within 10% of -100"
    print("✓ Negative values handled")

    # Test tight tolerance
    assert is_within_tolerance(100.5, 100.0, 1.0), "100.5 should be within 1% of 100"
    assert not is_within_tolerance(102.0, 100.0, 1.0), "102 should be outside 1% of 100"
    print("✓ Tight tolerance works")

    print("\n✅ All is_within_tolerance() tests passed!")


def test_format_file_size():
    """Test file size formatting."""
    print("\n" + "="*60)
    print("TEST: format_file_size()")
    print("="*60)

    assert format_file_size(500) == "500.0 B", "Should format bytes"
    print(f"✓ Bytes: {format_file_size(500)}")

    assert format_file_size(1536) == "1.5 KB", "Should format kilobytes"
    print(f"✓ Kilobytes: {format_file_size(1536)}")

    assert format_file_size(1572864) == "1.5 MB", "Should format megabytes"
    print(f"✓ Megabytes: {format_file_size(1572864)}")

    assert format_file_size(1610612736) == "1.5 GB", "Should format gigabytes"
    print(f"✓ Gigabytes: {format_file_size(1610612736)}")

    print("\n✅ All format_file_size() tests passed!")


def test_sanitize_filename():
    """Test filename sanitization."""
    print("\n" + "="*60)
    print("TEST: sanitize_filename()")
    print("="*60)

    # Test invalid characters
    result = sanitize_filename('file<name>:with|invalid*chars.txt')
    assert '<' not in result, "Should remove < character"
    assert '>' not in result, "Should remove > character"
    assert ':' not in result, "Should remove : character"
    assert '|' not in result, "Should remove | character"
    assert '*' not in result, "Should remove * character"
    print(f"✓ Invalid characters sanitized: '{result}'")

    # Test valid filename unchanged
    result2 = sanitize_filename('valid_filename.txt')
    assert result2 == 'valid_filename.txt', "Valid filename should be unchanged"
    print(f"✓ Valid filename unchanged: '{result2}'")

    print("\n✅ All sanitize_filename() tests passed!")


def test_merge_dicts_deep():
    """Test deep dictionary merging."""
    print("\n" + "="*60)
    print("TEST: merge_dicts_deep()")
    print("="*60)

    base = {
        'a': 1,
        'b': {
            'c': 2,
            'd': 3
        },
        'e': [1, 2, 3]
    }

    override = {
        'b': {
            'd': 30,
            'f': 4
        },
        'g': 5
    }

    result = merge_dicts_deep(base, override)

    assert result['a'] == 1, "Should preserve base values"
    assert result['b']['c'] == 2, "Should preserve nested base values"
    assert result['b']['d'] == 30, "Should override nested values"
    assert result['b']['f'] == 4, "Should add new nested values"
    assert result['g'] == 5, "Should add new top-level values"
    assert result['e'] == [1, 2, 3], "Should preserve lists"

    print(f"✓ Deep merge result: {result}")
    print("\n✅ All merge_dicts_deep() tests passed!")


def main():
    """Run all tests."""
    print("\n" + "="*60)
    print("VALIDATION UTILS TEST SUITE")
    print("="*60)

    try:
        test_load_yaml_file()
        test_extract_includefiles()
        test_resolve_include_path()
        test_extract_parameter_from_yaml()
        test_is_within_range()
        test_calculate_percentage_difference()
        test_is_within_tolerance()
        test_format_file_size()
        test_sanitize_filename()
        test_merge_dicts_deep()

        print("\n" + "="*60)
        print("✅ ALL TESTS PASSED!")
        print("="*60)
        print("\nAll utility functions are working correctly:")
        print("  ✓ load_yaml_file() - YAML loading and error handling")
        print("  ✓ extract_includefiles() - Recursive includefile extraction")
        print("  ✓ resolve_include_path() - Path resolution")
        print("  ✓ extract_parameter_from_yaml() - Nested parameter extraction")
        print("  ✓ is_within_range() - Range validation")
        print("  ✓ calculate_percentage_difference() - Percentage calculations")
        print("  ✓ is_within_tolerance() - Tolerance checking")
        print("  ✓ format_file_size() - File size formatting")
        print("  ✓ sanitize_filename() - Filename sanitization")
        print("  ✓ merge_dicts_deep() - Deep dictionary merging")

        return 0

    except AssertionError as e:
        print(f"\n❌ TEST FAILED: {e}")
        return 1
    except Exception as e:
        print(f"\n❌ UNEXPECTED ERROR: {e}")
        import traceback
        traceback.print_exc()
        return 1


if __name__ == '__main__':
    sys.exit(main())
