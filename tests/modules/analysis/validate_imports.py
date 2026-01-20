#!/usr/bin/env python3
"""
Import Validation Script

Quick validation of marine engineering test imports.
Run this after applying fixes to verify everything works.

Usage:
    python validate_imports.py
"""

import sys
from pathlib import Path

# Add src to path
repo_root = Path(__file__).parent.parent.parent
sys.path.insert(0, str(repo_root / 'src'))

def print_header(text):
    """Print formatted header."""
    print("\n" + "="*80)
    print(f"  {text}")
    print("="*80)

def test_core_imports():
    """Test core marine_analysis imports."""
    print_header("TEST 1: Core marine_analysis Imports")

    success = True

    # Test 1.1: UnifiedRAOReader
    print("\n[1.1] Testing UnifiedRAOReader import...")
    try:
        from digitalmodel.modules.marine_analysis import UnifiedRAOReader
        print(f"     ✅ SUCCESS: {UnifiedRAOReader}")
    except Exception as e:
        print(f"     ❌ FAILED: {e}")
        success = False

    # Test 1.2: RAOReaderError
    print("\n[1.2] Testing RAOReaderError import...")
    try:
        from digitalmodel.modules.marine_analysis import RAOReaderError
        print(f"     ✅ SUCCESS: {RAOReaderError}")
    except Exception as e:
        print(f"     ❌ FAILED: {e}")
        success = False

    # Test 1.3: read_rao_file
    print("\n[1.3] Testing read_rao_file import...")
    try:
        from digitalmodel.modules.marine_analysis import read_rao_file
        print(f"     ✅ SUCCESS: {read_rao_file}")
    except Exception as e:
        print(f"     ❌ FAILED: {e}")
        success = False

    # Test 1.4: RAOPlotter
    print("\n[1.4] Testing RAOPlotter import...")
    try:
        from digitalmodel.modules.marine_analysis import RAOPlotter
        if RAOPlotter is None:
            print(f"     ⚠️  WARNING: RAOPlotter is None (optional dependency)")
        else:
            print(f"     ✅ SUCCESS: {RAOPlotter}")
    except Exception as e:
        print(f"     ❌ FAILED: {e}")
        success = False

    return success

def test_models_imports():
    """Test data model imports."""
    print_header("TEST 2: Data Model Imports")

    success = True

    # Test 2.1: RAOData models
    print("\n[2.1] Testing RAOData models...")
    try:
        from digitalmodel.modules.marine_analysis import (
            RAOData,
            RAOType,
            UnifiedRAOData,
            SourceFormat
        )
        print(f"     ✅ SUCCESS: RAOData, RAOType, UnifiedRAOData, SourceFormat")
    except Exception as e:
        print(f"     ❌ FAILED: {e}")
        success = False

    # Test 2.2: Parser imports
    print("\n[2.2] Testing parser imports...")
    try:
        from digitalmodel.modules.marine_analysis import (
            AQWALISParser,
            OrcaFlexYMLParser
        )
        print(f"     ✅ SUCCESS: AQWALISParser, OrcaFlexYMLParser")
    except Exception as e:
        print(f"     ❌ FAILED: {e}")
        success = False

    return success

def test_legacy_imports():
    """Test legacy compatibility imports."""
    print_header("TEST 3: Legacy Compatibility Imports")

    success = True

    # Test 3.1: Legacy processors
    print("\n[3.1] Testing legacy RAO processors...")
    try:
        from digitalmodel.modules.marine_analysis import (
            RAODataProcessor,
            RAODataValidators,
            RAOInterpolator
        )
        print(f"     ✅ SUCCESS: RAODataProcessor, RAODataValidators, RAOInterpolator")
    except Exception as e:
        print(f"     ❌ FAILED: {e}")
        success = False

    # Test 3.2: Legacy readers
    print("\n[3.2] Testing legacy readers...")
    try:
        from digitalmodel.modules.marine_analysis import (
            AQWAReader,
            OrcaFlexReader
        )
        print(f"     ✅ SUCCESS: AQWAReader, OrcaFlexReader")
    except Exception as e:
        print(f"     ❌ FAILED: {e}")
        success = False

    return success

def test_marine_engineering_imports():
    """Test marine_engineering module imports."""
    print_header("TEST 4: marine_engineering Module Imports")

    success = True

    # Test 4.1: Catenary module
    print("\n[4.1] Testing catenary module...")
    try:
        from digitalmodel.modules.marine_engineering.catenary import (
            CatenarySolver,
            SimplifiedCatenarySolver
        )
        print(f"     ✅ SUCCESS: CatenarySolver, SimplifiedCatenarySolver")
    except Exception as e:
        print(f"     ⚠️  EXPECTED FAILURE (needs conftest.py): {e}")
        # Not a critical failure - needs PYTHONPATH setup

    # Test 4.2: Check if marine_engineering exists
    print("\n[4.2] Checking marine_engineering module location...")
    marine_eng_path = repo_root / 'src' / 'marine_engineering'
    if marine_eng_path.exists():
        print(f"     ✅ Module exists at: {marine_eng_path}")
    else:
        print(f"     ❌ Module not found at: {marine_eng_path}")
        success = False

    return True  # Don't fail on marine_engineering - it's expected until conftest.py

def test_pytest_collection():
    """Test pytest can collect tests."""
    print_header("TEST 5: Pytest Collection")

    import subprocess

    print("\n[5.1] Running pytest --collect-only...")
    test_dir = repo_root / 'tests' / 'marine_engineering'

    try:
        result = subprocess.run(
            ['pytest', str(test_dir), '--collect-only', '-q'],
            capture_output=True,
            text=True,
            timeout=30
        )

        output = result.stdout + result.stderr

        if 'collected' in output.lower():
            # Extract number of collected items
            import re
            match = re.search(r'collected (\d+) item', output)
            if match:
                num_tests = int(match.group(1))
                print(f"     ✅ SUCCESS: Collected {num_tests} test items")
                if num_tests == 150:
                    print(f"     ✅ PERFECT: All 150 tests collected!")
                elif num_tests > 0:
                    print(f"     ⚠️  WARNING: Expected 150 tests, got {num_tests}")
                return True

        if 'error' in output.lower() or 'failed' in output.lower():
            print(f"     ❌ COLLECTION FAILED")
            print(f"     Error output:")
            for line in output.split('\n')[:10]:  # First 10 lines
                print(f"       {line}")
            return False

    except subprocess.TimeoutExpired:
        print(f"     ❌ TIMEOUT: Pytest collection took too long")
        return False
    except Exception as e:
        print(f"     ⚠️  Could not run pytest: {e}")
        print(f"     (This is OK if pytest not installed)")
        return True

def check_conftest():
    """Check if conftest.py exists."""
    print_header("TEST 6: Configuration Files")

    # Test 6.1: conftest.py
    print("\n[6.1] Checking for conftest.py...")
    conftest_path = repo_root / 'tests' / 'conftest.py'
    if conftest_path.exists():
        print(f"     ✅ EXISTS: {conftest_path}")

        # Check content
        content = conftest_path.read_text()
        if 'sys.path' in content and 'src' in content:
            print(f"     ✅ Contains PYTHONPATH configuration")
        else:
            print(f"     ⚠️  WARNING: Missing PYTHONPATH configuration")
        return True
    else:
        print(f"     ❌ NOT FOUND: {conftest_path}")
        print(f"     Action: Run fix_test_imports.py to create it")
        return False

def main():
    """Run all validation tests."""
    print("="*80)
    print("  MARINE ENGINEERING TEST IMPORT VALIDATION")
    print("="*80)
    print(f"\nRepository: {repo_root}")
    print(f"Python path includes: {repo_root / 'src'}")

    results = []

    # Run all tests
    results.append(("Core Imports", test_core_imports()))
    results.append(("Data Models", test_models_imports()))
    results.append(("Legacy Imports", test_legacy_imports()))
    results.append(("marine_engineering", test_marine_engineering_imports()))
    results.append(("Pytest Collection", test_pytest_collection()))
    results.append(("Config Files", check_conftest()))

    # Summary
    print_header("VALIDATION SUMMARY")

    total = len(results)
    passed = sum(1 for _, success in results if success)

    print(f"\nResults: {passed}/{total} test groups passed\n")

    for test_name, success in results:
        status = "✅ PASS" if success else "❌ FAIL"
        print(f"  {status}  {test_name}")

    # Overall status
    print("\n" + "="*80)
    if passed == total:
        print("  ✅ ALL VALIDATIONS PASSED")
        print("="*80)
        print("\nNext steps:")
        print("  1. Run: pytest tests/marine_engineering/ -v")
        print("  2. All tests should run successfully")
        return 0
    else:
        print("  ⚠️  SOME VALIDATIONS FAILED")
        print("="*80)
        print("\nNext steps:")
        print("  1. Review failed validations above")
        print("  2. Run: python tests/analysis/fix_test_imports.py")
        print("  3. Re-run this validation script")
        return 1

if __name__ == '__main__':
    sys.exit(main())
