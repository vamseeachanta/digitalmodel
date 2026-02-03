#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Minimal test for Level 1 YAML validation
Tests YAML syntax and includefile resolution
"""

import sys
import io
from pathlib import Path

# Set UTF-8 encoding for Windows console
sys.stdout = io.TextIOWrapper(sys.stdout.buffer, encoding='utf-8')
sys.stderr = io.TextIOWrapper(sys.stderr.buffer, encoding='utf-8')

# Add src directory to path
repo_root = Path("D:/workspace-hub/digitalmodel")
sys.path.insert(0, str(repo_root / "src"))

from digitalmodel.solvers.orcaflex.modular_input_validation.level_1_yaml import Level1YAMLValidator

def test_level1_validation():
    """Test Level 1 YAML validation on CALM Buoy base file"""

    print("=" * 80)
    print("LEVEL 1 YAML VALIDATION TEST")
    print("=" * 80)

    # Initialize validator
    validator = Level1YAMLValidator()

    # Test file path
    test_file = repo_root / "specs/modules/orcaflex/modular-input-file/output/calm_buoy_base.yml"

    print(f"\nTest File: {test_file}")
    print(f"File Exists: {test_file.exists()}")

    if not test_file.exists():
        print("\n❌ FAIL: Test file does not exist")
        return False

    print("\n" + "-" * 80)
    print("Running Level 1 Validation...")
    print("-" * 80)

    # Run validation
    result = validator.validate(test_file)

    # Print results
    print(f"\nValidation Status: {result.status.value}")
    print(f"YAML Valid: {result.yaml_valid}")
    print(f"File Exists: {result.file_exists}")
    print(f"Includes Resolved: {result.includes_resolved}")
    print(f"Total Modules: {result.total_modules}")
    print(f"Sections Found: {', '.join(result.sections) if result.sections else 'None'}")

    # Print syntax errors if any
    if result.syntax_errors:
        print("\nSyntax Errors:")
        for error in result.syntax_errors:
            print(f"  ❌ {error}")

    # Print missing includes if any
    if result.missing_includes:
        print("\nMissing Includes:")
        for missing in result.missing_includes:
            print(f"  ⚠️  {missing}")

    print("\n" + "=" * 80)

    # Specific verifications
    print("\nVERIFICATION CHECKLIST:")
    print("-" * 80)

    checks_passed = 0
    checks_total = 3

    # Check 1: YAML syntax
    print(f"1. YAML Syntax Check: {'✅ PASS' if result.yaml_valid else '❌ FAIL'}")
    if result.yaml_valid:
        checks_passed += 1

    # Check 2: Includefile resolution
    print(f"2. Includefile Resolution: {'✅ PASS' if result.includes_resolved else '❌ FAIL'}")
    if result.includes_resolved:
        checks_passed += 1

    # Check 3: No syntax errors
    no_errors = len(result.syntax_errors) == 0
    print(f"3. No Syntax Errors: {'✅ PASS' if no_errors else '❌ FAIL'}")
    if no_errors:
        checks_passed += 1

    print("\n" + "=" * 80)
    print(f"FINAL RESULT: {checks_passed}/{checks_total} checks passed")
    print(f"Overall Status: {result.status.value}")

    if checks_passed == checks_total:
        print("✅ OVERALL: PASS")
        return True
    else:
        print("❌ OVERALL: FAIL")
        return False

if __name__ == "__main__":
    try:
        success = test_level1_validation()
        sys.exit(0 if success else 1)
    except Exception as e:
        print(f"\n❌ TEST FAILED WITH EXCEPTION: {e}")
        import traceback
        traceback.print_exc()
        sys.exit(1)
