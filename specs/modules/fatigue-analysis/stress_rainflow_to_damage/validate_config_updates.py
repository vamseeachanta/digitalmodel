#!/usr/bin/env python3
"""
Validate Configuration Files Against ABS'20 Database
Checks that all config files have correct ABS-E in-air parameters
"""

import yaml
from pathlib import Path
import sys

# ABS'20 Database reference values (Row 201)
ABS20_REFERENCE = {
    'curve_type': 'ABS-E',
    'environment': 'in-air',
    'log_a': 12.017,
    'm': 3.0,
    'fatigue_limit_cycles': 1.0e7,
    'fatigue_limit_stress': 47.0,
    'log_c': 15.362,  # Segment 2
    'r': 5.0          # Segment 2
}

TOLERANCE = 0.001  # Tolerance for floating point comparison

def validate_config_file(config_path: Path) -> tuple[bool, list]:
    """Validate a single config file against ABS'20 reference"""
    errors = []

    try:
        with open(config_path, 'r') as f:
            config = yaml.safe_load(f)

        # Check S-N curve section
        if 'sn_curve' not in config:
            errors.append("Missing 'sn_curve' section")
            return False, errors

        sn = config['sn_curve']
        params = sn.get('parameters', {})

        # Validate curve type
        if sn.get('curve_type') != ABS20_REFERENCE['curve_type']:
            errors.append(f"curve_type: expected '{ABS20_REFERENCE['curve_type']}', got '{sn.get('curve_type')}'")

        # Validate environment
        if sn.get('environment') != ABS20_REFERENCE['environment']:
            errors.append(f"environment: expected '{ABS20_REFERENCE['environment']}', got '{sn.get('environment')}'")

        # Validate Segment 1 parameters
        for key in ['log_a', 'm', 'fatigue_limit_cycles', 'fatigue_limit_stress']:
            value = params.get(key)
            ref_value = ABS20_REFERENCE[key]

            if value is None:
                errors.append(f"Missing parameter: {key}")
            elif isinstance(ref_value, float):
                if abs(float(value) - ref_value) > TOLERANCE:
                    errors.append(f"{key}: expected {ref_value}, got {value}")
            else:
                if value != ref_value:
                    errors.append(f"{key}: expected {ref_value}, got {value}")

        # Validate Segment 2 parameters
        two_seg = params.get('two_segment', {})

        if not two_seg.get('enabled'):
            errors.append("two_segment.enabled should be true")

        for key in ['log_c', 'r']:
            value = two_seg.get(key)
            ref_value = ABS20_REFERENCE[key]

            if value is None:
                errors.append(f"Missing parameter: two_segment.{key}")
            elif abs(float(value) - ref_value) > TOLERANCE:
                errors.append(f"two_segment.{key}: expected {ref_value}, got {value}")

        return len(errors) == 0, errors

    except Exception as e:
        errors.append(f"Error reading file: {str(e)}")
        return False, errors

def main():
    """Validate all configuration files"""
    config_dir = Path('input')
    config_files = list(config_dir.glob('damage_analysis_config*.yml'))

    print("="*80)
    print("CONFIGURATION FILES VALIDATION AGAINST ABS'20 DATABASE")
    print("="*80)
    print(f"\nReference: ABS'20 Guide for Fatigue Assessment (Row 201)")
    print(f"Curve: {ABS20_REFERENCE['curve_type']} - {ABS20_REFERENCE['environment']}")
    print(f"\nExpected Parameters:")
    print(f"  Segment 1: log_a={ABS20_REFERENCE['log_a']}, m={ABS20_REFERENCE['m']}")
    print(f"             fatigue_limit={ABS20_REFERENCE['fatigue_limit_stress']} MPa at {ABS20_REFERENCE['fatigue_limit_cycles']:.0e} cycles")
    print(f"  Segment 2: log_c={ABS20_REFERENCE['log_c']}, r={ABS20_REFERENCE['r']}")
    print(f"\nValidating {len(config_files)} configuration files...")
    print("="*80)

    all_valid = True
    results = {}

    for config_file in sorted(config_files):
        print(f"\n[FILE] {config_file.name}")
        print("-" * 80)

        valid, errors = validate_config_file(config_file)
        results[config_file.name] = (valid, errors)

        if valid:
            print("[PASS] VALID - All parameters match ABS'20 database")
        else:
            print("[FAIL] INVALID - Found errors:")
            for error in errors:
                print(f"   - {error}")
            all_valid = False

    print("\n" + "="*80)
    print("SUMMARY")
    print("="*80)

    valid_count = sum(1 for v, _ in results.values() if v)
    invalid_count = len(results) - valid_count

    print(f"Total files: {len(results)}")
    print(f"Valid: {valid_count}")
    print(f"Invalid: {invalid_count}")

    if all_valid:
        print("\n[SUCCESS] ALL CONFIGURATION FILES ARE VALID")
        return 0
    else:
        print("\n[ERROR] SOME CONFIGURATION FILES HAVE ERRORS")
        return 1

if __name__ == "__main__":
    sys.exit(main())
