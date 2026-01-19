#!/usr/bin/env python
"""Simple validation runner for CALM Buoy YAML files."""

import sys
from pathlib import Path

# Add src to path
sys.path.insert(0, str(Path(__file__).parent / "src"))

from digitalmodel.modules.orcaflex.modular_input_validation import (
    ModularInputValidator,
    ValidationConfig
)

def main():
    """Run CALM buoy validation."""

    # Define file paths
    files = [
        Path("specs/modules/orcaflex/modular-input-file/output/calm_buoy_base.yml"),
        Path("specs/modules/orcaflex/modular-input-file/output/discretised_calm_buoy_base.yml")
    ]

    # Create configuration
    config = ValidationConfig(
        tolerance_percent=10.0,
        enable_orcaflex=False,  # Skip OrcaFlex (not installed)
        calm_buoy_data_dir=Path("data"),
        reports_dir=Path("reports/validation/calm_buoy"),
        skip_levels=[2],  # Skip Level 2 (OrcaFlex API)
        generate_reports=True,
        report_formats=['console', 'csv', 'markdown', 'html'],
        enable_color=True
    )

    # Create validator
    validator = ModularInputValidator(config)

    # Validate files
    print("=" * 80)
    print("Starting CALM Buoy Validation")
    print("=" * 80)
    print(f"Files to validate: {len(files)}")
    for f in files:
        print(f"  - {f}")
    print()

    # Run validation
    results = validator.validate_files(files)

    # Generate reports
    validator.generate_reports(results)

    # Print summary
    summary = validator.get_validation_summary(results)

    print()
    print("=" * 80)
    print("VALIDATION SUMMARY")
    print("=" * 80)
    print(f"Total files: {summary['total_files']}")
    print(f"Passed: {summary['passed']}")
    print(f"Warnings: {summary['warnings']}")
    print(f"Failed: {summary['failed']}")
    print(f"Pass rate: {summary['pass_rate']:.1f}%")
    print(f"Total issues: {summary['total_issues']}")
    print(f"Critical issues: {summary['critical_issues']}")
    print(f"Warning issues: {summary['warning_issues']}")
    print("=" * 80)

    # Return exit code
    return 0 if summary['failed'] == 0 else 1

if __name__ == '__main__':
    sys.exit(main())
