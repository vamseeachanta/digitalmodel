"""
Validate CALM Buoy YAML files.

Usage:
    python scripts/validate_calm_buoy_files.py
"""

from pathlib import Path
from digitalmodel.orcaflex.modular_input_validation import (
    ModularInputValidator,
    ValidationConfig
)


def main():
    """Validate CALM buoy YAML files."""

    # Define file paths
    files = [
        Path("specs/modules/orcaflex/modular-input-file/output/calm_buoy_base.yml"),
        Path("specs/modules/orcaflex/modular-input-file/output/discretised_calm_buoy_base.yml")
    ]

    # Create configuration
    config = ValidationConfig(
        tolerance_percent=10.0,
        enable_orcaflex=True,  # Set to False if OrcaFlex not installed
        calm_buoy_data_dir=Path("data"),
        reports_dir=Path("reports/validation/calm_buoy"),
        skip_levels=[],  # Run all levels; use [2] to skip OrcaFlex
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
    print()
    print(f"Total issues: {summary['total_issues']}")
    print(f"Critical issues: {summary['critical_issues']}")
    print(f"Warning issues: {summary['warning_issues']}")
    print("=" * 80)

    # Print individual results
    print()
    print("DETAILED RESULTS:")
    print()
    for result in results:
        print(f"File: {result.file_path.name}")
        print(f"  Status: {result.overall_status.value}")
        print(f"  Validation time: {result.validation_time_seconds:.2f}s")

        if result.level_1:
            print(f"  Level 1: {result.level_1.status.value}")
            print(f"    - YAML valid: {result.level_1.yaml_valid}")
            print(f"    - Includes resolved: {result.level_1.includes_resolved}")
            print(f"    - Total modules: {result.level_1.total_modules}")

        if result.level_2:
            print(f"  Level 2: {result.level_2.status.value}")
            print(f"    - OrcaFlex available: {result.level_2.orcaflex_available.value}")
            print(f"    - File loadable: {result.level_2.orcaflex_loadable}")
            if result.level_2.orcaflex_version:
                print(f"    - OrcaFlex version: {result.level_2.orcaflex_version}")

        if result.level_3:
            print(f"  Level 3: {result.level_3.status.value}")
            print(f"    - Parameters validated: {result.level_3.parameters_validated}")
            print(f"    - Critical issues: {result.level_3.critical_issues}")
            print(f"    - Warnings: {result.level_3.warnings}")

        print()

    # Return exit code
    return 0 if summary['failed'] == 0 else 1


if __name__ == '__main__':
    import sys
    sys.exit(main())
